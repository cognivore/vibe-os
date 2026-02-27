use std::convert::Infallible;
use std::time::Instant;

use axum::extract::{Query, State};
use axum::response::sse::{Event, KeepAlive, Sse};
use axum::Json;
use core_model::domain::builtin_domains;
use core_model::event::EventEnvelope;
use core_model::time::TimeWindow;
use core_persona::identity::IdentityId;
use core_persona::provider::{LinearProviderPersona, SlackProviderPersona};
use domain_adapters::{load_linear_personas, load_slack_personas};
use futures::future::join_all;
use futures::stream::{self, StreamExt};
use serde::{Deserialize, Serialize};

use crate::error::AppError;
use crate::state::AppState;
use crate::utils::{build_window, parse_identity_id, select_domains};

const PROVIDER_PERSONAS_CACHE_TTL_SECONDS: u64 = 60;

#[derive(Deserialize)]
pub struct EventsQuery {
    pub domains: Option<String>,
    pub from: String,
    pub to: String,
    pub limit: Option<usize>,
    pub identity_id: Option<String>,
    pub cursor: Option<u64>,
}

#[derive(Debug, Serialize)]
pub struct ProviderPersonasResponse {
    pub slack: Vec<SlackProviderPersona>,
    pub linear: Vec<LinearProviderPersona>,
}

#[derive(Clone, Serialize)]
pub struct WindowBounds {
    pub from: String,
    pub to: String,
}

#[derive(Serialize)]
#[serde(tag = "mode", rename_all = "snake_case")]
pub enum TimelineEventsResponse {
    Snapshot {
        cursor: u64,
        window: WindowBounds,
        events: Vec<EventEnvelope>,
    },
    Delta {
        cursor: u64,
        window: WindowBounds,
        added: Vec<EventEnvelope>,
        removed: Vec<String>,
    },
}

pub async fn list_domains() -> impl axum::response::IntoResponse {
    Json(builtin_domains())
}

pub async fn list_events(
    State(state): State<AppState>,
    Query(query): Query<EventsQuery>,
) -> Result<Json<TimelineEventsResponse>, AppError> {
    let window = build_window(&query.from, &query.to)?;
    let domains = select_domains(query.domains.as_deref(), &state)?;
    let identity_filter = query
        .identity_id
        .as_deref()
        .map(parse_identity_id)
        .transpose()?;
    let limit = query.limit.unwrap_or(usize::MAX);

    if let Some(cursor) = query.cursor {
        let mut delta = state
            .timeline_cache
            .delta_since(&domains, &window, cursor)
            .await?;
        apply_identity_filter(&mut delta.added, identity_filter);
        if delta.added.len() > limit {
            delta.added.truncate(limit);
        }
        return Ok(Json(TimelineEventsResponse::Delta {
            cursor: delta.cursor,
            window: WindowBounds::from(&delta.window),
            added: delta.added,
            removed: delta.removed,
        }));
    }

    let mut snapshot = state.timeline_cache.snapshot(&domains, &window).await?;
    apply_identity_filter(&mut snapshot.events, identity_filter);
    if snapshot.events.len() > limit {
        snapshot.events.truncate(limit);
    }
    Ok(Json(TimelineEventsResponse::Snapshot {
        cursor: snapshot.cursor,
        window: WindowBounds::from(&snapshot.window),
        events: snapshot.events,
    }))
}

pub async fn list_provider_personas(
    State(state): State<AppState>,
) -> Result<Json<ProviderPersonasResponse>, AppError> {
    {
        let cache = state.provider_personas_cache.lock().await;
        if let Some((fetched_at, slack, linear)) = cache.as_ref() {
            if fetched_at.elapsed().as_secs() < PROVIDER_PERSONAS_CACHE_TTL_SECONDS {
                return Ok(Json(ProviderPersonasResponse {
                    slack: slack.clone(),
                    linear: linear.clone(),
                }));
            }
        }
    }

    let slack_mirror_dir = state.slack_mirror_dir.as_ref().clone();
    let linear_mirror_dir = state.linear_mirror_dir.as_ref().clone();
    let (slack, linear) = tokio::task::spawn_blocking(move || -> Result<_, anyhow::Error> {
        let slack = load_slack_personas(&slack_mirror_dir)?;
        let linear = load_linear_personas(&linear_mirror_dir)?;
        Ok((slack, linear))
    })
    .await
    .map_err(AppError::from)??;

    {
        let mut cache = state.provider_personas_cache.lock().await;
        *cache = Some((Instant::now(), slack.clone(), linear.clone()));
    }

    Ok(Json(ProviderPersonasResponse { slack, linear }))
}

pub async fn fetch_meta(State(state): State<AppState>) -> impl axum::response::IntoResponse {
    Json(state.meta.as_ref().clone())
}

// ============================================================================
// Multi-range endpoint for efficient incremental fetching
// ============================================================================

#[derive(Deserialize)]
pub struct RangeSpec {
    pub from: String,
    pub to: String,
}

#[derive(Deserialize)]
pub struct MultiRangeQuery {
    pub domains: Option<String>,
    pub ranges: Vec<RangeSpec>,
}

#[derive(Serialize)]
pub struct RangeEventsResult {
    pub window: WindowBounds,
    pub events: Vec<EventEnvelope>,
}

#[derive(Serialize)]
pub struct MultiRangeResponse {
    pub results: Vec<RangeEventsResult>,
}

/// Fetch events for multiple time ranges in a single request.
/// This is more efficient than making multiple requests when expanding
/// the timeline window.
pub async fn list_events_for_ranges(
    State(state): State<AppState>,
    Json(query): Json<MultiRangeQuery>,
) -> Result<Json<MultiRangeResponse>, AppError> {
    let domains = select_domains(query.domains.as_deref(), &state)?;

    // Fetch all ranges in parallel
    let futures = query.ranges.into_iter().map(|range_spec| {
        let domains = domains.clone();
        let state = state.clone();
        async move {
            let window = build_window(&range_spec.from, &range_spec.to)?;
            let snapshot = state.timeline_cache.snapshot(&domains, &window).await?;
            Ok::<_, AppError>(RangeEventsResult {
                window: WindowBounds::from(&snapshot.window),
                events: snapshot.events,
            })
        }
    });

    let results: Vec<Result<RangeEventsResult, AppError>> = join_all(futures).await;

    // Collect results, propagating the first error if any
    let results: Result<Vec<_>, _> = results.into_iter().collect();

    Ok(Json(MultiRangeResponse { results: results? }))
}

const STREAM_PAGE_SIZE: usize = 100;

#[derive(Deserialize)]
pub struct StreamEventsQuery {
    pub domains: Option<String>,
    pub from: String,
    pub to: String,
}

pub async fn stream_events(
    State(state): State<AppState>,
    Query(query): Query<StreamEventsQuery>,
) -> Result<Sse<impl futures::Stream<Item = Result<Event, Infallible>>>, AppError> {
    let window = build_window(&query.from, &query.to)?;
    let domains = select_domains(query.domains.as_deref(), &state)?;
    let snapshot = state.timeline_cache.snapshot(&domains, &window).await?;

    let cursor = snapshot.cursor;
    let window_bounds = WindowBounds::from(&snapshot.window);
    let total_events = snapshot.events.len();
    let pages: Vec<Vec<EventEnvelope>> = snapshot
        .events
        .chunks(STREAM_PAGE_SIZE)
        .map(|chunk| chunk.to_vec())
        .collect();

    let event_stream = stream::iter(pages.into_iter().enumerate().map(|(idx, events)| {
        let payload = serde_json::json!({ "events": events, "page_index": idx });
        Ok(Event::default()
            .event("page")
            .data(payload.to_string()))
    }))
    .chain(stream::once(async move {
        let payload = serde_json::json!({
            "cursor": cursor,
            "window": window_bounds,
            "total_events": total_events,
        });
        Ok(Event::default()
            .event("done")
            .data(payload.to_string()))
    }));

    Ok(Sse::new(event_stream).keep_alive(KeepAlive::default()))
}

impl From<&TimeWindow> for WindowBounds {
    fn from(window: &TimeWindow) -> Self {
        Self {
            from: window.start.to_rfc3339(),
            to: window.end.to_rfc3339(),
        }
    }
}

fn apply_identity_filter(events: &mut Vec<EventEnvelope>, identity_filter: Option<IdentityId>) {
    if let Some(identity_id) = identity_filter {
        events.retain(|event| event.actor_identity_id == Some(identity_id));
    }
}
