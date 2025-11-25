use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

use axum::extract::{Query, State};
use axum::Json;
use core_model::event::EventEnvelope;
use core_types::Domain;
use domain_adapters::SlackAdapter;
use futures::stream::{self, StreamExt};
use serde::{Deserialize, Serialize};

use crate::error::AppError;
use crate::routes::slack::fetch_thread_via_api_if_possible;
use crate::search::{rebuild_full_index, SearchRequest, SearchResult};
use crate::state::AppState;
use crate::utils::{build_window, parse_domains_csv};

#[derive(Debug, Deserialize)]
pub struct SearchQuery {
    pub q: Option<String>,
    pub domains: Option<String>,
    pub from: Option<String>,
    pub to: Option<String>,
    pub limit: Option<usize>,
    pub offset: Option<usize>,
}

#[derive(Debug, Serialize)]
pub struct SearchResponse {
    pub total: usize,
    pub hits: Vec<SearchHitResponse>,
}

#[derive(Debug, Serialize)]
pub struct SearchHitResponse {
    pub event: EventEnvelope,
    pub thread_name: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct ReindexResponse {
    pub indexed_events: usize,
}

pub async fn execute_search(
    State(state): State<AppState>,
    Query(query): Query<SearchQuery>,
) -> Result<Json<SearchResponse>, AppError> {
    let mut request = SearchRequest::default();
    request.query = query.q.unwrap_or_default();
    request.limit = query.limit.unwrap_or(request.limit);
    request.offset = query.offset.unwrap_or(request.offset);
    request.domains = parse_domains_csv(query.domains.as_deref()).unwrap_or_default();
    request.window = match (query.from.as_deref(), query.to.as_deref()) {
        (Some(from), Some(to)) => Some(build_window(from, to)?),
        _ => None,
    };

    let search = state.search.clone();
    let mut result = tokio::task::spawn_blocking(move || search.search(request))
        .await
        .map_err(AppError::from)??;

    // Enrich Slack thread results with root message text
    enrich_slack_thread_names(&mut result, &state).await;

    Ok(Json(result.into()))
}

pub async fn reindex(State(state): State<AppState>) -> Result<Json<ReindexResponse>, AppError> {
    let indexed_events = rebuild_full_index(&state).await?;
    Ok(Json(ReindexResponse { indexed_events }))
}

impl From<SearchResult> for SearchResponse {
    fn from(value: SearchResult) -> Self {
        Self {
            total: value.total,
            hits: value
                .hits
                .into_iter()
                .map(|hit| SearchHitResponse {
                    event: hit.event,
                    thread_name: hit.thread_name,
                })
                .collect(),
        }
    }
}

async fn enrich_slack_thread_names(result: &mut SearchResult, state: &AppState) {
    // Group hits by thread entity_id
    let mut thread_groups: HashMap<String, Vec<usize>> = HashMap::new();

    for (idx, hit) in result.hits.iter().enumerate() {
        if hit.event.domain == Domain::Slack {
            if let Some(entity_id) = &hit.event.entity_id {
                if entity_id.contains(':') {
                    // This is a thread
                    thread_groups
                        .entry(entity_id.clone())
                        .or_default()
                        .push(idx);
                }
            }
        }
    }

    // For each thread, try to load the root message
    let adapter = SlackAdapter::new(state.slack_mirror_dir.as_ref());

    for (entity_id, indices) in thread_groups {
        // Parse entity_id: "channel_id:thread_ts"
        let parts: Vec<&str> = entity_id.split(':').collect();
        if parts.len() != 2 {
            continue;
        }
        let channel_id = parts[0];
        let thread_ts = parts[1].replace('_', ".");

        // Load the thread from the adapter
        match adapter.load_thread(channel_id, &thread_ts) {
            Ok(events) => {
                // Find the root message (ts == thread_ts)
                if let Some(root_text) = find_root_message_text(&events, &thread_ts) {
                    // Apply this text to all hits in this thread
                    for &idx in &indices {
                        if let Some(hit) = result.hits.get_mut(idx) {
                            hit.thread_name = Some(root_text.clone());
                        }
                    }
                }
            }
            Err(e) => {
                tracing::debug!("Failed to load thread {} for enrichment: {}", entity_id, e);
            }
        }
    }
}

fn find_root_message_text(events: &[EventEnvelope], thread_ts: &str) -> Option<String> {
    for event in events {
        if let Some(data) = event.data.as_object() {
            if let Some(ts) = data.get("ts").and_then(|v| v.as_str()) {
                if ts == thread_ts {
                    // This is the root message
                    return data
                        .get("text")
                        .and_then(|v| v.as_str())
                        .map(|s| s.trim())
                        .filter(|s| !s.is_empty())
                        .map(|s| s.to_string());
                }
            }
        }
    }
    None
}

#[derive(Debug, Deserialize)]
pub struct ThreadTitlesRequest {
    pub thread_ids: Vec<String>,
}

#[derive(Debug, Serialize)]
pub struct ThreadTitlesResponse {
    pub titles: HashMap<String, String>,
}

/// Maximum number of concurrent thread title lookups
const MAX_CONCURRENT_TITLE_LOOKUPS: usize = 32;

pub async fn get_thread_titles(
    State(state): State<AppState>,
    Json(request): Json<ThreadTitlesRequest>,
) -> Result<Json<ThreadTitlesResponse>, AppError> {
    let slack_mirror_dir = Arc::new(state.slack_mirror_dir.clone());
    let state = Arc::new(state);

    // Process thread titles in parallel with bounded concurrency
    let results: Vec<Option<(String, String)>> = stream::iter(request.thread_ids)
        .map(|thread_id| {
            let slack_mirror_dir = Arc::clone(&slack_mirror_dir);
            let state = Arc::clone(&state);
            async move {
                fetch_single_thread_title(&thread_id, &slack_mirror_dir, &state).await
            }
        })
        .buffer_unordered(MAX_CONCURRENT_TITLE_LOOKUPS)
        .collect()
        .await;

    let titles: HashMap<String, String> = results.into_iter().flatten().collect();

    Ok(Json(ThreadTitlesResponse { titles }))
}

async fn fetch_single_thread_title(
    thread_id: &str,
    slack_mirror_dir: &Path,
    state: &AppState,
) -> Option<(String, String)> {
    // Parse thread_id: "channel_id:thread_ts"
    let parts: Vec<&str> = thread_id.split(':').collect();
    if parts.len() != 2 {
        return None;
    }
    let channel_id = parts[0].to_string();
    let thread_ts = parts[1].replace('_', ".");

    // Try to load from mirror first (blocking I/O in spawn_blocking)
    let mirror_dir = slack_mirror_dir.to_path_buf();
    let channel_id_clone = channel_id.clone();
    let thread_ts_clone = thread_ts.clone();
    let mirror_result = tokio::task::spawn_blocking(move || {
        let adapter = SlackAdapter::new(&mirror_dir);
        adapter.load_thread(&channel_id_clone, &thread_ts_clone)
    })
    .await
    .ok()
    .and_then(|r| r.ok());

    let events = match mirror_result {
        Some(events) => events,
        None => {
            // Fallback to Slack API if mirror failed
            tracing::debug!(
                "Failed to load thread {} from mirror for title lookup, trying API",
                thread_id
            );
            match fetch_thread_via_api_if_possible(state, &channel_id, &thread_ts, "fetching Slack thread title").await {
                Ok(events) => events,
                Err(api_err) => {
                    tracing::debug!(
                        "Unable to fetch Slack thread {} for title lookup: {:?}",
                        thread_id,
                        api_err
                    );
                    return None;
                }
            }
        }
    };

    // Find root message where ts == thread_ts
    find_root_message_text(&events, &thread_ts).map(|title| (thread_id.to_string(), title))
}
