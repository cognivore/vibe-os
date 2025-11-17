use anyhow::anyhow;
use axum::extract::{Query, State};
use axum::Json;
use core_model::domain::builtin_domains;
use core_model::event::EventEnvelope;
use core_persona::provider::{LinearProviderPersona, SlackProviderPersona};
use domain_adapters::{load_linear_personas, load_slack_personas};
use serde::{Deserialize, Serialize};
use tokio::task;

use crate::error::AppError;
use crate::state::AppState;
use crate::utils::{build_window, parse_identity_id, resolve_event_entities, select_domains};

#[derive(Deserialize)]
pub struct EventsQuery {
    pub domains: Option<String>,
    pub from: String,
    pub to: String,
    pub limit: Option<usize>,
    pub identity_id: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct ProviderPersonasResponse {
    pub slack: Vec<SlackProviderPersona>,
    pub linear: Vec<LinearProviderPersona>,
}

pub async fn list_domains() -> impl axum::response::IntoResponse {
    Json(builtin_domains())
}

pub async fn list_events(
    State(state): State<AppState>,
    Query(query): Query<EventsQuery>,
) -> Result<Json<Vec<EventEnvelope>>, AppError> {
    let window = build_window(&query.from, &query.to)?;
    let domains = select_domains(query.domains.as_deref(), &state)?;
    let identity_filter = query
        .identity_id
        .as_deref()
        .map(parse_identity_id)
        .transpose()?;
    let mut events = Vec::new();
    for domain in domains {
        if let Some(adapter) = state.adapters.get(&domain) {
            let adapter = adapter.clone();
            let window_clone = window.clone();
            let mut loaded = task::spawn_blocking(move || adapter.load_events(&window_clone))
                .await
                .map_err(|err| anyhow!("adapter task failed: {}", err))??;
            events.append(&mut loaded);
        }
    }
    resolve_event_entities(&mut events, state.identity_store.clone()).await;
    if let Some(identity_id) = identity_filter {
        events.retain(|event| event.actor_identity_id == Some(identity_id));
    }
    events.sort_by(|a, b| b.at.cmp(&a.at));
    if let Some(limit) = query.limit {
        events.truncate(limit);
    }
    Ok(Json(events))
}

pub async fn list_provider_personas(
    State(state): State<AppState>,
) -> Result<Json<ProviderPersonasResponse>, AppError> {
    let slack = load_slack_personas(&state.slack_mirror_dir)?;
    let linear = load_linear_personas(&state.linear_mirror_dir)?;
    Ok(Json(ProviderPersonasResponse { slack, linear }))
}

pub async fn fetch_meta(State(state): State<AppState>) -> impl axum::response::IntoResponse {
    Json(state.meta.as_ref().clone())
}
