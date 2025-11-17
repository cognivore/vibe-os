use anyhow::anyhow;
use axum::extract::State;
use axum::Json;
use core_model::arrow::Arrow;
use core_model::domain::Domain;
use core_model::operator::OperatorDescriptor;
use core_model::time::TimeWindow;
use core_operators::OperatorContext;
use serde::{Deserialize, Serialize};
use tokio::task;

use crate::error::AppError;
use crate::state::AppState;
use crate::utils::{
    build_window, load_events_for_domains, parse_domain_id, resolve_arrow_entities,
};

#[derive(Deserialize)]
pub struct RunOperatorBody {
    pub operator_id: String,
    pub domains: Option<Vec<String>>,
    pub from: String,
    pub to: String,
}

#[derive(Serialize)]
pub struct RunOperatorResponse {
    pub operator: OperatorDescriptor,
    pub window: TimeWindow,
    pub arrows: Vec<Arrow>,
}

pub async fn list_operators(State(state): State<AppState>) -> impl axum::response::IntoResponse {
    Json(state.operator_registry.descriptors())
}

pub async fn run_operator(
    State(state): State<AppState>,
    Json(body): Json<RunOperatorBody>,
) -> Result<Json<RunOperatorResponse>, AppError> {
    let operator = state
        .operator_registry
        .find(&body.operator_id)
        .ok_or_else(|| anyhow!("operator {} not found", body.operator_id))?;

    let window = build_window(&body.from, &body.to)?;
    let domains: Vec<Domain> = match &body.domains {
        Some(list) if !list.is_empty() => list.iter().map(|id| parse_domain_id(id)).collect(),
        _ => operator.descriptor().source_domains.clone(),
    };

    let events = load_events_for_domains(&state, &domains, &window).await?;
    let ctx = OperatorContext {
        descriptor: operator.descriptor(),
        window: window.clone(),
        events: &events,
        now: chrono::Utc::now(),
    };
    let mut arrows = operator.run(&ctx)?.arrows;

    if !arrows.is_empty() {
        let store = state.arrow_store.clone();
        let to_store = arrows.clone();
        task::spawn_blocking(move || store.append(&to_store)).await??;
    }

    resolve_arrow_entities(&mut arrows, state.identity_store.clone()).await;

    Ok(Json(RunOperatorResponse {
        operator: operator.descriptor().clone(),
        window,
        arrows,
    }))
}
