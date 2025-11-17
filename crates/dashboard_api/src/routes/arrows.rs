use axum::extract::{Query, State};
use axum::Json;
use core_model::arrow::Arrow;
use serde::Deserialize;
use tokio::task;

use crate::error::AppError;
use crate::state::AppState;
use crate::utils::{
    arrow_source_matches, arrow_target_matches, build_window, parse_direction, parse_domains_csv,
    resolve_arrow_entities,
};

#[derive(Deserialize)]
pub struct ArrowsQuery {
    pub from: String,
    pub to: String,
    pub direction: Option<String>,
    pub source_domains: Option<String>,
    pub target_domains: Option<String>,
}

pub async fn list_arrows(
    State(state): State<AppState>,
    Query(query): Query<ArrowsQuery>,
) -> Result<Json<Vec<Arrow>>, AppError> {
    let window = build_window(&query.from, &query.to)?;
    let direction = query
        .direction
        .as_deref()
        .map(parse_direction)
        .transpose()?;
    let source_filter = parse_domains_csv(query.source_domains.as_deref());
    let target_filter = parse_domains_csv(query.target_domains.as_deref());

    let store = state.arrow_store.clone();
    let mut arrows = task::spawn_blocking(move || store.load_in_window(&window, None)).await??;
    resolve_arrow_entities(&mut arrows, state.identity_store.clone()).await;

    let filtered = arrows
        .into_iter()
        .filter(|arrow| match &direction {
            Some(dir) => arrow.direction == *dir,
            None => true,
        })
        .filter(|arrow| match &source_filter {
            Some(domains) => arrow_source_matches(arrow, domains),
            None => true,
        })
        .filter(|arrow| match &target_filter {
            Some(domains) => arrow_target_matches(arrow, domains),
            None => true,
        })
        .collect();
    Ok(Json(filtered))
}
