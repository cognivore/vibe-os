use axum::extract::{Query, State};
use axum::Json;
use core_model::event::EventEnvelope;
use serde::{Deserialize, Serialize};

use crate::error::AppError;
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
    pub events: Vec<EventEnvelope>,
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
    let result = tokio::task::spawn_blocking(move || search.search(request))
        .await
        .map_err(AppError::from)??;

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
        events: value.hits.into_iter().map(|hit| hit.event).collect(),
        }
    }
}
