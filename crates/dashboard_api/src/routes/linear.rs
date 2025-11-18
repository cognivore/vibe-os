use axum::extract::{Path, State};
use axum::Json;
use domain_adapters::LinearAdapter;
use serde::Serialize;

use crate::error::AppError;
use crate::state::AppState;
use crate::utils::resolve_event_entities;

#[derive(Debug, Serialize)]
pub struct LinearIssueResponse {
    pub issue_id: String,
    pub issue_identifier: Option<String>,
    pub issue_title: Option<String>,
    pub issue_url: Option<String>,
    pub issue_description: Option<String>,
    pub events: Vec<core_model::event::EventEnvelope>,
    pub comments: Vec<core_model::event::EventEnvelope>,
}

pub async fn get_linear_issue(
    State(state): State<AppState>,
    Path(issue_ref): Path<String>,
) -> Result<Json<LinearIssueResponse>, AppError> {
    let adapter = LinearAdapter::new(state.linear_mirror_dir.as_ref());
    let mut thread = adapter.load_issue_thread(&issue_ref)?;

    resolve_event_entities(&mut thread.events, state.identity_store.clone()).await;
    resolve_event_entities(&mut thread.comments, state.identity_store.clone()).await;

    Ok(Json(LinearIssueResponse {
        issue_id: thread.issue_id,
        issue_identifier: thread.issue_identifier,
        issue_title: thread.issue_title,
        issue_url: thread.issue_url,
        issue_description: thread.issue_description,
        events: thread.events,
        comments: thread.comments,
    }))
}
