use std::fs::File;
use std::io::{BufRead, BufReader};

use axum::extract::{Path, State};
use axum::Json;
use chrono::{DateTime, Utc};
use domain_adapters::LinearAdapter;
use serde::{Deserialize, Serialize};

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

/// Full issue snapshot with cycle data for the dashboard.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LinearIssueSnapshot {
    pub id: String,
    pub identifier: String,
    pub title: String,
    pub description: Option<String>,
    pub url: Option<String>,
    pub team_id: Option<String>,
    pub team_key: Option<String>,
    pub team_name: Option<String>,
    pub state_id: Option<String>,
    pub state_name: Option<String>,
    pub state_type: Option<String>,
    pub assignee_id: Option<String>,
    pub assignee_name: Option<String>,
    pub labels: Vec<String>,
    pub priority: Option<i32>,
    pub estimate: Option<f32>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub completed_at: Option<DateTime<Utc>>,
    pub canceled_at: Option<DateTime<Utc>>,
    pub archived_at: Option<DateTime<Utc>>,
    // Cycle information
    #[serde(default)]
    pub cycle_id: Option<String>,
    #[serde(default)]
    pub cycle_number: Option<i32>,
    #[serde(default)]
    pub cycle_name: Option<String>,
    #[serde(default)]
    pub cycle_starts_at: Option<DateTime<Utc>>,
    #[serde(default)]
    pub cycle_ends_at: Option<DateTime<Utc>>,
    // Compact history for cycle changes
    #[serde(default)]
    pub cycle_history: Vec<CycleHistoryEntry>,
}

/// Compact representation of a cycle change event.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CycleHistoryEntry {
    pub at: DateTime<Utc>,
    pub from_cycle_number: Option<i32>,
    pub to_cycle_number: Option<i32>,
    #[serde(default)]
    pub to_cycle_starts_at: Option<DateTime<Utc>>,
}

/// Metadata about the mirror sync state.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LinearMirrorMeta {
    pub last_full_sync_at: Option<DateTime<Utc>>,
    pub workspace_name: Option<String>,
}

/// Response for listing all issues.
#[derive(Debug, Serialize)]
pub struct LinearIssuesResponse {
    pub issues: Vec<LinearIssueSnapshot>,
    pub last_sync_at: Option<DateTime<Utc>>,
    pub workspace_name: Option<String>,
}

/// GET /api/linear/issues - Returns all issues with cycle data.
pub async fn list_linear_issues(
    State(state): State<AppState>,
) -> Result<Json<LinearIssuesResponse>, AppError> {
    let mirror_dir = state.linear_mirror_dir.as_ref();
    let issues_path = mirror_dir.join("issues.jsonl");
    let meta_path = mirror_dir.join("meta.json");

    // Load metadata
    let meta = if meta_path.exists() {
        let file = File::open(&meta_path)?;
        serde_json::from_reader::<_, LinearMirrorMeta>(file).ok()
    } else {
        None
    };

    // Load issues
    let mut issues = Vec::new();
    if issues_path.exists() {
        let file = File::open(&issues_path)?;
        let reader = BufReader::new(file);
        for line in reader.lines() {
            let line = line?;
            if line.trim().is_empty() {
                continue;
            }
            let issue: LinearIssueSnapshot = serde_json::from_str(&line)?;
            issues.push(issue);
        }
    }

    Ok(Json(LinearIssuesResponse {
        issues,
        last_sync_at: meta.as_ref().and_then(|m| m.last_full_sync_at),
        workspace_name: meta.and_then(|m| m.workspace_name),
    }))
}
