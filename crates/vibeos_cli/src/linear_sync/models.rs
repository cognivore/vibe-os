use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use serde_json::Value;

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
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LinearIssueEvent {
    pub id: String,
    pub issue_id: String,
    pub issue_identifier: String,
    pub event_kind: String,
    pub created_at: DateTime<Utc>,
    pub actor_id: Option<String>,
    pub actor_name: Option<String>,
    pub from_state: Option<String>,
    pub to_state: Option<String>,
    pub from_priority: Option<i32>,
    pub to_priority: Option<i32>,
    pub extra: Value,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LinearUserSnapshot {
    pub fetched_at: DateTime<Utc>,
    pub user: LinearUserNode,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct LinearUserNode {
    pub id: String,
    pub name: Option<String>,
    pub display_name: Option<String>,
    pub email: Option<String>,
    pub active: bool,
    pub avatar_url: Option<String>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LinearMirrorMeta {
    pub last_full_sync_at: Option<DateTime<Utc>>,
    pub workspace_name: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LinearCommentRecord {
    pub id: String,
    pub issue_id: String,
    pub issue_identifier: Option<String>,
    pub body: String,
    pub url: Option<String>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub user_id: Option<String>,
    pub user_name: Option<String>,
    pub user_display_name: Option<String>,
}
