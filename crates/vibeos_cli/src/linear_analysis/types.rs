use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use crate::linear_sync::LinearIssueSnapshot;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LinearIssueSummary {
    pub identifier: String,
    pub title: String,
    pub url: Option<String>,
    pub team_key: Option<String>,
    pub state_name: Option<String>,
    pub state_type: Option<String>,
    pub assignee_name: Option<String>,
    pub priority: Option<i32>,
    pub updated_at: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LinearWindowContext {
    pub window_start: DateTime<Utc>,
    pub window_end: DateTime<Utc>,
    pub issues_created: usize,
    pub issues_completed: usize,
    pub issues_reopened: usize,
    pub issues_archived: usize,
    pub comments_added: usize,
    pub transitions: usize,
    pub top_active_issues: Vec<LinearIssueSummary>,
    pub stale_issues: Vec<LinearIssueSummary>,
}

impl LinearIssueSummary {
    pub fn from_issue(issue: &LinearIssueSnapshot) -> Self {
        Self {
            identifier: issue.identifier.clone(),
            title: issue.title.clone(),
            url: issue.url.clone(),
            team_key: issue.team_key.clone(),
            state_name: issue.state_name.clone(),
            state_type: issue.state_type.clone(),
            assignee_name: issue.assignee_name.clone(),
            priority: issue.priority,
            updated_at: issue.updated_at,
        }
    }
}
