use serde::{Deserialize, Serialize};

use crate::slack::SlackMessage;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LlmIssueSuggestion {
    pub title: String,
    pub description_markdown: String,
    #[serde(rename = "type")]
    pub r#type: String,
    pub priority: String,
    pub labels: Vec<String>,
    pub assignee_hint: Option<String>,
    pub estimate_hint: Option<f32>,
    pub related_linear_issue_identifiers: Vec<String>,
    pub linked_slack_message_ts: Vec<String>,
    pub why_this_issue: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LlmSuggestionResponse {
    pub thread_summary: String,
    pub should_create_issues: bool,
    pub reason: String,
    pub issues: Vec<LlmIssueSuggestion>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LlmLinearIssueRef {
    pub identifier: String,
    pub title: String,
    pub url: Option<String>,
    pub status: String,
    pub team_key: Option<String>,
    pub assignee: Option<String>,
    pub priority: Option<i32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LlmSlackThreadSuggestion {
    pub channel_hint: Option<String>,
    pub title: String,
    pub message_markdown: String,
    pub related_issue_identifiers: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LlmLinearWindowSummary {
    pub window_start: String,
    pub window_end: String,
    pub summary_markdown: String,
    pub key_metrics_markdown: String,
    pub noteworthy_issues: Vec<LlmLinearIssueRef>,
    pub stale_issues: Vec<LlmLinearIssueRef>,
    pub suggested_slack_threads: Vec<LlmSlackThreadSuggestion>,
}

#[derive(Debug, Clone)]
pub struct SlackThread {
    pub conversation_id: String,
    pub thread_ts: String,
    pub messages: Vec<SlackMessage>,
}
