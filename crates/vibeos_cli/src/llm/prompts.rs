use anyhow::{Context, Result};
use serde::Serialize;
use serde_json::json;

use crate::haskllm::JSONSchemaSpec;
use crate::linear_analysis::{LinearIssueSummary, LinearWindowContext};

pub const SYSTEM_PROMPT: &str = "You are an assistant that reads Slack threads and decides whether they should become Linear issues. You must ALWAYS respond with JSON only, following a strict schema.";

pub(super) fn suggestion_schema() -> JSONSchemaSpec {
    JSONSchemaSpec::new(
        "LlmSuggestionResponse",
        json!({
            "type": "object",
            "required": [
                "thread_summary",
                "should_create_issues",
                "reason",
                "issues"
            ],
            "properties": {
                "thread_summary": {"type": "string"},
                "should_create_issues": {"type": "boolean"},
                "reason": {"type": "string"},
                "issues": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "required": [
                            "title",
                            "description_markdown",
                            "type",
                            "priority",
                            "labels",
                            "assignee_hint",
                            "estimate_hint",
                            "related_linear_issue_identifiers",
                            "linked_slack_message_ts",
                            "why_this_issue"
                        ],
                        "properties": {
                            "title": {"type": "string"},
                            "description_markdown": {"type": "string"},
                            "type": {
                                "type": "string",
                                "enum": ["bug", "feature", "chore", "question"]
                            },
                            "priority": {
                                "type": "string",
                                "enum": ["P0", "P1", "P2", "P3"]
                            },
                            "labels": {
                                "type": "array",
                                "items": {"type": "string"}
                            },
                            "assignee_hint": {
                                "type": ["string", "null"]
                            },
                            "estimate_hint": {
                                "type": ["number", "null"]
                            },
                            "related_linear_issue_identifiers": {
                                "type": "array",
                                "items": {"type": "string"}
                            },
                            "linked_slack_message_ts": {
                                "type": "array",
                                "items": {"type": "string"}
                            },
                            "why_this_issue": {"type": "string"}
                        },
                        "additionalProperties": false
                    }
                }
            },
            "additionalProperties": false
        }),
        true,
    )
}

pub(super) fn build_prompt(thread_text: &str, schema_json: &str) -> String {
    format!(
        r#"
You are an assistant that reads Slack threads and decides whether they should be turned into Linear issues.

Respond with a SINGLE JSON object (no markdown, no prose) that validates against this schema:
{schema}

Rules:
- Always output valid JSON that matches the schema exactly.
- Do not include comments or additional explanations outside of the JSON object.
- If no issues should be created, set "should_create_issues" to false and return an empty "issues" array.
- Populate linked Slack timestamps in "linked_slack_message_ts".

Slack thread transcript:
----------------
{thread}
"#,
        schema = schema_json,
        thread = thread_text
    )
}

pub(super) fn build_window_prompt_context(window: &LinearWindowContext) -> Result<String> {
    #[derive(Serialize)]
    struct WindowPromptContext<'a> {
        window: &'a LinearWindowContext,
        top_active_issues: Vec<LinearIssueSummaryPayload>,
        stale_issues: Vec<LinearIssueSummaryPayload>,
    }

    #[derive(Serialize)]
    struct LinearIssueSummaryPayload {
        identifier: String,
        title: String,
        url: Option<String>,
        team_key: Option<String>,
        state_name: Option<String>,
        state_type: Option<String>,
        assignee_name: Option<String>,
        priority: Option<i32>,
        updated_at: String,
    }

    fn to_payload(issue: &LinearIssueSummary) -> LinearIssueSummaryPayload {
        LinearIssueSummaryPayload {
            identifier: issue.identifier.clone(),
            title: issue.title.clone(),
            url: issue.url.clone(),
            team_key: issue.team_key.clone(),
            state_name: issue.state_name.clone(),
            state_type: issue.state_type.clone(),
            assignee_name: issue.assignee_name.clone(),
            priority: issue.priority,
            updated_at: issue.updated_at.to_rfc3339(),
        }
    }

    let context = WindowPromptContext {
        window,
        top_active_issues: window.top_active_issues.iter().map(to_payload).collect(),
        stale_issues: window.stale_issues.iter().map(to_payload).collect(),
    };

    serde_json::to_string(&context).context("Failed to serialize Linear window prompt context")
}
