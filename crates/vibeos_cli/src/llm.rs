use std::cmp::Ordering;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};

use anyhow::{anyhow, Context, Result};
use reqwest::blocking::Client as BlockingClient;
use serde::{Deserialize, Serialize};
use serde_json::json;

use crate::config::Config;
use crate::haskllm::JSONSchemaSpec;
use crate::linear_analysis::LinearWindowContext;
use crate::linear_sync::LinearIssueSnapshot;
use crate::slack::SlackMessage;

const CONVERSATIONS_DIR: &str = "conversations";
const JSONL_EXTENSION: &str = "jsonl";
const SYSTEM_PROMPT: &str = "You are an assistant that reads Slack threads and decides whether they should become Linear issues. You must ALWAYS respond with JSON only, following a strict schema.";

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

pub struct LlmClient {
    http: BlockingClient,
    api_base: String,
    api_key: Option<String>,
    model: String,
    temperature: f32,
}

impl LlmClient {
    pub fn new_from_config(cfg: &Config) -> Result<Self> {
        let api_base = cfg.llm_api_base.as_ref()
            .with_context(|| "VIBEOS_LLM_API_BASE (legacy LLM_API_BASE) is missing. Set it to an OpenAI-compatible base URL (e.g. https://api.openai.com/v1) or rerun setup.")?;

        let model = cfg.llm_model.as_ref()
            .with_context(|| "VIBEOS_LLM_MODEL (legacy LLM_MODEL) is missing. Provide the model identifier to use for triage (e.g. gemma-7b-instruct).")?;

        Ok(Self {
            http: BlockingClient::new(),
            api_base: api_base.trim_end_matches('/').to_string(),
            api_key: cfg.llm_api_key.clone(),
            model: model.clone(),
            temperature: cfg.llm_temperature,
        })
    }

    pub fn suggest_issues_for_thread(&self, thread_text: &str) -> Result<LlmSuggestionResponse> {
        let schema = suggestion_schema();
        let schema_json = serde_json::to_string_pretty(&schema.schema)
            .context("Failed to serialize suggestion schema")?;
        let prompt = build_prompt(thread_text, &schema_json);

        let body = json!({
            "model": self.model,
            "temperature": self.temperature,
            "messages": [
                {"role": "system", "content": SYSTEM_PROMPT},
                {"role": "user", "content": prompt}
            ]
        });

        let url = format!("{}/chat/completions", self.api_base);
        let mut request = self
            .http
            .post(&url)
            .header("Content-Type", "application/json");

        if let Some(key) = &self.api_key {
            request = request.header("Authorization", format!("Bearer {}", key));
        }

        let response = request
            .json(&body)
            .send()
            .with_context(|| format!("Failed to send chat completion request to {}", url))?;

        let status = response.status();
        if !status.is_success() {
            let body_text = response.text().unwrap_or_default();
            anyhow::bail!("LLM server returned status {}: {}", status, body_text);
        }

        let completion: ChatCompletionResponse = response
            .json()
            .context("Failed to parse chat completion response JSON")?;

        let raw_content = completion
            .choices
            .into_iter()
            .next()
            .and_then(|choice| choice.message.content)
            .ok_or_else(|| anyhow!("LLM response did not include message content"))?;

        serde_json::from_str::<LlmSuggestionResponse>(&raw_content).with_context(|| {
            format!(
                "LLM response was not valid JSON. Snippet: {}",
                preview(&raw_content)
            )
        })
    }

    pub fn summarize_linear_window(
        &self,
        window_context: &LinearWindowContext,
        _issues: &[LinearIssueSnapshot],
    ) -> Result<LlmLinearWindowSummary> {
        let prompt_context = build_window_prompt_context(window_context)?;
        let prompt = format!(
            r##"
You are an assistant that summarizes recent activity in Linear (an issue tracking tool)
for an engineering team, and suggests Slack threads to communicate key updates.

You will receive compact JSON describing a time window's activity and notable issues.
Using ONLY that information, produce a SINGLE JSON object with this shape:
{{
  "window_start": "ISO string",
  "window_end": "ISO string",
  "summary_markdown": "High-level narrative summary in Markdown...",
  "key_metrics_markdown": "- bullet list of key metrics...",
  "noteworthy_issues": [
    {{
      "identifier": "ENG-123",
      "title": "Fix login crash",
      "url": "https://...",
      "status": "created|completed|reopened|stale|in_progress|...",
      "team_key": "ENG",
      "assignee": "Alice",
      "priority": 2
    }}
  ],
  "stale_issues": [
    {{
      "identifier": "ENG-99",
      "title": "Old untriaged bug",
      "url": "https://...",
      "status": "stale",
      "team_key": "ENG",
      "assignee": "Bob",
      "priority": 3
    }}
  ],
  "suggested_slack_threads": [
    {{
      "channel_hint": "#engineering or #product, or null if unsure",
      "title": "Release recap for {{date}}",
      "message_markdown": "Full Markdown message that could be pasted into Slack...",
      "related_issue_identifiers": ["ENG-123", "ENG-456"]
    }}
  ]
}}

Rules:
- Respond with ONE JSON object only. No extra text, no markdown fences.
- The JSON must parse with standard JSON parsers.
- Use the input JSON data to drive your summary and suggestions.
- Summaries should be concise but informative for busy engineers.

Here is the input data for the window you must summarize:
{prompt_context}
"##
        );

        let body = json!({
            "model": self.model,
            "temperature": self.temperature,
            "messages": [
                {"role": "system", "content": "You are an assistant that writes structured JSON summaries for Linear issue activity."},
                {"role": "user", "content": prompt}
            ]
        });

        let url = format!("{}/chat/completions", self.api_base);
        let mut request = self
            .http
            .post(&url)
            .header("Content-Type", "application/json");

        if let Some(key) = &self.api_key {
            request = request.header("Authorization", format!("Bearer {}", key));
        }

        let response = request
            .json(&body)
            .send()
            .with_context(|| format!("Failed to send chat completion request to {}", url))?;

        let status = response.status();
        if !status.is_success() {
            let body_text = response.text().unwrap_or_default();
            anyhow::bail!("LLM server returned status {}: {}", status, body_text);
        }

        let completion: ChatCompletionResponse = response
            .json()
            .context("Failed to parse chat completion response JSON")?;

        let raw_content = completion
            .choices
            .into_iter()
            .next()
            .and_then(|choice| choice.message.content)
            .ok_or_else(|| anyhow!("LLM response did not include message content"))?;

        serde_json::from_str::<LlmLinearWindowSummary>(&raw_content).with_context(|| {
            format!(
                "LLM response was not valid JSON. Snippet: {}",
                preview(&raw_content)
            )
        })
    }
}

#[derive(Deserialize)]
struct ChatCompletionResponse {
    choices: Vec<ChatChoice>,
}

#[derive(Deserialize)]
struct ChatChoice {
    message: ChatMessageContent,
}

#[derive(Deserialize)]
struct ChatMessageContent {
    content: Option<String>,
}

pub fn format_thread_for_llm(messages: &[SlackMessage]) -> String {
    let mut ordered: Vec<&SlackMessage> = messages.iter().collect();
    ordered.sort_by(|a, b| compare_ts(&a.ts, &b.ts));

    ordered
        .into_iter()
        .map(format_single_message)
        .collect::<Vec<_>>()
        .join("\n")
}

pub fn load_thread_from_mirror(
    mirror_root: &Path,
    conversation_id: &str,
    thread_ts: &str,
) -> Result<SlackThread> {
    let path = conversation_path(mirror_root, conversation_id);
    let file = File::open(&path).with_context(|| {
        format!(
            "Unable to open conversation mirror for {} at {}",
            conversation_id,
            path.display()
        )
    })?;
    let reader = BufReader::new(file);

    let mut messages = Vec::new();
    for (idx, line) in reader.lines().enumerate() {
        let line = line
            .with_context(|| format!("Failed to read line {} from {}", idx + 1, path.display()))?;

        if line.trim().is_empty() {
            continue;
        }

        let message: SlackMessage = serde_json::from_str(&line).with_context(|| {
            format!(
                "Failed to parse Slack message on line {} from {}",
                idx + 1,
                path.display()
            )
        })?;

        if message.ts == thread_ts || message.thread_ts.as_deref() == Some(thread_ts) {
            messages.push(message);
        }
    }

    if messages.is_empty() {
        anyhow::bail!(
            "No messages found for conversation {} thread {} in {}",
            conversation_id,
            thread_ts,
            path.display()
        );
    }

    messages.sort_by(|a, b| compare_ts(&a.ts, &b.ts));

    Ok(SlackThread {
        conversation_id: conversation_id.to_string(),
        thread_ts: thread_ts.to_string(),
        messages,
    })
}

pub fn suggestion_schema() -> JSONSchemaSpec {
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

fn compare_ts(a: &str, b: &str) -> Ordering {
    match (a.parse::<f64>(), b.parse::<f64>()) {
        (Ok(lhs), Ok(rhs)) => lhs.partial_cmp(&rhs).unwrap_or_else(|| a.cmp(b)),
        _ => a.cmp(b),
    }
}

fn format_single_message(message: &SlackMessage) -> String {
    let user = message
        .user
        .as_deref()
        .map(|u| format!("@{u}"))
        .unwrap_or_else(|| "<unknown>".to_string());
    let text = message
        .text
        .as_deref()
        .map(sanitize_text)
        .unwrap_or_else(|| "<no text>".to_string());
    format!("[{}] {}: {}", message.ts, user, text)
}

fn sanitize_text(input: &str) -> String {
    input
        .replace('\n', " ")
        .replace('\r', " ")
        .trim()
        .to_string()
}

fn conversation_path(root: &Path, conversation_id: &str) -> PathBuf {
    root.join(CONVERSATIONS_DIR)
        .join(format!("{}.{}", conversation_id, JSONL_EXTENSION))
}

fn build_prompt(thread_text: &str, schema_json: &str) -> String {
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

fn preview(text: &str) -> String {
    const MAX: usize = 200;
    if text.len() <= MAX {
        text.to_string()
    } else {
        format!("{}…", &text[..MAX])
    }
}

fn build_window_prompt_context(window: &LinearWindowContext) -> Result<String> {
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

    fn to_payload(issue: &crate::linear_analysis::LinearIssueSummary) -> LinearIssueSummaryPayload {
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
