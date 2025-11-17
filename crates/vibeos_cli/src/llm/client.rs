use anyhow::{anyhow, Context, Result};
use reqwest::blocking::Client as BlockingClient;
use serde::Deserialize;
use serde_json::json;

use crate::config::Config;
use crate::linear_analysis::LinearWindowContext;
use crate::linear_sync::LinearIssueSnapshot;

use super::prompts::{build_prompt, build_window_prompt_context, suggestion_schema, SYSTEM_PROMPT};
use super::types::{LlmLinearWindowSummary, LlmSuggestionResponse};

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

fn preview(text: &str) -> String {
    const MAX: usize = 200;
    if text.len() <= MAX {
        text.to_string()
    } else {
        format!("{}…", &text[..MAX])
    }
}
