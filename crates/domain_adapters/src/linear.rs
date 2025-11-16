use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use chrono::{DateTime, Utc};
use serde::Deserialize;
use serde_json::Value;

use core_model::adapters::EventAdapter;
use core_model::domain::Domain;
use core_model::event::EventEnvelope;
use core_model::time::TimeWindow;
use core_persona::persona::PersonaKey;

const EVENTS_PREFIX: &str = "events";
const EVENTS_EXTENSION: &str = "jsonl";
const COMMENTS_FILE: &str = "comments.jsonl";

pub struct LinearAdapter {
    root: PathBuf,
}

impl LinearAdapter {
    pub fn new(root: impl Into<PathBuf>) -> Self {
        Self { root: root.into() }
    }
}

#[derive(Debug, Deserialize)]
struct LinearEventRecord {
    id: String,
    issue_id: String,
    #[serde(default)]
    issue_identifier: Option<String>,
    event_kind: String,
    created_at: DateTime<Utc>,
    #[serde(default)]
    actor_id: Option<String>,
    #[serde(default)]
    actor_name: Option<String>,
    #[serde(flatten)]
    rest: serde_json::Map<String, Value>,
}

#[derive(Debug, Deserialize)]
struct LinearCommentRecord {
    id: String,
    issue_id: String,
    #[serde(default)]
    issue_identifier: Option<String>,
    body: String,
    #[serde(default)]
    url: Option<String>,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
    #[serde(default)]
    user_id: Option<String>,
    #[serde(default)]
    user_name: Option<String>,
    #[serde(default)]
    user_display_name: Option<String>,
}

#[derive(Debug, Deserialize)]
struct IssueSnapshotRow {
    id: String,
    title: Option<String>,
    description: Option<String>,
    identifier: Option<String>,
    url: Option<String>,
}

#[derive(Debug, Clone)]
struct LinearIssueSummary {
    title: Option<String>,
    description: Option<String>,
    identifier: Option<String>,
    url: Option<String>,
}

impl LinearEventRecord {
    fn into_envelope(self) -> EventEnvelope {
        let issue = self
            .issue_identifier
            .clone()
            .unwrap_or_else(|| self.issue_id.clone());
        let summary = format!("{} {}", issue, self.event_kind.replace('_', " "));
        let mut data_map = self.rest;
        data_map.insert("id".into(), Value::String(self.id.clone()));
        data_map.insert("issue_id".into(), Value::String(self.issue_id.clone()));
        if let Some(identifier) = &self.issue_identifier {
            data_map.insert("issue_identifier".into(), Value::String(identifier.clone()));
        }
        data_map.insert("event_kind".into(), Value::String(self.event_kind.clone()));
        data_map.insert(
            "created_at".into(),
            Value::String(self.created_at.to_rfc3339()),
        );
        if let Some(actor) = &self.actor_name {
            data_map.insert("actor_name".into(), Value::String(actor.clone()));
        }
        if let Some(actor_id) = &self.actor_id {
            data_map.insert("actor_id".into(), Value::String(actor_id.clone()));
        }

        EventEnvelope {
            id: format!("linear:{}", self.id),
            domain: Domain::Linear,
            at: self.created_at,
            kind: format!("linear.{}", self.event_kind),
            summary,
            entity_id: Some(issue),
            data: Value::Object(data_map),
            actor_persona_key: self.actor_id.as_ref().map(|id| PersonaKey {
                domain: Domain::Linear,
                local_id: id.clone(),
            }),
            actor_persona_id: None,
            actor_identity_id: None,
        }
    }
}

impl LinearCommentRecord {
    fn into_envelope(self, summary: Option<&LinearIssueSummary>) -> EventEnvelope {
        let mut data_map = serde_json::Map::new();
        data_map.insert("comment_body".into(), Value::String(self.body.clone()));
        if let Some(url) = &self.url {
            data_map.insert("comment_url".into(), Value::String(url.clone()));
        }
        data_map.insert("issue_id".into(), Value::String(self.issue_id.clone()));
        if let Some(identifier) = &self.issue_identifier {
            data_map.insert("issue_identifier".into(), Value::String(identifier.clone()));
        }
        if let Some(name) = &self.user_name {
            data_map.insert("actor_name".into(), Value::String(name.clone()));
        }
        if let Some(display) = &self.user_display_name {
            data_map.insert("actor_display_name".into(), Value::String(display.clone()));
        }
        if let Some(summary) = summary {
            if let Some(title) = &summary.title {
                data_map.insert("issue_title".into(), Value::String(title.clone()));
            }
            if let Some(description) = &summary.description {
                data_map.insert(
                    "issue_description".into(),
                    Value::String(description.clone()),
                );
            }
            if let Some(url) = &summary.url {
                data_map.insert("issue_url".into(), Value::String(url.clone()));
            }
        }
        let persona_key = self.user_id.as_ref().map(|id| PersonaKey {
            domain: Domain::Linear,
            local_id: id.clone(),
        });
        EventEnvelope {
            id: format!("linear_comment:{}", self.id),
            domain: Domain::Linear,
            at: self.updated_at,
            kind: "linear.comment".into(),
            summary: self.body.chars().take(140).collect(),
            entity_id: Some(self.issue_id.clone()),
            data: Value::Object(data_map),
            actor_persona_key: persona_key,
            actor_persona_id: None,
            actor_identity_id: None,
        }
    }
}

impl EventAdapter for LinearAdapter {
    fn domain(&self) -> Domain {
        Domain::Linear
    }

    fn load_events(&self, window: &TimeWindow) -> Result<Vec<EventEnvelope>> {
        let mut envelopes = Vec::new();
        let issue_summaries = load_issue_summaries(&self.root)?;
        let issue_comments = load_issue_comments(&self.root)?;
        for path in event_logs(&self.root)? {
            let file =
                File::open(&path).with_context(|| format!("failed to open {}", path.display()))?;
            let reader = BufReader::new(file);
            for line in reader.lines() {
                let line = line?;
                if line.trim().is_empty() {
                    continue;
                }
                let mut value: Value = serde_json::from_str(&line).with_context(|| {
                    format!("failed to parse Linear event in {}", path.display())
                })?;
                // Deserialize into record while retaining original JSON.
                let record: LinearEventRecord = serde_json::from_value(value.clone())
                    .with_context(|| {
                        format!("failed to map Linear event schema in {}", path.display())
                    })?;
                let issue_id_for_lookup = record.issue_id.clone();
                let envelope = record.into_envelope();
                if let Some(obj) = value.as_object_mut() {
                    if let Some(summary) = issue_summaries.get(&issue_id_for_lookup) {
                        if let Some(title) = &summary.title {
                            obj.insert("issue_title".into(), Value::String(title.clone()));
                        }
                        if let Some(description) = &summary.description {
                            obj.insert(
                                "issue_description".into(),
                                Value::String(description.clone()),
                            );
                        }
                        if let Some(url) = &summary.url {
                            obj.insert("issue_url".into(), Value::String(url.clone()));
                        }
                        if let Some(identifier) = &summary.identifier {
                            obj.insert(
                                "issue_identifier".into(),
                                Value::String(identifier.clone()),
                            );
                        }
                    }
                }
                if window.contains(&envelope.at) {
                    envelopes.push(EventEnvelope {
                        data: value,
                        ..envelope
                    });
                }
            }
        }
        for comment in issue_comments {
            let issue_id = comment.issue_id.clone();
            let envelope = comment.into_envelope(issue_summaries.get(&issue_id));
            if window.contains(&envelope.at) {
                envelopes.push(envelope);
            }
        }
        envelopes.sort_by(|a, b| a.at.cmp(&b.at));
        Ok(envelopes)
    }
}

fn event_logs(root: &Path) -> Result<Vec<PathBuf>> {
    let mut logs: Vec<(u32, PathBuf)> = Vec::new();
    if root.exists() {
        for entry in std::fs::read_dir(root)? {
            let path = entry?.path();
            if let Some(file_name) = path.file_name().and_then(|s| s.to_str()) {
                if let Some(num) = log_number(file_name) {
                    logs.push((num, path));
                }
            }
        }
    }
    logs.sort_by(|a, b| a.0.cmp(&b.0));
    Ok(logs.into_iter().map(|(_, path)| path).collect())
}

fn log_number(name: &str) -> Option<u32> {
    if name == format!("{EVENTS_PREFIX}.{EVENTS_EXTENSION}") {
        return Some(0);
    }
    let prefix = format!("{EVENTS_PREFIX}.");
    let suffix = format!(".{EVENTS_EXTENSION}");
    if name.starts_with(&prefix) && name.ends_with(&suffix) {
        let number_part = &name[prefix.len()..name.len() - suffix.len()];
        return number_part.parse().ok();
    }
    None
}

fn load_issue_summaries(root: &Path) -> Result<HashMap<String, LinearIssueSummary>> {
    let mut map = HashMap::new();
    let issues_path = root.join("issues.jsonl");
    if !issues_path.exists() {
        return Ok(map);
    }
    let file = File::open(&issues_path)
        .with_context(|| format!("failed to open {}", issues_path.display()))?;
    let reader = BufReader::new(file);
    for line in reader.lines() {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }
        let row: IssueSnapshotRow = serde_json::from_str(&line).with_context(|| {
            format!(
                "failed to parse issue snapshot line for {}",
                issues_path.display()
            )
        })?;
        map.insert(
            row.id.clone(),
            LinearIssueSummary {
                title: row.title.clone(),
                description: row.description.clone(),
                identifier: row.identifier.clone(),
                url: row.url.clone(),
            },
        );
    }
    Ok(map)
}

fn load_issue_comments(root: &Path) -> Result<Vec<LinearCommentRecord>> {
    let path = root.join(COMMENTS_FILE);
    if !path.exists() {
        return Ok(Vec::new());
    }
    let file = File::open(&path)
        .with_context(|| format!("failed to open {} for reading", path.display()))?;
    let reader = BufReader::new(file);
    let mut comments = Vec::new();
    for line in reader.lines() {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }
        let comment: LinearCommentRecord = serde_json::from_str(&line)
            .with_context(|| format!("failed to parse Linear comment from {}", path.display()))?;
        comments.push(comment);
    }
    Ok(comments)
}
