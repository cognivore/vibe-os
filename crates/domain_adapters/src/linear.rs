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

impl EventAdapter for LinearAdapter {
    fn domain(&self) -> Domain {
        Domain::Linear
    }

    fn load_events(&self, window: &TimeWindow) -> Result<Vec<EventEnvelope>> {
        let mut envelopes = Vec::new();
        for path in event_logs(&self.root)? {
            let file =
                File::open(&path).with_context(|| format!("failed to open {}", path.display()))?;
            let reader = BufReader::new(file);
            for line in reader.lines() {
                let line = line?;
                if line.trim().is_empty() {
                    continue;
                }
                let value: Value = serde_json::from_str(&line).with_context(|| {
                    format!("failed to parse Linear event in {}", path.display())
                })?;
                // Deserialize into record while retaining original JSON.
                let record: LinearEventRecord = serde_json::from_value(value.clone())
                    .with_context(|| {
                        format!("failed to map Linear event schema in {}", path.display())
                    })?;
                let envelope = record.into_envelope();
                if window.contains(&envelope.at) {
                    envelopes.push(EventEnvelope {
                        data: value,
                        ..envelope
                    });
                }
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
