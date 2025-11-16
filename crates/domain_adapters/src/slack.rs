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

const CONVERSATIONS_DIR: &str = "conversations";
const THREADS_DIR: &str = "threads";

pub struct SlackAdapter {
    root: PathBuf,
}

impl SlackAdapter {
    pub fn new(root: impl Into<PathBuf>) -> Self {
        Self { root: root.into() }
    }
}

#[derive(Debug, Deserialize, Clone)]
struct SlackMessageRecord {
    ts: String,
    #[serde(default)]
    user: Option<String>,
    #[serde(default)]
    text: Option<String>,
    #[serde(default)]
    thread_ts: Option<String>,
    #[serde(flatten)]
    extra: serde_json::Map<String, Value>,
}

impl SlackMessageRecord {
    fn into_envelope(self, channel_id: &str) -> Option<EventEnvelope> {
        let at = slack_ts_to_datetime(&self.ts)?;
        let mut data_map = self.extra;
        data_map.insert("ts".into(), Value::String(self.ts.clone()));
        if let Some(user) = &self.user {
            data_map.insert("user".into(), Value::String(user.clone()));
        }
        if let Some(text) = &self.text {
            data_map.insert("text".into(), Value::String(text.clone()));
        }
        if let Some(thread_ts) = &self.thread_ts {
            data_map.insert("thread_ts".into(), Value::String(thread_ts.clone()));
        }
        data_map.insert("channel".into(), Value::String(channel_id.to_string()));

        let summary = self
            .text
            .clone()
            .and_then(|t| {
                let trimmed = t.trim();
                if trimmed.is_empty() {
                    None
                } else {
                    Some(trimmed.chars().take(140).collect::<String>())
                }
            })
            .unwrap_or_else(|| format!("Message in {channel_id}"));

        Some(EventEnvelope {
            id: format!("slack:{channel_id}:{}", self.ts),
            domain: Domain::Slack,
            at,
            kind: "slack.message".into(),
            summary,
            entity_id: Some(channel_id.to_string()),
            data: Value::Object(data_map),
            actor_persona_key: self.user.as_ref().map(|user| PersonaKey {
                domain: Domain::Slack,
                local_id: user.clone(),
            }),
            actor_persona_id: None,
            actor_identity_id: None,
        })
    }
}

fn slack_ts_to_datetime(ts: &str) -> Option<DateTime<Utc>> {
    let mut parts = ts.split('.');
    let seconds = parts.next()?.parse::<i64>().ok()?;
    let subsecs = parts.next().unwrap_or("0");
    let micros = subsecs.parse::<u32>().unwrap_or(0);
    let nanos = micros.saturating_mul(1_000);
    DateTime::<Utc>::from_timestamp(seconds, nanos)
}

impl EventAdapter for SlackAdapter {
    fn domain(&self) -> Domain {
        Domain::Slack
    }

    fn load_events(&self, window: &TimeWindow) -> Result<Vec<EventEnvelope>> {
        let mut envelopes = Vec::new();
        let conversations_dir = self.root.join(CONVERSATIONS_DIR);
        if conversations_dir.exists() {
            envelopes.extend(read_dir_events(&conversations_dir, window)?);
        }

        // Threads: load ALL messages for threads that have ANY message in the window.
        // This ensures we show complete thread history even if some messages are old.
        let threads_dir = self.root.join(THREADS_DIR);
        if threads_dir.exists() {
            envelopes.extend(read_thread_events(&threads_dir, window)?);
        }

        envelopes.sort_by(|a, b| a.at.cmp(&b.at));
        Ok(envelopes)
    }
}

fn read_dir_events(dir: &Path, window: &TimeWindow) -> Result<Vec<EventEnvelope>> {
    let mut envelopes = Vec::new();
    if !dir.exists() {
        return Ok(envelopes);
    }

    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.extension().and_then(|ext| ext.to_str()) != Some("jsonl") {
            continue;
        }
        let channel_id = path
            .file_stem()
            .and_then(|stem| stem.to_str())
            .unwrap_or("unknown")
            .to_string();
        let file =
            File::open(&path).with_context(|| format!("failed to open {}", path.display()))?;
        let reader = BufReader::new(file);
        for line in reader.lines() {
            let line = line?;
            if line.trim().is_empty() {
                continue;
            }
            let record: SlackMessageRecord = serde_json::from_str(&line)
                .with_context(|| format!("failed to parse Slack message in {}", path.display()))?;
            if let Some(envelope) = record.into_envelope(&channel_id) {
                if window.contains(&envelope.at) {
                    envelopes.push(envelope);
                }
            }
        }
    }
    Ok(envelopes)
}

fn read_thread_events(dir: &Path, window: &TimeWindow) -> Result<Vec<EventEnvelope>> {
    let mut envelopes = Vec::new();
    if !dir.exists() {
        return Ok(envelopes);
    }

    for channel_entry in std::fs::read_dir(dir)? {
        let channel_entry = channel_entry?;
        if !channel_entry.file_type()?.is_dir() {
            continue;
        }
        let channel_path = channel_entry.path();
        let channel_id = channel_path
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown")
            .to_string();

        for thread_entry in std::fs::read_dir(&channel_path)? {
            let thread_entry = thread_entry?;
            if !thread_entry.file_type()?.is_file() {
                continue;
            }
            let path = thread_entry.path();
            if path.extension().and_then(|ext| ext.to_str()) != Some("jsonl") {
                continue;
            }

            // First pass: check if ANY message in this thread is in the window
            let file_check = File::open(&path).with_context(|| format!("failed to open {}", path.display()))?;
            let reader_check = BufReader::new(file_check);
            let mut has_message_in_window = false;
            for line in reader_check.lines() {
                let line = line?;
                if line.trim().is_empty() {
                    continue;
                }
                if let Ok(record) = serde_json::from_str::<SlackMessageRecord>(&line) {
                    if let Some(at) = slack_ts_to_datetime(&record.ts) {
                        if window.contains(&at) {
                            has_message_in_window = true;
                            break;
                        }
                    }
                }
            }

            // If no message in window, skip this entire thread
            if !has_message_in_window {
                continue;
            }

            // Second pass: load ALL messages from this thread (not just those in window)
            let file =
                File::open(&path).with_context(|| format!("failed to open {}", path.display()))?;
            let reader = BufReader::new(file);
            for line in reader.lines() {
                let line = line?;
                if line.trim().is_empty() {
                    continue;
                }
                let record: SlackMessageRecord =
                    serde_json::from_str(&line).with_context(|| {
                        format!("failed to parse Slack thread message in {}", path.display())
                    })?;
                let thread_ts = record.thread_ts.clone();
                if let Some(mut envelope) = record.into_envelope(&channel_id) {
                    envelope.kind = "slack.thread_message".into();
                    if let Some(thread_ts) = thread_ts {
                        envelope.entity_id = Some(format!("{channel_id}:{thread_ts}"));
                    }
                    // Load ALL messages, not just those in window
                    envelopes.push(envelope);
                }
            }
        }
    }

    Ok(envelopes)
}
