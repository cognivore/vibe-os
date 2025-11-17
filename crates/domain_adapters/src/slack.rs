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

    pub fn load_thread(&self, channel_id: &str, thread_ts: &str) -> Result<Vec<EventEnvelope>> {
        let threads_dir = self.root.join(THREADS_DIR);
        let filename = thread_file_name(channel_id, thread_ts);
        let direct = threads_dir.join(&filename);
        let fallback = threads_dir.join(channel_id).join(&filename);
        let path = if direct.exists() {
            direct
        } else if fallback.exists() {
            fallback
        } else {
            anyhow::bail!(
                "thread file not found for channel {} and ts {}",
                channel_id,
                thread_ts
            );
        };
        read_thread_file(&path, channel_id, thread_ts, None)
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

fn thread_file_name(channel_id: &str, thread_ts: &str) -> String {
    let sanitized = thread_ts.replace('.', "_");
    format!("{}_{}.jsonl", channel_id, sanitized)
}

fn parse_thread_file_info(path: &Path, fallback_channel: Option<&str>) -> Option<(String, String)> {
    let stem = path.file_stem()?.to_string_lossy();
    let parts: Vec<&str> = stem.split('_').collect();
    if parts.len() >= 3 {
        let channel_id = parts[0].to_string();
        let thread_ts = format!("{}.{}", parts[1], parts[2]);
        return Some((channel_id, thread_ts));
    }
    if parts.len() >= 2 {
        if let Some(channel) = fallback_channel {
            let thread_ts = format!("{}.{}", parts[0], parts[1]);
            return Some((channel.to_string(), thread_ts));
        }
    }
    None
}

fn read_thread_file(
    path: &Path,
    channel_id: &str,
    thread_ts: &str,
    window: Option<&TimeWindow>,
) -> Result<Vec<EventEnvelope>> {
    let file = File::open(path).with_context(|| format!("failed to open {}", path.display()))?;
    let reader = BufReader::new(file);
    let mut envelopes = Vec::new();
    let mut has_window_hit = window.is_none();

    for line in reader.lines() {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }
        let record: SlackMessageRecord = serde_json::from_str(&line).with_context(|| {
            format!("failed to parse Slack thread message in {}", path.display())
        })?;
        let mut envelope = match record.into_envelope(channel_id) {
            Some(env) => env,
            None => continue,
        };
        envelope.kind = "slack.thread_message".into();
        envelope.entity_id = Some(format!("{channel_id}:{thread_ts}"));
        if let Some(window) = window {
            if window.contains(&envelope.at) {
                has_window_hit = true;
            }
        }
        envelopes.push(envelope);
    }

    if window.is_some() && !has_window_hit {
        return Ok(Vec::new());
    }

    envelopes.sort_by(|a, b| a.at.cmp(&b.at));
    Ok(envelopes)
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

    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let file_type = entry.file_type()?;
        let path = entry.path();

        if file_type.is_file() {
            if path.extension().and_then(|ext| ext.to_str()) != Some("jsonl") {
                continue;
            }
            if let Some((channel_id, thread_ts)) = parse_thread_file_info(&path, None) {
                let mut events = read_thread_file(&path, &channel_id, &thread_ts, Some(window))?;
                envelopes.append(&mut events);
            }
        } else if file_type.is_dir() {
            let channel_id = entry.file_name().to_string_lossy().to_string();
            for nested in std::fs::read_dir(&path)? {
                let nested = nested?;
                if !nested.file_type()?.is_file() {
                    continue;
                }
                let nested_path = nested.path();
                if nested_path.extension().and_then(|ext| ext.to_str()) != Some("jsonl") {
                    continue;
                }
                if let Some((channel_id, thread_ts)) =
                    parse_thread_file_info(&nested_path, Some(&channel_id))
                {
                    let mut events =
                        read_thread_file(&nested_path, &channel_id, &thread_ts, Some(window))?;
                    envelopes.append(&mut events);
                }
            }
        }
    }

    Ok(envelopes)
}
