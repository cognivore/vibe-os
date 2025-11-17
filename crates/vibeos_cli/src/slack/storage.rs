use std::fs::{self, File, OpenOptions};
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::Path;

use anyhow::{Context, Result};
use serde::de::DeserializeOwned;
use serde::Serialize;
use serde_json::Value;

use super::mirror::compare_ts;
use super::types::{SlackMessage, SlackUserSnapshot, JSONL_EXTENSION};

pub(super) fn ensure_dir(path: &Path) -> Result<()> {
    fs::create_dir_all(path)
        .with_context(|| format!("Failed to create directory {}", path.display()))
}

pub(super) fn jsonl_name(stem: &str) -> String {
    format!("{}.{}", stem, JSONL_EXTENSION)
}

pub fn thread_filename(channel_id: &str, thread_ts: &str) -> String {
    let sanitized_ts = thread_ts.replace('.', "_");
    format!("{}_{}.{}", channel_id, sanitized_ts, JSONL_EXTENSION)
}

pub(super) fn latest_ts_from_file(path: &Path) -> Result<Option<String>> {
    if !path.exists() {
        return Ok(None);
    }

    let messages: Vec<SlackMessage> = read_jsonl(path)?;
    Ok(latest_ts(&messages))
}

pub(super) fn read_all_messages(path: &Path) -> Result<Vec<SlackMessage>> {
    if !path.exists() {
        return Ok(Vec::new());
    }
    read_jsonl(path)
}

pub(super) fn append_jsonl<T: Serialize>(path: &Path, entries: &[T]) -> Result<()> {
    if entries.is_empty() {
        return Ok(());
    }

    let file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)
        .with_context(|| format!("Failed to open file {} for appending", path.display()))?;

    let mut writer = BufWriter::new(file);
    for entry in entries {
        serde_json::to_writer(&mut writer, entry).with_context(|| {
            format!(
                "Failed to serialize entry while appending to {}",
                path.display()
            )
        })?;
        writer.write_all(b"\n").with_context(|| {
            format!(
                "Failed to write newline while appending to {}",
                path.display()
            )
        })?;
    }

    writer
        .flush()
        .with_context(|| format!("Failed to flush {}", path.display()))
}

pub fn write_jsonl<T: Serialize>(path: &Path, entries: &[T]) -> Result<()> {
    let file =
        File::create(path).with_context(|| format!("Failed to create file {}", path.display()))?;
    let mut writer = BufWriter::new(file);
    for entry in entries {
        serde_json::to_writer(&mut writer, entry)
            .with_context(|| format!("Failed to serialize entry to {}", path.display()))?;
        writer
            .write_all(b"\n")
            .with_context(|| format!("Failed to write newline to {}", path.display()))?;
    }
    writer
        .flush()
        .with_context(|| format!("Failed to flush {}", path.display()))
}

pub fn read_jsonl<T>(path: &Path) -> Result<Vec<T>>
where
    T: DeserializeOwned,
{
    let file =
        File::open(path).with_context(|| format!("Failed to open file {}", path.display()))?;
    let reader = BufReader::new(file);
    let mut entries = Vec::new();

    for (line_idx, line) in reader.lines().enumerate() {
        let line = line.with_context(|| {
            format!(
                "Failed to read line {} from {}",
                line_idx + 1,
                path.display()
            )
        })?;

        if line.trim().is_empty() {
            continue;
        }

        let entry = serde_json::from_str(&line).with_context(|| {
            format!(
                "Failed to deserialize line {} from {}",
                line_idx + 1,
                path.display()
            )
        })?;

        entries.push(entry);
    }

    Ok(entries)
}

pub(super) fn profile_changed(path: &Path, new_user: &Value) -> Result<bool> {
    if let Some(last) = read_last_snapshot::<SlackUserSnapshot>(path)? {
        Ok(last.user != *new_user)
    } else {
        Ok(true)
    }
}

pub(super) fn read_last_snapshot<T>(path: &Path) -> Result<Option<T>>
where
    T: DeserializeOwned,
{
    if !path.exists() {
        return Ok(None);
    }

    let file = File::open(path).with_context(|| format!("Failed to open {}", path.display()))?;
    let reader = BufReader::new(file);
    let mut last = None;
    for line in reader.lines() {
        let line = line.with_context(|| format!("Failed to read line from {}", path.display()))?;
        if line.trim().is_empty() {
            continue;
        }
        let entry: T = serde_json::from_str(&line)
            .with_context(|| format!("Failed to parse snapshot from {}", path.display()))?;
        last = Some(entry);
    }
    Ok(last)
}

fn latest_ts(messages: &[SlackMessage]) -> Option<String> {
    messages
        .iter()
        .max_by(|a, b| compare_ts(&a.ts, &b.ts))
        .map(|msg| msg.ts.clone())
}
