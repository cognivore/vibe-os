use std::collections::{HashMap, HashSet};
use std::ffi::OsStr;
use std::fs::{self, File, OpenOptions};
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use chrono::{DateTime, Utc};
use serde::Serialize;

use crate::linear_sync::models::{
    LinearCommentRecord, LinearIssueEvent, LinearIssueSnapshot, LinearMirrorMeta, LinearUserNode,
    LinearUserSnapshot,
};

pub(super) const USERS_DIR: &str = "users";
const EVENT_ROTATE_TRIGGER_BYTES: u64 = 1_000_000;
const EVENT_LOG_PREFIX: &str = "events";
const EVENT_LOG_EXTENSION: &str = "jsonl";

pub(super) fn read_last_sync(path: &Path) -> Result<Option<DateTime<Utc>>> {
    if !path.exists() {
        return Ok(None);
    }

    let meta_file = File::open(path)
        .with_context(|| format!("failed to open {} for reading", path.display()))?;
    let meta: LinearMirrorMeta =
        serde_json::from_reader(meta_file).with_context(|| "failed to parse meta.json")?;
    Ok(meta.last_full_sync_at)
}

pub(super) fn write_meta(path: &Path, meta: &LinearMirrorMeta) -> Result<()> {
    let file = File::create(path)
        .with_context(|| format!("failed to open {} for writing", path.display()))?;
    serde_json::to_writer_pretty(file, meta)
        .with_context(|| format!("failed to write meta at {}", path.display()))
}

pub(super) fn read_existing_issues(path: &Path) -> Result<HashMap<String, LinearIssueSnapshot>> {
    if !path.exists() {
        return Ok(HashMap::new());
    }

    let file = File::open(path)
        .with_context(|| format!("failed to open {} for reading", path.display()))?;
    let reader = BufReader::new(file);

    let mut issues = HashMap::new();
    for (idx, line) in reader.lines().enumerate() {
        let line = line.with_context(|| {
            format!(
                "failed to read issue line {} from {}",
                idx + 1,
                path.display()
            )
        })?;
        if line.trim().is_empty() {
            continue;
        }
        let issue: LinearIssueSnapshot = serde_json::from_str(&line).with_context(|| {
            format!(
                "failed to parse issue line {} from {}",
                idx + 1,
                path.display()
            )
        })?;
        issues.insert(issue.id.clone(), issue);
    }

    Ok(issues)
}

pub(super) fn read_existing_comments(
    path: &Path,
) -> Result<HashMap<String, Vec<LinearCommentRecord>>> {
    let mut comments = HashMap::new();
    if !path.exists() {
        return Ok(comments);
    }

    let file = File::open(path)
        .with_context(|| format!("failed to open {} for reading", path.display()))?;
    let reader = BufReader::new(file);

    for (idx, line) in reader.lines().enumerate() {
        let line = line.with_context(|| {
            format!(
                "failed to read comment line {} from {}",
                idx + 1,
                path.display()
            )
        })?;
        if line.trim().is_empty() {
            continue;
        }
        let comment: LinearCommentRecord = serde_json::from_str(&line).with_context(|| {
            format!(
                "failed to parse comment line {} from {}",
                idx + 1,
                path.display()
            )
        })?;
        comments
            .entry(comment.issue_id.clone())
            .or_insert_with(Vec::new)
            .push(comment);
    }

    Ok(comments)
}

pub(super) fn write_comments(
    path: &Path,
    comments: &HashMap<String, Vec<LinearCommentRecord>>,
) -> Result<()> {
    let file = File::create(path)
        .with_context(|| format!("failed to open {} for writing", path.display()))?;
    let mut writer = BufWriter::new(file);
    for list in comments.values() {
        for comment in list {
            serde_json::to_writer(&mut writer, comment).with_context(|| {
                format!(
                    "failed to serialize comment {} to {}",
                    comment.id,
                    path.display()
                )
            })?;
            writer
                .write_all(b"\n")
                .with_context(|| format!("failed to write newline to {}", path.display()))?;
        }
    }
    writer
        .flush()
        .with_context(|| format!("failed to flush {}", path.display()))
}

pub(super) fn write_issues(
    path: &Path,
    issues: &HashMap<String, LinearIssueSnapshot>,
) -> Result<()> {
    let file = File::create(path)
        .with_context(|| format!("failed to open {} for writing", path.display()))?;
    let mut writer = BufWriter::new(file);

    let mut entries: Vec<_> = issues.values().collect();
    entries.sort_by(|a, b| a.id.cmp(&b.id));

    for issue in entries {
        serde_json::to_writer(&mut writer, issue).with_context(|| {
            format!(
                "failed to serialize issue {} to {}",
                issue.id,
                path.display()
            )
        })?;
        writer
            .write_all(b"\n")
            .with_context(|| format!("failed to write newline while writing {}", path.display()))?;
    }

    writer
        .flush()
        .with_context(|| format!("failed to flush {}", path.display()))
}

pub(super) fn store_user_snapshot(dir: &Path, user: LinearUserNode) -> Result<bool> {
    let path = dir.join(format!("{}.jsonl", user.id));
    let snapshot = LinearUserSnapshot {
        fetched_at: Utc::now(),
        user,
    };
    if linear_user_changed(&path, &snapshot.user)? {
        append_snapshot(&path, &snapshot)?;
        Ok(true)
    } else {
        Ok(false)
    }
}

fn linear_user_changed(path: &Path, new_user: &LinearUserNode) -> Result<bool> {
    if let Some(last) = read_last_user_snapshot(path)? {
        Ok(last.user != *new_user)
    } else {
        Ok(true)
    }
}

fn read_last_user_snapshot(path: &Path) -> Result<Option<LinearUserSnapshot>> {
    if !path.exists() {
        return Ok(None);
    }

    let file = File::open(path)
        .with_context(|| format!("failed to open {} for reading", path.display()))?;
    let reader = BufReader::new(file);
    let mut last = None;
    for line in reader.lines() {
        let line = line.with_context(|| format!("failed to read line from {}", path.display()))?;
        if line.trim().is_empty() {
            continue;
        }
        let snapshot: LinearUserSnapshot = serde_json::from_str(&line)
            .with_context(|| format!("failed to parse snapshot from {}", path.display()))?;
        last = Some(snapshot);
    }
    Ok(last)
}

fn append_snapshot<T: Serialize>(path: &Path, entry: &T) -> Result<()> {
    let file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)
        .with_context(|| format!("failed to open {} for appending", path.display()))?;
    let mut writer = BufWriter::new(file);
    serde_json::to_writer(&mut writer, entry).with_context(|| {
        format!(
            "failed to serialize snapshot while appending to {}",
            path.display()
        )
    })?;
    writer.write_all(b"\n").with_context(|| {
        format!(
            "failed to write newline while appending to {}",
            path.display()
        )
    })?;
    writer.flush().with_context(|| {
        format!(
            "failed to flush snapshot while appending to {}",
            path.display()
        )
    })
}

pub(super) fn read_existing_event_ids(dir: &Path) -> Result<HashSet<String>> {
    migrate_legacy_event_log(dir)?;

    let mut ids = HashSet::new();
    if !dir.exists() {
        return Ok(ids);
    }

    for entry in fs::read_dir(dir).with_context(|| "failed to read mirror directory")? {
        let entry = entry?;
        let path = entry.path();
        if is_event_log_file(&path) {
            read_event_ids_from_file(&path, &mut ids)?;
        }
    }

    Ok(ids)
}

fn read_event_ids_from_file(path: &Path, ids: &mut HashSet<String>) -> Result<()> {
    let file = File::open(path)
        .with_context(|| format!("failed to open {} for reading", path.display()))?;
    let reader = BufReader::new(file);

    for (idx, line) in reader.lines().enumerate() {
        let line = line.with_context(|| {
            format!(
                "failed to read event line {} from {}",
                idx + 1,
                path.display()
            )
        })?;
        if line.trim().is_empty() {
            continue;
        }
        let event: LinearIssueEvent = serde_json::from_str(&line).with_context(|| {
            format!(
                "failed to parse event line {} from {}",
                idx + 1,
                path.display()
            )
        })?;
        ids.insert(event.id);
    }

    Ok(())
}

pub(super) fn append_events_with_rotation(dir: &Path, events: &[LinearIssueEvent]) -> Result<()> {
    if events.is_empty() {
        return Ok(());
    }

    migrate_legacy_event_log(dir)?;
    ensure_active_event_log(dir)?;
    rotate_if_needed(dir)?;

    let active_path = active_event_log_path(dir);
    let file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(&active_path)
        .with_context(|| format!("failed to open {} for appending", active_path.display()))?;
    let mut writer = BufWriter::new(file);

    for event in events {
        serde_json::to_writer(&mut writer, event).with_context(|| {
            format!(
                "failed to serialize event {} while appending to {}",
                event.id,
                active_path.display()
            )
        })?;
        writer.write_all(b"\n").with_context(|| {
            format!(
                "failed to write newline while appending to {}",
                active_path.display()
            )
        })?;
    }

    writer.flush().with_context(|| {
        format!(
            "failed to flush appended events to {}",
            active_path.display()
        )
    })?;

    rotate_if_needed(dir)?;

    Ok(())
}

fn rotate_if_needed(dir: &Path) -> Result<()> {
    let active_path = active_event_log_path(dir);
    if !active_path.exists() {
        ensure_active_event_log(dir)?;
        return Ok(());
    }

    let size = fs::metadata(&active_path)
        .with_context(|| format!("failed to read metadata for {}", active_path.display()))?
        .len();

    if size < EVENT_ROTATE_TRIGGER_BYTES {
        return Ok(());
    }

    rotate_event_logs(dir)?;
    Ok(())
}

fn rotate_event_logs(dir: &Path) -> Result<()> {
    ensure_active_event_log(dir)?;

    let mut logs: Vec<(u32, PathBuf)> = fs::read_dir(dir)
        .with_context(|| "failed to read mirror directory for rotation")?
        .filter_map(|entry| {
            entry.ok().and_then(|e| {
                let path = e.path();
                let name = path.file_name()?.to_string_lossy().into_owned();
                event_log_number(&name).map(|num| (num, path))
            })
        })
        .collect();

    logs.sort_by(|a, b| b.0.cmp(&a.0));

    for (number, path) in logs {
        let new_path = dir.join(event_log_filename(number + 1));
        fs::rename(&path, &new_path).with_context(|| {
            format!(
                "failed to rotate {} to {}",
                path.display(),
                new_path.display()
            )
        })?;
    }

    ensure_active_event_log(dir)?;
    Ok(())
}

fn migrate_legacy_event_log(dir: &Path) -> Result<()> {
    let legacy = dir.join(format!("{EVENT_LOG_PREFIX}.{EVENT_LOG_EXTENSION}"));
    let active = active_event_log_path(dir);

    if legacy.exists() && !active.exists() {
        fs::rename(&legacy, &active).with_context(|| {
            format!(
                "failed to migrate legacy event log {} to {}",
                legacy.display(),
                active.display()
            )
        })?;
    }

    Ok(())
}

fn ensure_active_event_log(dir: &Path) -> Result<()> {
    let path = active_event_log_path(dir);
    if path.exists() {
        return Ok(());
    }
    File::create(&path).with_context(|| format!("failed to create {}", path.display()))?;
    Ok(())
}

fn active_event_log_path(dir: &Path) -> PathBuf {
    dir.join(event_log_filename(0))
}

fn is_event_log_file(path: &Path) -> bool {
    path.file_name()
        .and_then(OsStr::to_str)
        .and_then(event_log_number)
        .is_some()
}

fn event_log_number(name: &str) -> Option<u32> {
    let legacy = format!("{EVENT_LOG_PREFIX}.{EVENT_LOG_EXTENSION}");
    if name == legacy {
        return Some(0);
    }

    let prefix = format!("{EVENT_LOG_PREFIX}.");
    let suffix = format!(".{EVENT_LOG_EXTENSION}");
    if name.starts_with(&prefix) && name.ends_with(&suffix) {
        let number_part = &name[prefix.len()..name.len() - suffix.len()];
        return number_part.parse().ok();
    }
    None
}

fn event_log_filename(number: u32) -> String {
    format!("{EVENT_LOG_PREFIX}.{number}.{EVENT_LOG_EXTENSION}")
}
