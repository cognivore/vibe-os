use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use chrono::{DateTime, Utc};
use serde::Deserialize;
use serde_json::Value;

const EVENTS_PREFIX: &str = "events";
const EVENTS_EXTENSION: &str = "jsonl";
const COMMENTS_FILE: &str = "comments.jsonl";

#[derive(Debug, Deserialize)]
pub struct LinearEventRecord {
    pub id: String,
    pub issue_id: String,
    #[serde(default)]
    pub issue_identifier: Option<String>,
    pub event_kind: String,
    pub created_at: DateTime<Utc>,
    #[serde(default)]
    pub actor_id: Option<String>,
    #[serde(default)]
    pub actor_name: Option<String>,
    #[serde(flatten)]
    pub rest: serde_json::Map<String, Value>,
}

#[derive(Debug, Deserialize, Clone)]
pub struct LinearCommentRecord {
    pub id: String,
    pub issue_id: String,
    #[serde(default)]
    pub issue_identifier: Option<String>,
    pub body: String,
    #[serde(default)]
    pub url: Option<String>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    #[serde(default)]
    pub user_id: Option<String>,
    #[serde(default)]
    pub user_name: Option<String>,
    #[serde(default)]
    pub user_display_name: Option<String>,
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
pub struct LinearIssueSummary {
    pub title: Option<String>,
    pub description: Option<String>,
    pub identifier: Option<String>,
    pub url: Option<String>,
}

pub fn event_logs(root: &Path) -> Result<Vec<PathBuf>> {
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

pub fn load_issue_summaries(root: &Path) -> Result<HashMap<String, LinearIssueSummary>> {
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

pub fn load_issue_comments(root: &Path) -> Result<Vec<LinearCommentRecord>> {
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
