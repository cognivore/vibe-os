use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

use anyhow::{Context, Result};
use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};

use crate::linear_sync::{LinearIssueEvent, LinearIssueSnapshot};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LinearIssueSummary {
    pub identifier: String,
    pub title: String,
    pub url: Option<String>,
    pub team_key: Option<String>,
    pub state_name: Option<String>,
    pub state_type: Option<String>,
    pub assignee_name: Option<String>,
    pub priority: Option<i32>,
    pub updated_at: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LinearWindowContext {
    pub window_start: DateTime<Utc>,
    pub window_end: DateTime<Utc>,
    pub issues_created: usize,
    pub issues_completed: usize,
    pub issues_reopened: usize,
    pub issues_archived: usize,
    pub comments_added: usize,
    pub transitions: usize,
    pub top_active_issues: Vec<LinearIssueSummary>,
    pub stale_issues: Vec<LinearIssueSummary>,
}

pub fn load_issues(mirror_dir: &Path) -> Result<Vec<LinearIssueSnapshot>> {
    let path = mirror_dir.join("issues.jsonl");
    let file = File::open(&path)
        .with_context(|| format!("Unable to open Linear issues mirror at {}", path.display()))?;
    let reader = BufReader::new(file);

    reader
        .lines()
        .enumerate()
        .filter_map(|(idx, line)| match line {
            Ok(content) if content.trim().is_empty() => None,
            Ok(content) => Some(
                serde_json::from_str::<LinearIssueSnapshot>(&content).with_context(|| {
                    format!(
                        "Failed to parse Linear issue on line {} in {}",
                        idx + 1,
                        path.display()
                    )
                }),
            ),
            Err(err) => Some(Err(err).with_context(|| {
                format!(
                    "Failed to read Linear issue line {} from {}",
                    idx + 1,
                    path.display()
                )
            })),
        })
        .collect()
}

pub fn load_events(mirror_dir: &Path) -> Result<Vec<LinearIssueEvent>> {
    let path = mirror_dir.join("events.jsonl");
    let file = File::open(&path)
        .with_context(|| format!("Unable to open Linear events mirror at {}", path.display()))?;
    let reader = BufReader::new(file);

    reader
        .lines()
        .enumerate()
        .filter_map(|(idx, line)| match line {
            Ok(content) if content.trim().is_empty() => None,
            Ok(content) => Some(
                serde_json::from_str::<LinearIssueEvent>(&content).with_context(|| {
                    format!(
                        "Failed to parse Linear event on line {} in {}",
                        idx + 1,
                        path.display()
                    )
                }),
            ),
            Err(err) => Some(Err(err).with_context(|| {
                format!(
                    "Failed to read Linear event line {} from {}",
                    idx + 1,
                    path.display()
                )
            })),
        })
        .collect()
}

pub fn compute_window_context(
    issues: &[LinearIssueSnapshot],
    events: &[LinearIssueEvent],
    window_start: DateTime<Utc>,
    window_end: DateTime<Utc>,
    stale_days: i64,
) -> LinearWindowContext {
    let issues_created = issues
        .iter()
        .filter(|issue| in_window(&issue.created_at, window_start, window_end))
        .count();

    let issues_completed = issues
        .iter()
        .filter(|issue| {
            issue
                .completed_at
                .as_ref()
                .map(|ts| in_window(ts, window_start, window_end))
                .unwrap_or(false)
        })
        .count();

    let issues_archived = issues
        .iter()
        .filter(|issue| {
            issue
                .archived_at
                .as_ref()
                .map(|ts| in_window(ts, window_start, window_end))
                .unwrap_or(false)
        })
        .count();

    let issues_reopened = events
        .iter()
        .filter(|event| {
            event.event_kind == "reopened" && in_window(&event.created_at, window_start, window_end)
        })
        .count();

    let transitions = events
        .iter()
        .filter(|event| {
            event.event_kind == "state_changed"
                && in_window(&event.created_at, window_start, window_end)
        })
        .count();

    let top_active_issues = compute_top_active(issues, events, window_start, window_end);
    let stale_issues = compute_stale_issues(issues, window_end, stale_days);

    LinearWindowContext {
        window_start,
        window_end,
        issues_created,
        issues_completed,
        issues_reopened,
        issues_archived,
        comments_added: 0,
        transitions,
        top_active_issues,
        stale_issues,
    }
}

fn compute_top_active(
    issues: &[LinearIssueSnapshot],
    events: &[LinearIssueEvent],
    window_start: DateTime<Utc>,
    window_end: DateTime<Utc>,
) -> Vec<LinearIssueSummary> {
    let mut activity: HashMap<&str, usize> = HashMap::new();
    for event in events {
        if in_window(&event.created_at, window_start, window_end) {
            *activity.entry(event.issue_id.as_str()).or_default() += 1;
        }
    }

    if activity.is_empty() {
        return Vec::new();
    }

    let issue_lookup: HashMap<&str, &LinearIssueSnapshot> = issues
        .iter()
        .map(|issue| (issue.id.as_str(), issue))
        .collect();

    let mut scored: Vec<(LinearIssueSummary, usize)> = activity
        .into_iter()
        .filter_map(|(issue_id, score)| {
            issue_lookup
                .get(issue_id)
                .map(|issue| (LinearIssueSummary::from_issue(issue), score))
        })
        .collect();

    scored.sort_by(|(left_issue, left_score), (right_issue, right_score)| {
        right_score
            .cmp(left_score)
            .then_with(|| right_issue.updated_at.cmp(&left_issue.updated_at))
    });

    scored
        .into_iter()
        .take(10)
        .map(|(summary, _score)| summary)
        .collect()
}

fn compute_stale_issues(
    issues: &[LinearIssueSnapshot],
    window_end: DateTime<Utc>,
    stale_days: i64,
) -> Vec<LinearIssueSummary> {
    let cutoff = window_end - Duration::days(stale_days.max(0));
    let mut stale: Vec<LinearIssueSummary> = issues
        .iter()
        .filter(|issue| {
            issue.completed_at.is_none()
                && issue.canceled_at.is_none()
                && issue.archived_at.is_none()
                && issue.updated_at < cutoff
        })
        .map(LinearIssueSummary::from_issue)
        .collect();

    stale.sort_by(|a, b| a.updated_at.cmp(&b.updated_at));
    stale.truncate(50);
    stale
}

impl LinearIssueSummary {
    fn from_issue(issue: &LinearIssueSnapshot) -> Self {
        Self {
            identifier: issue.identifier.clone(),
            title: issue.title.clone(),
            url: issue.url.clone(),
            team_key: issue.team_key.clone(),
            state_name: issue.state_name.clone(),
            state_type: issue.state_type.clone(),
            assignee_name: issue.assignee_name.clone(),
            priority: issue.priority,
            updated_at: issue.updated_at.clone(),
        }
    }
}

fn in_window(ts: &DateTime<Utc>, start: DateTime<Utc>, end: DateTime<Utc>) -> bool {
    *ts >= start && *ts < end
}
