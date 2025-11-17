use std::collections::HashMap;

use chrono::{DateTime, Duration, Utc};

use crate::linear_sync::{LinearIssueEvent, LinearIssueSnapshot};

use super::types::{LinearIssueSummary, LinearWindowContext};

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

fn in_window(ts: &DateTime<Utc>, start: DateTime<Utc>, end: DateTime<Utc>) -> bool {
    *ts >= start && *ts < end
}
