use std::collections::HashMap;
use std::io::BufRead;
use std::path::{Path, PathBuf};

use anyhow::{anyhow, Context, Result};
use serde_json::Value;

use core_model::adapters::EventAdapter;
use core_model::domain::Domain;
use core_model::event::EventEnvelope;
use core_model::time::TimeWindow;

mod parser;
mod transform;

use parser::{
    event_logs, load_issue_comments, load_issue_summaries, LinearCommentRecord, LinearEventRecord,
    LinearIssueSummary,
};
use transform::{enrich_issue_metadata, map_comment_record, map_event_record};

pub struct LinearAdapter {
    root: PathBuf,
}

impl LinearAdapter {
    pub fn new(root: impl Into<PathBuf>) -> Self {
        Self { root: root.into() }
    }

    pub fn load_issue_thread(&self, reference: &str) -> Result<LinearIssueThread> {
        let summaries = load_issue_summaries(&self.root)?;
        let (issue_id, summary) = summaries
            .iter()
            .find_map(|(id, summary)| {
                if id == reference
                    || summary
                        .identifier
                        .as_deref()
                        .map(|value| value == reference)
                        .unwrap_or(false)
                {
                    Some((id.clone(), summary.clone()))
                } else {
                    None
                }
            })
            .ok_or_else(|| anyhow!("Linear issue `{reference}` not found in mirror"))?;

        let issue_identifier = summary.identifier.clone();
        let mut events = load_issue_events(&self.root, &issue_id, &summary)?;
        events.sort_by(|a, b| a.at.cmp(&b.at));

        let issue_identifier_str = issue_identifier.as_deref();
        let mut comments = load_issue_comments(&self.root)?
            .into_iter()
            .filter(|comment| {
                if comment.issue_id == issue_id {
                    return true;
                }
                match (comment.issue_identifier.as_deref(), issue_identifier_str) {
                    (Some(candidate), Some(target)) => candidate == target,
                    _ => false,
                }
            })
            .map(|comment| map_comment_record(comment, Some(&summary)))
            .collect::<Vec<_>>();
        comments.sort_by(|a, b| a.at.cmp(&b.at));

        Ok(LinearIssueThread {
            issue_id,
            issue_identifier,
            issue_title: summary.title,
            issue_url: summary.url,
            issue_description: summary.description,
            events,
            comments,
        })
    }
}

#[derive(Debug, Clone)]
pub struct LinearIssueThread {
    pub issue_id: String,
    pub issue_identifier: Option<String>,
    pub issue_title: Option<String>,
    pub issue_url: Option<String>,
    pub issue_description: Option<String>,
    pub events: Vec<EventEnvelope>,
    pub comments: Vec<EventEnvelope>,
}

fn load_issue_events(
    root: &Path,
    issue_id: &str,
    summary: &LinearIssueSummary,
) -> Result<Vec<EventEnvelope>> {
    let mut events = Vec::new();
    let issue_identifier = summary.identifier.as_deref();

    for path in event_logs(root)? {
        let file = std::fs::File::open(&path)
            .with_context(|| format!("failed to open {}", path.display()))?;
        let reader = std::io::BufReader::new(file);
        for line in reader.lines() {
            let line = line?;
            if line.trim().is_empty() {
                continue;
            }
            let record: LinearEventRecord = serde_json::from_str(&line)
                .with_context(|| format!("failed to parse Linear event in {}", path.display()))?;
            let matches_issue = record.issue_id == issue_id
                || issue_identifier.is_some_and(|target| {
                    record
                        .issue_identifier
                        .as_deref()
                        .map(|candidate| candidate == target)
                        .unwrap_or(false)
                });
            if !matches_issue {
                continue;
            }
            let mut envelope = map_event_record(record);
            enrich_issue_metadata(&mut envelope.data, Some(summary));
            events.push(envelope);
        }
    }

    Ok(events)
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
            let file = std::fs::File::open(&path)
                .with_context(|| format!("failed to open {}", path.display()))?;
            let reader = std::io::BufReader::new(file);
            for line in reader.lines() {
                let line = line?;
                if line.trim().is_empty() {
                    continue;
                }
                let value: Value = serde_json::from_str(&line).with_context(|| {
                    format!("failed to parse Linear event in {}", path.display())
                })?;
                let record = serde_json::from_value(value.clone()).with_context(|| {
                    format!("failed to map Linear event schema in {}", path.display())
                })?;
                let mut envelope = map_event_record(record);
                // Extract the actual issue_id (UUID) from the data field for lookup
                let issue_id = envelope.data.get("issue_id").and_then(|v| v.as_str());
                if let Some(issue_id) = issue_id {
                    let summary = issue_summaries.get(issue_id);
                    enrich_issue_metadata(&mut envelope.data, summary);
                }
                if window.contains(&envelope.at) {
                    envelopes.push(envelope);
                }
            }
        }

        append_comment_events(window, &issue_summaries, issue_comments, &mut envelopes);
        envelopes.sort_by(|a, b| a.at.cmp(&b.at));
        Ok(envelopes)
    }
}

fn append_comment_events(
    window: &TimeWindow,
    summaries: &HashMap<String, LinearIssueSummary>,
    comments: Vec<LinearCommentRecord>,
    envelopes: &mut Vec<EventEnvelope>,
) {
    for comment in comments {
        let summary = summaries.get(&comment.issue_id);
        let envelope = map_comment_record(comment, summary);
        if window.contains(&envelope.at) {
            envelopes.push(envelope);
        }
    }
}
