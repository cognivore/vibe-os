use std::collections::HashMap;
use std::io::BufRead;
use std::path::PathBuf;

use anyhow::{Context, Result};
use serde_json::Value;

use core_model::adapters::EventAdapter;
use core_model::domain::Domain;
use core_model::event::EventEnvelope;
use core_model::time::TimeWindow;

mod parser;
mod transform;

use parser::{
    event_logs, load_issue_comments, load_issue_summaries, LinearCommentRecord, LinearIssueSummary,
};
use transform::{enrich_issue_metadata, map_comment_record, map_event_record};

pub struct LinearAdapter {
    root: PathBuf,
}

impl LinearAdapter {
    pub fn new(root: impl Into<PathBuf>) -> Self {
        Self { root: root.into() }
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
            let file = std::fs::File::open(&path)
                .with_context(|| format!("failed to open {}", path.display()))?;
            let reader = std::io::BufReader::new(file);
            for line in reader.lines() {
                let line = line?;
                if line.trim().is_empty() {
                    continue;
                }
                let mut value: Value = serde_json::from_str(&line).with_context(|| {
                    format!("failed to parse Linear event in {}", path.display())
                })?;
                let record = serde_json::from_value(value.clone()).with_context(|| {
                    format!("failed to map Linear event schema in {}", path.display())
                })?;
                let envelope = map_event_record(record);
                if let Some(issue_id) = envelope.entity_id.as_ref() {
                    let summary = issue_summaries.get(issue_id);
                    enrich_issue_metadata(&mut value, summary);
                }
                if window.contains(&envelope.at) {
                    envelopes.push(EventEnvelope {
                        data: value,
                        ..envelope
                    });
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
