use std::cmp::Ordering;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};

use crate::slack::SlackMessage;

use super::types::SlackThread;

const CONVERSATIONS_DIR: &str = "conversations";
const JSONL_EXTENSION: &str = "jsonl";

pub fn format_thread_for_llm(messages: &[SlackMessage]) -> String {
    let mut ordered: Vec<&SlackMessage> = messages.iter().collect();
    ordered.sort_by(|a, b| compare_ts(&a.ts, &b.ts));

    ordered
        .into_iter()
        .map(format_single_message)
        .collect::<Vec<_>>()
        .join("\n")
}

pub fn load_thread_from_mirror(
    mirror_root: &Path,
    conversation_id: &str,
    thread_ts: &str,
) -> Result<SlackThread> {
    let path = conversation_path(mirror_root, conversation_id);
    let file = File::open(&path).with_context(|| {
        format!(
            "Unable to open conversation mirror for {} at {}",
            conversation_id,
            path.display()
        )
    })?;
    let reader = BufReader::new(file);

    let mut messages = Vec::new();
    for (idx, line) in reader.lines().enumerate() {
        let line = line
            .with_context(|| format!("Failed to read line {} from {}", idx + 1, path.display()))?;

        if line.trim().is_empty() {
            continue;
        }

        let message: SlackMessage = serde_json::from_str(&line).with_context(|| {
            format!(
                "Failed to parse Slack message on line {} from {}",
                idx + 1,
                path.display()
            )
        })?;

        if message.ts == thread_ts || message.thread_ts.as_deref() == Some(thread_ts) {
            messages.push(message);
        }
    }

    if messages.is_empty() {
        anyhow::bail!(
            "No messages found for conversation {} thread {} in {}",
            conversation_id,
            thread_ts,
            path.display()
        );
    }

    messages.sort_by(|a, b| compare_ts(&a.ts, &b.ts));

    Ok(SlackThread {
        conversation_id: conversation_id.to_string(),
        thread_ts: thread_ts.to_string(),
        messages,
    })
}

fn compare_ts(a: &str, b: &str) -> Ordering {
    match (a.parse::<f64>(), b.parse::<f64>()) {
        (Ok(lhs), Ok(rhs)) => lhs.partial_cmp(&rhs).unwrap_or_else(|| a.cmp(b)),
        _ => a.cmp(b),
    }
}

fn format_single_message(message: &SlackMessage) -> String {
    let user = message
        .user
        .as_deref()
        .map(|u| format!("@{u}"))
        .unwrap_or_else(|| "<unknown>".to_string());
    let text = message
        .text
        .as_deref()
        .map(sanitize_text)
        .unwrap_or_else(|| "<no text>".to_string());
    format!("[{}] {}: {}", message.ts, user, text)
}

fn sanitize_text(input: &str) -> String {
    input.replace(['\n', '\r'], " ").trim().to_string()
}

fn conversation_path(root: &Path, conversation_id: &str) -> PathBuf {
    root.join(CONVERSATIONS_DIR)
        .join(format!("{}.{}", conversation_id, JSONL_EXTENSION))
}
