use std::time::Duration;

use anyhow::{anyhow, Context, Result};
use axum::extract::{Path, State};
use axum::Json;
use core_model::event::EventEnvelope;
use domain_adapters::SlackAdapter;
use serde::Deserialize;
use tokio::fs;
use tokio::io::AsyncWriteExt;
use tracing::{debug, info, warn};

use crate::error::AppError;
use crate::state::AppState;
use crate::utils::resolve_event_entities;

#[derive(Debug, serde::Serialize)]
pub struct SlackThreadResponse {
    pub thread_id: String,
    pub channel_id: String,
    pub root: EventEnvelope,
    pub replies: Vec<EventEnvelope>,
}

pub async fn get_slack_thread(
    State(state): State<AppState>,
    Path((channel_id, thread_ts)): Path<(String, String)>,
) -> Result<Json<SlackThreadResponse>, AppError> {
    info!(
        "fetching slack thread channel={} thread_ts={}",
        channel_id, thread_ts
    );

    // Always fetch fresh from API if token available for on-demand active sync
    let mut events = if state.slack_token.is_some() {
        info!(
            "Fetching fresh Slack thread from API channel={} thread_ts={}",
            channel_id, thread_ts
        );
        fetch_thread_via_api_if_possible(
            &state,
            &channel_id,
            &thread_ts,
            "fetching latest thread data",
        )
        .await?
    } else {
        // Fallback to mirror if no token
        info!(
            "No Slack token; loading thread from mirror channel={} thread_ts={}",
            channel_id, thread_ts
        );
        let adapter = SlackAdapter::new(state.slack_mirror_dir.as_ref());
        adapter
            .load_thread(&channel_id, &thread_ts)
            .map_err(|e| AppError(anyhow!("Thread not in mirror and no Slack token: {}", e)))?
    };

    resolve_event_entities(&mut events, state.identity_store.clone()).await;
    events.sort_by(|a, b| a.at.cmp(&b.at));
    let (root, replies) = split_slack_thread(events, &thread_ts)?;
    debug!(
        "loaded slack thread channel={} thread_ts={} root_ts={} replies={}",
        &channel_id,
        &thread_ts,
        root.at,
        replies.len()
    );
    Ok(Json(SlackThreadResponse {
        thread_id: format!("{channel_id}:{thread_ts}"),
        channel_id,
        root,
        replies,
    }))
}

pub(crate) async fn fetch_thread_via_api_if_possible(
    state: &AppState,
    channel_id: &str,
    thread_ts: &str,
    reason: &str,
) -> Result<Vec<EventEnvelope>, AppError> {
    if let Some(token) = state.slack_token.as_ref() {
        info!(
            "Slack thread channel={} thread_ts={} unavailable in mirror ({}); fetching from Slack API",
            channel_id, thread_ts, reason
        );
        let token = token.clone();
        let events = fetch_and_save_slack_thread(
            token.as_ref(),
            state.slack_mirror_dir.as_ref(),
            channel_id,
            thread_ts,
        )
        .await?;
        info!(
            "Fetched {} Slack messages from API for channel={} thread_ts={}",
            events.len(),
            channel_id,
            thread_ts
        );
        Ok(events)
    } else {
        warn!(
            "Slack thread channel={} thread_ts={} unavailable in mirror ({}) and no Slack token configured",
            channel_id, thread_ts, reason
        );
        Err(AppError(anyhow!(
            "Thread not in mirror and VIBEOS_SLACK_TOKEN not set: {}",
            reason
        )))
    }
}

#[derive(Deserialize)]
struct SlackApiResponse {
    ok: bool,
    messages: Option<Vec<serde_json::Value>>,
    response_metadata: Option<SlackResponseMetadata>,
    error: Option<String>,
}

#[derive(Deserialize)]
struct SlackResponseMetadata {
    next_cursor: Option<String>,
}

async fn fetch_slack_thread_from_api(
    slack_token: &str,
    channel_id: &str,
    thread_ts: &str,
) -> Result<Vec<serde_json::Value>> {
    let client = reqwest::Client::new();
    let url = "https://slack.com/api/conversations.replies";
    let mut messages = Vec::new();
    let mut cursor: Option<String> = None;

    loop {
        let mut request = client
            .get(url)
            .header("Authorization", format!("Bearer {}", slack_token))
            .query(&[("channel", channel_id), ("ts", thread_ts)]);
        if let Some(ref token) = cursor {
            request = request.query(&[("cursor", token)]);
        }

        let response = request
            .send()
            .await
            .context("Failed to fetch Slack thread from API")?;

        if response.status() == reqwest::StatusCode::TOO_MANY_REQUESTS {
            let wait = retry_after(&response);
            warn!(
                "Rate limited calling Slack API conversations.replies; retrying in {:?}",
                wait
            );
            tokio::time::sleep(wait).await;
            continue;
        }

        if !response.status().is_success() {
            anyhow::bail!(
                "Slack API returned status {}: {:?}",
                response.status(),
                response.text().await
            );
        }

        let api_response: SlackApiResponse = response
            .json()
            .await
            .context("Failed to parse Slack API response")?;

        if !api_response.ok {
            anyhow::bail!(
                "Slack API returned ok=false: {}",
                api_response
                    .error
                    .unwrap_or_else(|| "unknown error".to_string())
            );
        }

        messages.extend(api_response.messages.unwrap_or_default());
        cursor = api_response
            .response_metadata
            .and_then(|meta| meta.next_cursor)
            .filter(|token| !token.is_empty());
        if cursor.is_none() {
            break;
        }
    }

    Ok(messages)
}

fn retry_after(response: &reqwest::Response) -> Duration {
    response
        .headers()
        .get("Retry-After")
        .and_then(|value| value.to_str().ok())
        .and_then(|raw| raw.parse::<u64>().ok())
        .map(Duration::from_secs)
        .unwrap_or_else(|| Duration::from_secs(60))
}

async fn fetch_and_save_slack_thread(
    slack_token: &str,
    slack_mirror_dir: &std::path::Path,
    channel_id: &str,
    thread_ts: &str,
) -> Result<Vec<EventEnvelope>> {
    info!(
        "Fetching Slack thread channel={} thread_ts={} from Slack API",
        channel_id, thread_ts
    );
    let messages = fetch_slack_thread_from_api(slack_token, channel_id, thread_ts).await?;
    if messages.is_empty() {
        anyhow::bail!(
            "Slack API returned no messages for channel {} thread {}",
            channel_id,
            thread_ts
        );
    }

    let threads_dir = slack_mirror_dir.join("threads");
    fs::create_dir_all(&threads_dir)
        .await
        .with_context(|| format!("failed to ensure {}", threads_dir.display()))?;
    let filename = format!("{}_{}.jsonl", channel_id, thread_ts.replace('.', "_"));
    let file_path = threads_dir.join(&filename);
    let mut file = fs::File::create(&file_path)
        .await
        .with_context(|| format!("failed to create {}", file_path.display()))?;
    for message in &messages {
        let payload = serde_json::to_vec(message)?;
        file.write_all(&payload).await?;
        file.write_all(b"\n").await?;
    }
    file.flush().await?;
    info!(
        "Persisted Slack thread channel={} thread_ts={} to {}",
        channel_id,
        thread_ts,
        file_path.display()
    );

    let adapter = SlackAdapter::new(slack_mirror_dir);
    let events = adapter
        .load_thread(channel_id, thread_ts)
        .with_context(|| {
            format!(
                "failed to reload Slack thread channel={} thread_ts={} after API fetch",
                channel_id, thread_ts
            )
        })?;
    if events.is_empty() {
        anyhow::bail!(
            "Slack thread channel={} thread_ts={} produced no events after fetch",
            channel_id,
            thread_ts
        );
    }
    Ok(events)
}

fn split_slack_thread(
    mut events: Vec<EventEnvelope>,
    thread_ts: &str,
) -> Result<(EventEnvelope, Vec<EventEnvelope>)> {
    if events.is_empty() {
        anyhow::bail!("thread is empty");
    }
    events.sort_by(|a, b| a.at.cmp(&b.at));
    let root_index = events.iter().position(|event| {
        slack_event_thread_ts(event)
            .map(|ts| ts == thread_ts)
            .unwrap_or(false)
    });
    let root = match root_index {
        Some(idx) => events.remove(idx),
        None => events.remove(0),
    };
    Ok((root, events))
}

fn slack_event_thread_ts(event: &EventEnvelope) -> Option<String> {
    match &event.data {
        serde_json::Value::Object(map) => map
            .get("thread_ts")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string())
            .or_else(|| {
                map.get("ts")
                    .and_then(|v| v.as_str())
                    .map(|s| s.to_string())
            }),
        _ => None,
    }
}
