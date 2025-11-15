use anyhow::{Context, Result};
use reqwest::StatusCode;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;
use tokio::time::{sleep, Duration};

const API_BASE: &str = "https://slack.com/api/";
const CONVERSATIONS_LIST: &str = "conversations.list";
const CONVERSATIONS_HISTORY: &str = "conversations.history";
const CONVERSATIONS_REPLIES: &str = "conversations.replies";
const CONVERSATIONS_JOIN: &str = "conversations.join";
const CONVERSATION_TYPES: &str = "public_channel,private_channel,im,mpim";
const DEFAULT_RETRY_AFTER_SECS: u64 = 60;
const CONVERSATIONS_DIR: &str = "conversations";
const THREADS_DIR: &str = "threads";
const JSONL_EXTENSION: &str = "jsonl";

#[derive(Debug, Deserialize)]
struct SlackConversation {
    id: String,
    name: Option<String>,
    #[allow(dead_code)]
    is_channel: Option<bool>,
    #[allow(dead_code)]
    is_group: Option<bool>,
    #[allow(dead_code)]
    is_im: Option<bool>,
    #[allow(dead_code)]
    is_mpim: Option<bool>,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct SlackMessage {
    pub ts: String,
    pub user: Option<String>,
    pub text: Option<String>,
    pub thread_ts: Option<String>,
    pub reply_count: Option<u32>,
    pub subtype: Option<String>,
    #[serde(flatten)]
    pub extra: HashMap<String, Value>,
}

#[derive(Debug, Deserialize)]
struct ConversationsListResponse {
    ok: bool,
    channels: Option<Vec<SlackConversation>>,
    response_metadata: Option<ResponseMetadata>,
    error: Option<String>,
}

#[derive(Debug, Deserialize)]
struct ConversationsHistoryResponse {
    ok: bool,
    messages: Option<Vec<SlackMessage>>,
    response_metadata: Option<ResponseMetadata>,
    error: Option<String>,
}

#[derive(Debug, Deserialize)]
struct ConversationsRepliesResponse {
    ok: bool,
    messages: Option<Vec<SlackMessage>>,
    response_metadata: Option<ResponseMetadata>,
    error: Option<String>,
}

#[derive(Debug, Deserialize)]
struct ConversationsJoinResponse {
    ok: bool,
    error: Option<String>,
}

#[derive(Debug, Deserialize)]
struct ResponseMetadata {
    next_cursor: Option<String>,
}

pub struct SlackClient {
    http: reqwest::Client,
    token: String,
}

impl SlackClient {
    pub fn new(token: String) -> Self {
        Self {
            http: reqwest::Client::new(),
            token,
        }
    }

    pub async fn join_all_public_channels(&self) -> Result<()> {
        let conversations = self.list_all_conversations().await?;
        let public_channels: Vec<_> = conversations
            .iter()
            .filter(|c| c.is_channel.unwrap_or(false) && !c.is_group.unwrap_or(false))
            .collect();

        println!("Found {} public channels", public_channels.len());

        for conv in &public_channels {
            let conv_name = conv.name.as_deref().unwrap_or(&conv.id);
            print!("Joining {} ({})... ", conv_name, conv.id);

            match self.join_channel(&conv.id).await {
                Ok(()) => println!("✓ joined"),
                Err(e) => {
                    let err_str = e.to_string();
                    if err_str.contains("already_in_channel") {
                        println!("already in channel");
                    } else {
                        println!("✗ failed: {}", e);
                    }
                }
            }
        }

        Ok(())
    }

    pub async fn mirror_all(&self, output_dir: &Path) -> Result<()> {
        ensure_dir(output_dir)?;

        let conversations_dir = output_dir.join(CONVERSATIONS_DIR);
        let threads_dir = output_dir.join(THREADS_DIR);
        ensure_dir(&conversations_dir)?;
        ensure_dir(&threads_dir)?;

        let conversations = self.list_all_conversations().await?;
        println!("Found {} conversations to mirror", conversations.len());

        for conv in &conversations {
            let conv_name = conv.name.as_deref().unwrap_or(&conv.id);
            println!("Mirroring conversation {} ({})...", conv_name, conv.id);

            let messages = self.fetch_conversation_history(&conv.id).await?;
            println!("  Fetched {} messages", messages.len());

            let conversation_path = conversations_dir.join(jsonl_name(&conv.id));
            write_jsonl(&conversation_path, &messages)?;
            println!(
                "  Wrote {} messages to {}",
                messages.len(),
                conversation_path.display()
            );

            for msg in &messages {
                if should_fetch_thread(msg) {
                    let thread_ts = msg.thread_ts.as_deref().unwrap_or(&msg.ts);
                    let replies = self.fetch_thread_replies(&conv.id, thread_ts).await?;
                    let thread_path = threads_dir.join(thread_filename(&conv.id, thread_ts));
                    write_jsonl(&thread_path, &replies)?;
                    println!(
                        "  Wrote {} thread messages to {}",
                        replies.len(),
                        thread_path.display()
                    );
                }
            }
        }

        Ok(())
    }

    async fn list_all_conversations(&self) -> Result<Vec<SlackConversation>> {
        let mut all_conversations = Vec::new();
        let mut cursor: Option<String> = None;

        loop {
            let mut params = vec![("types", CONVERSATION_TYPES)];
            if let Some(ref c) = cursor {
                params.push(("cursor", c));
            }

            let response = self
                .execute_request(
                    self.http
                        .get(api_url(CONVERSATIONS_LIST))
                        .header("Authorization", format!("Bearer {}", self.token))
                        .query(&params),
                    CONVERSATIONS_LIST,
                )
                .await?;

            let resp: ConversationsListResponse = response
                .json()
                .await
                .context("Failed to parse conversations.list response")?;

            if !resp.ok {
                let error_msg = resp.error.as_deref().unwrap_or("unknown error");
                anyhow::bail!("conversations.list returned ok=false: {}", error_msg);
            }

            all_conversations.extend(resp.channels.unwrap_or_default());
            cursor = next_cursor(resp.response_metadata);

            if cursor.is_none() {
                break;
            }
        }

        Ok(all_conversations)
    }

    async fn join_channel(&self, channel_id: &str) -> Result<()> {
        let params = vec![("channel", channel_id)];

        let response = self
            .execute_request(
                self.http
                    .post(api_url(CONVERSATIONS_JOIN))
                    .header("Authorization", format!("Bearer {}", self.token))
                    .query(&params),
                CONVERSATIONS_JOIN,
            )
            .await
            .with_context(|| format!("Failed to request conversations.join for channel {}", channel_id))?;

        let resp: ConversationsJoinResponse = response
            .json()
            .await
            .context("Failed to parse conversations.join response")?;

        if !resp.ok {
            let error_msg = resp.error.as_deref().unwrap_or("unknown error");
            anyhow::bail!(
                "conversations.join returned ok=false: {} (channel: {})",
                error_msg,
                channel_id
            );
        }

        Ok(())
    }

    async fn fetch_conversation_history(&self, channel_id: &str) -> Result<Vec<SlackMessage>> {
        let mut all_messages = Vec::new();
        let mut cursor: Option<String> = None;
        let mut tried_join = false;

        loop {
            let mut params = vec![("channel", channel_id)];
            if let Some(ref c) = cursor {
                params.push(("cursor", c));
            }

            let response = self
                .execute_request(
                    self.http
                        .get(api_url(CONVERSATIONS_HISTORY))
                        .header("Authorization", format!("Bearer {}", self.token))
                        .query(&params),
                    CONVERSATIONS_HISTORY,
                )
                .await
                .with_context(|| {
                    format!(
                        "Failed to request conversations.history for channel {}",
                        channel_id
                    )
                })?;

            let resp: ConversationsHistoryResponse = response
                .json()
                .await
                .context("Failed to parse conversations.history response")?;

            if !resp.ok {
                let error_msg = resp.error.as_deref().unwrap_or("unknown error");

                // If not in channel and haven't tried joining yet, try to join
                if error_msg == "not_in_channel" && !tried_join {
                    println!("  Not in channel, attempting to join...");
                    match self.join_channel(channel_id).await {
                        Ok(()) => {
                            println!("  Successfully joined channel");
                            tried_join = true;
                            continue; // Retry fetching history
                        }
                        Err(e) => {
                            println!("  Failed to join channel: {}", e);
                            println!("  (Private channels require manual invitation via /invite @bot_name)");
                            return Err(e);
                        }
                    }
                }

                anyhow::bail!(
                    "conversations.history returned ok=false: {} (channel: {})",
                    error_msg,
                    channel_id
                );
            }

            all_messages.extend(resp.messages.unwrap_or_default());
            cursor = next_cursor(resp.response_metadata);

            if cursor.is_none() {
                break;
            }
        }

        Ok(all_messages)
    }

    async fn fetch_thread_replies(
        &self,
        channel_id: &str,
        thread_ts: &str,
    ) -> Result<Vec<SlackMessage>> {
        let mut all_replies = Vec::new();
        let mut cursor: Option<String> = None;

        loop {
            let mut params = vec![("channel", channel_id), ("ts", thread_ts)];
            if let Some(ref c) = cursor {
                params.push(("cursor", c));
            }

            let response = self
                .execute_request(
                    self.http
                        .get(api_url(CONVERSATIONS_REPLIES))
                        .header("Authorization", format!("Bearer {}", self.token))
                        .query(&params),
                    CONVERSATIONS_REPLIES,
                )
                .await
                .with_context(|| {
                    format!(
                        "Failed to request conversations.replies for channel {} thread {}",
                        channel_id, thread_ts
                    )
                })?;

            let resp: ConversationsRepliesResponse = response
                .json()
                .await
                .context("Failed to parse conversations.replies response")?;

            if !resp.ok {
                let error_msg = resp.error.as_deref().unwrap_or("unknown error");
                anyhow::bail!(
                    "conversations.replies returned ok=false: {} (channel: {}, thread: {})",
                    error_msg,
                    channel_id,
                    thread_ts
                );
            }

            all_replies.extend(resp.messages.unwrap_or_default());
            cursor = next_cursor(resp.response_metadata);

            if cursor.is_none() {
                break;
            }
        }

        Ok(all_replies)
    }

    async fn execute_request(
        &self,
        builder: reqwest::RequestBuilder,
        label: &str,
    ) -> Result<reqwest::Response> {
        let base_builder = builder;

        loop {
            let request = base_builder
                .try_clone()
                .context("Unable to clone Slack request for retry")?;

            let response = request
                .send()
                .await
                .with_context(|| format!("Failed to send {}", label))?;

            if response.status() == StatusCode::TOO_MANY_REQUESTS {
                let wait = retry_after(&response);
                println!(
                    "Rate limited on {}. Waiting {}s before retrying...",
                    label,
                    wait.as_secs()
                );
                sleep(wait).await;
                continue;
            }

            if !response.status().is_success() {
                anyhow::bail!("{} returned status: {}", label, response.status());
            }

            return Ok(response);
        }
    }
}

fn should_fetch_thread(message: &SlackMessage) -> bool {
    message
        .thread_ts
        .as_ref()
        .map(|thread_ts| thread_ts == &message.ts)
        .unwrap_or(false)
        && message.reply_count.unwrap_or(0) > 0
}

fn jsonl_name(stem: &str) -> String {
    format!("{}.{}", stem, JSONL_EXTENSION)
}

fn thread_filename(channel_id: &str, thread_ts: &str) -> String {
    let sanitized_ts = thread_ts.replace('.', "_");
    format!("{}_{}.{}", channel_id, sanitized_ts, JSONL_EXTENSION)
}

fn ensure_dir(path: &Path) -> Result<()> {
    std::fs::create_dir_all(path)
        .with_context(|| format!("Failed to create directory {}", path.display()))
}

fn write_jsonl<T: Serialize>(path: &Path, entries: &[T]) -> Result<()> {
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

fn api_url(endpoint: &str) -> String {
    format!("{API_BASE}{endpoint}")
}

fn next_cursor(metadata: Option<ResponseMetadata>) -> Option<String> {
    metadata
        .and_then(|meta| meta.next_cursor)
        .filter(|cursor| !cursor.is_empty())
}

fn retry_after(response: &reqwest::Response) -> Duration {
    response
        .headers()
        .get("Retry-After")
        .and_then(|h| h.to_str().ok())
        .and_then(|s| s.parse::<u64>().ok())
        .map(Duration::from_secs)
        .unwrap_or_else(|| Duration::from_secs(DEFAULT_RETRY_AFTER_SECS))
}
