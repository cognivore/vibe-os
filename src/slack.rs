use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::io::Write;
use std::path::Path;

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

#[derive(Debug, Deserialize, Serialize)]
pub struct SlackMessage {
    pub ts: String,
    pub user: Option<String>,
    pub text: Option<String>,
    pub thread_ts: Option<String>,
    pub reply_count: Option<u32>,
    pub subtype: Option<String>,
    #[serde(flatten)]
    pub extra: HashMap<String, serde_json::Value>,
}

#[derive(Debug, Deserialize)]
struct ConversationsListResponse {
    ok: bool,
    channels: Vec<SlackConversation>,
    response_metadata: Option<ResponseMetadata>,
}

#[derive(Debug, Deserialize)]
struct ConversationsHistoryResponse {
    ok: bool,
    messages: Vec<SlackMessage>,
    has_more: bool,
    response_metadata: Option<ResponseMetadata>,
}

#[derive(Debug, Deserialize)]
struct ConversationsRepliesResponse {
    ok: bool,
    messages: Vec<SlackMessage>,
    has_more: bool,
    response_metadata: Option<ResponseMetadata>,
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

    pub async fn mirror_all(&self, output_dir: &Path) -> Result<()> {
        std::fs::create_dir_all(output_dir)
            .context("Failed to create output directory")?;

        let conversations_dir = output_dir.join("conversations");
        let threads_dir = output_dir.join("threads");
        std::fs::create_dir_all(&conversations_dir)
            .context("Failed to create conversations directory")?;
        std::fs::create_dir_all(&threads_dir)
            .context("Failed to create threads directory")?;

        let conversations = self.list_all_conversations().await?;
        println!("Found {} conversations to mirror", conversations.len());

        for conv in &conversations {
            let conv_name = conv.name.as_deref().unwrap_or(&conv.id);
            println!("Mirroring conversation {} ({})...", conv_name, conv.id);

            let messages = self.fetch_conversation_history(&conv.id).await?;
            println!("  Fetched {} messages", messages.len());

            let file_path = conversations_dir.join(format!("{}.jsonl", conv.id));
            let file = std::fs::File::create(&file_path)
                .with_context(|| format!("Failed to create file: {:?}", file_path))?;
            let mut writer = std::io::BufWriter::new(file);

            for msg in &messages {
                serde_json::to_writer(&mut writer, msg)
                    .context("Failed to serialize message")?;
                writer.write_all(b"\n")
                    .context("Failed to write newline")?;
            }
            writer.flush()
                .context("Failed to flush file")?;

            println!("  Wrote {} messages to {:?}", messages.len(), file_path);

            // Fetch threads for messages that have replies
            for msg in &messages {
                if let Some(thread_ts) = &msg.thread_ts {
                    if msg.ts == *thread_ts && msg.reply_count.unwrap_or(0) > 0 {
                        if let Ok(replies) = self.fetch_thread_replies(&conv.id, thread_ts).await {
                            let thread_file = threads_dir.join(format!("{}_{}.jsonl", conv.id, thread_ts));
                            let thread_file_handle = std::fs::File::create(&thread_file)
                                .with_context(|| format!("Failed to create thread file: {:?}", thread_file))?;
                            let mut thread_writer = std::io::BufWriter::new(thread_file_handle);

                            for reply in &replies {
                                serde_json::to_writer(&mut thread_writer, reply)
                                    .context("Failed to serialize reply")?;
                                thread_writer.write_all(b"\n")
                                    .context("Failed to write newline")?;
                            }
                            thread_writer.flush()
                                .context("Failed to flush thread file")?;
                        }
                    }
                }
            }
        }

        Ok(())
    }

    async fn list_all_conversations(&self) -> Result<Vec<SlackConversation>> {
        let mut all_conversations = Vec::new();
        let mut cursor: Option<String> = None;

        loop {
            let mut params = vec![
                ("types", "public_channel,private_channel,im,mpim"),
            ];
            if let Some(ref c) = cursor {
                params.push(("cursor", c));
            }

            let response = self
                .http
                .get("https://slack.com/api/conversations.list")
                .header("Authorization", format!("Bearer {}", self.token))
                .query(&params)
                .send()
                .await
                .context("Failed to send request to conversations.list")?;

            let status = response.status();
            if status == 429 {
                let retry_after = response
                    .headers()
                    .get("Retry-After")
                    .and_then(|h| h.to_str().ok())
                    .and_then(|s| s.parse::<u64>().ok())
                    .unwrap_or(60);
                println!("Rate limited. Waiting {} seconds...", retry_after);
                tokio::time::sleep(tokio::time::Duration::from_secs(retry_after)).await;
                continue;
            }

            if !status.is_success() {
                anyhow::bail!("conversations.list returned status: {}", status);
            }

            let resp: ConversationsListResponse = response
                .json()
                .await
                .context("Failed to parse conversations.list response")?;

            if !resp.ok {
                anyhow::bail!("conversations.list returned ok=false");
            }

            all_conversations.extend(resp.channels);

            cursor = resp.response_metadata
                .and_then(|m| m.next_cursor)
                .filter(|c| !c.is_empty());

            if cursor.is_none() {
                break;
            }
        }

        Ok(all_conversations)
    }

    async fn fetch_conversation_history(&self, channel_id: &str) -> Result<Vec<SlackMessage>> {
        let mut all_messages = Vec::new();
        let mut cursor: Option<String> = None;

        loop {
            let mut params = vec![("channel", channel_id)];
            if let Some(ref c) = cursor {
                params.push(("cursor", c));
            }

            let response = self
                .http
                .get("https://slack.com/api/conversations.history")
                .header("Authorization", format!("Bearer {}", self.token))
                .query(&params)
                .send()
                .await
                .with_context(|| format!("Failed to send request to conversations.history for {}", channel_id))?;

            let status = response.status();
            if status == 429 {
                let retry_after = response
                    .headers()
                    .get("Retry-After")
                    .and_then(|h| h.to_str().ok())
                    .and_then(|s| s.parse::<u64>().ok())
                    .unwrap_or(60);
                println!("Rate limited. Waiting {} seconds...", retry_after);
                tokio::time::sleep(tokio::time::Duration::from_secs(retry_after)).await;
                continue;
            }

            if !status.is_success() {
                anyhow::bail!("conversations.history returned status: {}", status);
            }

            let resp: ConversationsHistoryResponse = response
                .json()
                .await
                .context("Failed to parse conversations.history response")?;

            if !resp.ok {
                anyhow::bail!("conversations.history returned ok=false");
            }

            all_messages.extend(resp.messages);

            if !resp.has_more {
                break;
            }

            cursor = resp.response_metadata
                .and_then(|m| m.next_cursor)
                .filter(|c| !c.is_empty());

            if cursor.is_none() {
                break;
            }
        }

        Ok(all_messages)
    }

    async fn fetch_thread_replies(&self, channel_id: &str, thread_ts: &str) -> Result<Vec<SlackMessage>> {
        let mut all_replies = Vec::new();
        let mut cursor: Option<String> = None;

        loop {
            let mut params = vec![
                ("channel", channel_id),
                ("ts", thread_ts),
            ];
            if let Some(ref c) = cursor {
                params.push(("cursor", c));
            }

            let response = self
                .http
                .get("https://slack.com/api/conversations.replies")
                .header("Authorization", format!("Bearer {}", self.token))
                .query(&params)
                .send()
                .await
                .with_context(|| format!("Failed to send request to conversations.replies for {} {}", channel_id, thread_ts))?;

            let status = response.status();
            if status == 429 {
                let retry_after = response
                    .headers()
                    .get("Retry-After")
                    .and_then(|h| h.to_str().ok())
                    .and_then(|s| s.parse::<u64>().ok())
                    .unwrap_or(60);
                println!("Rate limited. Waiting {} seconds...", retry_after);
                tokio::time::sleep(tokio::time::Duration::from_secs(retry_after)).await;
                continue;
            }

            if !status.is_success() {
                anyhow::bail!("conversations.replies returned status: {}", status);
            }

            let resp: ConversationsRepliesResponse = response
                .json()
                .await
                .context("Failed to parse conversations.replies response")?;

            if !resp.ok {
                anyhow::bail!("conversations.replies returned ok=false");
            }

            all_replies.extend(resp.messages);

            if !resp.has_more {
                break;
            }

            cursor = resp.response_metadata
                .and_then(|m| m.next_cursor)
                .filter(|c| !c.is_empty());

            if cursor.is_none() {
                break;
            }
        }

        Ok(all_replies)
    }
}

