use anyhow::{Context, Result};
use reqwest::{RequestBuilder, Response, StatusCode};
use serde_json::Value;
use tokio::time::{sleep, Duration};

use super::client::SlackClient;
use super::types::{
    ChatPostMessageResponse, ChatUpdateResponse, ConversationsHistoryResponse,
    ConversationsJoinResponse, ConversationsListResponse, ConversationsRepliesResponse,
    ResponseMetadata, SlackConversation, SlackMessage, UsersListResponse, API_BASE,
    CHAT_POST_MESSAGE, CHAT_UPDATE, CONVERSATIONS_HISTORY, CONVERSATIONS_JOIN, CONVERSATIONS_LIST,
    CONVERSATIONS_REPLIES, CONVERSATION_TYPES, DEFAULT_RETRY_AFTER_SECS, USERS_LIST,
};

impl SlackClient {
    pub(super) async fn list_all_conversations(&self) -> Result<Vec<SlackConversation>> {
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

    pub(super) async fn join_channel(&self, channel_id: &str) -> Result<()> {
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
            .with_context(|| {
                format!(
                    "Failed to request conversations.join for channel {}",
                    channel_id
                )
            })?;

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

    pub(super) async fn fetch_conversation_history_since(
        &self,
        channel_id: &str,
        oldest: Option<&str>,
    ) -> Result<Vec<SlackMessage>> {
        let mut all_messages = Vec::new();
        let mut cursor: Option<String> = None;
        let mut tried_join = false;

        loop {
            let mut params = vec![("channel", channel_id)];
            if let Some(oldest_ts) = oldest {
                params.push(("oldest", oldest_ts));
            }
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

                if error_msg == "not_in_channel" && !tried_join {
                    println!("  Not in channel, attempting to join...");
                    match self.join_channel(channel_id).await {
                        Ok(()) => {
                            println!("  Successfully joined channel");
                            tried_join = true;
                            continue;
                        }
                        Err(e) => {
                            let err_str = e.to_string();
                            if err_str.contains("is_archived") {
                                println!("  Channel is archived, skipping...");
                                return Ok(Vec::new());
                            }
                            println!("  Failed to join channel: {}", e);
                            println!(
                                "  (Private channels require manual invitation via /invite @bot_name)"
                            );
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

    pub(super) async fn fetch_thread_replies_since(
        &self,
        channel_id: &str,
        thread_ts: &str,
        oldest: Option<&str>,
    ) -> Result<Vec<SlackMessage>> {
        let mut all_replies = Vec::new();
        let mut cursor: Option<String> = None;

        loop {
            let mut params = vec![("channel", channel_id), ("ts", thread_ts)];
            if let Some(oldest_ts) = oldest {
                params.push(("oldest", oldest_ts));
            }
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

    pub(super) async fn list_all_users(&self) -> Result<Vec<Value>> {
        let mut all_users = Vec::new();
        let mut cursor: Option<String> = None;

        loop {
            let mut params = Vec::new();
            if let Some(ref c) = cursor {
                params.push(("cursor", c.as_str()));
            }

            let response = self
                .execute_request(
                    self.http
                        .get(api_url(USERS_LIST))
                        .header("Authorization", format!("Bearer {}", self.token))
                        .query(&params),
                    USERS_LIST,
                )
                .await?;

            let resp: UsersListResponse = response
                .json()
                .await
                .context("Failed to parse users.list response")?;

            if !resp.ok {
                let error_msg = resp.error.as_deref().unwrap_or("unknown error");
                anyhow::bail!("users.list returned ok=false: {}", error_msg);
            }

            all_users.extend(resp.members.unwrap_or_default());
            cursor = next_cursor(resp.response_metadata);

            if cursor.is_none() {
                break;
            }
        }

        Ok(all_users)
    }

    pub(super) async fn post_message(&self, channel: &str, text: &str) -> Result<String> {
        self.post_message_with_options(channel, text, None, false, None)
            .await
    }

    pub(super) async fn post_thread_message(
        &self,
        channel: &str,
        thread_ts: &str,
        text: &str,
        reply_broadcast: bool,
        blocks: Option<Value>,
    ) -> Result<String> {
        self.post_message_with_options(channel, text, Some(thread_ts), reply_broadcast, blocks)
            .await
    }

    async fn post_message_with_options(
        &self,
        channel: &str,
        text: &str,
        thread_ts: Option<&str>,
        reply_broadcast: bool,
        blocks: Option<Value>,
    ) -> Result<String> {
        let mut body = serde_json::json!({
            "channel": channel,
            "text": text,
        });
        let payload = body
            .as_object_mut()
            .context("Slack message payload must be a JSON object")?;

        if let Some(thread_ts) = thread_ts {
            payload.insert("thread_ts".into(), Value::String(thread_ts.to_owned()));
            if reply_broadcast {
                payload.insert("reply_broadcast".into(), Value::Bool(true));
            }
        }

        if let Some(blocks) = blocks {
            payload.insert("blocks".into(), blocks);
            payload.insert("unfurl_links".into(), Value::Bool(false));
            payload.insert("unfurl_media".into(), Value::Bool(false));
        }

        let response = self
            .execute_request(
                self.http
                    .post(api_url(CHAT_POST_MESSAGE))
                    .header("Authorization", format!("Bearer {}", self.token))
                    .json(&body),
                CHAT_POST_MESSAGE,
            )
            .await?;

        let resp: ChatPostMessageResponse = response
            .json()
            .await
            .context("Failed to parse chat.postMessage response")?;

        if !resp.ok {
            anyhow::bail!(
                "chat.postMessage returned ok=false: {}",
                resp.error.as_deref().unwrap_or("unknown error")
            );
        }

        resp.ts
            .ok_or_else(|| anyhow::anyhow!("chat.postMessage response missing ts"))
    }

    pub(super) async fn update_message(&self, channel: &str, ts: &str, text: &str) -> Result<()> {
        let body = serde_json::json!({
            "channel": channel,
            "ts": ts,
            "text": text,
        });

        let response = self
            .execute_request(
                self.http
                    .post(api_url(CHAT_UPDATE))
                    .header("Authorization", format!("Bearer {}", self.token))
                    .json(&body),
                CHAT_UPDATE,
            )
            .await?;

        let resp: ChatUpdateResponse = response
            .json()
            .await
            .context("Failed to parse chat.update response")?;

        if !resp.ok {
            anyhow::bail!(
                "chat.update returned ok=false: {}",
                resp.error.as_deref().unwrap_or("unknown error")
            );
        }

        Ok(())
    }

    pub(super) async fn find_channel_by_name(&self, name: &str) -> Result<String> {
        let conversations = self.list_all_conversations().await?;
        conversations
            .iter()
            .find(|c| c.name.as_deref() == Some(name))
            .map(|c| c.id.clone())
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "Channel #{} not found. Set VIBEOS_FAP_CHANNEL to the channel ID instead.",
                    name
                )
            })
    }

    pub(super) async fn execute_request(
        &self,
        builder: RequestBuilder,
        label: &str,
    ) -> Result<Response> {
        const MAX_CONNECTION_RETRIES: u32 = 3;
        const MAX_RATE_LIMIT_RETRIES: u32 = 5;
        const INITIAL_BACKOFF_SECS: u64 = 2;

        let base_builder = builder;
        let mut connection_attempts = 0u32;
        let mut rate_limit_attempts = 0u32;

        loop {
            let request = base_builder
                .try_clone()
                .context("Unable to clone Slack request for retry")?;

            let result = request.send().await;

            match result {
                Ok(response) => {
                    if response.status() == StatusCode::TOO_MANY_REQUESTS {
                        rate_limit_attempts += 1;
                        if rate_limit_attempts >= MAX_RATE_LIMIT_RETRIES {
                            anyhow::bail!(
                                "{} rate limited {} times, giving up to avoid blocking other requests",
                                label,
                                MAX_RATE_LIMIT_RETRIES
                            );
                        }

                        let wait = retry_after(&response);
                        println!(
                            "Rate limited on {} (attempt {}/{}). Waiting {}s before retrying...",
                            label,
                            rate_limit_attempts,
                            MAX_RATE_LIMIT_RETRIES,
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
                Err(e) => {
                    connection_attempts += 1;
                    if connection_attempts >= MAX_CONNECTION_RETRIES {
                        return Err(e).with_context(|| {
                            format!(
                                "Failed to send {} after {} retries",
                                label, MAX_CONNECTION_RETRIES
                            )
                        });
                    }

                    let backoff = Duration::from_secs(
                        INITIAL_BACKOFF_SECS * (1 << (connection_attempts - 1)),
                    );
                    println!(
                        "Connection error on {} (attempt {}/{}): {}. Retrying in {}s...",
                        label,
                        connection_attempts,
                        MAX_CONNECTION_RETRIES,
                        e,
                        backoff.as_secs()
                    );
                    sleep(backoff).await;
                }
            }
        }
    }
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
