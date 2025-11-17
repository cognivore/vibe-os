use anyhow::{Context, Result};
use reqwest::{RequestBuilder, Response, StatusCode};
use serde_json::Value;
use tokio::time::{sleep, Duration};

use super::client::SlackClient;
use super::types::{
    ConversationsHistoryResponse, ConversationsJoinResponse, ConversationsListResponse,
    ConversationsRepliesResponse, ResponseMetadata, SlackConversation, SlackMessage,
    UsersListResponse, API_BASE, CONVERSATIONS_HISTORY, CONVERSATIONS_JOIN, CONVERSATIONS_LIST,
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

    pub(super) async fn execute_request(
        &self,
        builder: RequestBuilder,
        label: &str,
    ) -> Result<Response> {
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
