use std::path::Path;
use std::time::Duration;

use super::mirror::{filter_newer, should_fetch_thread};
use super::storage::{
    append_jsonl, ensure_dir, jsonl_name, latest_ts_from_file, profile_changed, thread_filename,
    write_jsonl,
};
use super::types::{SlackMessage, SlackUserSnapshot, CONVERSATIONS_DIR, PROFILES_DIR, THREADS_DIR};
use anyhow::{anyhow, Context, Result};
use chrono::Utc;

/// Default timeout for Slack API requests (60 seconds)
const REQUEST_TIMEOUT_SECS: u64 = 60;
/// Default timeout for establishing connections (30 seconds)
const CONNECT_TIMEOUT_SECS: u64 = 30;

pub struct SlackClient {
    pub(super) http: reqwest::Client,
    pub(super) token: String,
}

impl SlackClient {
    pub fn new(token: String) -> Self {
        let http = reqwest::Client::builder()
            .timeout(Duration::from_secs(REQUEST_TIMEOUT_SECS))
            .connect_timeout(Duration::from_secs(CONNECT_TIMEOUT_SECS))
            .build()
            .expect("Failed to build HTTP client");

        Self { http, token }
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
                    } else if err_str.contains("is_archived") {
                        println!("archived, skipping");
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

        let mirror_result = self
            .mirror_conversations(&conversations_dir, &threads_dir)
            .await;

        println!("Syncing Slack user profiles...");
        let profile_result = self.sync_user_directory(output_dir).await;

        match (mirror_result, profile_result) {
            (Ok(()), Ok(())) => Ok(()),
            (Err(err), Ok(())) => Err(err),
            (Ok(()), Err(err)) => Err(err),
            (Err(mirror_err), Err(profile_err)) => {
                let profile_msg = profile_err.to_string();
                Err::<(), anyhow::Error>(mirror_err).context(format!(
                    "Slack user directory sync also failed: {}",
                    profile_msg
                ))
            }
        }
    }

    async fn mirror_conversations(
        &self,
        conversations_dir: &Path,
        threads_dir: &Path,
    ) -> Result<()> {
        let conversations = self.list_all_conversations().await?;
        println!("Found {} conversations to mirror", conversations.len());

        for conv in &conversations {
            let conv_name = conv.name.as_deref().unwrap_or(&conv.id);
            println!("Mirroring conversation {} ({})...", conv_name, conv.id);

            let conversation_path = conversations_dir.join(jsonl_name(&conv.id));
            let last_conversation_ts = latest_ts_from_file(&conversation_path)?;

            let fetched_messages = self
                .fetch_conversation_history_since(&conv.id, last_conversation_ts.as_deref())
                .await?;
            let new_messages = filter_newer(fetched_messages, last_conversation_ts.as_deref());

            match (last_conversation_ts.is_some(), new_messages.is_empty()) {
                (_, true) => println!("  Conversation already up to date"),
                (true, false) => {
                    append_jsonl(&conversation_path, &new_messages)?;
                    println!(
                        "  Appended {} messages to {}",
                        new_messages.len(),
                        conversation_path.display()
                    );
                }
                (false, false) => {
                    write_jsonl(&conversation_path, &new_messages)?;
                    println!(
                        "  Wrote {} messages to {}",
                        new_messages.len(),
                        conversation_path.display()
                    );
                }
            }

            // Sync threads: prioritize new messages first, then check existing threads
            // This balances fresh content with keeping existing threads up to date
            // Thread syncs are best-effort: errors are logged but don't fail the entire sync

            // First, sync threads for new messages (high priority)
            for msg in &new_messages {
                if should_fetch_thread(msg) {
                    let thread_ts = msg.thread_ts.as_deref().unwrap_or(&msg.ts);
                    if let Err(e) = self.sync_thread(&conv.id, thread_ts, threads_dir).await {
                        println!("  Warning: failed to sync thread {}: {}", thread_ts, e);
                    }
                }
            }

            // Skip checking existing threads for updates during regular syncs
            // to avoid rate limits. Existing threads are updated when we see new
            // messages in the conversation that reference them.
            // Full thread refresh can be done via a separate command if needed.
        }

        Ok(())
    }

    pub async fn fetch_full_thread(
        &self,
        channel_id: &str,
        thread_ts: &str,
    ) -> Result<Vec<SlackMessage>> {
        self.fetch_thread_replies_since(channel_id, thread_ts, None)
            .await
    }

    async fn sync_thread(
        &self,
        channel_id: &str,
        thread_ts: &str,
        threads_dir: &Path,
    ) -> Result<()> {
        let thread_path = threads_dir.join(thread_filename(channel_id, thread_ts));
        let last_thread_ts = latest_ts_from_file(&thread_path)?;

        let fetched_replies = self
            .fetch_thread_replies_since(channel_id, thread_ts, last_thread_ts.as_deref())
            .await?;
        let new_replies = filter_newer(fetched_replies, last_thread_ts.as_deref());

        match (last_thread_ts.is_some(), new_replies.is_empty()) {
            (_, true) => {
                // Thread up to date
            }
            (true, false) => {
                append_jsonl(&thread_path, &new_replies)?;
                println!(
                    "  Appended {} thread messages to {}",
                    new_replies.len(),
                    thread_path.display()
                );
            }
            (false, false) => {
                write_jsonl(&thread_path, &new_replies)?;
                println!(
                    "  Wrote {} thread messages to {}",
                    new_replies.len(),
                    thread_path.display()
                );
            }
        }

        Ok(())
    }

    async fn sync_user_directory(&self, output_dir: &Path) -> Result<()> {
        let profiles_dir = output_dir.join(PROFILES_DIR);
        ensure_dir(&profiles_dir)?;

        let users = self.list_all_users().await?;
        let mut updated = 0usize;
        let fetched_at = Utc::now();

        for user in users {
            let user_id = user
                .get("id")
                .and_then(|v| v.as_str())
                .ok_or_else(|| anyhow!("Slack user missing id field"))?;
            let snapshot = SlackUserSnapshot {
                fetched_at,
                user: user.clone(),
            };
            let path = profiles_dir.join(jsonl_name(user_id));
            if profile_changed(&path, &snapshot.user)? {
                append_jsonl(&path, &[snapshot])?;
                updated += 1;
            }
        }

        println!(
            "Slack profile directory updated ({} user snapshots written)",
            updated
        );

        Ok(())
    }
}
