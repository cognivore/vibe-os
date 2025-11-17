use std::path::Path;

use super::mirror::{filter_newer, should_fetch_thread};
use super::storage::{
    append_jsonl, ensure_dir, jsonl_name, latest_ts_from_file, profile_changed, thread_filename,
    write_jsonl,
};
use super::types::{SlackUserSnapshot, CONVERSATIONS_DIR, PROFILES_DIR, THREADS_DIR};
use anyhow::{anyhow, Result};
use chrono::Utc;

pub struct SlackClient {
    pub(super) http: reqwest::Client,
    pub(super) token: String,
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

            for msg in &new_messages {
                if should_fetch_thread(msg) {
                    let thread_ts = msg.thread_ts.as_deref().unwrap_or(&msg.ts);
                    let thread_path = threads_dir.join(thread_filename(&conv.id, thread_ts));
                    let last_thread_ts = latest_ts_from_file(&thread_path)?;

                    let fetched_replies = self
                        .fetch_thread_replies_since(&conv.id, thread_ts, last_thread_ts.as_deref())
                        .await?;
                    let new_replies = filter_newer(fetched_replies, last_thread_ts.as_deref());

                    match (last_thread_ts.is_some(), new_replies.is_empty()) {
                        (_, true) => {
                            println!("  Thread {} already up to date", thread_path.display())
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
                }
            }
        }

        println!("Syncing Slack user profiles...");
        self.sync_user_directory(output_dir).await?;

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
