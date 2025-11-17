use std::path::PathBuf;

use anyhow::Result;
use async_trait::async_trait;
use clap::Subcommand;

use crate::app::AppContext;
use crate::commands::CliCommand;
use crate::config;
use crate::slack;
use crate::support::fs::{ensure_directory, resolve_output_dir};

#[derive(Subcommand, Debug, Clone)]
pub enum SlackCommand {
    /// Mirror all accessible Slack conversations to local disk
    Mirror {
        /// Optional output directory. Defaults to VIBEOS_SLACK_MIRROR_DIR or ./slack_mirror
        #[arg(long)]
        output_dir: Option<PathBuf>,
    },
    /// Join all public channels (bot will auto-join during mirror anyway)
    JoinChannels,
    /// Force-refresh a single Slack thread from the API
    ThreadFetch {
        /// Channel/conversation ID (e.g. C12345678)
        channel: String,
        /// Thread timestamp (full Slack ts, e.g. 1700000000.123456)
        thread_ts: String,
        /// Optional output directory override
        #[arg(long)]
        output_dir: Option<PathBuf>,
        /// Skip writing to disk (just print to stdout)
        #[arg(long)]
        no_save: bool,
        /// Print a formatted summary of the thread
        #[arg(long)]
        print: bool,
        /// Only read the thread from the local mirror (skip API fetch)
        #[arg(long)]
        mirror_only: bool,
    },
}

#[async_trait]
impl CliCommand for SlackCommand {
    async fn execute(&self, _ctx: &AppContext) -> Result<()> {
        match self {
            SlackCommand::Mirror { output_dir } => {
                let output_path = resolve_output_dir(output_dir.clone())?;
                ensure_directory(&output_path)?;

                let token = config::slack_token()?;
                let client = slack::SlackClient::new(token);
                client.mirror_all(&output_path).await
            }
            SlackCommand::JoinChannels => {
                let token = config::slack_token()?;
                let client = slack::SlackClient::new(token);
                client.join_all_public_channels().await
            }
            SlackCommand::ThreadFetch {
                channel,
                thread_ts,
                output_dir,
                no_save,
                print,
                mirror_only,
            } => {
                let channel_id = channel.as_str();
                let thread_ts_val = thread_ts.as_str();
                let output_path = resolve_output_dir(output_dir.clone())?;
                ensure_directory(&output_path)?;
                let threads_dir = output_path.join(slack::THREADS_DIR);
                ensure_directory(&threads_dir)?;
                let file_path = threads_dir.join(slack::thread_filename(channel_id, thread_ts_val));
                let mirror_only = *mirror_only;
                let print = *print;
                let no_save = *no_save;

                if mirror_only {
                    if !file_path.exists() {
                        anyhow::bail!(
                            "Thread file {} does not exist. Run without --mirror-only to fetch it.",
                            file_path.display()
                        );
                    }

                    let messages: Vec<slack::SlackMessage> = slack::read_jsonl(&file_path)?;
                    println!(
                        "Loaded {} Slack messages from mirror ({})",
                        messages.len(),
                        file_path.display()
                    );
                    if print || mirror_only {
                        print_slack_messages(&messages);
                    }
                    return Ok(());
                }

                let token = config::slack_token()?;
                let client = slack::SlackClient::new(token);
                println!(
                    "Fetching Slack thread channel={} thread_ts={}...",
                    channel_id, thread_ts_val
                );
                let messages = client.fetch_full_thread(channel_id, thread_ts_val).await?;
                println!(
                    "Fetched {} Slack messages for channel={} thread_ts={}",
                    messages.len(),
                    channel_id,
                    thread_ts_val
                );

                if !no_save {
                    slack::write_jsonl(&file_path, &messages)?;
                    println!("Persisted thread to {}", file_path.display());
                } else {
                    println!("Skipping persistence (--no-save specified)");
                }

                if print || no_save {
                    print_slack_messages(&messages);
                }

                Ok(())
            }
        }
    }
}

fn print_slack_messages(messages: &[slack::SlackMessage]) {
    println!("--- Thread transcript start ---");
    for message in messages {
        let user = message
            .user
            .as_deref()
            .map(|id| format!("@{id}"))
            .unwrap_or_else(|| "<unknown>".into());
        let text = message
            .text
            .as_deref()
            .unwrap_or("<no text>")
            .replace('\n', "\\n");
        println!("[{}] {}: {}", message.ts, user, text);
    }
    println!("--- Thread transcript end ---");
}
