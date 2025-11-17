use std::path::PathBuf;

use anyhow::Result;
use async_trait::async_trait;
use clap::Subcommand;

use crate::app::AppContext;
use crate::commands::CliCommand;
use crate::linear;
use crate::linear_analysis;
use crate::llm;
use crate::support::io::prompt_yes;
use crate::support::llm::{compose_linear_description, map_priority};
use crate::support::print::print_llm_window_summary;
use crate::support::time::resolve_llm_window;

#[derive(Subcommand, Debug, Clone)]
pub enum LlmCommand {
    /// Run LLM triage on a Slack thread (from local mirror) and print suggested issues as JSON
    TriageThread {
        /// Slack conversation (channel) ID, e.g. C0123456789
        #[arg(long)]
        conversation_id: String,
        /// Root message ts of the thread
        #[arg(long)]
        thread_ts: String,
        /// Optional data dir; defaults to VIBEOS_SLACK_MIRROR_DIR
        #[arg(long)]
        data_dir: Option<PathBuf>,
        /// If set, pretty-print the JSON result
        #[arg(long)]
        pretty: bool,
    },
    /// Run LLM triage AND create Linear issues interactively
    TriageAndCreate {
        #[arg(long)]
        conversation_id: String,
        #[arg(long)]
        thread_ts: String,
        #[arg(long)]
        data_dir: Option<PathBuf>,
        /// Linear team ID for created issues
        #[arg(long)]
        team_id: String,
        /// If true, create all suggested issues without asking
        #[arg(long)]
        yes: bool,
    },
    /// Summarize Linear activity for a time window and suggest Slack threads
    SummarizeLinear {
        /// Window selector: "yesterday", "last-week", or "custom"
        #[arg(long, default_value = "yesterday")]
        window: String,
        /// Optional explicit start (overrides window) as ISO date or datetime
        #[arg(long)]
        since: Option<String>,
        /// Optional explicit end (overrides window) as ISO date or datetime
        #[arg(long)]
        until: Option<String>,
        /// If set, print the raw JSON returned by the LLM
        #[arg(long)]
        json: bool,
    },
}

#[async_trait]
impl CliCommand for LlmCommand {
    async fn execute(&self, ctx: &AppContext) -> Result<()> {
        match self {
            LlmCommand::TriageThread {
                conversation_id,
                thread_ts,
                data_dir,
                pretty,
            } => {
                let cfg = ctx.config()?;
                let mirror_dir = data_dir
                    .clone()
                    .unwrap_or_else(|| cfg.slack_mirror_dir.clone());
                let thread = llm::load_thread_from_mirror(&mirror_dir, conversation_id, thread_ts)?;
                let thread_text = llm::format_thread_for_llm(&thread.messages);
                let llm_client = llm::LlmClient::new_from_config(cfg)?;
                let suggestions = llm_client.suggest_issues_for_thread(&thread_text)?;

                let output = if *pretty {
                    serde_json::to_string_pretty(&suggestions)?
                } else {
                    serde_json::to_string(&suggestions)?
                };

                println!("{}", output);
                Ok(())
            }
            LlmCommand::TriageAndCreate {
                conversation_id,
                thread_ts,
                data_dir,
                team_id,
                yes,
            } => {
                let cfg = ctx.config()?;
                let mirror_dir = data_dir
                    .clone()
                    .unwrap_or_else(|| cfg.slack_mirror_dir.clone());
                let thread = llm::load_thread_from_mirror(&mirror_dir, conversation_id, thread_ts)?;
                let thread_text = llm::format_thread_for_llm(&thread.messages);
                let llm_client = llm::LlmClient::new_from_config(cfg)?;
                let suggestions = llm_client.suggest_issues_for_thread(&thread_text)?;

                eprintln!(
                    "LLM summary: {}\nReason: {}",
                    suggestions.thread_summary, suggestions.reason
                );

                if !suggestions.should_create_issues || suggestions.issues.is_empty() {
                    eprintln!("No issues suggested for this thread.");
                    return Ok(());
                }

                let linear_client = linear::LinearClient::new(cfg.linear_api_key.clone());
                for issue in suggestions.issues.iter() {
                    if !yes {
                        eprintln!(
                            "\nTitle: {}\nPriority: {}\nPreview: {}",
                            issue.title,
                            issue.priority,
                            issue
                                .description_markdown
                                .chars()
                                .take(120)
                                .collect::<String>()
                        );
                        if !prompt_yes("Create this issue in Linear? [y/N] ")? {
                            continue;
                        }
                    }

                    let description = compose_linear_description(issue, &thread, &suggestions);
                    let priority = map_priority(&issue.priority);
                    let created = linear_client
                        .create_issue(team_id, &issue.title, &description, priority, None)
                        .await?;

                    if let Some(url) = &created.url {
                        println!(
                            "Created {}: {} ({})",
                            created.identifier, created.title, url
                        );
                    } else {
                        println!("Created {}: {}", created.identifier, created.title);
                    }
                }

                Ok(())
            }
            LlmCommand::SummarizeLinear {
                window,
                since,
                until,
                json,
            } => {
                let cfg = ctx.config()?;
                let (window_start, window_end) =
                    resolve_llm_window(window, since.as_deref(), until.as_deref())?;

                let issues = linear_analysis::load_issues(&cfg.linear_mirror_dir)?;
                let events = linear_analysis::load_events(&cfg.linear_mirror_dir)?;
                const DEFAULT_LOOKBACK_DAYS: i64 = 14;
                let window_context = linear_analysis::compute_window_context(
                    &issues,
                    &events,
                    window_start,
                    window_end,
                    DEFAULT_LOOKBACK_DAYS,
                );

                let llm_client = llm::LlmClient::new_from_config(cfg)?;
                let summary = llm_client.summarize_linear_window(&window_context, &issues)?;

                if *json {
                    println!("{}", serde_json::to_string_pretty(&summary)?);
                } else {
                    print_llm_window_summary(&summary);
                }
                Ok(())
            }
        }
    }
}
