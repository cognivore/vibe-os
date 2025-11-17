use std::path::PathBuf;

use anyhow::Result;
use async_trait::async_trait;
use chrono::{Duration, Utc};
use clap::Subcommand;

use crate::app::AppContext;
use crate::commands::CliCommand;
use crate::config;
use crate::easy_send;
use crate::linear;
use crate::linear_analysis;
use crate::linear_sync;
use crate::support::fs::ensure_directory;
use crate::support::print::{print_browse_results, print_stale_issues, print_window_summary};
use crate::support::time::resolve_events_window;

#[derive(Subcommand, Debug, Clone)]
pub enum LinearCommand {
    /// Create a new Linear issue
    CreateIssue {
        /// Linear team ID (UUID-like string)
        #[arg(long)]
        team_id: String,
        /// Issue title
        #[arg(long)]
        title: String,
        /// Issue description (short)
        #[arg(long)]
        description: String,
        /// Optional numeric priority (0..4)
        #[arg(long)]
        priority: Option<i32>,
        /// Optional assignee ID (UUID-like string)
        #[arg(long)]
        assignee_id: Option<String>,
    },
    /// Full sync of Linear workspace issues and history to local disk
    Sync {
        /// Optional output dir; defaults to LINEAR_MIRROR_DIR from config
        #[arg(long)]
        output_dir: Option<PathBuf>,
    },
    /// Show a quick summary of recent Linear events (purely algorithmic)
    Events {
        /// ISO date like 2025-03-01, or full RFC3339 string. Defaults to "yesterday".
        #[arg(long)]
        since: Option<String>,
        /// ISO date/time; defaults to "now".
        #[arg(long)]
        until: Option<String>,
        /// If set, output JSON rather than human-readable text
        #[arg(long)]
        json: bool,
    },
    /// List stale issues (purely algorithmic)
    Stale {
        /// Number of days without updates to consider an issue stale
        #[arg(long, default_value_t = 14)]
        days: i64,
        /// Optional max number of issues to show
        #[arg(long)]
        limit: Option<usize>,
        /// If set, output JSON rather than human-readable text
        #[arg(long)]
        json: bool,
    },
    /// Explore issues from the mirror with simple filters
    Browse {
        /// Optional substring filter on issue title
        #[arg(long)]
        search: Option<String>,
        /// Optional exact state name filter, e.g. "In Progress"
        #[arg(long)]
        state: Option<String>,
        /// Optional assignee name substring
        #[arg(long)]
        assignee: Option<String>,
        /// Optional label substring
        #[arg(long)]
        label: Option<String>,
        /// Max results
        #[arg(long, default_value_t = 50)]
        limit: usize,
        /// If set, output JSON records instead of pretty text
        #[arg(long)]
        json: bool,
    },
    /// Create issue from friendly YAML or pipe format (user@team)
    EasySend {
        /// Path to YAML file (if not provided, reads from stdin)
        file: Option<PathBuf>,
    },
}

#[async_trait]
impl CliCommand for LinearCommand {
    async fn execute(&self, ctx: &AppContext) -> Result<()> {
        match self {
            LinearCommand::CreateIssue {
                team_id,
                title,
                description,
                priority,
                assignee_id,
            } => {
                let api_key = config::linear_api_key()?;
                let client = linear::LinearClient::new(api_key);
                let issue = client
                    .create_issue(team_id, title, description, *priority, assignee_id.as_deref())
                    .await?;

                println!("Created issue {} (id={})", issue.identifier, issue.id);
                if let Some(url) = issue.url {
                    println!("  URL: {}", url);
                }
                Ok(())
            }
            LinearCommand::Sync { output_dir } => {
                let cfg = ctx.config()?;
                let target = output_dir
                    .clone()
                    .unwrap_or_else(|| cfg.linear_mirror_dir.clone());
                ensure_directory(&target)?;

                let client = linear::LinearClient::new(cfg.linear_api_key.clone());
                let sync = linear_sync::LinearSync::new(&client);
                println!("Syncing Linear workspace into {}", target.display());
                sync.full_sync(&target).await?;
                println!("Linear sync complete.");
                Ok(())
            }
            LinearCommand::Events { since, until, json } => {
                let cfg = ctx.config()?;
                let (window_start, window_end) =
                    resolve_events_window(since.as_deref(), until.as_deref())?;

                let issues = linear_analysis::load_issues(&cfg.linear_mirror_dir)?;
                let events = linear_analysis::load_events(&cfg.linear_mirror_dir)?;
                let window = linear_analysis::compute_window_context(
                    &issues,
                    &events,
                    window_start,
                    window_end,
                    14,
                );

                if *json {
                    println!("{}", serde_json::to_string_pretty(&window)?);
                } else {
                    print_window_summary(&window);
                }
                Ok(())
            }
            LinearCommand::Stale { days, limit, json } => {
                let cfg = ctx.config()?;
                let window_end = Utc::now();
                let stale_days = (*days).max(1);
                let window_start = window_end - Duration::days(stale_days);
                let issues = linear_analysis::load_issues(&cfg.linear_mirror_dir)?;
                let events = linear_analysis::load_events(&cfg.linear_mirror_dir)?;
                let window = linear_analysis::compute_window_context(
                    &issues,
                    &events,
                    window_start,
                    window_end,
                    *days,
                );
                let mut stale = window.stale_issues.clone();
                if let Some(limit) = limit {
                    stale.truncate(*limit);
                }

                if *json {
                    println!("{}", serde_json::to_string_pretty(&stale)?);
                } else {
                    print_stale_issues(*days, &stale, window_end);
                }

                Ok(())
            }
            LinearCommand::Browse {
                search,
                state,
                assignee,
                label,
                limit,
                json,
            } => {
                let cfg = ctx.config()?;
                let issues = linear_analysis::load_issues(&cfg.linear_mirror_dir)?;

                let mut filtered: Vec<&linear_sync::LinearIssueSnapshot> = issues.iter().collect();
                if let Some(term) = search {
                    let needle = term.to_lowercase();
                    filtered.retain(|issue| issue.title.to_lowercase().contains(&needle));
                }
                if let Some(state_name) = state {
                    filtered
                        .retain(|issue| issue.state_name.as_deref() == Some(state_name.as_str()));
                }
                if let Some(assignee_term) = assignee {
                    let needle = assignee_term.to_lowercase();
                    filtered.retain(|issue| {
                        issue
                            .assignee_name
                            .as_deref()
                            .map(|name| name.to_lowercase().contains(&needle))
                            .unwrap_or(false)
                    });
                }
                if let Some(label_term) = label {
                    let needle = label_term.to_lowercase();
                    filtered.retain(|issue| {
                        issue
                            .labels
                            .iter()
                            .any(|label| label.to_lowercase().contains(&needle))
                    });
                }

                filtered.sort_by(|a, b| b.updated_at.cmp(&a.updated_at));
                filtered.truncate(*limit);

                if *json {
                    println!("{}", serde_json::to_string_pretty(&filtered)?);
                } else {
                    print_browse_results(&filtered);
                }

                Ok(())
            }
            LinearCommand::EasySend { file } => {
                let cfg = ctx.config()?;

                // Read input from file or stdin
                let input = easy_send::read_input_from_file_or_stdin(file.as_deref())?;

                // Parse the input (YAML or pipe format)
                let request = easy_send::parse_easy_send_input(&input)?;

                // Load Linear mirror to match names to IDs
                let issues = linear_analysis::load_issues(&cfg.linear_mirror_dir)?;

                // Match assignee and team, fail on ambiguity
                let assignee_id = easy_send::match_assignee(&request.assignee, &issues)?;
                let team_id = easy_send::match_team(&request.team, &issues)?;

                // Compose the description
                let description = format!("# Why\n\n{}\n\n# What\n\n{}", request.why, request.what);

                // Create a title from the first line of "why"
                let title = request
                    .why
                    .lines()
                    .find(|l| !l.trim().is_empty())
                    .unwrap_or("Untitled issue")
                    .trim()
                    .chars()
                    .take(80)
                    .collect::<String>();

                // Create the issue
                let api_key = config::linear_api_key()?;
                let client = linear::LinearClient::new(api_key);
                let issue = client
                    .create_issue(&team_id, &title, &description, Some(1), Some(&assignee_id))
                    .await?;

                println!("Created issue {} (id={})", issue.identifier, issue.id);
                if let Some(url) = issue.url {
                    println!("  URL: {}", url);
                }
                println!("  Team: {}", request.team);
                println!("  Assignee: {}", request.assignee);

                Ok(())
            }
        }
    }
}
