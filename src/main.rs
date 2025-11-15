use anyhow::{anyhow, Context, Result};
use chrono::{DateTime, Duration, NaiveDate, TimeZone, Utc};
use clap::{Parser, Subcommand};
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use vibeos::{config, linear, linear_analysis, linear_sync, llm, setup, slack};

#[tokio::main]
async fn main() -> Result<()> {
    dotenvy::dotenv().ok();
    let cli = Cli::parse();

    match cli.command {
        Commands::Setup => setup::run_setup().await,
        Commands::Slack { slack_cmd } => handle_slack(slack_cmd).await,
        Commands::Linear { linear_cmd } => handle_linear(linear_cmd).await,
        Commands::Llm { llm_cmd } => handle_llm(llm_cmd).await,
    }
}

async fn handle_slack(command: SlackCommands) -> Result<()> {
    match command {
        SlackCommands::Mirror { output_dir } => {
            let output_path = resolve_output_dir(output_dir)?;
            ensure_directory(&output_path)?;

            let token = config::slack_token()?;
            let client = slack::SlackClient::new(token);
            client.mirror_all(&output_path).await
        }
        SlackCommands::JoinChannels => {
            let token = config::slack_token()?;
            let client = slack::SlackClient::new(token);
            client.join_all_public_channels().await
        }
    }
}

async fn handle_linear(command: LinearCommands) -> Result<()> {
    match command {
        LinearCommands::CreateIssue {
            team_id,
            title,
            description,
            priority,
        } => {
            let api_key = config::linear_api_key()?;
            let client = linear::LinearClient::new(api_key);
            let issue = client
                .create_issue(&team_id, &title, &description, priority)
                .await?;

            println!("Created issue {} (id={})", issue.identifier, issue.id);
            if let Some(url) = issue.url {
                println!("  URL: {}", url);
            }
            Ok(())
        }
        LinearCommands::Sync { output_dir } => {
            let cfg = config::load_config()?;
            let target = output_dir.unwrap_or_else(|| cfg.linear_mirror_dir.clone());
            ensure_directory(&target)?;

            let client = linear::LinearClient::new(cfg.linear_api_key.clone());
            let sync = linear_sync::LinearSync::new(&client);
            println!("Syncing Linear workspace into {}", target.display());
            sync.full_sync(&target).await?;
            println!("Linear sync complete.");
            Ok(())
        }
        LinearCommands::Events { since, until, json } => {
            let cfg = config::load_config()?;
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

            if json {
                println!("{}", serde_json::to_string_pretty(&window)?);
            } else {
                print_window_summary(&window);
            }
            Ok(())
        }
        LinearCommands::Stale { days, limit, json } => {
            let cfg = config::load_config()?;
            let window_end = Utc::now();
            let window_start = window_end - Duration::days(days.max(1));
            let issues = linear_analysis::load_issues(&cfg.linear_mirror_dir)?;
            let events = linear_analysis::load_events(&cfg.linear_mirror_dir)?;
            let window = linear_analysis::compute_window_context(
                &issues,
                &events,
                window_start,
                window_end,
                days,
            );
            let mut stale = window.stale_issues.clone();
            if let Some(limit) = limit {
                stale.truncate(limit);
            }

            if json {
                println!("{}", serde_json::to_string_pretty(&stale)?);
            } else {
                print_stale_issues(days, &stale, window_end);
            }

            Ok(())
        }
        LinearCommands::Browse {
            search,
            state,
            assignee,
            label,
            limit,
            json,
        } => {
            let cfg = config::load_config()?;
            let issues = linear_analysis::load_issues(&cfg.linear_mirror_dir)?;

            let mut filtered: Vec<&linear_sync::LinearIssueSnapshot> = issues.iter().collect();
            if let Some(term) = search {
                let needle = term.to_lowercase();
                filtered.retain(|issue| issue.title.to_lowercase().contains(&needle));
            }
            if let Some(state_name) = state {
                filtered.retain(|issue| issue.state_name.as_deref() == Some(state_name.as_str()));
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
            filtered.truncate(limit);

            if json {
                println!("{}", serde_json::to_string_pretty(&filtered)?);
            } else {
                print_browse_results(&filtered);
            }

            Ok(())
        }
    }
}

async fn handle_llm(command: LlmCommands) -> Result<()> {
    match command {
        LlmCommands::TriageThread {
            conversation_id,
            thread_ts,
            data_dir,
            pretty,
        } => {
            let cfg = config::load_config()?;
            let mirror_dir = data_dir.unwrap_or_else(|| cfg.slack_mirror_dir.clone());
            let thread = llm::load_thread_from_mirror(&mirror_dir, &conversation_id, &thread_ts)?;
            let thread_text = llm::format_thread_for_llm(&thread.messages);
            let llm_client = llm::LlmClient::new_from_config(&cfg)?;
            let suggestions = llm_client.suggest_issues_for_thread(&thread_text)?;

            let output = if pretty {
                serde_json::to_string_pretty(&suggestions)?
            } else {
                serde_json::to_string(&suggestions)?
            };

            println!("{}", output);
            Ok(())
        }
        LlmCommands::TriageAndCreate {
            conversation_id,
            thread_ts,
            data_dir,
            team_id,
            yes,
        } => {
            let cfg = config::load_config()?;
            let mirror_dir = data_dir.unwrap_or_else(|| cfg.slack_mirror_dir.clone());
            let thread = llm::load_thread_from_mirror(&mirror_dir, &conversation_id, &thread_ts)?;
            let thread_text = llm::format_thread_for_llm(&thread.messages);
            let llm_client = llm::LlmClient::new_from_config(&cfg)?;
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
                    .create_issue(&team_id, &issue.title, &description, priority)
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
        LlmCommands::SummarizeLinear {
            window,
            since,
            until,
            json,
        } => {
            let cfg = config::load_config()?;
            let (window_start, window_end) =
                resolve_llm_window(&window, since.as_deref(), until.as_deref())?;

            let issues = linear_analysis::load_issues(&cfg.linear_mirror_dir)?;
            let events = linear_analysis::load_events(&cfg.linear_mirror_dir)?;
            let window_context = linear_analysis::compute_window_context(
                &issues,
                &events,
                window_start,
                window_end,
                14,
            );

            let llm_client = llm::LlmClient::new_from_config(&cfg)?;
            let summary = llm_client.summarize_linear_window(&window_context, &issues)?;

            if json {
                println!("{}", serde_json::to_string_pretty(&summary)?);
            } else {
                print_llm_window_summary(&summary);
            }
            Ok(())
        }
    }
}

fn resolve_output_dir(arg: Option<PathBuf>) -> Result<PathBuf> {
    arg.map_or_else(config::mirror_dir, Ok)
}

fn ensure_directory(dir: &Path) -> Result<()> {
    fs::create_dir_all(dir).with_context(|| format!("Failed to create output directory {:?}", dir))
}

fn prompt_yes(prompt: &str) -> Result<bool> {
    print!("{prompt}");
    io::stdout().flush().context("Failed to flush stdout")?;
    let mut buffer = String::new();
    io::stdin()
        .read_line(&mut buffer)
        .context("Failed to read response")?;
    let decision = buffer.trim().to_ascii_lowercase();
    Ok(matches!(decision.as_str(), "y" | "yes"))
}

fn map_priority(priority: &str) -> Option<i32> {
    match priority {
        "P0" => Some(1),
        "P1" => Some(2),
        "P2" => Some(3),
        "P3" => Some(4),
        _ => None,
    }
}

fn compose_linear_description(
    issue: &llm::LlmIssueSuggestion,
    thread: &llm::SlackThread,
    suggestions: &llm::LlmSuggestionResponse,
) -> String {
    let mut description = issue.description_markdown.clone();

    description.push_str("\n\n---\n");
    description.push_str("Why this issue:\n");
    description.push_str(&issue.why_this_issue);

    description.push_str("\n\nSuggested from Slack thread:\n");
    description.push_str(&suggestions.thread_summary);

    if !issue.linked_slack_message_ts.is_empty() {
        description.push_str("\n\nLinked Slack messages:\n");
        for ts in &issue.linked_slack_message_ts {
            description.push_str(&format!(
                "- Conversation {} thread {} message {}\n",
                thread.conversation_id, thread.thread_ts, ts
            ));
        }
    }

    description
}

fn resolve_events_window(
    since: Option<&str>,
    until: Option<&str>,
) -> Result<(DateTime<Utc>, DateTime<Utc>)> {
    let now = Utc::now();
    let default_start = now - Duration::days(1);
    let start = match since {
        Some(raw) => parse_datetime_input(raw)?,
        None => default_start,
    };
    let end = match until {
        Some(raw) => parse_datetime_input(raw)?,
        None => now,
    };

    if end <= start {
        anyhow::bail!("`until` must be after `since`");
    }
    Ok((start, end))
}

fn resolve_llm_window(
    selector: &str,
    since: Option<&str>,
    until: Option<&str>,
) -> Result<(DateTime<Utc>, DateTime<Utc>)> {
    let mut bounds = default_window_for_selector(selector)?;

    if selector == "custom" && since.is_none() && until.is_none() {
        anyhow::bail!("custom window requires --since and/or --until");
    }

    if let Some(raw) = since {
        bounds.0 = parse_datetime_input(raw)?;
    }
    if let Some(raw) = until {
        bounds.1 = parse_datetime_input(raw)?;
    }
    if bounds.1 <= bounds.0 {
        anyhow::bail!("window end must be after start");
    }
    Ok(bounds)
}

fn default_window_for_selector(selector: &str) -> Result<(DateTime<Utc>, DateTime<Utc>)> {
    let now = Utc::now();
    match selector {
        "yesterday" => {
            let yesterday = now.date_naive() - Duration::days(1);
            let start = Utc.from_utc_datetime(
                &yesterday
                    .and_hms_opt(0, 0, 0)
                    .ok_or_else(|| anyhow!("invalid midnight for yesterday"))?,
            );
            Ok((start, start + Duration::days(1)))
        }
        "last-week" => Ok((now - Duration::days(7), now)),
        "custom" => Ok((now - Duration::days(1), now)),
        other => Err(anyhow!("unknown window selector `{}`", other)),
    }
}

fn parse_datetime_input(raw: &str) -> Result<DateTime<Utc>> {
    if let Ok(date) = NaiveDate::parse_from_str(raw, "%Y-%m-%d") {
        let naive = date
            .and_hms_opt(0, 0, 0)
            .ok_or_else(|| anyhow!("invalid midnight for date {}", raw))?;
        return Ok(DateTime::<Utc>::from_naive_utc_and_offset(naive, Utc));
    }

    DateTime::parse_from_rfc3339(raw)
        .map(|dt| dt.with_timezone(&Utc))
        .map_err(|err| anyhow!("failed to parse `{}` as ISO datetime: {}", raw, err))
}

fn print_window_summary(context: &linear_analysis::LinearWindowContext) {
    println!(
        "Linear activity from {} to {}",
        context.window_start, context.window_end
    );
    println!(
        "Created: {} | Completed: {} | Reopened: {} | Archived: {}",
        context.issues_created,
        context.issues_completed,
        context.issues_reopened,
        context.issues_archived
    );
    println!(
        "State transitions: {} | Comments: {}",
        context.transitions, context.comments_added
    );

    if context.top_active_issues.is_empty() {
        println!("No active issues in this window.");
    } else {
        println!("\nTop active issues:");
        for issue in &context.top_active_issues {
            print_issue_summary(issue);
        }
    }
}

fn print_stale_issues(
    stale_days: i64,
    issues: &[linear_analysis::LinearIssueSummary],
    as_of: DateTime<Utc>,
) {
    println!(
        "{} stale issues (> {} days without updates) as of {}",
        issues.len(),
        stale_days,
        as_of
    );

    for issue in issues {
        let age_days = (as_of - issue.updated_at).num_days().max(0);
        let assignee = issue
            .assignee_name
            .as_deref()
            .unwrap_or("Unassigned")
            .to_string();
        println!(
            "- {} [{}] {} (assignee: {}, updated {} days ago)",
            issue.identifier,
            issue
                .state_name
                .as_deref()
                .or(issue.state_type.as_deref())
                .unwrap_or("Unknown"),
            issue.title,
            assignee,
            age_days
        );
        if let Some(url) = &issue.url {
            println!("  {}", url);
        }
    }
}

fn print_browse_results(issues: &[&linear_sync::LinearIssueSnapshot]) {
    if issues.is_empty() {
        println!("No issues match the provided filters.");
        return;
    }

    for issue in issues {
        let assignee = issue
            .assignee_name
            .as_deref()
            .unwrap_or("Unassigned")
            .to_string();
        let team = issue
            .team_key
            .as_deref()
            .or(issue.team_name.as_deref())
            .unwrap_or("Unknown");
        println!(
            "{} [{} | team {}] {} (assignee: {}, updated: {})",
            issue.identifier,
            issue
                .state_name
                .as_deref()
                .or(issue.state_type.as_deref())
                .unwrap_or("Unknown"),
            team,
            issue.title,
            assignee,
            issue.updated_at
        );
        if let Some(url) = &issue.url {
            println!("  {}", url);
        }
        if !issue.labels.is_empty() {
            println!("  labels: {}", issue.labels.join(", "));
        }
    }
}

fn print_issue_summary(issue: &linear_analysis::LinearIssueSummary) {
    let assignee = issue
        .assignee_name
        .as_deref()
        .unwrap_or("Unassigned")
        .to_string();
    println!(
        "- {} [{}] {} (assignee: {}, updated: {})",
        issue.identifier,
        issue
            .state_name
            .as_deref()
            .or(issue.state_type.as_deref())
            .unwrap_or("Unknown"),
        issue.title,
        assignee,
        issue.updated_at
    );
    if let Some(url) = &issue.url {
        println!("  {}", url);
    }
}

fn print_llm_window_summary(summary: &llm::LlmLinearWindowSummary) {
    println!(
        "Linear summary for {} to {}",
        summary.window_start, summary.window_end
    );
    println!("--------------------------------------------------");
    println!("{}", summary.summary_markdown);
    println!("\nKey metrics:\n{}", summary.key_metrics_markdown);

    if summary.suggested_slack_threads.is_empty() {
        println!("\nNo Slack thread suggestions.");
    } else {
        println!("\nSuggested Slack threads:");
        for (idx, suggestion) in summary.suggested_slack_threads.iter().enumerate() {
            println!(
                "{}. {} (channel: {})",
                idx + 1,
                suggestion.title,
                suggestion
                    .channel_hint
                    .as_deref()
                    .unwrap_or("<unspecified>")
            );
            if !suggestion.related_issue_identifiers.is_empty() {
                println!(
                    "   Issues: {}",
                    suggestion.related_issue_identifiers.join(", ")
                );
            }
            println!("   Message:\n{}\n", suggestion.message_markdown);
        }
    }
}

#[derive(Parser)]
#[command(name = "vibeos", version, about)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Setup,
    Slack {
        #[command(subcommand)]
        slack_cmd: SlackCommands,
    },
    Linear {
        #[command(subcommand)]
        linear_cmd: LinearCommands,
    },
    Llm {
        #[command(subcommand)]
        llm_cmd: LlmCommands,
    },
}

#[derive(Subcommand)]
enum SlackCommands {
    /// Mirror all accessible Slack conversations to local disk
    Mirror {
        /// Optional output directory. Defaults to VIBEOS_SLACK_MIRROR_DIR or ./slack_mirror
        #[arg(long)]
        output_dir: Option<PathBuf>,
    },
    /// Join all public channels (bot will auto-join during mirror anyway)
    JoinChannels,
}

#[derive(Subcommand)]
enum LinearCommands {
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
}

#[derive(Subcommand)]
enum LlmCommands {
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
