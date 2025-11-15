use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use slack_linear_tools::{config, linear, llm, setup, slack};
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};

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

#[derive(Parser)]
#[command(name = "slack-linear", version, about)]
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
        /// Optional output directory. Defaults to SLACK_MIRROR_DIR or ./slack_mirror
        #[arg(long)]
        output_dir: Option<PathBuf>,
    },
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
        /// Optional data dir; defaults to SLACK_MIRROR_DIR
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
}
