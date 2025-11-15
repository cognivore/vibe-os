use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use slack_linear_tools::{config, linear, setup, slack};
use std::path::PathBuf;

#[tokio::main]
async fn main() -> Result<()> {
    dotenvy::dotenv().ok();
    let cli = Cli::parse();

    match cli.command {
        Commands::Setup => {
            setup::run_setup().await?;
        }
        Commands::Slack { slack_cmd } => {
            match slack_cmd {
                SlackCommands::Mirror { output_dir } => {
                    let output_path = output_dir
                        .unwrap_or_else(|| config::mirror_dir().unwrap_or_else(|_| PathBuf::from("./slack_mirror")));

                    std::fs::create_dir_all(&output_path)
                        .context("Failed to create output directory")?;

                    let token = config::slack_token()?;
                    let client = slack::SlackClient::new(token);
                    client.mirror_all(&output_path).await?;
                }
            }
        }
        Commands::Linear { linear_cmd } => {
            match linear_cmd {
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
                }
            }
        }
    }

    Ok(())
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

