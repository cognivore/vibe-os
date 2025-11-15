use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use slack_linear_tools::{config, linear, setup, slack};
use std::fs;
use std::path::{Path, PathBuf};

#[tokio::main]
async fn main() -> Result<()> {
    dotenvy::dotenv().ok();
    let cli = Cli::parse();

    match cli.command {
        Commands::Setup => setup::run_setup().await,
        Commands::Slack { slack_cmd } => handle_slack(slack_cmd).await,
        Commands::Linear { linear_cmd } => handle_linear(linear_cmd).await,
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

fn resolve_output_dir(arg: Option<PathBuf>) -> Result<PathBuf> {
    arg.map_or_else(config::mirror_dir, Ok)
}

fn ensure_directory(dir: &Path) -> Result<()> {
    fs::create_dir_all(dir).with_context(|| format!("Failed to create output directory {:?}", dir))
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
