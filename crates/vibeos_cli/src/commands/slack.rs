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
        }
    }
}
