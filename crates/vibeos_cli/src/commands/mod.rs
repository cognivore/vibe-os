pub mod linear;
pub mod llm;
pub mod serve;
pub mod setup;
pub mod slack;

use anyhow::Result;
use async_trait::async_trait;
use clap::{Parser, Subcommand};

use crate::app::AppContext;

pub use linear::LinearCommand;
pub use llm::LlmCommand;
pub use serve::ServeCommand;
pub use setup::SetupCommand;
pub use slack::SlackCommand;

#[async_trait]
pub trait CliCommand {
    async fn execute(&self, ctx: &AppContext) -> Result<()>;
}

#[derive(Parser, Debug, Clone)]
#[command(name = "vibeos", version, about)]
pub struct Cli {
    #[command(subcommand)]
    pub command: RootCommand,
}

#[derive(Subcommand, Debug, Clone)]
pub enum RootCommand {
    Setup(SetupCommand),
    Serve(ServeCommand),
    #[command(subcommand)]
    Slack(SlackCommand),
    #[command(subcommand)]
    Linear(LinearCommand),
    #[command(subcommand)]
    Llm(LlmCommand),
}

impl Cli {
    pub async fn execute(self, ctx: &AppContext) -> Result<()> {
        match self.command {
            RootCommand::Setup(cmd) => cmd.execute(ctx).await,
            RootCommand::Serve(cmd) => cmd.execute(ctx).await,
            RootCommand::Slack(cmd) => cmd.execute(ctx).await,
            RootCommand::Linear(cmd) => cmd.execute(ctx).await,
            RootCommand::Llm(cmd) => cmd.execute(ctx).await,
        }
    }
}
