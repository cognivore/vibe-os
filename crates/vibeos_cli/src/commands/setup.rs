use anyhow::Result;
use async_trait::async_trait;
use clap::Args;

use crate::app::AppContext;
use crate::commands::CliCommand;
use crate::setup;

#[derive(Args, Debug, Clone, Default)]
#[command(about = "Run the interactive setup flow")]
pub struct SetupCommand;

#[async_trait]
impl CliCommand for SetupCommand {
    async fn execute(&self, _ctx: &AppContext) -> Result<()> {
        setup::run_setup().await
    }
}
