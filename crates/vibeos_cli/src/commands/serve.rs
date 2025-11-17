use anyhow::Result;
use async_trait::async_trait;
use clap::Args;

use crate::app::AppContext;
use crate::commands::CliCommand;
use crate::dashboard;

#[derive(Args, Debug, Clone, Default)]
#[command(about = "Run the dashboard + API server")]
pub struct ServeCommand;

#[async_trait]
impl CliCommand for ServeCommand {
    async fn execute(&self, ctx: &AppContext) -> Result<()> {
        let cfg = ctx.config()?;
        dashboard::serve(cfg).await
    }
}
