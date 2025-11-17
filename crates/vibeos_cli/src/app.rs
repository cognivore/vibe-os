use anyhow::Result;
use clap::Parser;
use once_cell::sync::OnceCell;
use tracing_subscriber::fmt::format::FmtSpan;
use tracing_subscriber::EnvFilter;

use crate::commands::Cli;
use crate::config;

#[derive(Default)]
pub struct AppContext {
    config: OnceCell<config::Config>,
}

impl AppContext {
    pub fn new() -> Self {
        Self {
            config: OnceCell::new(),
        }
    }

    pub fn config(&self) -> Result<&config::Config> {
        self.config.get_or_try_init(config::load_config)
    }
}

pub async fn run() -> Result<()> {
    dotenvy::dotenv().ok();
    init_tracing();
    let cli = Cli::parse();
    let ctx = AppContext::new();
    cli.execute(&ctx).await
}

fn init_tracing() {
    let filter =
        EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info,tracing=info"));
    let _ = tracing_subscriber::fmt()
        .with_env_filter(filter)
        .with_target(true)
        .with_span_events(FmtSpan::CLOSE)
        .try_init();
}
