use std::net::SocketAddr;

use anyhow::{Context, Result};
use dashboard_api::{run_dashboard_server, DashboardServerSettings};

use crate::config::Config;

pub async fn serve(cfg: &Config) -> Result<()> {
    let bind: SocketAddr = cfg
        .dashboard_bind
        .parse()
        .with_context(|| format!("failed to parse dashboard bind `{}`", cfg.dashboard_bind))?;

    let settings = DashboardServerSettings {
        slack_mirror_dir: cfg.slack_mirror_dir.clone(),
        linear_mirror_dir: cfg.linear_mirror_dir.clone(),
        arrow_store_dir: cfg.arrow_store_dir.clone(),
        persona_root_dir: cfg.persona_root.clone(),
        bind,
        static_dir: cfg.dashboard_static_dir.clone(),
        slack_token: Some(cfg.slack_token.clone()),
    };

    run_dashboard_server(settings).await
}
