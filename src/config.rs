use anyhow::{Context, Result};
use std::path::PathBuf;

pub fn slack_token() -> Result<String> {
    std::env::var("SLACK_TOKEN")
        .context("SLACK_TOKEN not found. Please run 'slack-linear setup' to configure your Slack token.")
}

pub fn linear_api_key() -> Result<String> {
    std::env::var("LINEAR_API_KEY")
        .context("LINEAR_API_KEY not found. Please run 'slack-linear setup' to configure your Linear API key.")
}

pub fn mirror_dir() -> Result<PathBuf> {
    let dir = std::env::var("SLACK_MIRROR_DIR")
        .unwrap_or_else(|_| "./slack_mirror".to_string());
    Ok(PathBuf::from(dir))
}

