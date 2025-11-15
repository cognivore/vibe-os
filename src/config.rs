use anyhow::{Context, Result};
use std::env;
use std::path::PathBuf;

const SLACK_TOKEN_VAR: &str = "SLACK_TOKEN";
const LINEAR_TOKEN_VAR: &str = "LINEAR_API_KEY";
const MIRROR_DIR_VAR: &str = "SLACK_MIRROR_DIR";
const DEFAULT_MIRROR_DIR: &str = "./slack_mirror";

pub fn slack_token() -> Result<String> {
    env::var(SLACK_TOKEN_VAR).context(
        "SLACK_TOKEN not found. Please run 'slack-linear setup' to configure your Slack token.",
    )
}

pub fn linear_api_key() -> Result<String> {
    env::var(LINEAR_TOKEN_VAR)
        .context("LINEAR_API_KEY not found. Please run 'slack-linear setup' to configure your Linear API key.")
}

pub fn mirror_dir() -> Result<PathBuf> {
    env::var(MIRROR_DIR_VAR)
        .map(PathBuf::from)
        .or_else(|_| Ok(PathBuf::from(DEFAULT_MIRROR_DIR)))
}
