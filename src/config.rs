use anyhow::{Context, Result};
use std::env;
use std::path::PathBuf;

const SLACK_TOKEN_VAR: &str = "SLACK_TOKEN";
const LINEAR_TOKEN_VAR: &str = "LINEAR_API_KEY";
const MIRROR_DIR_VAR: &str = "SLACK_MIRROR_DIR";
const DEFAULT_MIRROR_DIR: &str = "./slack_mirror";
const LLM_API_BASE_VAR: &str = "LLM_API_BASE";
const LLM_API_KEY_VAR: &str = "LLM_API_KEY";
const LLM_MODEL_VAR: &str = "LLM_MODEL";
const LLM_TEMPERATURE_VAR: &str = "LLM_TEMPERATURE";
const DEFAULT_LLM_TEMPERATURE: f32 = 0.2;

#[derive(Debug, Clone)]
pub struct Config {
    pub slack_token: String,
    pub linear_api_key: String,
    pub slack_mirror_dir: PathBuf,
    pub llm_api_base: String,
    pub llm_api_key: Option<String>,
    pub llm_model: String,
    pub llm_temperature: f32,
}

impl Config {
    pub fn load() -> Result<Self> {
        load_config()
    }
}

pub fn load_config() -> Result<Config> {
    dotenvy::dotenv().ok();

    let slack_token = env::var(SLACK_TOKEN_VAR).context(
        "SLACK_TOKEN not found. Please run 'slack-linear setup' to configure your Slack token.",
    )?;

    let linear_api_key = env::var(LINEAR_TOKEN_VAR).context(
        "LINEAR_API_KEY not found. Please run 'slack-linear setup' to configure your Linear API key.",
    )?;

    let slack_mirror_dir = env::var(MIRROR_DIR_VAR)
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from(DEFAULT_MIRROR_DIR));

    let llm_api_base = env::var(LLM_API_BASE_VAR).context(
        "LLM_API_BASE is missing. Set it to an OpenAI-compatible base URL (e.g. https://api.openai.com/v1) or rerun setup.",
    )?;

    let llm_model = env::var(LLM_MODEL_VAR).context(
        "LLM_MODEL is missing. Provide the model identifier to use for triage (e.g. gemma-7b-instruct).",
    )?;

    let llm_api_key = env::var(LLM_API_KEY_VAR).ok().and_then(|key| {
        let trimmed = key.trim().to_owned();
        if trimmed.is_empty() {
            None
        } else {
            Some(trimmed)
        }
    });

    let llm_temperature = match env::var(LLM_TEMPERATURE_VAR) {
        Ok(raw) => raw.parse::<f32>().with_context(|| {
            format!("LLM_TEMPERATURE `{raw}` is not a valid floating point number")
        })?,
        Err(_) => DEFAULT_LLM_TEMPERATURE,
    };

    Ok(Config {
        slack_token,
        linear_api_key,
        slack_mirror_dir,
        llm_api_base,
        llm_api_key,
        llm_model,
        llm_temperature,
    })
}

pub fn slack_token() -> Result<String> {
    dotenvy::dotenv().ok();
    env::var(SLACK_TOKEN_VAR).context(
        "SLACK_TOKEN not found. Please run 'slack-linear setup' to configure your Slack token.",
    )
}

pub fn linear_api_key() -> Result<String> {
    dotenvy::dotenv().ok();
    env::var(LINEAR_TOKEN_VAR).context(
        "LINEAR_API_KEY not found. Please run 'slack-linear setup' to configure your Linear API key.",
    )
}

pub fn mirror_dir() -> Result<PathBuf> {
    dotenvy::dotenv().ok();
    Ok(env::var(MIRROR_DIR_VAR)
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from(DEFAULT_MIRROR_DIR)))
}
