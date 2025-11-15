use anyhow::{Context, Result};
use std::env;
use std::path::PathBuf;

const SLACK_TOKEN_VAR: &str = "VIBEOS_SLACK_TOKEN";
const LEGACY_SLACK_TOKEN_VAR: &str = "SLACK_TOKEN";
const LINEAR_TOKEN_VAR: &str = "VIBEOS_LINEAR_API_KEY";
const LEGACY_LINEAR_TOKEN_VAR: &str = "LINEAR_API_KEY";
const MIRROR_DIR_VAR: &str = "VIBEOS_SLACK_MIRROR_DIR";
const LEGACY_MIRROR_DIR_VAR: &str = "SLACK_MIRROR_DIR";
const DEFAULT_MIRROR_DIR: &str = "./slack_mirror";
const LINEAR_MIRROR_DIR_VAR: &str = "LINEAR_MIRROR_DIR";
const LEGACY_LINEAR_MIRROR_DIR_VAR: &str = "VIBEOS_LINEAR_MIRROR_DIR";
const DEFAULT_LINEAR_MIRROR_DIR: &str = "./linear_mirror";
const LLM_API_BASE_VAR: &str = "VIBEOS_LLM_API_BASE";
const LEGACY_LLM_API_BASE_VAR: &str = "LLM_API_BASE";
const LLM_API_KEY_VAR: &str = "VIBEOS_LLM_API_KEY";
const LEGACY_LLM_API_KEY_VAR: &str = "LLM_API_KEY";
const LLM_MODEL_VAR: &str = "VIBEOS_LLM_MODEL";
const LEGACY_LLM_MODEL_VAR: &str = "LLM_MODEL";
const LLM_TEMPERATURE_VAR: &str = "VIBEOS_LLM_TEMPERATURE";
const LEGACY_LLM_TEMPERATURE_VAR: &str = "LLM_TEMPERATURE";
const DEFAULT_LLM_TEMPERATURE: f32 = 0.2;

#[derive(Debug, Clone)]
pub struct Config {
    pub slack_token: String,
    pub linear_api_key: String,
    pub slack_mirror_dir: PathBuf,
    pub linear_mirror_dir: PathBuf,
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

    let slack_token = read_required_env(
        SLACK_TOKEN_VAR,
        LEGACY_SLACK_TOKEN_VAR,
        "VIBEOS_SLACK_TOKEN (legacy SLACK_TOKEN) not found. Please run 'vibeos setup' to configure your Slack token.",
    )?;

    let linear_api_key = read_required_env(
        LINEAR_TOKEN_VAR,
        LEGACY_LINEAR_TOKEN_VAR,
        "VIBEOS_LINEAR_API_KEY (legacy LINEAR_API_KEY) not found. Please run 'vibeos setup' to configure your Linear API key.",
    )?;

    let slack_mirror_dir = read_optional_env(MIRROR_DIR_VAR, LEGACY_MIRROR_DIR_VAR)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(DEFAULT_MIRROR_DIR));

    let linear_mirror_dir = read_optional_env(LINEAR_MIRROR_DIR_VAR, LEGACY_LINEAR_MIRROR_DIR_VAR)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(DEFAULT_LINEAR_MIRROR_DIR));

    let llm_api_base = read_required_env(
        LLM_API_BASE_VAR,
        LEGACY_LLM_API_BASE_VAR,
        "VIBEOS_LLM_API_BASE (legacy LLM_API_BASE) is missing. Set it to an OpenAI-compatible base URL (e.g. https://api.openai.com/v1) or rerun setup.",
    )?;

    let llm_model = read_required_env(
        LLM_MODEL_VAR,
        LEGACY_LLM_MODEL_VAR,
        "VIBEOS_LLM_MODEL (legacy LLM_MODEL) is missing. Provide the model identifier to use for triage (e.g. gemma-7b-instruct).",
    )?;

    let llm_api_key = read_optional_env(LLM_API_KEY_VAR, LEGACY_LLM_API_KEY_VAR)
        .map(|key| key.trim().to_owned())
        .filter(|s| !s.is_empty());

    let llm_temperature = match read_optional_env(LLM_TEMPERATURE_VAR, LEGACY_LLM_TEMPERATURE_VAR) {
        Some(raw) => raw.parse::<f32>().with_context(|| {
            format!("VIBEOS_LLM_TEMPERATURE value `{raw}` is not a valid floating point number")
        })?,
        None => DEFAULT_LLM_TEMPERATURE,
    };

    Ok(Config {
        slack_token,
        linear_api_key,
        slack_mirror_dir,
        linear_mirror_dir,
        llm_api_base,
        llm_api_key,
        llm_model,
        llm_temperature,
    })
}

pub fn slack_token() -> Result<String> {
    dotenvy::dotenv().ok();
    read_required_env(
        SLACK_TOKEN_VAR,
        LEGACY_SLACK_TOKEN_VAR,
        "VIBEOS_SLACK_TOKEN (legacy SLACK_TOKEN) not found. Please run 'vibeos setup' to configure your Slack token.",
    )
}

pub fn linear_api_key() -> Result<String> {
    dotenvy::dotenv().ok();
    read_required_env(
        LINEAR_TOKEN_VAR,
        LEGACY_LINEAR_TOKEN_VAR,
        "VIBEOS_LINEAR_API_KEY (legacy LINEAR_API_KEY) not found. Please run 'vibeos setup' to configure your Linear API key.",
    )
}

pub fn mirror_dir() -> Result<PathBuf> {
    dotenvy::dotenv().ok();
    Ok(read_optional_env(MIRROR_DIR_VAR, LEGACY_MIRROR_DIR_VAR)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(DEFAULT_MIRROR_DIR)))
}

pub fn linear_mirror_dir() -> Result<PathBuf> {
    dotenvy::dotenv().ok();
    Ok(
        read_optional_env(LINEAR_MIRROR_DIR_VAR, LEGACY_LINEAR_MIRROR_DIR_VAR)
            .map(PathBuf::from)
            .unwrap_or_else(|| PathBuf::from(DEFAULT_LINEAR_MIRROR_DIR)),
    )
}

fn read_required_env(var: &str, legacy: &str, err: &str) -> Result<String> {
    read_optional_env(var, legacy).with_context(|| err.to_string())
}

fn read_optional_env(var: &str, legacy: &str) -> Option<String> {
    env::var(var).or_else(|_| env::var(legacy)).ok()
}
