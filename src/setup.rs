use anyhow::{Context, Result};
use std::collections::BTreeMap;
use std::io::{self, Write};
use std::path::Path;

const SLACK_APPS_URL: &str = "https://api.slack.com/apps";
const LINEAR_SECURITY_URL: &str = "https://linear.app/settings/account/security";
const ENV_FILE: &str = ".env";
const GITIGNORE_FILE: &str = ".gitignore";
const DEFAULT_MIRROR_DIR: &str = "./slack_mirror";
const OPENAI_KEYS_URL: &str = "https://platform.openai.com/settings/organization/api-keys";

pub async fn run_setup() -> Result<()> {
    println!("Welcome to slack-linear setup!");
    println!();
    println!("This tool will help you configure:");
    println!("  - A Slack token with broad read access");
    println!("  - A Linear personal API key");
    println!();
    println!("These credentials will be stored in a `.env` file in the current directory.");
    println!("The `.env` file will be added to `.gitignore` if it's not already there.");
    println!();

    // Slack token step
    println!("=== Slack Token Setup ===");
    println!("Opening Slack app dashboard in your browser...");
    open_url(SLACK_APPS_URL, "Slack app dashboard")?;

    println!();
    println!("Instructions:");
    println!("  1. Create a Slack app or open an existing one");
    println!("  2. Install it to your workspace");
    println!("  3. Give it the broadest read scopes you're comfortable with");
    println!("     (e.g., conversations:history for channels, groups, IMs, etc.)");
    println!("  4. Generate a Bot/User OAuth token");
    println!();

    let slack_token = prompt_value(
        "Please paste your Slack OAuth token (starting with xox...): ",
        "Slack token",
    )?;
    println!();

    // Linear API key step
    println!("=== Linear API Key Setup ===");
    println!("Opening Linear personal API key page in your browser...");
    open_url(LINEAR_SECURITY_URL, "Linear API key page")?;

    println!();
    println!("Instructions:");
    println!("  1. Create a new Personal API key for this tool");
    println!("  2. Grant it permissions to create issues");
    println!();

    let linear_key = prompt_value(
        "Please paste your Linear personal API key: ",
        "Linear API key",
    )?;
    println!();

    // OpenAI GPT-5 key
    println!("=== OpenAI GPT-5 Setup ===");
    println!("Opening OpenAI API key management in your browser...");
    open_url(OPENAI_KEYS_URL, "OpenAI API key page")?;
    println!();

    let openai_key = prompt_value(
        "Please paste your OpenAI GPT-5 Responses API key: ",
        "OpenAI API key",
    )?;
    println!();

    // Blood Money / vLLM key
    println!("=== Blood Money / vLLM Setup ===");
    println!("Use the Blood Money control panel to retrieve the API key for your deployment.");
    let blood_money_key = prompt_value(
        "Please paste your Blood Money API key: ",
        "Blood Money API key",
    )?;
    println!();

    // Write .env file
    println!("Writing credentials to .env file...");
    write_env_file(&[
        ("SLACK_TOKEN", slack_token.as_str()),
        ("LINEAR_API_KEY", linear_key.as_str()),
        ("OPENAI_API_KEY", openai_key.as_str()),
        ("BLOOD_MONEY_API_KEY", blood_money_key.as_str()),
    ])?;
    ensure_gitignore_contains_env()?;

    println!();
    println!("✓ Setup complete!");
    println!("You can now run:");
    println!("  - `slack-linear slack mirror` to mirror Slack conversations");
    println!("  - `slack-linear linear create-issue` to create Linear issues");

    Ok(())
}

fn prompt_value(prompt: &str, field_name: &str) -> Result<String> {
    print!("{prompt}");
    io::stdout().flush().context("Failed to flush stdout")?;
    let mut buffer = String::new();
    io::stdin()
        .read_line(&mut buffer)
        .context("Failed to read input")?;
    let value = buffer.trim().to_owned();
    if value.is_empty() {
        anyhow::bail!("{field_name} cannot be empty");
    }
    Ok(value)
}

fn write_env_file(entries: &[(&str, &str)]) -> Result<()> {
    let mut env_map = read_env_file()?;
    for (key, value) in entries {
        env_map.insert((*key).to_string(), (*value).to_string());
    }
    env_map
        .entry("SLACK_MIRROR_DIR".into())
        .or_insert_with(|| DEFAULT_MIRROR_DIR.to_string());

    let mut content = String::new();
    for (key, value) in env_map {
        content.push_str(&format!("{key}={value}\n"));
    }
    std::fs::write(ENV_FILE, content).context("Failed to write .env file")
}

fn read_env_file() -> Result<BTreeMap<String, String>> {
    let path = Path::new(ENV_FILE);
    if !path.exists() {
        return Ok(BTreeMap::new());
    }
    let data = std::fs::read_to_string(path).context("Failed to read existing .env file")?;
    let mut map = BTreeMap::new();
    for line in data.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        if let Some((key, value)) = line.split_once('=') {
            map.insert(key.trim().to_string(), value.trim().to_string());
        }
    }
    Ok(map)
}

fn ensure_gitignore_contains_env() -> Result<()> {
    let path = Path::new(GITIGNORE_FILE);
    if path.exists() {
        let mut content = std::fs::read_to_string(path).context("Failed to read .gitignore")?;
        if !content.lines().any(|line| line.trim() == ENV_FILE) {
            if !content.ends_with('\n') {
                content.push('\n');
            }
            content.push_str(ENV_FILE);
            content.push('\n');
            std::fs::write(path, content).context("Failed to update .gitignore")?;
        }
    } else {
        std::fs::write(path, format!("{ENV_FILE}\n")).context("Failed to create .gitignore")?;
    }
    Ok(())
}

fn open_url(url: &str, description: &str) -> Result<()> {
    open::that(url).with_context(|| format!("Failed to open {description} in browser"))
}
