use anyhow::{Context, Result};
use std::io::{self, Write};

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
    open::that("https://api.slack.com/apps")
        .context("Failed to open Slack app dashboard in browser")?;

    println!();
    println!("Instructions:");
    println!("  1. Create a Slack app or open an existing one");
    println!("  2. Install it to your workspace");
    println!("  3. Give it the broadest read scopes you're comfortable with");
    println!("     (e.g., conversations:history for channels, groups, IMs, etc.)");
    println!("  4. Generate a Bot/User OAuth token");
    println!();

    print!("Please paste your Slack OAuth token (starting with xox...): ");
    io::stdout().flush()?;
    let mut slack_token = String::new();
    io::stdin().read_line(&mut slack_token)?;
    let slack_token = slack_token.trim();

    if slack_token.is_empty() {
        anyhow::bail!("Slack token cannot be empty");
    }

    println!();

    // Linear API key step
    println!("=== Linear API Key Setup ===");
    println!("Opening Linear personal API key page in your browser...");
    open::that("https://linear.app/settings/account/security")
        .context("Failed to open Linear API key page in browser")?;

    println!();
    println!("Instructions:");
    println!("  1. Create a new Personal API key for this tool");
    println!("  2. Grant it permissions to create issues");
    println!();

    print!("Please paste your Linear personal API key: ");
    io::stdout().flush()?;
    let mut linear_key = String::new();
    io::stdin().read_line(&mut linear_key)?;
    let linear_key = linear_key.trim();

    if linear_key.is_empty() {
        anyhow::bail!("Linear API key cannot be empty");
    }

    println!();

    // Write .env file
    println!("Writing credentials to .env file...");
    let env_content = format!(
        "SLACK_TOKEN={}\nLINEAR_API_KEY={}\nSLACK_MIRROR_DIR=./slack_mirror\n",
        slack_token, linear_key
    );
    std::fs::write(".env", env_content)
        .context("Failed to write .env file")?;

    // Ensure .gitignore contains .env
    let gitignore_path = ".gitignore";
    if std::path::Path::new(gitignore_path).exists() {
        let mut gitignore_content = std::fs::read_to_string(gitignore_path)
            .context("Failed to read .gitignore")?;

        if !gitignore_content.contains(".env") {
            if !gitignore_content.ends_with('\n') {
                gitignore_content.push('\n');
            }
            gitignore_content.push_str(".env\n");
            std::fs::write(gitignore_path, gitignore_content)
                .context("Failed to update .gitignore")?;
        }
    } else {
        std::fs::write(gitignore_path, ".env\n")
            .context("Failed to create .gitignore")?;
    }

    println!();
    println!("✓ Setup complete!");
    println!("You can now run:");
    println!("  - `slack-linear slack mirror` to mirror Slack conversations");
    println!("  - `slack-linear linear create-issue` to create Linear issues");

    Ok(())
}

