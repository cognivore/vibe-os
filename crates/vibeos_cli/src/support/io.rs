use std::io::{self, Write};

use anyhow::{Context, Result};

pub fn prompt_yes(prompt: &str) -> Result<bool> {
    print!("{prompt}");
    io::stdout().flush().context("Failed to flush stdout")?;
    let mut buffer = String::new();
    io::stdin()
        .read_line(&mut buffer)
        .context("Failed to read response")?;
    let decision = buffer.trim().to_ascii_lowercase();
    Ok(matches!(decision.as_str(), "y" | "yes"))
}
