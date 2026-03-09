use std::fs;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use chrono::{Datelike, Local, Weekday};
use serde::{Deserialize, Serialize};

use super::client::SlackClient;

const FAP_STATE_FILE: &str = "fap_state.json";
const DEFAULT_FAP_CHANNEL: &str = "fap";
const SCRYFALL_API_BASE: &str = "https://api.scryfall.com";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum MessageKind {
    Retro,
    Sprint,
    Mtg,
    Fap,
}

const ALL_KINDS: [MessageKind; 4] = [
    MessageKind::Retro,
    MessageKind::Sprint,
    MessageKind::Mtg,
    MessageKind::Fap,
];

impl MessageKind {
    fn emoji(self) -> &'static str {
        match self {
            Self::Retro => ":mirror:",
            Self::Sprint => ":stopwatch:",
            Self::Mtg => ":mtg_card:",
            Self::Fap => ":dna:",
        }
    }

    fn label(self) -> &'static str {
        match self {
            Self::Retro => "RETRO",
            Self::Sprint => "SPRINT",
            Self::Mtg => "MTG",
            Self::Fap => "FAP",
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PostedMessage {
    pub kind: MessageKind,
    pub ts: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WeekState {
    pub year: i32,
    pub week: u32,
    pub channel_id: String,
    pub messages: Vec<PostedMessage>,
    pub card_name: Option<String>,
    pub card_emoji: Option<String>,
}

#[derive(Debug, Deserialize)]
struct ScryfallCard {
    name: String,
}

// ---------------------------------------------------------------------------
// Pure helpers
// ---------------------------------------------------------------------------

fn format_message(
    kind: MessageKind,
    week: u32,
    card_name: Option<&str>,
    card_emoji: Option<&str>,
) -> String {
    let name = card_name.unwrap_or("???");
    let suffix = card_emoji.unwrap_or("?");
    format!(
        "{} [{}]-[WEEK {}]-[{}] {}",
        kind.emoji(),
        kind.label(),
        week,
        name,
        suffix,
    )
}

fn current_iso_week() -> (i32, u32) {
    let now = Local::now();
    let iso = now.iso_week();
    (iso.year(), iso.week())
}

pub fn is_monday() -> bool {
    Local::now().weekday() == Weekday::Mon
}

// ---------------------------------------------------------------------------
// State I/O
// ---------------------------------------------------------------------------

fn state_path(mirror_dir: &Path) -> PathBuf {
    mirror_dir.join(FAP_STATE_FILE)
}

fn load_state(mirror_dir: &Path) -> Result<Option<WeekState>> {
    let path = state_path(mirror_dir);
    if !path.exists() {
        return Ok(None);
    }
    let contents =
        fs::read_to_string(&path).with_context(|| format!("Failed to read {}", path.display()))?;
    let state: WeekState = serde_json::from_str(&contents)
        .with_context(|| format!("Failed to parse {}", path.display()))?;
    Ok(Some(state))
}

fn save_state(mirror_dir: &Path, state: &WeekState) -> Result<()> {
    let path = state_path(mirror_dir);
    let contents = serde_json::to_string_pretty(state).context("Failed to serialize FAP state")?;
    fs::write(&path, contents).with_context(|| format!("Failed to write {}", path.display()))
}

// ---------------------------------------------------------------------------
// Scryfall
// ---------------------------------------------------------------------------

fn scryfall_api_url(scryfall_url: &str) -> Result<String> {
    let parsed =
        reqwest::Url::parse(scryfall_url).with_context(|| format!("Invalid URL: {scryfall_url}"))?;

    let segments: Vec<&str> = parsed
        .path_segments()
        .ok_or_else(|| anyhow::anyhow!("URL has no path segments"))?
        .collect();

    if segments.len() < 3 || segments[0] != "card" {
        anyhow::bail!(
            "Expected Scryfall URL like https://scryfall.com/card/<set>/<number>[/<name>]"
        );
    }

    Ok(format!(
        "{}/cards/{}/{}",
        SCRYFALL_API_BASE, segments[1], segments[2]
    ))
}

async fn fetch_card_name(http: &reqwest::Client, scryfall_url: &str) -> Result<String> {
    let api_url = scryfall_api_url(scryfall_url)?;
    let response = http
        .get(&api_url)
        .send()
        .await
        .with_context(|| format!("Failed to fetch Scryfall card at {api_url}"))?;

    if !response.status().is_success() {
        anyhow::bail!("Scryfall API returned status {}", response.status());
    }

    let card: ScryfallCard = response
        .json()
        .await
        .context("Failed to parse Scryfall response")?;

    Ok(card.name)
}

// ---------------------------------------------------------------------------
// SlackClient high-level FAP operations
// ---------------------------------------------------------------------------

impl SlackClient {
    pub async fn fap_post(&self, mirror_dir: &Path, force: bool) -> Result<()> {
        let (year, week) = current_iso_week();

        if !force {
            if let Some(state) = load_state(mirror_dir)? {
                if state.year == year && state.week == week {
                    println!(
                        "FAP messages already posted for week {} of {}. Use --force to re-post.",
                        week, year
                    );
                    return Ok(());
                }
            }
        }

        let channel_id = self.resolve_fap_channel().await?;
        println!(
            "Posting FAP messages for week {} to #{}...",
            week, channel_id
        );

        let mut posted = Vec::new();
        for kind in ALL_KINDS {
            let text = format_message(kind, week, None, None);
            match self.post_message(&channel_id, &text).await {
                Ok(ts) => {
                    println!("  Posted {} (ts={})", kind.label(), ts);
                    posted.push(PostedMessage { kind, ts });
                }
                Err(e) => {
                    println!("  Failed to post {}: {}", kind.label(), e);
                }
            }
        }

        if posted.is_empty() {
            anyhow::bail!("Failed to post any FAP messages");
        }

        let state = WeekState {
            year,
            week,
            channel_id,
            messages: posted,
            card_name: None,
            card_emoji: None,
        };
        save_state(mirror_dir, &state)?;
        println!("FAP state saved for week {} of {}", week, year);

        Ok(())
    }

    pub async fn fap_update(
        &self,
        mirror_dir: &Path,
        scryfall_url: &str,
        emoji: Option<&str>,
    ) -> Result<()> {
        let state = load_state(mirror_dir)?
            .ok_or_else(|| anyhow::anyhow!("No FAP state found. Run `vibeos slack fap-post` first."))?;

        println!("Fetching card from Scryfall: {}", scryfall_url);
        let card_name = fetch_card_name(&self.http, scryfall_url).await?;
        println!("Card: {}", card_name);

        for msg in &state.messages {
            let new_text = format_message(msg.kind, state.week, Some(&card_name), emoji);
            match self
                .update_message(&state.channel_id, &msg.ts, &new_text)
                .await
            {
                Ok(()) => println!("  Updated {} (ts={})", msg.kind.label(), msg.ts),
                Err(e) => println!("  Failed to update {}: {}", msg.kind.label(), e),
            }
        }

        let updated_state = WeekState {
            card_name: Some(card_name),
            card_emoji: emoji.map(str::to_owned),
            ..state
        };
        save_state(mirror_dir, &updated_state)?;
        println!("FAP state updated with card info");

        Ok(())
    }

    async fn resolve_fap_channel(&self) -> Result<String> {
        let channel =
            std::env::var("VIBEOS_FAP_CHANNEL").unwrap_or_else(|_| DEFAULT_FAP_CHANNEL.to_owned());

        if channel.starts_with('C')
            && channel.len() >= 9
            && channel[1..].chars().all(|c| c.is_alphanumeric())
        {
            return Ok(channel);
        }

        println!("Resolving #{} to channel ID...", channel);
        self.find_channel_by_name(&channel).await
    }
}
