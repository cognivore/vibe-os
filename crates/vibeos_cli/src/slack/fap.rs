use std::fs;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use chrono::{Datelike, Local, LocalResult, NaiveTime, TimeZone, Weekday};
use serde::{Deserialize, Serialize};
use serde_json::Value;

use super::client::SlackClient;
use super::types::SlackMessage;

const FAP_STATE_FILE: &str = "fap_state.json";
const DEFAULT_FAP_CHANNEL: &str = "fap";
const SCRYFALL_API_BASE: &str = "https://api.scryfall.com";
const SCRYFALL_ACCEPT: &str = "application/json";
const SCRYFALL_USER_AGENT: &str = concat!(
    "vibeos/",
    env!("CARGO_PKG_VERSION"),
    " (+https://github.com/your-org/vibe-os)"
);

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
struct ScryfallImageUris {
    large: Option<String>,
    normal: Option<String>,
}

impl ScryfallImageUris {
    fn preferred_url(&self) -> Option<&str> {
        self.large.as_deref().or(self.normal.as_deref())
    }
}

#[derive(Debug, Deserialize)]
struct ScryfallCardFace {
    flavor_text: Option<String>,
    image_uris: Option<ScryfallImageUris>,
}

#[derive(Debug, Deserialize)]
struct ScryfallCard {
    name: String,
    scryfall_uri: Option<String>,
    flavor_text: Option<String>,
    image_uris: Option<ScryfallImageUris>,
    card_faces: Option<Vec<ScryfallCardFace>>,
}

impl ScryfallCard {
    fn display_url<'a>(&'a self, fallback_url: &'a str) -> &'a str {
        self.scryfall_uri.as_deref().unwrap_or(fallback_url)
    }

    fn image_url(&self) -> Option<&str> {
        self.image_uris
            .as_ref()
            .and_then(ScryfallImageUris::preferred_url)
            .or_else(|| {
                self.card_faces.as_deref().and_then(|faces| {
                    faces.iter().find_map(|face| {
                        face.image_uris
                            .as_ref()
                            .and_then(ScryfallImageUris::preferred_url)
                    })
                })
            })
    }

    fn flavor_text(&self) -> Option<String> {
        self.flavor_text
            .clone()
            .filter(|text| !text.trim().is_empty())
            .or_else(|| {
                self.card_faces.as_ref().and_then(|faces| {
                    let combined = faces
                        .iter()
                        .filter_map(|face| face.flavor_text.as_deref().map(str::trim))
                        .filter(|text| !text.is_empty())
                        .map(str::to_owned)
                        .collect::<Vec<_>>();

                    if combined.is_empty() {
                        None
                    } else {
                        Some(combined.join("\n\n"))
                    }
                })
            })
    }
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

fn escape_slack_mrkdwn(text: &str) -> String {
    text.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
}

fn format_slack_italic(text: &str) -> String {
    let escaped = escape_slack_mrkdwn(text)
        .replace('_', "\\_")
        .replace('*', "\\*");
    format!("_{escaped}_")
}

fn mtg_thread_ts(state: &WeekState) -> Result<&str> {
    state
        .messages
        .iter()
        .find(|message| message.kind == MessageKind::Mtg)
        .map(|message| message.ts.as_str())
        .ok_or_else(|| anyhow::anyhow!("Current week FAP state is missing the MTG root message"))
}

fn build_mtg_card_announcement(card: &ScryfallCard, fallback_url: &str) -> (String, Value) {
    let card_link = format!(
        "<{}|{}>",
        card.display_url(fallback_url),
        escape_slack_mrkdwn(&card.name)
    );
    let flavor_text = card.flavor_text();
    let text = flavor_text.as_deref().map_or_else(
        || card_link.clone(),
        |flavor| format!("{card_link}\n{}", format_slack_italic(flavor)),
    );

    let mut blocks = vec![serde_json::json!({
        "type": "section",
        "text": {
            "type": "mrkdwn",
            "text": card_link,
        }
    })];

    if let Some(image_url) = card.image_url() {
        blocks.push(serde_json::json!({
            "type": "image",
            "image_url": image_url,
            "alt_text": card.name,
        }));
    }

    if let Some(flavor_text) = flavor_text {
        blocks.push(serde_json::json!({
            "type": "section",
            "text": {
                "type": "mrkdwn",
                "text": format_slack_italic(&flavor_text),
            }
        }));
    }

    (text, Value::Array(blocks))
}

fn current_iso_week() -> (i32, u32) {
    let now = Local::now();
    let iso = now.iso_week();
    (iso.year(), iso.week())
}

fn current_week_start_ts() -> Result<String> {
    let now = Local::now();
    let days_since_monday = i64::from(now.weekday().num_days_from_monday());
    let week_start_date = now.date_naive() - chrono::Duration::days(days_since_monday);
    let week_start_naive = week_start_date.and_time(NaiveTime::MIN);
    let week_start = match Local.from_local_datetime(&week_start_naive) {
        LocalResult::Single(dt) => dt,
        LocalResult::Ambiguous(earliest, _) => earliest,
        LocalResult::None => anyhow::bail!(
            "Could not resolve the local start of week for {}",
            week_start_naive
        ),
    };

    Ok(week_start.timestamp().to_string())
}

pub fn is_monday() -> bool {
    Local::now().weekday() == Weekday::Mon
}

fn is_top_level_message(message: &SlackMessage) -> bool {
    match message.thread_ts.as_deref() {
        None => true,
        Some(thread_ts) => thread_ts == message.ts,
    }
}

fn is_current_week_fap_message(message: &SlackMessage, kind: MessageKind, week: u32) -> bool {
    let Some(text) = message.text.as_deref() else {
        return false;
    };
    let marker = format!("[{}]-[WEEK {}]-[", kind.label(), week);
    is_top_level_message(message) && text.starts_with(kind.emoji()) && text.contains(&marker)
}

fn recover_posted_messages(messages: &[SlackMessage], week: u32) -> Result<Vec<PostedMessage>> {
    ALL_KINDS
        .iter()
        .copied()
        .map(|kind| {
            messages
                .iter()
                .filter(|message| is_current_week_fap_message(message, kind, week))
                .max_by(|left, right| left.ts.cmp(&right.ts))
                .map(|message| PostedMessage {
                    kind,
                    ts: message.ts.clone(),
                })
                .ok_or_else(|| {
                    anyhow::anyhow!(
                        "Could not find this week's {} FAP message in Slack history",
                        kind.label()
                    )
                })
        })
        .collect()
}

fn state_has_all_kinds(state: &WeekState) -> bool {
    ALL_KINDS
        .iter()
        .all(|kind| state.messages.iter().any(|message| message.kind == *kind))
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
    let parsed = reqwest::Url::parse(scryfall_url)
        .with_context(|| format!("Invalid URL: {scryfall_url}"))?;

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

async fn fetch_card(http: &reqwest::Client, scryfall_url: &str) -> Result<ScryfallCard> {
    let api_url = scryfall_api_url(scryfall_url)?;
    let response = http
        .get(&api_url)
        .header(reqwest::header::ACCEPT, SCRYFALL_ACCEPT)
        .header(reqwest::header::USER_AGENT, SCRYFALL_USER_AGENT)
        .send()
        .await
        .with_context(|| format!("Failed to fetch Scryfall card at {api_url}"))?;

    if !response.status().is_success() {
        let status = response.status();
        let details = response
            .text()
            .await
            .unwrap_or_else(|_| "<failed to read Scryfall error body>".to_owned());
        anyhow::bail!(
            "Scryfall API returned status {}: {}",
            status,
            details.trim()
        );
    }

    response
        .json()
        .await
        .context("Failed to parse Scryfall response")
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
        let (year, week) = current_iso_week();
        let state = match load_state(mirror_dir)? {
            Some(state)
                if state.year == year && state.week == week && state_has_all_kinds(&state) =>
            {
                state
            }
            Some(state) => {
                println!(
                    "Local FAP state is missing or out of date (found week {} of {}). Recovering week {} of {} from Slack history...",
                    state.week, state.year, week, year
                );
                self.recover_current_week_state(mirror_dir, year, week)
                    .await?
            }
            None => {
                println!(
                    "No local FAP state found. Recovering week {} of {} from Slack history...",
                    week, year
                );
                self.recover_current_week_state(mirror_dir, year, week)
                    .await?
            }
        };

        println!("Fetching card from Scryfall: {}", scryfall_url);
        let card = fetch_card(&self.http, scryfall_url).await?;
        let card_name = card.name.clone();
        println!("Card: {}", card_name);
        let channel_id = state.channel_id.clone();
        let mtg_root_ts = mtg_thread_ts(&state)?.to_owned();
        let card_reply_ts = self
            .post_mtg_card_announcement(&channel_id, &mtg_root_ts, &card, scryfall_url)
            .await?;
        println!(
            "Posted {} to the MTG thread and broadcast it to the channel (thread={}, ts={})",
            card_name, mtg_root_ts, card_reply_ts
        );

        let mut updated_count = 0usize;
        for msg in &state.messages {
            let new_text = format_message(msg.kind, state.week, Some(&card_name), emoji);
            match self.update_message(&channel_id, &msg.ts, &new_text).await {
                Ok(()) => {
                    updated_count += 1;
                    println!("  Updated {} (ts={})", msg.kind.label(), msg.ts);
                }
                Err(e) => println!("  Failed to update {}: {}", msg.kind.label(), e),
            }
        }

        let updated_state = WeekState {
            card_name: Some(card_name),
            card_emoji: emoji.map(str::to_owned),
            ..state
        };
        save_state(mirror_dir, &updated_state)?;
        println!("Saved FAP state with chosen card info");
        if updated_count == 0 {
            println!("  No weekly root messages were editable by this token.");
        }

        Ok(())
    }

    async fn recover_current_week_state(
        &self,
        mirror_dir: &Path,
        year: i32,
        week: u32,
    ) -> Result<WeekState> {
        let channel_id = self.resolve_fap_channel().await?;
        let oldest = current_week_start_ts()?;
        println!(
            "Fetching FAP channel history since the start of week {}...",
            week
        );
        let messages = self
            .fetch_conversation_history_since(&channel_id, Some(&oldest))
            .await?;
        let posted = recover_posted_messages(&messages, week).with_context(|| {
            format!(
                "Unable to recover this week's FAP messages from {}",
                channel_id
            )
        })?;

        let state = WeekState {
            year,
            week,
            channel_id,
            messages: posted,
            card_name: None,
            card_emoji: None,
        };
        save_state(mirror_dir, &state)?;
        println!("Recovered FAP state for week {} of {}", week, year);

        Ok(state)
    }

    async fn post_mtg_card_announcement(
        &self,
        channel: &str,
        thread_ts: &str,
        card: &ScryfallCard,
        fallback_url: &str,
    ) -> Result<String> {
        let (text, blocks) = build_mtg_card_announcement(card, fallback_url);
        self.post_thread_message(channel, thread_ts, &text, true, Some(blocks))
            .await
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

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use serde_json::json;

    fn slack_message(ts: &str, text: &str) -> SlackMessage {
        SlackMessage {
            ts: ts.to_owned(),
            user: None,
            text: Some(text.to_owned()),
            thread_ts: None,
            reply_count: None,
            subtype: None,
            extra: HashMap::new(),
        }
    }

    fn test_card(flavor_text: Option<&str>, image_url: Option<&str>) -> ScryfallCard {
        ScryfallCard {
            name: "Turnabout".to_owned(),
            scryfall_uri: Some("https://scryfall.com/card/dmr/70/turnabout?utm_source=api".into()),
            flavor_text: flavor_text.map(str::to_owned),
            image_uris: image_url.map(|url| ScryfallImageUris {
                large: Some(url.to_owned()),
                normal: None,
            }),
            card_faces: None,
        }
    }

    #[test]
    fn recover_posted_messages_picks_latest_message_per_kind() {
        let week = 11;
        let messages = vec![
            slack_message("1741600000.000100", ":mirror: [RETRO]-[WEEK 11]-[???] ?"),
            slack_message(
                "1741600000.000200",
                ":stopwatch: [SPRINT]-[WEEK 11]-[???] ?",
            ),
            slack_message(
                "1741600000.000300",
                ":mtg_card: [MTG]-[WEEK 11]-[Old Card] :old:",
            ),
            slack_message(
                "1741600000.000400",
                ":mtg_card: [MTG]-[WEEK 11]-[Turnabout] :mana2: :manau: :manau:",
            ),
            slack_message("1741600000.000500", ":dna: [FAP]-[WEEK 11]-[???] ?"),
            slack_message("1740995200.000000", ":mirror: [RETRO]-[WEEK 10]-[???] ?"),
        ];

        let posted = recover_posted_messages(&messages, week).expect("should recover current week");

        assert_eq!(
            posted
                .iter()
                .map(|message| (message.kind, message.ts.as_str()))
                .collect::<Vec<_>>(),
            vec![
                (MessageKind::Retro, "1741600000.000100"),
                (MessageKind::Sprint, "1741600000.000200"),
                (MessageKind::Mtg, "1741600000.000400"),
                (MessageKind::Fap, "1741600000.000500"),
            ]
        );
    }

    #[test]
    fn recover_posted_messages_ignores_thread_replies() {
        let week = 11;
        let mut threaded = slack_message("1741600000.000600", ":dna: [FAP]-[WEEK 11]-[???] ?");
        threaded.thread_ts = Some("1741600000.000100".to_owned());

        let messages = vec![
            slack_message("1741600000.000100", ":mirror: [RETRO]-[WEEK 11]-[???] ?"),
            slack_message(
                "1741600000.000200",
                ":stopwatch: [SPRINT]-[WEEK 11]-[???] ?",
            ),
            slack_message("1741600000.000300", ":mtg_card: [MTG]-[WEEK 11]-[???] ?"),
            threaded,
            slack_message("1741600000.000700", ":dna: [FAP]-[WEEK 11]-[???] ?"),
        ];

        let posted =
            recover_posted_messages(&messages, week).expect("should recover top-level posts");

        assert_eq!(
            posted
                .iter()
                .find(|message| message.kind == MessageKind::Fap)
                .map(|message| message.ts.as_str()),
            Some("1741600000.000700")
        );
    }

    #[test]
    fn build_mtg_card_announcement_places_italic_flavor_below_image() {
        let card = test_card(
            Some("The best cure for a big ego is a little failure."),
            Some("https://cards.scryfall.io/large/front/0/4/turnabout.jpg"),
        );

        let (text, blocks) =
            build_mtg_card_announcement(&card, "https://scryfall.com/card/dmr/70/turnabout");

        assert_eq!(
            text,
            "<https://scryfall.com/card/dmr/70/turnabout?utm_source=api|Turnabout>\n_The best cure for a big ego is a little failure._"
        );
        assert_eq!(
            blocks,
            Value::Array(vec![
                json!({
                    "type": "section",
                    "text": {
                        "type": "mrkdwn",
                        "text": "<https://scryfall.com/card/dmr/70/turnabout?utm_source=api|Turnabout>",
                    }
                }),
                json!({
                    "type": "image",
                    "image_url": "https://cards.scryfall.io/large/front/0/4/turnabout.jpg",
                    "alt_text": "Turnabout",
                }),
                json!({
                    "type": "section",
                    "text": {
                        "type": "mrkdwn",
                        "text": "_The best cure for a big ego is a little failure._",
                    }
                }),
            ])
        );
    }

    #[test]
    fn scryfall_card_flavor_text_falls_back_to_faces() {
        let card = ScryfallCard {
            name: "Fire // Ice".to_owned(),
            scryfall_uri: None,
            flavor_text: None,
            image_uris: None,
            card_faces: Some(vec![
                ScryfallCardFace {
                    flavor_text: Some("Fire flavor".to_owned()),
                    image_uris: None,
                },
                ScryfallCardFace {
                    flavor_text: Some("Ice flavor".to_owned()),
                    image_uris: None,
                },
            ]),
        };

        assert_eq!(
            card.flavor_text(),
            Some("Fire flavor\n\nIce flavor".to_owned())
        );
    }
}
