use std::fs::{self, File};
use std::io::{BufRead, BufReader};
use std::path::Path;

use anyhow::{Context, Result};
use core_persona::provider::{LinearProviderPersona, SlackProviderPersona};
use serde_json::Value;

const SLACK_PROFILES_DIR: &str = "profiles";
const LINEAR_USERS_DIR: &str = "users";

pub fn load_slack_personas(root: &Path) -> Result<Vec<SlackProviderPersona>> {
    let profiles_dir = root.join(SLACK_PROFILES_DIR);
    if !profiles_dir.exists() {
        return Ok(Vec::new());
    }

    let mut personas = Vec::new();
    for entry in fs::read_dir(&profiles_dir)
        .with_context(|| format!("failed to read {}", profiles_dir.display()))?
    {
        let entry = entry?;
        let path = entry.path();
        if !is_jsonl(&path) {
            continue;
        }
        if let Some(snapshot) = read_last_value(&path)? {
            if let Some(user) = snapshot.get("user") {
                if let Some(persona) = build_slack_persona(user) {
                    personas.push(persona);
                }
            }
        }
    }
    Ok(personas)
}

pub fn load_linear_personas(root: &Path) -> Result<Vec<LinearProviderPersona>> {
    let users_dir = root.join(LINEAR_USERS_DIR);
    if !users_dir.exists() {
        return Ok(Vec::new());
    }

    let mut personas = Vec::new();
    for entry in fs::read_dir(&users_dir)
        .with_context(|| format!("failed to read {}", users_dir.display()))?
    {
        let entry = entry?;
        let path = entry.path();
        if !is_jsonl(&path) {
            continue;
        }
        if let Some(snapshot) = read_last_value(&path)? {
            if let Some(user) = snapshot.get("user") {
                if let Some(persona) = build_linear_persona(user) {
                    personas.push(persona);
                }
            }
        }
    }

    Ok(personas)
}

fn build_slack_persona(user: &Value) -> Option<SlackProviderPersona> {
    let user_id = user.get("id")?.as_str()?.to_string();
    let team_id = user
        .get("team_id")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());
    let username = user
        .get("name")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());
    let is_bot = user.get("is_bot").and_then(|v| v.as_bool());
    let updated = user.get("updated").and_then(|v| v.as_i64());
    let profile = user.get("profile").cloned().unwrap_or(Value::Null);
    let real_name = profile
        .get("real_name")
        .or_else(|| profile.get("real_name_normalized"))
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());
    let display_name = profile
        .get("display_name")
        .or_else(|| profile.get("display_name_normalized"))
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());
    let email = profile
        .get("email")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());
    let title = profile
        .get("title")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());
    let phone = profile
        .get("phone")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());
    let image_url = profile
        .get("image_512")
        .or_else(|| profile.get("image_192"))
        .or_else(|| profile.get("image_72"))
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());

    Some(SlackProviderPersona {
        user_id,
        team_id,
        username,
        real_name,
        display_name,
        email,
        title,
        phone,
        image_url,
        is_bot,
        updated,
        profile,
    })
}

fn build_linear_persona(user: &Value) -> Option<LinearProviderPersona> {
    let user_id = user.get("id")?.as_str()?.to_string();
    let name = user
        .get("name")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());
    let display_name = user
        .get("displayName")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());
    let email = user
        .get("email")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());
    let avatar_url = user
        .get("avatarUrl")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());
    let active = user.get("active").and_then(|v| v.as_bool()).unwrap_or(true);
    let created_at = parse_datetime(user.get("createdAt")?)?;
    let updated_at = parse_datetime(user.get("updatedAt")?)?;

    Some(LinearProviderPersona {
        user_id,
        name,
        display_name,
        email,
        active,
        avatar_url,
        created_at,
        updated_at,
    })
}

fn parse_datetime(value: &Value) -> Option<chrono::DateTime<chrono::Utc>> {
    value.as_str().and_then(|s| s.parse::<DateTime<Utc>>().ok())
}

fn is_jsonl(path: &Path) -> bool {
    path.extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext == "jsonl")
        .unwrap_or(false)
}

fn read_last_value(path: &Path) -> Result<Option<Value>> {
    if !path.exists() {
        return Ok(None);
    }

    let file = File::open(path)
        .with_context(|| format!("failed to open {} for reading", path.display()))?;
    let reader = BufReader::new(file);
    let mut last = None;
    for line in reader.lines() {
        let line = line.with_context(|| format!("failed to read line from {}", path.display()))?;
        if line.trim().is_empty() {
            continue;
        }
        let value: Value = serde_json::from_str(&line)
            .with_context(|| format!("failed to parse snapshot from {}", path.display()))?;
        last = Some(value);
    }
    Ok(last)
}

use chrono::{DateTime, Utc};
