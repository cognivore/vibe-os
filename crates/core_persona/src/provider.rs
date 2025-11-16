use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use serde_json::Value;

/// Provider-specific persona data pulled from Slack.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SlackProviderPersona {
    pub user_id: String,
    pub team_id: Option<String>,
    pub username: Option<String>,
    pub real_name: Option<String>,
    pub display_name: Option<String>,
    pub email: Option<String>,
    pub title: Option<String>,
    pub phone: Option<String>,
    pub image_url: Option<String>,
    pub is_bot: Option<bool>,
    pub updated: Option<i64>,
    pub profile: Value,
}

/// Provider-specific persona data pulled from Linear.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LinearProviderPersona {
    pub user_id: String,
    pub name: Option<String>,
    pub display_name: Option<String>,
    pub email: Option<String>,
    pub active: bool,
    pub avatar_url: Option<String>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}
