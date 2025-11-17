use std::collections::HashMap;

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use serde_json::Value;

pub const API_BASE: &str = "https://slack.com/api/";
pub const CONVERSATIONS_LIST: &str = "conversations.list";
pub const CONVERSATIONS_HISTORY: &str = "conversations.history";
pub const CONVERSATIONS_REPLIES: &str = "conversations.replies";
pub const CONVERSATIONS_JOIN: &str = "conversations.join";
pub const CONVERSATION_TYPES: &str = "public_channel,private_channel,im,mpim";
pub const USERS_LIST: &str = "users.list";
pub const DEFAULT_RETRY_AFTER_SECS: u64 = 60;
pub const CONVERSATIONS_DIR: &str = "conversations";
pub const THREADS_DIR: &str = "threads";
pub const PROFILES_DIR: &str = "profiles";
pub const JSONL_EXTENSION: &str = "jsonl";

#[derive(Debug, Deserialize)]
pub struct SlackConversation {
    pub id: String,
    pub name: Option<String>,
    #[allow(dead_code)]
    pub is_channel: Option<bool>,
    #[allow(dead_code)]
    pub is_group: Option<bool>,
    #[allow(dead_code)]
    pub is_im: Option<bool>,
    #[allow(dead_code)]
    pub is_mpim: Option<bool>,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct SlackMessage {
    pub ts: String,
    pub user: Option<String>,
    pub text: Option<String>,
    pub thread_ts: Option<String>,
    pub reply_count: Option<u32>,
    pub subtype: Option<String>,
    #[serde(flatten)]
    pub extra: HashMap<String, Value>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct SlackUserSnapshot {
    pub fetched_at: DateTime<Utc>,
    pub user: Value,
}

#[derive(Debug, Deserialize)]
pub struct ResponseMetadata {
    pub next_cursor: Option<String>,
}

#[derive(Debug, Deserialize)]
pub(super) struct ConversationsListResponse {
    pub ok: bool,
    pub channels: Option<Vec<SlackConversation>>,
    pub response_metadata: Option<ResponseMetadata>,
    pub error: Option<String>,
}

#[derive(Debug, Deserialize)]
pub(super) struct ConversationsHistoryResponse {
    pub ok: bool,
    pub messages: Option<Vec<SlackMessage>>,
    pub response_metadata: Option<ResponseMetadata>,
    pub error: Option<String>,
}

#[derive(Debug, Deserialize)]
pub(super) struct ConversationsRepliesResponse {
    pub ok: bool,
    pub messages: Option<Vec<SlackMessage>>,
    pub response_metadata: Option<ResponseMetadata>,
    pub error: Option<String>,
}

#[derive(Debug, Deserialize)]
pub(super) struct ConversationsJoinResponse {
    pub ok: bool,
    pub error: Option<String>,
}

#[derive(Debug, Deserialize)]
pub(super) struct UsersListResponse {
    pub ok: bool,
    pub members: Option<Vec<Value>>,
    pub response_metadata: Option<ResponseMetadata>,
    pub error: Option<String>,
}
