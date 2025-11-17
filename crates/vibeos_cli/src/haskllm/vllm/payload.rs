use anyhow::{anyhow, Result};
use serde_json::{json, Value};

use crate::haskllm::ChatMessage;

pub fn chat_messages_payload(messages: &[ChatMessage]) -> Vec<Value> {
    messages
        .iter()
        .map(|msg| {
            json!({
                "role": msg.role,
                "content": msg.content,
            })
        })
        .collect()
}

pub fn concretize_chat_endpoint(base: &str) -> String {
    let trimmed = base.trim_end_matches('/');
    if trimmed.ends_with("/chat/completions") {
        trimmed.to_string()
    } else if trimmed.ends_with("/02") {
        format!("{trimmed}/v1/chat/completions")
    } else {
        format!("{trimmed}/02/v1/chat/completions")
    }
}

pub fn extract_chat_content(value: &Value) -> Result<String> {
    let choices = value
        .get("choices")
        .and_then(|v| v.as_array())
        .ok_or_else(|| anyhow!("vLLM: missing `choices` array"))?;

    let first = choices
        .first()
        .and_then(|c| c.as_object())
        .ok_or_else(|| anyhow!("vLLM: empty `choices` array"))?;

    first
        .get("message")
        .and_then(|msg| msg.get("content"))
        .and_then(|text| text.as_str())
        .map(|s| s.to_string())
        .ok_or_else(|| anyhow!("vLLM: missing `message.content`"))
}
