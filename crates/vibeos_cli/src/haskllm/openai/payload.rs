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

pub fn extract_responses_text(value: &Value) -> Result<String> {
    let output = value
        .get("output")
        .and_then(|v| v.as_array())
        .ok_or_else(|| anyhow!("OpenAI: missing `output` array"))?;

    let first = output
        .first()
        .and_then(|v| v.get("content"))
        .and_then(|c| c.as_array())
        .and_then(|arr| arr.first())
        .and_then(|entry| entry.get("text"))
        .and_then(|text| text.get("value"))
        .and_then(|val| val.as_str())
        .ok_or_else(|| anyhow!("OpenAI: missing `output.content[0].text.value`"))?;

    Ok(first.to_string())
}
