use std::collections::BTreeMap;

use anyhow::{anyhow, bail, Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

use super::{ChatMessage, Credentials, JSONSchemaSpec, LLMFormatChat};

pub type PromptMap = BTreeMap<String, String>;
pub type Body = Vec<Value>;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AttachmentBody {
    pub markdown: String,
    pub blocks: Body,
}

impl AttachmentBody {
    pub fn new(markdown: impl Into<String>, blocks: Body) -> Self {
        Self {
            markdown: markdown.into(),
            blocks,
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Focus {
    pub index: usize,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "op", rename_all = "snake_case")]
pub enum SimpleOp {
    Replace { focus: Focus, block: Value },
    InsertBefore { focus: Focus, block: Value },
    InsertAfter { focus: Focus, block: Value },
}

pub async fn respond_pandoc_chat<P>(
    provider: &P,
    credentials: &Credentials,
    model: &str,
    prompts: &PromptMap,
    attachments: Option<&[AttachmentBody]>,
) -> Result<(PromptMap, Option<Vec<Vec<SimpleOp>>>)>
where
    P: LLMFormatChat + Send + Sync,
{
    respond_pandoc_chat_with_tokens(provider, credentials, model, prompts, attachments, None).await
}

pub async fn respond_pandoc_chat_with_tokens<P>(
    provider: &P,
    credentials: &Credentials,
    model: &str,
    prompts: &PromptMap,
    attachments: Option<&[AttachmentBody]>,
    max_tokens: Option<u32>,
) -> Result<(PromptMap, Option<Vec<Vec<SimpleOp>>>)>
where
    P: LLMFormatChat + Send + Sync,
{
    let base_messages = prompts_to_messages(prompts);

    match attachments {
        None => {
            let assistant = provider
                .respond_text_with_tokens(credentials, model, &base_messages, max_tokens)
                .await?;
            Ok((BTreeMap::from([("assistant".to_string(), assistant)]), None))
        }
        Some(bodies) => {
            if bodies.is_empty() {
                let assistant = provider
                    .respond_text_with_tokens(credentials, model, &base_messages, max_tokens)
                    .await?;
                return Ok((
                    BTreeMap::from([("assistant".to_string(), assistant)]),
                    Some(Vec::new()),
                ));
            }
            let mut messages = base_messages;
            messages.insert(0, ChatMessage::new("system", contract_system().to_string()));
            messages.push(ChatMessage::new("user", attachments_user_message(bodies)));

            let schema = schema_for_patches(bodies.len());
            let value = provider
                .respond_json_with_tokens(credentials, model, &messages, &schema, max_tokens)
                .await?;

            let assistant = value
                .get("assistant")
                .and_then(|v| v.as_str())
                .ok_or_else(|| anyhow!("Provider JSON parse error: missing `assistant` field"))?
                .to_string();

            let patches_value = value
                .get("patches")
                .cloned()
                .ok_or_else(|| anyhow!("Provider JSON parse error: missing `patches` field"))?;

            let patches: Vec<Vec<SimpleOp>> = serde_json::from_value(patches_value.clone())
                .context("Provider JSON parse error: invalid `patches` payload")?;

            if patches.len() != bodies.len() {
                bail!("Mismatch: patches length does not match attachments length");
            }

            Ok((
                BTreeMap::from([("assistant".to_string(), assistant)]),
                Some(patches),
            ))
        }
    }
}

pub fn apply_edits_to_bodies(bodies: &[Body], patches: &[Vec<SimpleOp>]) -> Result<Vec<Body>> {
    if bodies.len() != patches.len() {
        bail!("apply_edits_to_bodies: length mismatch");
    }

    bodies
        .iter()
        .zip(patches.iter())
        .map(|(body, ops)| apply_ops(body, ops))
        .collect()
}

fn apply_ops(body: &Body, ops: &[SimpleOp]) -> Result<Body> {
    let mut next = body.clone();
    for op in ops {
        match op {
            SimpleOp::Replace { focus, block } => {
                ensure_index(next.len(), focus.index)?;
                next[focus.index] = block.clone();
            }
            SimpleOp::InsertBefore { focus, block } => {
                ensure_insertion_index(next.len(), focus.index)?;
                next.insert(focus.index, block.clone());
            }
            SimpleOp::InsertAfter { focus, block } => {
                ensure_insertion_index(next.len(), focus.index + 1)?;
                next.insert(focus.index + 1, block.clone());
            }
        }
    }
    Ok(next)
}

fn ensure_index(len: usize, idx: usize) -> Result<()> {
    if idx >= len {
        bail!("focus index {idx} out of range {len}");
    }
    Ok(())
}

fn ensure_insertion_index(len: usize, idx: usize) -> Result<()> {
    if idx > len {
        bail!("focus index {idx} out of range {len}");
    }
    Ok(())
}

fn prompts_to_messages(prompts: &PromptMap) -> Vec<ChatMessage> {
    let mut system_or_aux = Vec::new();
    let mut user = Vec::new();
    for (key, value) in prompts {
        match key.to_lowercase().as_str() {
            "user" => user.push(ChatMessage::new("user", value.clone())),
            "assistant" => system_or_aux.push(ChatMessage::new("assistant", value.clone())),
            _ => system_or_aux.push(ChatMessage::new("system", value.clone())),
        }
    }
    system_or_aux.extend(user);
    system_or_aux
}

fn attachments_user_message(attachments: &[AttachmentBody]) -> String {
    attachments
        .iter()
        .enumerate()
        .map(|(idx, body)| {
            let json_repr =
                serde_json::to_string_pretty(&body.blocks).unwrap_or_else(|_| "[]".into());
            format!(
                "Attachment {} (Markdown):\n{}\n\nAttachment {} (AST JSON):\n```json\n{}\n```",
                idx + 1,
                body.markdown,
                idx + 1,
                json_repr
            )
        })
        .collect::<Vec<_>>()
        .join("\n\n")
}

fn schema_for_patches(len: usize) -> JSONSchemaSpec {
    let op_schema = json!({
        "type": "object",
        "properties": {
            "op": { "type": "string" },
            "focus": {
                "type": "object",
                "properties": { "index": { "type": "integer", "minimum": 0 } },
                "required": ["index"]
            },
            "block": { "type": "object" }
        },
        "required": ["op", "focus"],
    });

    let inner = json!({
        "type": "array",
        "items": op_schema,
    });

    let patches = json!({
        "type": "array",
        "minItems": len,
        "maxItems": len,
        "items": inner,
    });

    let root = json!({
        "type": "object",
        "properties": {
            "assistant": { "type": "string" },
            "patches": patches,
        },
        "required": ["assistant", "patches"],
        "additionalProperties": false,
    });

    JSONSchemaSpec {
        schema_name: "PandocChatResponse".to_string(),
        schema: root,
        strict: false,
    }
}

const fn contract_system() -> &'static str {
    "You are a precise Pandoc editor. You will receive one or more attachments.\n\
Each attachment is supplied as Markdown and canonical JSON.\n\
Return a JSON object with exactly:\n\
{\n  \"assistant\": <markdown string>,\n  \"patches\": [ [<SimpleOp>], ... ]\n}\n\
`patches` length MUST equal the number of attachments."
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn apply_ops_handles_insert_and_replace() {
        let bodies = vec![json!({"text": "one"}), json!({"text": "two"})];
        let patches = vec![
            SimpleOp::Replace {
                focus: Focus { index: 1 },
                block: json!({"text": "beta"}),
            },
            SimpleOp::InsertBefore {
                focus: Focus { index: 0 },
                block: json!({"text": "alpha"}),
            },
            SimpleOp::InsertAfter {
                focus: Focus { index: 2 },
                block: json!({"text": "gamma"}),
            },
        ];
        let updated = apply_ops(&bodies, &patches).expect("ops apply");
        assert_eq!(updated.len(), 4);
        assert_eq!(updated[0], json!({"text": "alpha"}));
        assert_eq!(updated[1], json!({"text": "one"}));
        assert_eq!(updated[2], json!({"text": "beta"}));
        assert_eq!(updated[3], json!({"text": "gamma"}));
    }
}
