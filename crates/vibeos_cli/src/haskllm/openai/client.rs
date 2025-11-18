use std::time::Duration;

use anyhow::{Context, Result};
use async_trait::async_trait;
use once_cell::sync::Lazy;
use reqwest::Client;
use serde_json::{json, Value};

use crate::haskllm::{
    retry_with_backoff, ChatMessage, Credentials, JSONSchemaSpec, LLMFormatChat, RequestConfig,
};

use super::payload::{chat_messages_payload, extract_responses_text};

const RESPONSES_URL: &str = "https://api.openai.com/v1/responses";
const DEFAULT_MAX_OUTPUT_TOKENS: u32 = 8_192;

static OPENAI_CLIENT: Lazy<Client> = Lazy::new(|| {
    Client::builder()
        .connect_timeout(Duration::from_secs(10))
        .build()
        .expect("failed to create OpenAI client")
});

#[derive(Clone, Debug, Default)]
pub struct OpenAI;

#[async_trait]
impl LLMFormatChat for OpenAI {
    async fn respond_text(
        &self,
        credentials: &Credentials,
        model: &str,
        messages: &[ChatMessage],
    ) -> Result<String> {
        self.make_text_request(
            credentials,
            model,
            messages,
            None,
            &RequestConfig::default(),
        )
        .await
    }

    async fn respond_json(
        &self,
        credentials: &Credentials,
        model: &str,
        messages: &[ChatMessage],
        schema: &JSONSchemaSpec,
    ) -> Result<Value> {
        self.make_json_request(
            credentials,
            model,
            messages,
            schema,
            None,
            &RequestConfig::default(),
        )
        .await
    }

    async fn respond_text_with_tokens(
        &self,
        credentials: &Credentials,
        model: &str,
        messages: &[ChatMessage],
        max_tokens: Option<u32>,
    ) -> Result<String> {
        self.make_text_request(
            credentials,
            model,
            messages,
            max_tokens,
            &RequestConfig::default(),
        )
        .await
    }

    async fn respond_json_with_tokens(
        &self,
        credentials: &Credentials,
        model: &str,
        messages: &[ChatMessage],
        schema: &JSONSchemaSpec,
        max_tokens: Option<u32>,
    ) -> Result<Value> {
        self.make_json_request(
            credentials,
            model,
            messages,
            schema,
            max_tokens,
            &RequestConfig::default(),
        )
        .await
    }

    async fn respond_text_with_config(
        &self,
        credentials: &Credentials,
        model: &str,
        messages: &[ChatMessage],
        config: &RequestConfig,
    ) -> Result<String> {
        let cfg = config.clone();
        retry_with_backoff(cfg.max_retries, || async {
            self.make_text_request(credentials, model, messages, None, &cfg)
                .await
        })
        .await
    }

    async fn respond_json_with_config(
        &self,
        credentials: &Credentials,
        model: &str,
        messages: &[ChatMessage],
        schema: &JSONSchemaSpec,
        config: &RequestConfig,
    ) -> Result<Value> {
        let cfg = config.clone();
        retry_with_backoff(cfg.max_retries, || async {
            self.make_json_request(credentials, model, messages, schema, None, &cfg)
                .await
        })
        .await
    }

    async fn respond_text_with_tokens_and_config(
        &self,
        credentials: &Credentials,
        model: &str,
        messages: &[ChatMessage],
        max_tokens: Option<u32>,
        config: &RequestConfig,
    ) -> Result<String> {
        let cfg = config.clone();
        retry_with_backoff(cfg.max_retries, || async {
            self.make_text_request(credentials, model, messages, max_tokens, &cfg)
                .await
        })
        .await
    }

    async fn respond_json_with_tokens_and_config(
        &self,
        credentials: &Credentials,
        model: &str,
        messages: &[ChatMessage],
        schema: &JSONSchemaSpec,
        max_tokens: Option<u32>,
        config: &RequestConfig,
    ) -> Result<Value> {
        let cfg = config.clone();
        retry_with_backoff(cfg.max_retries, || async {
            self.make_json_request(credentials, model, messages, schema, max_tokens, &cfg)
                .await
        })
        .await
    }
}

impl OpenAI {
    async fn make_text_request(
        &self,
        credentials: &Credentials,
        model: &str,
        messages: &[ChatMessage],
        max_tokens: Option<u32>,
        config: &RequestConfig,
    ) -> Result<String> {
        let api_key = credentials.required_with_env("openai_api_key", "OPENAI_API_KEY")?;
        let payload = json!({
            "model": model,
            "input": chat_messages_payload(messages),
            "max_output_tokens": max_tokens.unwrap_or(DEFAULT_MAX_OUTPUT_TOKENS),
        });

        let value = self
            .execute_request(&api_key, payload, config.timeout)
            .await?;
        extract_responses_text(&value).context("OpenAI: missing output text")
    }

    async fn make_json_request(
        &self,
        credentials: &Credentials,
        model: &str,
        messages: &[ChatMessage],
        schema: &JSONSchemaSpec,
        max_tokens: Option<u32>,
        config: &RequestConfig,
    ) -> Result<Value> {
        let api_key = credentials.required_with_env("openai_api_key", "OPENAI_API_KEY")?;
        let payload = json!({
            "model": model,
            "input": chat_messages_payload(messages),
            "text": {
                "format": {
                    "type": "json_schema",
                    "json_schema": {
                        "name": schema.schema_name,
                        "schema": schema.schema,
                        "strict": schema.strict,
                    }
                }
            },
            "max_output_tokens": max_tokens.unwrap_or(DEFAULT_MAX_OUTPUT_TOKENS),
        });

        let value = self
            .execute_request(&api_key, payload, config.timeout)
            .await?;
        let content = extract_responses_text(&value)?;
        serde_json::from_str(&content)
            .with_context(|| "OpenAI: schema-enforced output was not valid JSON")
    }

    async fn execute_request(
        &self,
        api_key: &str,
        payload: Value,
        timeout: Option<Duration>,
    ) -> Result<Value> {
        let mut builder = OPENAI_CLIENT
            .post(RESPONSES_URL)
            .header("Content-Type", "application/json")
            .header("Authorization", format!("Bearer {api_key}"))
            .json(&payload);

        if let Some(duration) = timeout {
            builder = builder.timeout(duration);
        }

        let response = builder
            .send()
            .await
            .context("OpenAI: failed to send request")?
            .error_for_status()
            .context("OpenAI: HTTP error")?;

        response
            .json::<Value>()
            .await
            .context("OpenAI: failed to decode JSON payload")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn responses_payload_contains_messages() {
        let payload = chat_messages_payload(&[ChatMessage::new("user", "hi")]);
        assert_eq!(payload.len(), 1);
    }
}
