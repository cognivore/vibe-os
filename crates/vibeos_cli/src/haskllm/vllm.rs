use std::time::Duration;

use anyhow::{anyhow, Context, Result};
use async_trait::async_trait;
use once_cell::sync::Lazy;
use reqwest::Client;
use serde_json::{json, Value};

use super::{
    retry_with_backoff, ChatMessage, Credentials, JSONSchemaSpec, LLMFormatChat, RequestConfig,
};

const DEFAULT_BASE_URL: &str = "https://outland-dev-1.doubling-season.geosurge.ai";
const DEFAULT_TEMPERATURE: f64 = 0.7;

static VLLM_CLIENT: Lazy<Client> = Lazy::new(|| {
    Client::builder()
        .connect_timeout(Duration::from_secs(10))
        .build()
        .expect("failed to create vLLM client")
});

#[derive(Clone, Debug, Default)]
pub struct Qwen;

#[async_trait]
impl LLMFormatChat for Qwen {
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

impl Qwen {
    async fn make_text_request(
        &self,
        credentials: &Credentials,
        model: &str,
        messages: &[ChatMessage],
        max_tokens: Option<u32>,
        config: &RequestConfig,
    ) -> Result<String> {
        let (base_url, api_key) = self.credential_pair(credentials)?;
        let payload = self.build_payload(model, messages, max_tokens, None);
        let value = self
            .execute_request(&base_url, &api_key, payload, config.timeout)
            .await?;
        extract_chat_content(&value).context("vLLM: missing content")
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
        let (base_url, api_key) = self.credential_pair(credentials)?;
        let response_format = json!({
            "type": "json_schema",
            "json_schema": {
                "name": schema.schema_name,
                "schema": schema.schema,
                "strict": schema.strict,
            }
        });
        let payload = self.build_payload(model, messages, max_tokens, Some(response_format));
        let value = self
            .execute_request(&base_url, &api_key, payload, config.timeout)
            .await?;
        let content = extract_chat_content(&value).context("vLLM: missing content")?;
        serde_json::from_str(&content)
            .with_context(|| "vLLM: schema-enforced output was not valid JSON")
    }

    fn build_payload(
        &self,
        model: &str,
        messages: &[ChatMessage],
        max_tokens: Option<u32>,
        response_format: Option<Value>,
    ) -> Value {
        let mut payload = json!({
            "model": model,
            "messages": chat_messages_payload(messages),
            "temperature": DEFAULT_TEMPERATURE,
        });

        if let Some(tokens) = max_tokens {
            payload["max_tokens"] = json!(tokens);
        }

        if let Some(format) = response_format {
            payload["response_format"] = format;
        }

        payload
    }

    async fn execute_request(
        &self,
        base_url: &str,
        api_key: &str,
        payload: Value,
        timeout: Option<Duration>,
    ) -> Result<Value> {
        let url = concretize_chat_endpoint(base_url);
        let mut builder = VLLM_CLIENT
            .post(url)
            .header("Content-Type", "application/json")
            .header("x-api-key", api_key)
            .json(&payload);

        if let Some(duration) = timeout {
            builder = builder.timeout(duration);
        }

        let response = builder
            .send()
            .await
            .context("vLLM: failed to send request")?
            .error_for_status()
            .context("vLLM: HTTP error")?;

        response
            .json::<Value>()
            .await
            .context("vLLM: failed to decode JSON payload")
    }

    fn credential_pair(&self, credentials: &Credentials) -> Result<(String, String)> {
        let base = credentials
            .get("base_url")
            .map(|s| s.to_string())
            .or_else(|| std::env::var("BLOOD_MONEY_BASE_URL").ok())
            .unwrap_or_else(|| DEFAULT_BASE_URL.to_string());
        let api_key = credentials.required_with_env("api_key", "BLOOD_MONEY_API_KEY")?;
        Ok((base, api_key))
    }
}

fn chat_messages_payload(messages: &[ChatMessage]) -> Vec<Value> {
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

fn extract_chat_content(value: &Value) -> Result<String> {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn endpoint_already_complete() {
        let input = "https://example.com/v1/chat/completions";
        assert_eq!(
            concretize_chat_endpoint(input),
            "https://example.com/v1/chat/completions"
        );
    }

    #[test]
    fn endpoint_with_two_route() {
        let input = "https://example.com/02";
        assert_eq!(
            concretize_chat_endpoint(input),
            "https://example.com/02/v1/chat/completions"
        );
    }

    #[test]
    fn endpoint_without_two_route() {
        let input = "https://example.com";
        assert_eq!(
            concretize_chat_endpoint(input),
            "https://example.com/02/v1/chat/completions"
        );
    }
}
