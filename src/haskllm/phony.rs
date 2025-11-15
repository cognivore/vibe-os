use anyhow::{bail, Result};
use async_trait::async_trait;

use super::{ChatMessage, Credentials, JSONSchemaSpec, LLMFormatChat, RequestConfig};

#[derive(Clone, Debug, Default)]
pub struct Phony;

#[async_trait]
impl LLMFormatChat for Phony {
    async fn respond_text(
        &self,
        _credentials: &Credentials,
        _model: &str,
        _messages: &[ChatMessage],
    ) -> Result<String> {
        bail!("Phony LLM provider: respond_text intentionally failed")
    }

    async fn respond_json(
        &self,
        _credentials: &Credentials,
        _model: &str,
        _messages: &[ChatMessage],
        _schema: &JSONSchemaSpec,
    ) -> Result<serde_json::Value> {
        bail!("Phony LLM provider: respond_json intentionally failed")
    }

    async fn respond_text_with_tokens(
        &self,
        _credentials: &Credentials,
        _model: &str,
        _messages: &[ChatMessage],
        _max_tokens: Option<u32>,
    ) -> Result<String> {
        bail!("Phony LLM provider: respond_text_with_tokens intentionally failed")
    }

    async fn respond_json_with_tokens(
        &self,
        _credentials: &Credentials,
        _model: &str,
        _messages: &[ChatMessage],
        _schema: &JSONSchemaSpec,
        _max_tokens: Option<u32>,
    ) -> Result<serde_json::Value> {
        bail!("Phony LLM provider: respond_json_with_tokens intentionally failed")
    }

    async fn respond_text_with_config(
        &self,
        _credentials: &Credentials,
        _model: &str,
        _messages: &[ChatMessage],
        _config: &RequestConfig,
    ) -> Result<String> {
        bail!("Phony LLM provider: respond_text_with_config intentionally failed")
    }

    async fn respond_json_with_config(
        &self,
        _credentials: &Credentials,
        _model: &str,
        _messages: &[ChatMessage],
        _schema: &JSONSchemaSpec,
        _config: &RequestConfig,
    ) -> Result<serde_json::Value> {
        bail!("Phony LLM provider: respond_json_with_config intentionally failed")
    }

    async fn respond_text_with_tokens_and_config(
        &self,
        _credentials: &Credentials,
        _model: &str,
        _messages: &[ChatMessage],
        _max_tokens: Option<u32>,
        _config: &RequestConfig,
    ) -> Result<String> {
        bail!("Phony LLM provider: respond_text_with_tokens_and_config intentionally failed")
    }

    async fn respond_json_with_tokens_and_config(
        &self,
        _credentials: &Credentials,
        _model: &str,
        _messages: &[ChatMessage],
        _schema: &JSONSchemaSpec,
        _max_tokens: Option<u32>,
        _config: &RequestConfig,
    ) -> Result<serde_json::Value> {
        bail!("Phony LLM provider: respond_json_with_tokens_and_config intentionally failed")
    }
}
