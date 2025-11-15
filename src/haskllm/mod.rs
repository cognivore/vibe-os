use std::collections::BTreeMap;
use std::fmt;
use std::time::Duration;

use anyhow::{Context, Result};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use serde_json::Value;

pub mod fallback;
pub mod openai;
pub mod pandoc_chat;
pub mod phony;
pub mod vllm;

/// Simple credential bag backed by a map.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct Credentials {
    inner: BTreeMap<String, String>,
}

impl Credentials {
    pub fn new<K, V, I>(iter: I) -> Self
    where
        K: Into<String>,
        V: Into<String>,
        I: IntoIterator<Item = (K, V)>,
    {
        let inner = iter
            .into_iter()
            .map(|(k, v)| (k.into(), v.into()))
            .collect();
        Self { inner }
    }

    pub fn inner(&self) -> &BTreeMap<String, String> {
        &self.inner
    }

    pub fn insert<K: Into<String>, V: Into<String>>(&mut self, key: K, value: V) {
        self.inner.insert(key.into(), value.into());
    }

    pub fn get(&self, key: &str) -> Option<&str> {
        self.inner.get(key).map(|s| s.as_str())
    }

    /// Fetch a credential value, falling back to the provided environment variable.
    pub fn required_with_env(&self, key: &str, env_var: &str) -> Result<String> {
        self.get(key)
            .map(|v| v.to_owned())
            .or_else(|| std::env::var(env_var).ok())
            .with_context(|| {
                format!(
                    "Missing credential: `{key}` (expected in credentials map or `{env_var}` env var)"
                )
            })
    }

    pub fn merge(&self, other: &Credentials) -> Credentials {
        let mut merged = self.inner.clone();
        merged.extend(other.inner.clone());
        Credentials { inner: merged }
    }
}

/// Minimal chat message.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct ChatMessage {
    pub role: String,
    pub content: String,
}

impl ChatMessage {
    pub fn new(role: impl Into<String>, content: impl Into<String>) -> Self {
        Self {
            role: role.into(),
            content: content.into(),
        }
    }
}

/// JSON Schema spec portable across providers.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct JSONSchemaSpec {
    pub schema_name: String,
    pub schema: Value,
    pub strict: bool,
}

impl JSONSchemaSpec {
    pub fn new(name: impl Into<String>, schema: Value, strict: bool) -> Self {
        Self {
            schema_name: name.into(),
            schema,
            strict,
        }
    }
}

/// Configuration for HTTP requests.
#[derive(Clone, Debug)]
pub struct RequestConfig {
    pub timeout: Option<Duration>,
    pub max_retries: u32,
}

impl RequestConfig {
    pub fn new(timeout: Option<Duration>, max_retries: u32) -> Self {
        Self {
            timeout,
            max_retries,
        }
    }

    pub fn with_timeout_seconds(seconds: u64) -> Self {
        Self {
            timeout: Some(Duration::from_secs(seconds)),
            ..Default::default()
        }
    }

    pub fn no_timeout() -> Self {
        Self {
            timeout: None,
            ..Default::default()
        }
    }
}

impl Default for RequestConfig {
    fn default() -> Self {
        Self {
            timeout: None,
            max_retries: 3,
        }
    }
}

impl fmt::Display for RequestConfig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.timeout {
            Some(t) => write!(
                f,
                "RequestConfig(timeout={:?}, retries={})",
                t, self.max_retries
            ),
            None => write!(f, "RequestConfig(timeout=∞, retries={})", self.max_retries),
        }
    }
}

#[async_trait]
pub trait LLMFormatChat: Send + Sync {
    async fn respond_text(
        &self,
        credentials: &Credentials,
        model: &str,
        messages: &[ChatMessage],
    ) -> Result<String>;

    async fn respond_json(
        &self,
        credentials: &Credentials,
        model: &str,
        messages: &[ChatMessage],
        schema: &JSONSchemaSpec,
    ) -> Result<Value>;

    async fn respond_text_with_tokens(
        &self,
        credentials: &Credentials,
        model: &str,
        messages: &[ChatMessage],
        _max_tokens: Option<u32>,
    ) -> Result<String> {
        self.respond_text(credentials, model, messages).await
    }

    async fn respond_json_with_tokens(
        &self,
        credentials: &Credentials,
        model: &str,
        messages: &[ChatMessage],
        schema: &JSONSchemaSpec,
        _max_tokens: Option<u32>,
    ) -> Result<Value> {
        self.respond_json(credentials, model, messages, schema)
            .await
    }

    async fn respond_text_with_config(
        &self,
        credentials: &Credentials,
        model: &str,
        messages: &[ChatMessage],
        config: &RequestConfig,
    ) -> Result<String> {
        let _ = config;
        self.respond_text(credentials, model, messages).await
    }

    async fn respond_json_with_config(
        &self,
        credentials: &Credentials,
        model: &str,
        messages: &[ChatMessage],
        schema: &JSONSchemaSpec,
        config: &RequestConfig,
    ) -> Result<Value> {
        let _ = config;
        self.respond_json(credentials, model, messages, schema)
            .await
    }

    async fn respond_text_with_tokens_and_config(
        &self,
        credentials: &Credentials,
        model: &str,
        messages: &[ChatMessage],
        _max_tokens: Option<u32>,
        config: &RequestConfig,
    ) -> Result<String> {
        let _ = config;
        self.respond_text(credentials, model, messages).await
    }

    async fn respond_json_with_tokens_and_config(
        &self,
        credentials: &Credentials,
        model: &str,
        messages: &[ChatMessage],
        schema: &JSONSchemaSpec,
        _max_tokens: Option<u32>,
        config: &RequestConfig,
    ) -> Result<Value> {
        let _ = config;
        self.respond_json(credentials, model, messages, schema)
            .await
    }
}

/// Generic retry helper with exponential backoff.
pub async fn retry_with_backoff<F, Fut, T>(max_retries: u32, mut f: F) -> Result<T>
where
    F: FnMut() -> Fut + Send,
    Fut: std::future::Future<Output = Result<T>> + Send,
    T: Send,
{
    let mut delay = Duration::from_secs(1);
    let mut retries_left = max_retries;
    loop {
        match f().await {
            Ok(val) => return Ok(val),
            Err(err) if retries_left == 0 => return Err(err),
            Err(_) => {
                retries_left -= 1;
                tokio::time::sleep(delay).await;
                delay = std::cmp::min(delay * 2, Duration::from_secs(8));
            }
        }
    }
}
