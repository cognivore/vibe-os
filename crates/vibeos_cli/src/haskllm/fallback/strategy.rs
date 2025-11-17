use std::future::Future;

use anyhow::Result;
use async_trait::async_trait;

use crate::haskllm::{ChatMessage, Credentials, JSONSchemaSpec, LLMFormatChat, RequestConfig};

use super::config::ProviderConfig;

#[derive(Clone)]
pub struct FallbackProvider<P1, P2> {
    pub primary: ProviderConfig<P1>,
    pub secondary: ProviderConfig<P2>,
}

impl<P1, P2> FallbackProvider<P1, P2> {
    pub fn new(primary: ProviderConfig<P1>, secondary: ProviderConfig<P2>) -> Self {
        Self { primary, secondary }
    }

    async fn run_with_fallback<T, FutPrimary, FutSecondary>(
        &self,
        primary: FutPrimary,
        secondary: FutSecondary,
    ) -> Result<T>
    where
        FutPrimary: Future<Output = Result<T>>,
        FutSecondary: Future<Output = Result<T>>,
    {
        match primary.await {
            Ok(value) => Ok(value),
            Err(_) => secondary.await,
        }
    }
}

#[async_trait]
impl<P1, P2> LLMFormatChat for FallbackProvider<P1, P2>
where
    P1: LLMFormatChat + Send + Sync,
    P2: LLMFormatChat + Send + Sync,
{
    async fn respond_text(
        &self,
        _: &Credentials,
        _: &str,
        messages: &[ChatMessage],
    ) -> Result<String> {
        self.run_with_fallback(
            async {
                self.primary
                    .provider
                    .respond_text(
                        &self.primary.credentials,
                        &self.primary.model_name,
                        messages,
                    )
                    .await
            },
            async {
                self.secondary
                    .provider
                    .respond_text(
                        &self.secondary.credentials,
                        &self.secondary.model_name,
                        messages,
                    )
                    .await
            },
        )
        .await
    }

    async fn respond_json(
        &self,
        _: &Credentials,
        _: &str,
        messages: &[ChatMessage],
        schema: &JSONSchemaSpec,
    ) -> Result<serde_json::Value> {
        self.run_with_fallback(
            async {
                self.primary
                    .provider
                    .respond_json(
                        &self.primary.credentials,
                        &self.primary.model_name,
                        messages,
                        schema,
                    )
                    .await
            },
            async {
                self.secondary
                    .provider
                    .respond_json(
                        &self.secondary.credentials,
                        &self.secondary.model_name,
                        messages,
                        schema,
                    )
                    .await
            },
        )
        .await
    }

    async fn respond_text_with_tokens(
        &self,
        _: &Credentials,
        _: &str,
        messages: &[ChatMessage],
        max_tokens: Option<u32>,
    ) -> Result<String> {
        self.run_with_fallback(
            async {
                self.primary
                    .provider
                    .respond_text_with_tokens(
                        &self.primary.credentials,
                        &self.primary.model_name,
                        messages,
                        max_tokens,
                    )
                    .await
            },
            async {
                self.secondary
                    .provider
                    .respond_text_with_tokens(
                        &self.secondary.credentials,
                        &self.secondary.model_name,
                        messages,
                        max_tokens,
                    )
                    .await
            },
        )
        .await
    }

    async fn respond_json_with_tokens(
        &self,
        _: &Credentials,
        _: &str,
        messages: &[ChatMessage],
        schema: &JSONSchemaSpec,
        max_tokens: Option<u32>,
    ) -> Result<serde_json::Value> {
        self.run_with_fallback(
            async {
                self.primary
                    .provider
                    .respond_json_with_tokens(
                        &self.primary.credentials,
                        &self.primary.model_name,
                        messages,
                        schema,
                        max_tokens,
                    )
                    .await
            },
            async {
                self.secondary
                    .provider
                    .respond_json_with_tokens(
                        &self.secondary.credentials,
                        &self.secondary.model_name,
                        messages,
                        schema,
                        max_tokens,
                    )
                    .await
            },
        )
        .await
    }

    async fn respond_text_with_config(
        &self,
        _: &Credentials,
        _: &str,
        messages: &[ChatMessage],
        config: &RequestConfig,
    ) -> Result<String> {
        self.run_with_fallback(
            async {
                self.primary
                    .provider
                    .respond_text_with_config(
                        &self.primary.credentials,
                        &self.primary.model_name,
                        messages,
                        config,
                    )
                    .await
            },
            async {
                self.secondary
                    .provider
                    .respond_text_with_config(
                        &self.secondary.credentials,
                        &self.secondary.model_name,
                        messages,
                        config,
                    )
                    .await
            },
        )
        .await
    }

    async fn respond_json_with_config(
        &self,
        _: &Credentials,
        _: &str,
        messages: &[ChatMessage],
        schema: &JSONSchemaSpec,
        config: &RequestConfig,
    ) -> Result<serde_json::Value> {
        self.run_with_fallback(
            async {
                self.primary
                    .provider
                    .respond_json_with_config(
                        &self.primary.credentials,
                        &self.primary.model_name,
                        messages,
                        schema,
                        config,
                    )
                    .await
            },
            async {
                self.secondary
                    .provider
                    .respond_json_with_config(
                        &self.secondary.credentials,
                        &self.secondary.model_name,
                        messages,
                        schema,
                        config,
                    )
                    .await
            },
        )
        .await
    }

    async fn respond_text_with_tokens_and_config(
        &self,
        _: &Credentials,
        _: &str,
        messages: &[ChatMessage],
        max_tokens: Option<u32>,
        config: &RequestConfig,
    ) -> Result<String> {
        self.run_with_fallback(
            async {
                self.primary
                    .provider
                    .respond_text_with_tokens_and_config(
                        &self.primary.credentials,
                        &self.primary.model_name,
                        messages,
                        max_tokens,
                        config,
                    )
                    .await
            },
            async {
                self.secondary
                    .provider
                    .respond_text_with_tokens_and_config(
                        &self.secondary.credentials,
                        &self.secondary.model_name,
                        messages,
                        max_tokens,
                        config,
                    )
                    .await
            },
        )
        .await
    }

    async fn respond_json_with_tokens_and_config(
        &self,
        _: &Credentials,
        _: &str,
        messages: &[ChatMessage],
        schema: &JSONSchemaSpec,
        max_tokens: Option<u32>,
        config: &RequestConfig,
    ) -> Result<serde_json::Value> {
        self.run_with_fallback(
            async {
                self.primary
                    .provider
                    .respond_json_with_tokens_and_config(
                        &self.primary.credentials,
                        &self.primary.model_name,
                        messages,
                        schema,
                        max_tokens,
                        config,
                    )
                    .await
            },
            async {
                self.secondary
                    .provider
                    .respond_json_with_tokens_and_config(
                        &self.secondary.credentials,
                        &self.secondary.model_name,
                        messages,
                        schema,
                        max_tokens,
                        config,
                    )
                    .await
            },
        )
        .await
    }
}

pub fn chain3<P1, P2, P3>(
    first: ProviderConfig<P1>,
    second: ProviderConfig<P2>,
    third: ProviderConfig<P3>,
) -> FallbackProvider<P1, FallbackProvider<P2, P3>> {
    let nested = FallbackProvider::new(second, third);
    FallbackProvider::new(
        first,
        ProviderConfig::new(nested, Credentials::default(), ""),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::haskllm::phony::Phony;

    #[tokio::test]
    async fn fallback_moves_to_secondary() {
        let primary = ProviderConfig::new(Phony, Credentials::default(), "phony");
        let secondary = ProviderConfig::new(Phony, Credentials::default(), "phony");
        let fallback = FallbackProvider::new(primary, secondary);
        let result = fallback
            .respond_text(&Credentials::default(), "", &[])
            .await;
        assert!(result.is_err());
    }
}
