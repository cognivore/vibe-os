use crate::haskllm::Credentials;

#[derive(Clone)]
pub struct ProviderConfig<P> {
    pub provider: P,
    pub credentials: Credentials,
    pub model_name: String,
}

impl<P> ProviderConfig<P> {
    pub fn new(provider: P, credentials: Credentials, model_name: impl Into<String>) -> Self {
        Self {
            provider,
            credentials,
            model_name: model_name.into(),
        }
    }
}
