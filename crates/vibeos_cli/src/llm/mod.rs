mod client;
mod formatting;
mod prompts;
mod types;

pub use client::LlmClient;
pub use formatting::{format_thread_for_llm, load_thread_from_mirror};
pub use prompts::SYSTEM_PROMPT;
pub use types::{
    LlmIssueSuggestion, LlmLinearIssueRef, LlmLinearWindowSummary, LlmSlackThreadSuggestion,
    LlmSuggestionResponse, SlackThread,
};
