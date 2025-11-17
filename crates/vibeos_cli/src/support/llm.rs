use crate::llm;

pub fn map_priority(priority: &str) -> Option<i32> {
    match priority {
        "P0" => Some(1),
        "P1" => Some(2),
        "P2" => Some(3),
        "P3" => Some(4),
        _ => None,
    }
}

pub fn compose_linear_description(
    issue: &llm::LlmIssueSuggestion,
    thread: &llm::SlackThread,
    suggestions: &llm::LlmSuggestionResponse,
) -> String {
    let mut description = issue.description_markdown.clone();

    description.push_str("\n\n---\n");
    description.push_str("Why this issue:\n");
    description.push_str(&issue.why_this_issue);

    description.push_str("\n\nSuggested from Slack thread:\n");
    description.push_str(&suggestions.thread_summary);

    if !issue.linked_slack_message_ts.is_empty() {
        description.push_str("\n\nLinked Slack messages:\n");
        for ts in &issue.linked_slack_message_ts {
            description.push_str(&format!(
                "- Conversation {} thread {} message {}\n",
                thread.conversation_id, thread.thread_ts, ts
            ));
        }
    }

    description
}
