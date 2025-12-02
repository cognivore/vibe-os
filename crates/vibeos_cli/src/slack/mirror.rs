use std::cmp::Ordering;

use super::types::SlackMessage;

/// Determines if a thread should be fetched for a given message.
///
/// Returns true if:
/// - Message is a thread ROOT with replies (thread_ts == ts && reply_count > 0)
/// - Message is a thread REPLY (thread_ts != ts) - this ensures we capture threads
///   where we missed the root message but see a reply in the conversation feed
///
/// This fixes the bug where threads started before mirroring began would never
/// get captured if we only saw replies (not the root) in new messages.
pub(super) fn should_fetch_thread(message: &SlackMessage) -> bool {
    match &message.thread_ts {
        Some(thread_ts) => {
            if thread_ts == &message.ts {
                // Thread root - only fetch if it has replies
                message.reply_count.unwrap_or(0) > 0
            } else {
                // Thread reply - always fetch the thread (we might have missed the root)
                true
            }
        }
        None => false,
    }
}

pub(super) fn filter_newer(
    messages: Vec<SlackMessage>,
    last_ts: Option<&str>,
) -> Vec<SlackMessage> {
    match last_ts {
        Some(ts) => messages
            .into_iter()
            .filter(|msg| compare_ts(&msg.ts, ts) == Ordering::Greater)
            .collect(),
        None => messages,
    }
}

pub(super) fn compare_ts(a: &str, b: &str) -> Ordering {
    match (parse_ts_parts(a), parse_ts_parts(b)) {
        (Some(a_parts), Some(b_parts)) => a_parts.cmp(&b_parts),
        (Some(_), None) => Ordering::Greater,
        (None, Some(_)) => Ordering::Less,
        (None, None) => Ordering::Equal,
    }
}

fn parse_ts_parts(ts: &str) -> Option<(i64, i64)> {
    let mut parts = ts.splitn(2, '.');
    let seconds = parts.next()?.parse().ok()?;
    let fraction = parts.next().unwrap_or("0").parse().ok()?;
    Some((seconds, fraction))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    fn make_message(ts: &str, thread_ts: Option<&str>, reply_count: Option<u32>) -> SlackMessage {
        SlackMessage {
            ts: ts.to_string(),
            user: None,
            text: None,
            thread_ts: thread_ts.map(String::from),
            reply_count,
            subtype: None,
            extra: HashMap::new(),
        }
    }

    #[test]
    fn test_should_fetch_thread_regular_message_no_thread() {
        // Regular message with no thread - should NOT fetch
        let msg = make_message("1234567890.000001", None, None);
        assert!(!should_fetch_thread(&msg));
    }

    #[test]
    fn test_should_fetch_thread_root_no_replies() {
        // Thread root with no replies yet - should NOT fetch (nothing to get)
        let msg = make_message("1234567890.000001", Some("1234567890.000001"), Some(0));
        assert!(!should_fetch_thread(&msg));
    }

    #[test]
    fn test_should_fetch_thread_root_with_replies() {
        // Thread root with replies - should fetch
        let msg = make_message("1234567890.000001", Some("1234567890.000001"), Some(5));
        assert!(should_fetch_thread(&msg));
    }

    #[test]
    fn test_should_fetch_thread_reply_captures_uncaptured_threads() {
        // Thread reply (thread_ts != ts) - MUST fetch to capture missed threads
        // This is the critical case: we see a reply to a thread we never captured
        let msg = make_message("1234567899.000001", Some("1234567890.000001"), None);
        assert!(
            should_fetch_thread(&msg),
            "Thread replies MUST trigger fetch to capture threads started before mirroring"
        );
    }

    #[test]
    fn test_compare_ts() {
        assert_eq!(
            compare_ts("1234567890.000001", "1234567890.000001"),
            std::cmp::Ordering::Equal
        );
        assert_eq!(
            compare_ts("1234567890.000002", "1234567890.000001"),
            std::cmp::Ordering::Greater
        );
        assert_eq!(
            compare_ts("1234567891.000001", "1234567890.000001"),
            std::cmp::Ordering::Greater
        );
        assert_eq!(
            compare_ts("1234567890.000001", "1234567890.000002"),
            std::cmp::Ordering::Less
        );
    }
}
