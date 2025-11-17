use std::cmp::Ordering;

use super::types::SlackMessage;

pub(super) fn should_fetch_thread(message: &SlackMessage) -> bool {
    message
        .thread_ts
        .as_ref()
        .map(|thread_ts| thread_ts == &message.ts)
        .unwrap_or(false)
        && message.reply_count.unwrap_or(0) > 0
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
