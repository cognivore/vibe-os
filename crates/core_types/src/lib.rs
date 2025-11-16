use serde::{Deserialize, Serialize};

/// Logical identifier of a domain (Slack, Linear, GitHub, Grain, Calendar, etc.)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "snake_case")]
pub enum Domain {
    Slack,
    Linear,
    Github,
    Grain,
    GoogleCalendar,
    Other(String),
}
