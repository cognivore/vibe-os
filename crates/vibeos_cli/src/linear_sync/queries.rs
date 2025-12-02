use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use super::models::LinearUserNode;

pub const ISSUES_QUERY: &str = r#"
query SyncIssues($after: String, $updatedSince: DateTimeOrDuration) {
  viewer {
    organization {
      name
    }
  }
  issues(
    first: 50,
    after: $after,
    filter: { updatedAt: { gt: $updatedSince } }
  ) {
    nodes {
      id
      identifier
      title
      description
      url
      priority
      estimate
      createdAt
      updatedAt
      completedAt
      canceledAt
      archivedAt
      team {
        id
        key
        name
      }
      state {
        id
        name
        type
      }
      assignee {
        id
        name
      }
      labels {
        nodes {
          id
          name
        }
      }
      cycle {
        id
        number
        name
        startsAt
        endsAt
      }
      history(first: 100) {
        nodes {
          id
          createdAt
          actor {
            id
            name
          }
          fromState {
            name
            type
          }
          toState {
            name
            type
          }
          fromPriority
          toPriority
          fromCycle {
            id
            number
            startsAt
          }
          toCycle {
            id
            number
            startsAt
          }
        }
      }
      comments(first: 100) {
        nodes {
          id
          body
          url
          createdAt
          updatedAt
          user {
            id
            name
            displayName
          }
        }
      }
    }
    pageInfo {
      hasNextPage
      endCursor
    }
  }
}
"#;

pub const USERS_QUERY: &str = r#"
query SyncUsers($after: String) {
  users(first: 100, after: $after) {
    nodes {
      id
      name
      displayName
      email
      active
      avatarUrl
      createdAt
      updatedAt
    }
    pageInfo {
      hasNextPage
      endCursor
    }
  }
}
"#;

#[derive(Debug, Deserialize)]
pub struct IssuesQueryData {
    pub viewer: Option<Viewer>,
    pub issues: IssuesConnection,
}

#[derive(Debug, Deserialize)]
pub struct UsersQueryData {
    pub users: UsersConnection,
}

#[derive(Debug, Deserialize)]
pub struct Viewer {
    pub organization: Option<Organization>,
}

#[derive(Debug, Deserialize)]
pub struct Organization {
    pub name: String,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct IssuesConnection {
    pub nodes: Vec<IssueNode>,
    pub page_info: PageInfo,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UsersConnection {
    pub nodes: Vec<LinearUserNode>,
    pub page_info: PageInfo,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PageInfo {
    pub has_next_page: bool,
    pub end_cursor: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct IssueNode {
    pub id: String,
    pub identifier: String,
    pub title: String,
    pub description: Option<String>,
    pub url: Option<String>,
    pub priority: Option<i32>,
    pub estimate: Option<f64>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub completed_at: Option<DateTime<Utc>>,
    pub canceled_at: Option<DateTime<Utc>>,
    pub archived_at: Option<DateTime<Utc>>,
    pub team: Option<TeamRef>,
    pub state: Option<StateRefExtended>,
    pub assignee: Option<UserRef>,
    pub labels: Option<LabelConnection>,
    pub cycle: Option<CycleRef>,
    pub history: Option<HistoryConnection>,
    pub comments: Option<CommentConnection>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CycleRef {
    pub id: String,
    pub number: i32,
    pub name: Option<String>,
    pub starts_at: Option<DateTime<Utc>>,
    pub ends_at: Option<DateTime<Utc>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CycleHistoryRef {
    pub id: String,
    pub number: i32,
    pub starts_at: Option<DateTime<Utc>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TeamRef {
    pub id: String,
    pub key: String,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StateRefExtended {
    pub id: String,
    pub name: String,
    #[serde(rename = "type")]
    pub r#type: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UserRef {
    pub id: String,
    pub name: String,
    #[serde(default)]
    pub display_name: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LabelConnection {
    pub nodes: Vec<LabelRef>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LabelRef {
    pub id: String,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CommentConnection {
    pub nodes: Vec<CommentNode>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CommentNode {
    pub id: String,
    pub body: Option<String>,
    pub url: Option<String>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub user: Option<UserRef>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HistoryConnection {
    pub nodes: Vec<HistoryEntry>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct HistoryEntry {
    pub id: String,
    pub created_at: DateTime<Utc>,
    pub actor: Option<UserRef>,
    pub from_state: Option<StateRef>,
    pub to_state: Option<StateRef>,
    pub from_priority: Option<i32>,
    pub to_priority: Option<i32>,
    pub from_cycle: Option<CycleHistoryRef>,
    pub to_cycle: Option<CycleHistoryRef>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StateRef {
    pub name: String,
    #[serde(rename = "type")]
    pub r#type: Option<String>,
}
