use std::fs::{self, File};
use std::io::{BufWriter, Write};
use std::path::Path;

use anyhow::{Context, Result};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use crate::linear::LinearClient;

const ISSUES_QUERY: &str = r#"
query SyncIssues($after: String) {
  viewer {
    organization {
      name
    }
  }
  issues(first: 50, after: $after) {
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LinearIssueSnapshot {
    pub id: String,
    pub identifier: String,
    pub title: String,
    pub description: Option<String>,
    pub url: Option<String>,
    pub team_id: Option<String>,
    pub team_key: Option<String>,
    pub team_name: Option<String>,
    pub state_id: Option<String>,
    pub state_name: Option<String>,
    pub state_type: Option<String>,
    pub assignee_id: Option<String>,
    pub assignee_name: Option<String>,
    pub labels: Vec<String>,
    pub priority: Option<i32>,
    pub estimate: Option<f32>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub completed_at: Option<DateTime<Utc>>,
    pub canceled_at: Option<DateTime<Utc>>,
    pub archived_at: Option<DateTime<Utc>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LinearIssueEvent {
    pub id: String,
    pub issue_id: String,
    pub issue_identifier: String,
    pub event_kind: String,
    pub created_at: DateTime<Utc>,
    pub actor_id: Option<String>,
    pub actor_name: Option<String>,
    pub from_state: Option<String>,
    pub to_state: Option<String>,
    pub from_priority: Option<i32>,
    pub to_priority: Option<i32>,
    pub extra: serde_json::Value,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LinearMirrorMeta {
    pub last_full_sync_at: Option<DateTime<Utc>>,
    pub workspace_name: Option<String>,
}

pub struct LinearSync<'a> {
    client: &'a LinearClient,
}

impl<'a> LinearSync<'a> {
    pub fn new(client: &'a LinearClient) -> Self {
        Self { client }
    }

    pub async fn full_sync(&self, output_dir: &Path) -> Result<()> {
        fs::create_dir_all(output_dir).with_context(|| {
            format!(
                "unable to create Linear mirror dir {}",
                output_dir.display()
            )
        })?;

        let issues_path = output_dir.join("issues.jsonl");
        let events_path = output_dir.join("events.jsonl");
        let meta_path = output_dir.join("meta.json");

        let issues_file = File::create(&issues_path)
            .with_context(|| format!("failed to open {} for writing", issues_path.display()))?;
        let events_file = File::create(&events_path)
            .with_context(|| format!("failed to open {} for writing", events_path.display()))?;

        let mut issues_writer = BufWriter::new(issues_file);
        let mut events_writer = BufWriter::new(events_file);

        let mut after: Option<String> = None;
        let mut workspace_name: Option<String> = None;

        loop {
            let data: IssuesQueryData = self
                .client
                .graphql_query(
                    ISSUES_QUERY,
                    serde_json::json!({
                        "after": after
                    }),
                )
                .await
                .context("failed to fetch issues page from Linear")?;

            if workspace_name.is_none() {
                workspace_name = data
                    .viewer
                    .and_then(|viewer| viewer.organization.map(|org| org.name));
            }

            for node in data.issues.nodes.into_iter() {
                let snapshot = LinearIssueSnapshot::from_node(&node);
                write_json_line(&mut issues_writer, &snapshot)?;

                let mut issue_events = LinearIssueEvent::from_issue_node(&node);
                for event in issue_events.drain(..) {
                    write_json_line(&mut events_writer, &event)?;
                }
            }

            if !data.issues.page_info.has_next_page {
                break;
            }

            after = data.issues.page_info.end_cursor;
        }

        issues_writer
            .flush()
            .context("failed to flush issues mirror")?;
        events_writer
            .flush()
            .context("failed to flush events mirror")?;

        let meta = LinearMirrorMeta {
            last_full_sync_at: Some(Utc::now()),
            workspace_name,
        };

        let meta_file = File::create(&meta_path)
            .with_context(|| format!("failed to open {} for writing", meta_path.display()))?;
        serde_json::to_writer_pretty(meta_file, &meta)
            .with_context(|| format!("failed to write meta at {}", meta_path.display()))?;

        Ok(())
    }
}

fn write_json_line<W, T>(writer: &mut W, value: &T) -> Result<()>
where
    W: Write,
    T: Serialize,
{
    serde_json::to_writer(&mut *writer, value)?;
    writer.write_all(b"\n")?;
    Ok(())
}

impl LinearIssueSnapshot {
    fn from_node(node: &IssueNode) -> Self {
        Self {
            id: node.id.clone(),
            identifier: node.identifier.clone(),
            title: node.title.clone(),
            description: node.description.clone(),
            url: node.url.clone(),
            team_id: node.team.as_ref().map(|team| team.id.clone()),
            team_key: node.team.as_ref().map(|team| team.key.clone()),
            team_name: node.team.as_ref().map(|team| team.name.clone()),
            state_id: node.state.as_ref().map(|state| state.id.clone()),
            state_name: node.state.as_ref().map(|state| state.name.clone()),
            state_type: node.state.as_ref().and_then(|state| state.r#type.clone()),
            assignee_id: node.assignee.as_ref().map(|assignee| assignee.id.clone()),
            assignee_name: node.assignee.as_ref().map(|assignee| assignee.name.clone()),
            labels: node
                .labels
                .as_ref()
                .map(|labels| {
                    labels
                        .nodes
                        .iter()
                        .map(|label| label.name.clone())
                        .collect()
                })
                .unwrap_or_default(),
            priority: node.priority,
            estimate: node.estimate.map(|v| v as f32),
            created_at: node.created_at.clone(),
            updated_at: node.updated_at.clone(),
            completed_at: node.completed_at.clone(),
            canceled_at: node.canceled_at.clone(),
            archived_at: node.archived_at.clone(),
        }
    }
}

impl LinearIssueEvent {
    fn from_issue_node(node: &IssueNode) -> Vec<Self> {
        let mut events = Vec::new();

        events.push(Self {
            id: format!("{}:created", node.id),
            issue_id: node.id.clone(),
            issue_identifier: node.identifier.clone(),
            event_kind: "created".to_string(),
            created_at: node.created_at.clone(),
            actor_id: None,
            actor_name: None,
            from_state: None,
            to_state: node.state.as_ref().map(|s| s.name.clone()),
            from_priority: None,
            to_priority: node.priority,
            extra: serde_json::json!({ "source": "issue.createdAt" }),
        });

        if let Some(history) = &node.history {
            for entry in history.nodes.iter() {
                let extra = serde_json::to_value(entry).unwrap_or(serde_json::Value::Null);
                let actor_id = entry.actor.as_ref().map(|actor| actor.id.clone());
                let actor_name = entry.actor.as_ref().map(|actor| actor.name.clone());

                if let (Some(from_state), Some(to_state)) = (&entry.from_state, &entry.to_state) {
                    if from_state.name != to_state.name {
                        events.push(Self {
                            id: format!("{}:state", entry.id),
                            issue_id: node.id.clone(),
                            issue_identifier: node.identifier.clone(),
                            event_kind: "state_changed".to_string(),
                            created_at: entry.created_at.clone(),
                            actor_id: actor_id.clone(),
                            actor_name: actor_name.clone(),
                            from_state: Some(from_state.name.clone()),
                            to_state: Some(to_state.name.clone()),
                            from_priority: None,
                            to_priority: None,
                            extra: extra.clone(),
                        });
                    }

                    if is_done_state(&from_state.r#type) && !is_done_state(&to_state.r#type) {
                        events.push(Self {
                            id: format!("{}:reopened", entry.id),
                            issue_id: node.id.clone(),
                            issue_identifier: node.identifier.clone(),
                            event_kind: "reopened".to_string(),
                            created_at: entry.created_at.clone(),
                            actor_id: actor_id.clone(),
                            actor_name: actor_name.clone(),
                            from_state: Some(from_state.name.clone()),
                            to_state: Some(to_state.name.clone()),
                            from_priority: None,
                            to_priority: None,
                            extra: extra.clone(),
                        });
                    }
                }

                if entry.from_priority != entry.to_priority {
                    events.push(Self {
                        id: format!("{}:priority", entry.id),
                        issue_id: node.id.clone(),
                        issue_identifier: node.identifier.clone(),
                        event_kind: "priority_changed".to_string(),
                        created_at: entry.created_at.clone(),
                        actor_id: actor_id.clone(),
                        actor_name: actor_name.clone(),
                        from_state: None,
                        to_state: None,
                        from_priority: entry.from_priority,
                        to_priority: entry.to_priority,
                        extra,
                    });
                }
            }
        }

        events
    }
}

fn is_done_state(state_type: &Option<String>) -> bool {
    matches!(
        state_type.as_deref(),
        Some("completed") | Some("canceled") | Some("done")
    )
}

#[derive(Debug, Deserialize)]
struct IssuesQueryData {
    viewer: Option<Viewer>,
    issues: IssuesConnection,
}

#[derive(Debug, Deserialize)]
struct Viewer {
    organization: Option<Organization>,
}

#[derive(Debug, Deserialize)]
struct Organization {
    name: String,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct IssuesConnection {
    nodes: Vec<IssueNode>,
    page_info: PageInfo,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct PageInfo {
    has_next_page: bool,
    end_cursor: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct IssueNode {
    id: String,
    identifier: String,
    title: String,
    description: Option<String>,
    url: Option<String>,
    priority: Option<i32>,
    estimate: Option<f64>,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
    completed_at: Option<DateTime<Utc>>,
    canceled_at: Option<DateTime<Utc>>,
    archived_at: Option<DateTime<Utc>>,
    team: Option<TeamRef>,
    state: Option<StateRefExtended>,
    assignee: Option<UserRef>,
    labels: Option<LabelConnection>,
    history: Option<HistoryConnection>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct TeamRef {
    id: String,
    key: String,
    name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct StateRefExtended {
    id: String,
    name: String,
    #[serde(rename = "type")]
    r#type: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct UserRef {
    id: String,
    name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct LabelConnection {
    nodes: Vec<LabelRef>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct LabelRef {
    id: String,
    name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct HistoryConnection {
    nodes: Vec<HistoryEntry>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct HistoryEntry {
    id: String,
    created_at: DateTime<Utc>,
    actor: Option<UserRef>,
    from_state: Option<StateRef>,
    to_state: Option<StateRef>,
    from_priority: Option<i32>,
    to_priority: Option<i32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct StateRef {
    name: String,
    #[serde(rename = "type")]
    r#type: Option<String>,
}
