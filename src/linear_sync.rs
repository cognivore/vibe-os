use std::collections::{HashMap, HashSet};
use std::ffi::OsStr;
use std::fs::{self, File, OpenOptions};
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use crate::linear::LinearClient;

const ISSUES_QUERY: &str = r#"
query SyncIssues($after: String, $updatedSince: DateTime) {
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

const EVENT_ROTATE_TRIGGER_BYTES: u64 = 1_000_000;
const EVENT_LOG_PREFIX: &str = "events";
const EVENT_LOG_EXTENSION: &str = "jsonl";

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
        let meta_path = output_dir.join("meta.json");

        let mut issues_by_id = read_existing_issues(&issues_path)?;

        let last_sync_at = read_last_sync(meta_path.as_path())?;

        let mut seen_event_ids = read_existing_event_ids(output_dir)?;
        let mut pending_events = Vec::new();
        let mut after: Option<String> = None;
        let mut workspace_name: Option<String> = None;

        loop {
            let data: IssuesQueryData = self
                .client
                .graphql_query(
                    ISSUES_QUERY,
                    serde_json::json!({
                        "after": after,
                        "updatedSince": last_sync_at
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
                issues_by_id.insert(snapshot.id.clone(), snapshot.clone());

                pending_events.extend(collect_new_events(&node, &mut seen_event_ids));
            }

            if !data.issues.page_info.has_next_page {
                break;
            }

            after = data.issues.page_info.end_cursor;
        }

        write_issues(&issues_path, &issues_by_id)?;
        append_events_with_rotation(output_dir, &pending_events)?;

        let meta = LinearMirrorMeta {
            last_full_sync_at: Some(Utc::now()),
            workspace_name,
        };

        write_meta(&meta_path, &meta)?;

        Ok(())
    }
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

fn read_last_sync(path: &Path) -> Result<Option<DateTime<Utc>>> {
    if !path.exists() {
        return Ok(None);
    }

    let meta_file = File::open(path)
        .with_context(|| format!("failed to open {} for reading", path.display()))?;
    let meta: LinearMirrorMeta =
        serde_json::from_reader(meta_file).with_context(|| "failed to parse meta.json")?;
    Ok(meta.last_full_sync_at)
}

fn write_meta(path: &Path, meta: &LinearMirrorMeta) -> Result<()> {
    let file = File::create(path)
        .with_context(|| format!("failed to open {} for writing", path.display()))?;
    serde_json::to_writer_pretty(file, meta)
        .with_context(|| format!("failed to write meta at {}", path.display()))
}

fn read_existing_issues(path: &Path) -> Result<HashMap<String, LinearIssueSnapshot>> {
    if !path.exists() {
        return Ok(HashMap::new());
    }

    let file = File::open(path)
        .with_context(|| format!("failed to open {} for reading", path.display()))?;
    let reader = BufReader::new(file);

    let mut issues = HashMap::new();
    for (idx, line) in reader.lines().enumerate() {
        let line = line.with_context(|| {
            format!(
                "failed to read issue line {} from {}",
                idx + 1,
                path.display()
            )
        })?;
        if line.trim().is_empty() {
            continue;
        }
        let issue: LinearIssueSnapshot = serde_json::from_str(&line).with_context(|| {
            format!(
                "failed to parse issue line {} from {}",
                idx + 1,
                path.display()
            )
        })?;
        issues.insert(issue.id.clone(), issue);
    }

    Ok(issues)
}

fn write_issues(path: &Path, issues: &HashMap<String, LinearIssueSnapshot>) -> Result<()> {
    let file = File::create(path)
        .with_context(|| format!("failed to open {} for writing", path.display()))?;
    let mut writer = BufWriter::new(file);

    let mut entries: Vec<_> = issues.values().collect();
    entries.sort_by(|a, b| a.id.cmp(&b.id));

    for issue in entries {
        serde_json::to_writer(&mut writer, issue).with_context(|| {
            format!(
                "failed to serialize issue {} to {}",
                issue.id,
                path.display()
            )
        })?;
        writer
            .write_all(b"\n")
            .with_context(|| format!("failed to write newline while writing {}", path.display()))?;
    }

    writer
        .flush()
        .with_context(|| format!("failed to flush {}", path.display()))
}

fn read_existing_event_ids(dir: &Path) -> Result<HashSet<String>> {
    migrate_legacy_event_log(dir)?;

    let mut ids = HashSet::new();
    if !dir.exists() {
        return Ok(ids);
    }

    for entry in fs::read_dir(dir).with_context(|| "failed to read mirror directory")? {
        let entry = entry?;
        let path = entry.path();
        if is_event_log_file(&path) {
            read_event_ids_from_file(&path, &mut ids)?;
        }
    }

    Ok(ids)
}

fn read_event_ids_from_file(path: &Path, ids: &mut HashSet<String>) -> Result<()> {
    let file = File::open(path)
        .with_context(|| format!("failed to open {} for reading", path.display()))?;
    let reader = BufReader::new(file);

    for (idx, line) in reader.lines().enumerate() {
        let line = line.with_context(|| {
            format!(
                "failed to read event line {} from {}",
                idx + 1,
                path.display()
            )
        })?;
        if line.trim().is_empty() {
            continue;
        }
        let event: LinearIssueEvent = serde_json::from_str(&line).with_context(|| {
            format!(
                "failed to parse event line {} from {}",
                idx + 1,
                path.display()
            )
        })?;
        ids.insert(event.id);
    }

    Ok(())
}

fn collect_new_events<'a>(
    node: &'a IssueNode,
    seen: &mut HashSet<String>,
) -> Vec<LinearIssueEvent> {
    LinearIssueEvent::from_issue_node(node)
        .into_iter()
        .filter(|event| seen.insert(event.id.clone()))
        .collect()
}

fn append_events_with_rotation(dir: &Path, events: &[LinearIssueEvent]) -> Result<()> {
    if events.is_empty() {
        return Ok(());
    }

    migrate_legacy_event_log(dir)?;
    ensure_active_event_log(dir)?;
    rotate_if_needed(dir)?;

    let active_path = active_event_log_path(dir);
    let file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(&active_path)
        .with_context(|| format!("failed to open {} for appending", active_path.display()))?;
    let mut writer = BufWriter::new(file);

    for event in events {
        serde_json::to_writer(&mut writer, event).with_context(|| {
            format!(
                "failed to serialize event {} while appending to {}",
                event.id,
                active_path.display()
            )
        })?;
        writer.write_all(b"\n").with_context(|| {
            format!(
                "failed to write newline while appending to {}",
                active_path.display()
            )
        })?;
    }

    writer.flush().with_context(|| {
        format!(
            "failed to flush appended events to {}",
            active_path.display()
        )
    })?;

    rotate_if_needed(dir)?;

    Ok(())
}

fn rotate_if_needed(dir: &Path) -> Result<()> {
    let active_path = active_event_log_path(dir);
    if !active_path.exists() {
        ensure_active_event_log(dir)?;
        return Ok(());
    }

    let size = fs::metadata(&active_path)
        .with_context(|| format!("failed to read metadata for {}", active_path.display()))?
        .len();

    if size < EVENT_ROTATE_TRIGGER_BYTES {
        return Ok(());
    }

    rotate_event_logs(dir)?;
    Ok(())
}

fn rotate_event_logs(dir: &Path) -> Result<()> {
    ensure_active_event_log(dir)?;

    let mut logs: Vec<(u32, PathBuf)> = fs::read_dir(dir)
        .with_context(|| "failed to read mirror directory for rotation")?
        .filter_map(|entry| {
            entry.ok().and_then(|e| {
                let path = e.path();
                let name = path.file_name()?.to_string_lossy().into_owned();
                event_log_number(&name).map(|num| (num, path))
            })
        })
        .collect();

    logs.sort_by(|a, b| b.0.cmp(&a.0));

    for (number, path) in logs {
        let new_path = dir.join(event_log_filename(number + 1));
        fs::rename(&path, &new_path).with_context(|| {
            format!(
                "failed to rotate {} to {}",
                path.display(),
                new_path.display()
            )
        })?;
    }

    ensure_active_event_log(dir)?;
    Ok(())
}

fn migrate_legacy_event_log(dir: &Path) -> Result<()> {
    let legacy = dir.join(format!("{EVENT_LOG_PREFIX}.{EVENT_LOG_EXTENSION}"));
    let active = active_event_log_path(dir);

    if legacy.exists() && !active.exists() {
        fs::rename(&legacy, &active).with_context(|| {
            format!(
                "failed to migrate legacy event log {} to {}",
                legacy.display(),
                active.display()
            )
        })?;
    }

    Ok(())
}

fn ensure_active_event_log(dir: &Path) -> Result<()> {
    let path = active_event_log_path(dir);
    if path.exists() {
        return Ok(());
    }
    File::create(&path).with_context(|| format!("failed to create {}", path.display()))?;
    Ok(())
}

fn active_event_log_path(dir: &Path) -> PathBuf {
    dir.join(event_log_filename(0))
}

fn is_event_log_file(path: &Path) -> bool {
    path.file_name()
        .and_then(OsStr::to_str)
        .and_then(event_log_number)
        .is_some()
}

fn event_log_number(name: &str) -> Option<u32> {
    let legacy = format!("{EVENT_LOG_PREFIX}.{EVENT_LOG_EXTENSION}");
    if name == legacy {
        return Some(0);
    }

    let prefix = format!("{EVENT_LOG_PREFIX}.");
    let suffix = format!(".{EVENT_LOG_EXTENSION}");
    if name.starts_with(&prefix) && name.ends_with(&suffix) {
        let number_part = &name[prefix.len()..name.len() - suffix.len()];
        return number_part.parse().ok();
    }
    None
}

fn event_log_filename(number: u32) -> String {
    format!("{EVENT_LOG_PREFIX}.{number}.{EVENT_LOG_EXTENSION}")
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
