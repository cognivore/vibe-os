use std::collections::HashSet;

use crate::linear_sync::models::{
    CycleHistoryEntry, LinearCommentRecord, LinearIssueEvent, LinearIssueSnapshot,
};
use crate::linear_sync::queries::{HistoryEntry, IssueNode};

pub(super) fn collect_new_events(
    node: &IssueNode,
    seen: &mut HashSet<String>,
) -> Vec<LinearIssueEvent> {
    LinearIssueEvent::from_issue_node(node)
        .into_iter()
        .filter(|event| seen.insert(event.id.clone()))
        .collect()
}

impl LinearIssueSnapshot {
    pub(super) fn from_node(node: &IssueNode) -> Self {
        // Extract cycle history from history entries
        let cycle_history = extract_cycle_history(node);

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
            created_at: node.created_at,
            updated_at: node.updated_at,
            completed_at: node.completed_at,
            canceled_at: node.canceled_at,
            archived_at: node.archived_at,
            // Cycle information
            cycle_id: node.cycle.as_ref().map(|c| c.id.clone()),
            cycle_number: node.cycle.as_ref().map(|c| c.number),
            cycle_name: node.cycle.as_ref().and_then(|c| c.name.clone()),
            cycle_starts_at: node.cycle.as_ref().and_then(|c| c.starts_at),
            cycle_ends_at: node.cycle.as_ref().and_then(|c| c.ends_at),
            cycle_history,
        }
    }
}

/// Extract cycle change history from issue history entries.
fn extract_cycle_history(node: &IssueNode) -> Vec<CycleHistoryEntry> {
    let mut entries = Vec::new();
    if let Some(history) = &node.history {
        for entry in &history.nodes {
            // Only record if there was a cycle change (either from or to is present)
            if entry.from_cycle.is_some() || entry.to_cycle.is_some() {
                entries.push(CycleHistoryEntry {
                    at: entry.created_at,
                    from_cycle_number: entry.from_cycle.as_ref().map(|c| c.number),
                    to_cycle_number: entry.to_cycle.as_ref().map(|c| c.number),
                    to_cycle_starts_at: entry.to_cycle.as_ref().and_then(|c| c.starts_at),
                });
            }
        }
    }
    // Sort by timestamp
    entries.sort_by_key(|e| e.at);
    entries
}

impl LinearIssueEvent {
    fn from_issue_node(node: &IssueNode) -> Vec<Self> {
        let mut events = Vec::new();

        events.push(Self {
            id: format!("{}:created", node.id),
            issue_id: node.id.clone(),
            issue_identifier: node.identifier.clone(),
            event_kind: "created".to_string(),
            created_at: node.created_at,
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
                push_state_events(node, entry, &mut events);
                push_priority_event(node, entry, &mut events);
            }
        }

        events
    }
}

fn push_state_events(node: &IssueNode, entry: &HistoryEntry, events: &mut Vec<LinearIssueEvent>) {
    let extra = serde_json::to_value(entry).unwrap_or(serde_json::Value::Null);
    let actor_id = entry.actor.as_ref().map(|actor| actor.id.clone());
    let actor_name = entry.actor.as_ref().map(|actor| actor.name.clone());

    if let (Some(from_state), Some(to_state)) = (&entry.from_state, &entry.to_state) {
        if from_state.name != to_state.name {
            events.push(LinearIssueEvent {
                id: format!("{}:state", entry.id),
                issue_id: node.id.clone(),
                issue_identifier: node.identifier.clone(),
                event_kind: "state_changed".to_string(),
                created_at: entry.created_at,
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
            events.push(LinearIssueEvent {
                id: format!("{}:reopened", entry.id),
                issue_id: node.id.clone(),
                issue_identifier: node.identifier.clone(),
                event_kind: "reopened".to_string(),
                created_at: entry.created_at,
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
}

fn push_priority_event(node: &IssueNode, entry: &HistoryEntry, events: &mut Vec<LinearIssueEvent>) {
    if entry.from_priority == entry.to_priority {
        return;
    }

    let extra = serde_json::to_value(entry).unwrap_or(serde_json::Value::Null);
    let actor_id = entry.actor.as_ref().map(|actor| actor.id.clone());
    let actor_name = entry.actor.as_ref().map(|actor| actor.name.clone());

    events.push(LinearIssueEvent {
        id: format!("{}:priority", entry.id),
        issue_id: node.id.clone(),
        issue_identifier: node.identifier.clone(),
        event_kind: "priority_changed".to_string(),
        created_at: entry.created_at,
        actor_id,
        actor_name,
        from_state: None,
        to_state: None,
        from_priority: entry.from_priority,
        to_priority: entry.to_priority,
        extra,
    });
}

impl LinearCommentRecord {
    pub(super) fn from_issue_node(node: &IssueNode) -> Vec<Self> {
        if let Some(comments_conn) = &node.comments {
            comments_conn
                .nodes
                .iter()
                .map(|comment| Self {
                    id: comment.id.clone(),
                    issue_id: node.id.clone(),
                    issue_identifier: Some(node.identifier.clone()),
                    body: comment.body.clone().unwrap_or_default(),
                    url: comment.url.clone(),
                    created_at: comment.created_at,
                    updated_at: comment.updated_at,
                    user_id: comment.user.as_ref().map(|u| u.id.clone()),
                    user_name: comment.user.as_ref().map(|u| u.name.clone()),
                    user_display_name: comment.user.as_ref().and_then(|u| u.display_name.clone()),
                })
                .collect()
        } else {
            Vec::new()
        }
    }
}

fn is_done_state(state_type: &Option<String>) -> bool {
    matches!(
        state_type.as_deref(),
        Some("completed") | Some("canceled") | Some("done")
    )
}
