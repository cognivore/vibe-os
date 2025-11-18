use serde_json::Value;

use core_model::domain::Domain;
use core_model::event::EventEnvelope;
use core_persona::persona::PersonaKey;

use super::parser::{LinearCommentRecord, LinearEventRecord, LinearIssueSummary};

pub fn map_event_record(record: LinearEventRecord) -> EventEnvelope {
    let issue = record
        .issue_identifier
        .clone()
        .unwrap_or_else(|| record.issue_id.clone());
    let summary = format!("{} {}", issue, record.event_kind.replace('_', " "));
    let mut data_map = record.rest;
    data_map.insert("id".into(), Value::String(record.id.clone()));
    data_map.insert("issue_id".into(), Value::String(record.issue_id.clone()));
    if let Some(identifier) = &record.issue_identifier {
        data_map.insert("issue_identifier".into(), Value::String(identifier.clone()));
    }
    data_map.insert(
        "event_kind".into(),
        Value::String(record.event_kind.clone()),
    );
    data_map.insert(
        "created_at".into(),
        Value::String(record.created_at.to_rfc3339()),
    );
    if let Some(actor) = &record.actor_name {
        data_map.insert("actor_name".into(), Value::String(actor.clone()));
    }
    if let Some(actor_id) = &record.actor_id {
        data_map.insert("actor_id".into(), Value::String(actor_id.clone()));
    }

    EventEnvelope {
        id: format!("linear:{}", record.id),
        domain: Domain::Linear,
        at: record.created_at,
        kind: format!("linear.{}", record.event_kind),
        summary,
        entity_id: Some(issue),
        data: Value::Object(data_map),
        actor_persona_key: record.actor_id.as_ref().map(|id| PersonaKey {
            domain: Domain::Linear,
            local_id: id.clone(),
        }),
        actor_persona_id: None,
        actor_identity_id: None,
    }
}

pub fn enrich_issue_metadata(value: &mut Value, summary: Option<&LinearIssueSummary>) {
    let Some(obj) = value.as_object_mut() else {
        return;
    };

    if let Some(summary) = summary {
        if let Some(title) = &summary.title {
            obj.insert("issue_title".into(), Value::String(title.clone()));
        }
        if let Some(description) = &summary.description {
            obj.insert(
                "issue_description".into(),
                Value::String(description.clone()),
            );
        }
        if let Some(url) = &summary.url {
            obj.insert("issue_url".into(), Value::String(url.clone()));
        }
        if let Some(identifier) = &summary.identifier {
            obj.insert("issue_identifier".into(), Value::String(identifier.clone()));
        }
        if let Some(assignee) = &summary.assignee_name {
            obj.insert("issue_assignee".into(), Value::String(assignee.clone()));
        }
        if let Some(state) = &summary.state_name {
            obj.insert("issue_state".into(), Value::String(state.clone()));
        }
        if let Some(state_type) = &summary.state_type {
            obj.insert("issue_state_type".into(), Value::String(state_type.clone()));
        }
        if let Some(team) = &summary.team_name {
            obj.insert("issue_team".into(), Value::String(team.clone()));
        }
        if let Some(priority) = summary.priority {
            obj.insert("issue_priority".into(), Value::Number(priority.into()));
        }
        if !summary.labels.is_empty() {
            obj.insert(
                "issue_labels".into(),
                Value::Array(
                    summary
                        .labels
                        .iter()
                        .map(|label| Value::String(label.clone()))
                        .collect(),
                ),
            );
        }
    }
}

pub fn map_comment_record(
    record: LinearCommentRecord,
    summary: Option<&LinearIssueSummary>,
) -> EventEnvelope {
    let mut data_map = serde_json::Map::new();
    data_map.insert("comment_body".into(), Value::String(record.body.clone()));
    if let Some(url) = &record.url {
        data_map.insert("comment_url".into(), Value::String(url.clone()));
    }
    data_map.insert("issue_id".into(), Value::String(record.issue_id.clone()));
    if let Some(identifier) = &record.issue_identifier {
        data_map.insert("issue_identifier".into(), Value::String(identifier.clone()));
    }
    data_map.insert(
        "created_at".into(),
        Value::String(record.created_at.to_rfc3339()),
    );
    data_map.insert(
        "updated_at".into(),
        Value::String(record.updated_at.to_rfc3339()),
    );
    if let Some(name) = &record.user_name {
        data_map.insert("actor_name".into(), Value::String(name.clone()));
    }
    if let Some(display) = &record.user_display_name {
        data_map.insert("actor_display_name".into(), Value::String(display.clone()));
    }
    if let Some(summary) = summary {
        if let Some(title) = &summary.title {
            data_map.insert("issue_title".into(), Value::String(title.clone()));
        }
        if let Some(description) = &summary.description {
            data_map.insert(
                "issue_description".into(),
                Value::String(description.clone()),
            );
        }
        if let Some(url) = &summary.url {
            data_map.insert("issue_url".into(), Value::String(url.clone()));
        }
        if let Some(assignee) = &summary.assignee_name {
            data_map.insert("issue_assignee".into(), Value::String(assignee.clone()));
        }
        if let Some(state) = &summary.state_name {
            data_map.insert("issue_state".into(), Value::String(state.clone()));
        }
        if let Some(state_type) = &summary.state_type {
            data_map.insert("issue_state_type".into(), Value::String(state_type.clone()));
        }
        if let Some(team) = &summary.team_name {
            data_map.insert("issue_team".into(), Value::String(team.clone()));
        }
        if let Some(priority) = summary.priority {
            data_map.insert("issue_priority".into(), Value::Number(priority.into()));
        }
        if !summary.labels.is_empty() {
            data_map.insert(
                "issue_labels".into(),
                Value::Array(
                    summary
                        .labels
                        .iter()
                        .map(|label| Value::String(label.clone()))
                        .collect(),
                ),
            );
        }
    }
    let persona_key = record.user_id.as_ref().map(|id| PersonaKey {
        domain: Domain::Linear,
        local_id: id.clone(),
    });
    let issue_reference = record
        .issue_identifier
        .clone()
        .unwrap_or_else(|| record.issue_id.clone());
    EventEnvelope {
        id: format!("linear_comment:{}", record.id),
        domain: Domain::Linear,
        at: record.updated_at,
        kind: "linear.comment".into(),
        summary: record.body.chars().take(140).collect(),
        entity_id: Some(issue_reference),
        data: Value::Object(data_map),
        actor_persona_key: persona_key,
        actor_persona_id: None,
        actor_identity_id: None,
    }
}
