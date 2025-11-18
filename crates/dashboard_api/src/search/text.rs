use core_model::event::EventEnvelope;
use serde_json::Value;

pub fn event_text_blob(event: &EventEnvelope) -> String {
    let mut parts = Vec::new();

    if !event.summary.trim().is_empty() {
        parts.push(event.summary.clone());
    }
    parts.push(event.kind.clone());

    if let Some(entity) = &event.entity_id {
        parts.push(entity.clone());
    }

    collect_from_value(&event.data, &mut parts);

    parts
        .into_iter()
        .filter_map(|part| {
            let trimmed = part.trim();
            if trimmed.is_empty() {
                None
            } else {
                Some(trimmed.to_owned())
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn collect_from_value(value: &Value, parts: &mut Vec<String>) {
    match value {
        Value::String(s) => parts.push(s.clone()),
        Value::Number(num) => parts.push(num.to_string()),
        Value::Bool(flag) => parts.push(flag.to_string()),
        Value::Array(items) => items
            .iter()
            .for_each(|item| collect_from_value(item, parts)),
        Value::Object(map) => map
            .values()
            .for_each(|child| collect_from_value(child, parts)),
        Value::Null => {}
    }
}
