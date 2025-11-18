use anyhow::{Context, Result};
use core_model::event::EventEnvelope;
use core_persona::identity::IdentityId;
use core_persona::persona::PersonaId;
use core_types::Domain;
use serde_json;
use tantivy::schema::{Field, Schema, FAST, STORED, STRING, TEXT};
use tantivy::TantivyDocument;

use super::text::event_text_blob;

#[derive(Clone)]
pub struct SearchSchema {
    schema: Schema,
    pub id: Field,
    pub domain: Field,
    pub kind: Field,
    pub summary: Field,
    pub body: Field,
    pub entity: Field,
    pub persona: Field,
    pub identity: Field,
    pub at: Field,
    pub raw_event: Field,
}

impl SearchSchema {
    pub fn new() -> Self {
        let mut builder = Schema::builder();
        let id = builder.add_text_field("id", STRING | STORED);
        let domain = builder.add_text_field("domain", STRING | STORED);
        let kind = builder.add_text_field("kind", STRING | STORED);
        let summary = builder.add_text_field("summary", TEXT | STORED);
        let body = builder.add_text_field("body", TEXT | STORED);
        let entity = builder.add_text_field("entity", STRING | STORED);
        let persona = builder.add_text_field("persona", STRING | STORED);
        let identity = builder.add_text_field("identity", STRING | STORED);
        let at = builder.add_i64_field("at", FAST | STORED);
        let raw_event = builder.add_bytes_field("event", STORED);
        let schema = builder.build();
        Self {
            schema,
            id,
            domain,
            kind,
            summary,
            body,
            entity,
            persona,
            identity,
            at,
            raw_event,
        }
    }

    pub fn schema(&self) -> Schema {
        self.schema.clone()
    }

    pub fn default_fields(&self) -> Vec<Field> {
        vec![self.summary, self.body]
    }

    pub fn document(&self, event: &EventEnvelope) -> Result<TantivyDocument> {
        let mut doc = TantivyDocument::new();
        doc.add_text(self.id, &event.id);
        doc.add_text(self.domain, &Self::encode_domain(&event.domain));
        doc.add_text(self.kind, event.kind.as_str());

        if !event.summary.trim().is_empty() {
            doc.add_text(self.summary, event.summary.trim());
        }

        if let Some(entity_id) = event.entity_id.as_deref() {
            doc.add_text(self.entity, entity_id);
        }

        if let Some(persona_id) = event.actor_persona_id {
            doc.add_text(self.persona, &persona_id_to_str(persona_id));
        }

        if let Some(identity_id) = event.actor_identity_id {
            doc.add_text(self.identity, &identity_id_to_str(identity_id));
        }

        doc.add_i64(self.at, event.at.timestamp_millis());

        let body_blob = event_text_blob(event);
        if !body_blob.trim().is_empty() {
            doc.add_text(self.body, &body_blob);
        }

        let bytes = serde_json::to_vec(event)
            .context("failed to serialize event when building search document")?;
        doc.add_bytes(self.raw_event, bytes);

        Ok(doc)
    }

    pub fn encode_domain(domain: &Domain) -> String {
        match domain {
            Domain::Slack => "slack",
            Domain::Linear => "linear",
            Domain::Github => "github",
            Domain::Grain => "grain",
            Domain::GoogleCalendar => "google_calendar",
            Domain::Other(custom) => custom.as_str(),
        }
        .to_string()
    }
}

fn persona_id_to_str(persona_id: PersonaId) -> String {
    persona_id.0.to_string()
}

fn identity_id_to_str(identity_id: IdentityId) -> String {
    identity_id.0.to_string()
}
