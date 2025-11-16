use chrono::{DateTime, Utc};
use core_persona::identity::IdentityId;
use core_persona::persona::{PersonaId, PersonaKey};
use serde::{Deserialize, Serialize};

use crate::domain::Domain;

/// Reference to an event in a specific domain.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EventRef {
    pub domain: Domain,
    /// Domain-local identifier (e.g., Slack ts, Linear event id).
    pub local_id: String,
}

/// Canonical event envelope, independent of any specific domain.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EventEnvelope {
    /// Stable global id: e.g. `${domain}:${local_id}`
    pub id: String,
    pub domain: Domain,
    pub at: DateTime<Utc>,
    /// Short classification within the domain, e.g. "slack.message", "linear.issue_state_changed".
    pub kind: String,
    /// Short human-readable summary for feeds.
    pub summary: String,
    /// Optional linkable entity in the domain, e.g. channel, thread, issue, PR id.
    pub entity_id: Option<String>,
    /// Domain-specific payload; structured JSON.
    pub data: serde_json::Value,
    /// Optional domain-level persona key observed from the source log.
    pub actor_persona_key: Option<PersonaKey>,
    /// Persona record resolved in the persona store.
    pub actor_persona_id: Option<PersonaId>,
    /// Canonical identity that owns the persona.
    pub actor_identity_id: Option<IdentityId>,
}
