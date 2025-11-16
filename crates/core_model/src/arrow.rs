use chrono::{DateTime, Utc};
use core_persona::identity::IdentityId;
use core_persona::persona::{PersonaId, PersonaKey};
use serde::{Deserialize, Serialize};

use crate::domain::Domain;
use crate::event::EventRef;
use crate::operator::ArrowDirection;

/// Identifies a domain or a specific object within a domain.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ArrowEndpoint {
    Domain { domain: Domain },
    DomainObject { domain: Domain, object_id: String },
}

/// A single arrow: a proposed relation or “move” from one place to another.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Arrow {
    pub id: String,
    pub created_at: DateTime<Utc>,
    pub operator_id: String,
    pub direction: ArrowDirection,
    pub source: ArrowEndpoint,
    pub target: ArrowEndpoint,
    /// One-line description for feeds.
    pub title: String,
    /// Detailed markdown; may contain analysis, reasoning.
    pub detail_markdown: String,
    /// Evidence events that led to this arrow.
    pub evidence: Vec<EventRef>,
    /// Domain-level persona key for the author.
    pub author_persona_key: Option<PersonaKey>,
    /// Resolved persona id of the author, if known.
    pub author_persona_id: Option<PersonaId>,
    /// Canonical identity id of the author, if known.
    pub author_identity_id: Option<IdentityId>,
}
