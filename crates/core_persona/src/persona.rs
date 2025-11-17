use serde::{Deserialize, Serialize};
use uuid::Uuid;

use core_types::Domain;

/// Globally stable identifier for a domain-specific persona (alias).
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct PersonaId(pub Uuid);

impl PersonaId {
    pub fn new() -> Self {
        Self(Uuid::new_v4())
    }
}

impl Default for PersonaId {
    fn default() -> Self {
        Self::new()
    }
}

/// Uniquely identifies a persona/alias within a specific domain.
///
/// The `local_id` field stores the provider's own identifier (e.g., Slack user ID,
/// Linear user UUID). "Local" means "local to that domain/provider", not "local to
/// our system". The UI labels this as "Remote ID" to avoid confusion.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct PersonaKey {
    pub domain: Domain,
    pub local_id: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Persona {
    pub id: PersonaId,
    pub key: PersonaKey,
    pub label: Option<String>,
    pub display_name: Option<String>,
}

impl Persona {
    pub fn new(
        domain: Domain,
        local_id: impl Into<String>,
        label: Option<String>,
        display_name: Option<String>,
    ) -> Self {
        Self {
            id: PersonaId::new(),
            key: PersonaKey {
                domain,
                local_id: local_id.into(),
            },
            label,
            display_name,
        }
    }
}
