use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::persona::Persona;

/// Stable identifier for a canonical identity (natural person).
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct IdentityId(pub Uuid);

impl IdentityId {
    pub fn new() -> Self {
        Self(Uuid::new_v4())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Identity {
    pub id: IdentityId,
    pub canonical_email: String,
    pub preferred_name: Option<String>,
    pub avatar: Option<String>,
    pub notes: Option<String>,
    pub personas: Vec<Persona>,
}

impl Identity {
    pub fn new(canonical_email: impl Into<String>) -> Self {
        Self {
            id: IdentityId::new(),
            canonical_email: canonical_email.into(),
            preferred_name: None,
            avatar: None,
            notes: None,
            personas: Vec::new(),
        }
    }
}
