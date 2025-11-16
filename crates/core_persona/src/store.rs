use std::collections::HashMap;
use std::fs::{self, File};
use std::io::{BufReader, BufWriter};
use std::path::PathBuf;

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};

use crate::identity::{Identity, IdentityId};
use crate::persona::{Persona, PersonaId, PersonaKey};

const IDENTITIES_FILE: &str = "identities.json";

#[derive(Debug, Serialize, Deserialize)]
pub struct IdentityStoreSnapshot {
    pub identities: Vec<Identity>,
}

/// Persistent store tying canonical identities to their domain personas.
pub struct IdentityStore {
    root: PathBuf,
    identities: HashMap<IdentityId, Identity>,
    by_email: HashMap<String, IdentityId>,
    persona_to_identity: HashMap<PersonaId, IdentityId>,
    persona_keys: HashMap<PersonaKey, PersonaId>,
}

impl IdentityStore {
    pub fn load(root: impl Into<PathBuf>) -> Result<Self> {
        let root = root.into();
        fs::create_dir_all(&root)
            .with_context(|| format!("failed to create identity store dir {}", root.display()))?;
        let snapshot_path = root.join(IDENTITIES_FILE);
        let snapshot = if snapshot_path.exists() {
            let file = File::open(&snapshot_path)
                .with_context(|| format!("failed to open {}", snapshot_path.display()))?;
            let reader = BufReader::new(file);
            serde_json::from_reader(reader)
                .with_context(|| format!("failed to parse {}", snapshot_path.display()))?
        } else {
            IdentityStoreSnapshot {
                identities: Vec::new(),
            }
        };

        let mut identities = HashMap::new();
        let mut by_email = HashMap::new();
        let mut persona_to_identity = HashMap::new();
        let mut persona_keys = HashMap::new();

        for identity in snapshot.identities {
            for persona in &identity.personas {
                persona_to_identity.insert(persona.id, identity.id);
                persona_keys.insert(persona.key.clone(), persona.id);
            }
            by_email.insert(identity.canonical_email.to_lowercase(), identity.id);
            identities.insert(identity.id, identity);
        }

        Ok(Self {
            root,
            identities,
            by_email,
            persona_to_identity,
            persona_keys,
        })
    }

    fn snapshot_path(&self) -> PathBuf {
        self.root.join(IDENTITIES_FILE)
    }

    pub fn save(&self) -> Result<()> {
        let snapshot = IdentityStoreSnapshot {
            identities: self.identities.values().cloned().collect(),
        };
        let path = self.snapshot_path();
        let file =
            File::create(&path).with_context(|| format!("failed to create {}", path.display()))?;
        let writer = BufWriter::new(file);
        serde_json::to_writer_pretty(writer, &snapshot)
            .with_context(|| format!("failed to write {}", path.display()))
    }

    pub fn all_identities(&self) -> Vec<&Identity> {
        self.identities.values().collect()
    }

    pub fn find_identity(&self, id: IdentityId) -> Option<&Identity> {
        self.identities.get(&id)
    }

    pub fn find_identity_by_email(&self, email: &str) -> Option<&Identity> {
        self.by_email
            .get(&email.to_lowercase())
            .and_then(|id| self.identities.get(id))
    }

    pub fn lookup_persona_ids(&self, key: &PersonaKey) -> Option<(PersonaId, IdentityId)> {
        self.persona_keys.get(key).and_then(|persona_id| {
            self.persona_to_identity
                .get(persona_id)
                .map(|identity_id| (*persona_id, *identity_id))
        })
    }

    pub fn identity_for_persona(&self, persona_id: PersonaId) -> Option<IdentityId> {
        self.persona_to_identity.get(&persona_id).copied()
    }

    pub fn ensure_identity(&mut self, canonical_email: impl Into<String>) -> IdentityId {
        let email = canonical_email.into();
        let key = email.to_lowercase();
        if let Some(identity_id) = self.by_email.get(&key) {
            return *identity_id;
        }
        let identity = Identity::new(email);
        let id = identity.id;
        self.by_email
            .insert(identity.canonical_email.to_lowercase(), id);
        self.identities.insert(id, identity);
        id
    }

    pub fn attach_persona(
        &mut self,
        identity_id: IdentityId,
        persona: Persona,
    ) -> Result<PersonaId> {
        if let Some(existing) = self.persona_keys.get(&persona.key) {
            let existing_identity = self
                .persona_to_identity
                .get(existing)
                .copied()
                .unwrap_or(identity_id);
            if existing_identity != identity_id {
                anyhow::bail!("persona key already linked to another identity");
            }
            return Ok(*existing);
        }

        let identity = self
            .identities
            .get_mut(&identity_id)
            .context("identity not found")?;
        let persona_id = persona.id;
        identity.personas.push(persona.clone());
        self.persona_to_identity.insert(persona_id, identity_id);
        self.persona_keys.insert(persona.key, persona_id);
        Ok(persona_id)
    }

    pub fn merge_identities(
        &mut self,
        primary: IdentityId,
        secondary: IdentityId,
    ) -> Result<IdentityId> {
        if primary == secondary {
            return Ok(primary);
        }
        let mut secondary_identity = self
            .identities
            .remove(&secondary)
            .context("secondary identity not found")?;
        let primary_identity = self
            .identities
            .get_mut(&primary)
            .context("primary identity not found")?;

        if primary_identity.preferred_name.is_none() {
            primary_identity.preferred_name = secondary_identity.preferred_name.take();
        }
        if primary_identity.avatar.is_none() {
            primary_identity.avatar = secondary_identity.avatar.take();
        }
        if primary_identity.notes.is_none() {
            primary_identity.notes = secondary_identity.notes.take();
        }

        for persona in secondary_identity.personas.into_iter() {
            let persona_id = persona.id;
            if self.persona_keys.contains_key(&persona.key) {
                continue;
            }
            self.persona_to_identity.insert(persona_id, primary);
            self.persona_keys.insert(persona.key.clone(), persona_id);
            primary_identity.personas.push(persona);
        }

        self.by_email
            .retain(|_, identity_id| *identity_id != secondary);
        Ok(primary)
    }

    pub fn identities_mut(&mut self) -> impl Iterator<Item = &mut Identity> {
        self.identities.values_mut()
    }

    pub fn update_identity<F>(&mut self, id: IdentityId, mut f: F) -> Result<()>
    where
        F: FnMut(&mut Identity),
    {
        let identity = self.identities.get_mut(&id).context("identity not found")?;
        f(identity);
        Ok(())
    }
}
