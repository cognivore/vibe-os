use anyhow::anyhow;
use axum::extract::{Path, Query, State};
use axum::routing::{delete, get, post};
use axum::{Json, Router};
use core_persona::identity::{Identity, IdentityId};
use core_persona::persona::{Persona, PersonaKey};
use serde::Deserialize;

use crate::error::AppError;
use crate::state::AppState;
use crate::utils::{parse_domain_id, parse_identity_id, parse_persona_id};

pub fn router() -> Router<AppState> {
    Router::new()
        .route("/", get(list_identities).post(create_identity))
        .route("/lookup", get(lookup_identity))
        .route("/merge", post(merge_identities))
        .route("/:id/personas", post(attach_persona))
        .route("/:id/personas/:persona_id", delete(detach_persona))
        .route("/:id", get(get_identity))
}

#[derive(Debug, Deserialize)]
pub struct CreateIdentityRequest {
    pub canonical_email: String,
    pub preferred_name: Option<String>,
    pub avatar: Option<String>,
    pub notes: Option<String>,
}

#[derive(Debug, Deserialize)]
pub struct AttachPersonaRequest {
    pub domain: String,
    pub local_id: String,
    pub label: Option<String>,
    pub display_name: Option<String>,
}

#[derive(Debug, Deserialize)]
pub struct MergeIdentitiesRequest {
    pub primary: IdentityId,
    pub secondary: IdentityId,
}

#[derive(Debug, Deserialize)]
pub struct LookupIdentityQuery {
    pub email: Option<String>,
    pub domain: Option<String>,
    pub local_id: Option<String>,
}

#[derive(Debug, serde::Serialize)]
pub struct IdentityListItem {
    pub id: IdentityId,
    pub canonical_email: String,
    pub preferred_name: Option<String>,
    pub avatar: Option<String>,
    pub personas: Vec<Persona>,
}

pub async fn list_identities(
    State(state): State<AppState>,
) -> Result<Json<Vec<IdentityListItem>>, AppError> {
    let store = state.identity_store.lock().await;
    let items = store
        .all_identities()
        .into_iter()
        .map(|identity| IdentityListItem {
            id: identity.id,
            canonical_email: identity.canonical_email.clone(),
            preferred_name: identity.preferred_name.clone(),
            avatar: identity.avatar.clone(),
            personas: identity.personas.clone(),
        })
        .collect();
    Ok(Json(items))
}

pub async fn get_identity(
    State(state): State<AppState>,
    Path(id): Path<String>,
) -> Result<Json<Identity>, AppError> {
    let identity_id = parse_identity_id(&id)?;
    let store = state.identity_store.lock().await;
    let identity = store
        .find_identity(identity_id)
        .cloned()
        .ok_or_else(|| anyhow!("identity not found"))?;
    Ok(Json(identity))
}

pub async fn create_identity(
    State(state): State<AppState>,
    Json(body): Json<CreateIdentityRequest>,
) -> Result<Json<Identity>, AppError> {
    if body.canonical_email.trim().is_empty() {
        return Err(AppError(anyhow!("canonical_email is required")));
    }
    let mut store = state.identity_store.lock().await;
    let identity_id = store.ensure_identity(body.canonical_email.trim());
    store.update_identity(identity_id, |identity| {
        if body.preferred_name.is_some() {
            identity.preferred_name = body.preferred_name.clone();
        }
        if body.avatar.is_some() {
            identity.avatar = body.avatar.clone();
        }
        if body.notes.is_some() {
            identity.notes = body.notes.clone();
        }
    })?;
    store.save()?;
    let identity = store
        .find_identity(identity_id)
        .cloned()
        .ok_or_else(|| anyhow!("failed to reload identity"))?;
    Ok(Json(identity))
}

pub async fn attach_persona(
    State(state): State<AppState>,
    Path(id): Path<String>,
    Json(body): Json<AttachPersonaRequest>,
) -> Result<Json<Identity>, AppError> {
    let identity_id = parse_identity_id(&id)?;
    let domain = parse_domain_id(&body.domain);
    let persona = Persona::new(
        domain,
        body.local_id.clone(),
        body.label.clone(),
        body.display_name.clone(),
    );
    let mut store = state.identity_store.lock().await;
    store.attach_persona(identity_id, persona)?;
    store.save()?;
    let identity = store
        .find_identity(identity_id)
        .cloned()
        .ok_or_else(|| anyhow!("identity not found"))?;
    Ok(Json(identity))
}

pub async fn detach_persona(
    State(state): State<AppState>,
    Path((identity_id_str, persona_id_str)): Path<(String, String)>,
) -> Result<Json<Identity>, AppError> {
    let identity_id = parse_identity_id(&identity_id_str)?;
    let persona_id = parse_persona_id(&persona_id_str)?;
    let mut store = state.identity_store.lock().await;
    store.detach_persona(identity_id, persona_id)?;
    store.save()?;
    let identity = store
        .find_identity(identity_id)
        .cloned()
        .ok_or_else(|| anyhow!("identity not found"))?;
    Ok(Json(identity))
}

pub async fn merge_identities(
    State(state): State<AppState>,
    Json(body): Json<MergeIdentitiesRequest>,
) -> Result<Json<Identity>, AppError> {
    let mut store = state.identity_store.lock().await;
    let survivor = store.merge_identities(body.primary, body.secondary)?;
    store.save()?;
    let identity = store
        .find_identity(survivor)
        .cloned()
        .ok_or_else(|| anyhow!("identity not found after merge"))?;
    Ok(Json(identity))
}

pub async fn lookup_identity(
    State(state): State<AppState>,
    Query(query): Query<LookupIdentityQuery>,
) -> Result<Json<Option<Identity>>, AppError> {
    let store = state.identity_store.lock().await;
    if let Some(email) = &query.email {
        let identity = store.find_identity_by_email(email).cloned();
        return Ok(Json(identity));
    }

    if let (Some(domain), Some(local_id)) = (&query.domain, &query.local_id) {
        let key = PersonaKey {
            domain: parse_domain_id(domain),
            local_id: local_id.clone(),
        };
        let identity = store
            .lookup_persona_ids(&key)
            .and_then(|(_, identity_id)| store.find_identity(identity_id).cloned());
        return Ok(Json(identity));
    }

    Ok(Json(None))
}
