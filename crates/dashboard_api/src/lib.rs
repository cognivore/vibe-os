use std::collections::HashMap;
use std::net::SocketAddr;
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::{anyhow, Context, Result};
use arrow_store::ArrowStore;
use axum::extract::{Path, Query, State};
use axum::http::StatusCode;
use axum::response::{IntoResponse, Response};
use axum::routing::{get, get_service, post};
use axum::{Json, Router};
use chrono::{DateTime, Utc};
use core_model::adapters::EventAdapter;
use core_model::arrow::Arrow;
use core_model::domain::{builtin_domains, Domain};
use core_model::event::EventEnvelope;
use core_model::operator::{ArrowDirection, OperatorDescriptor};
use core_model::time::TimeWindow;
use core_operators::{OperatorContext, OperatorRegistry};
use core_persona::identity::{Identity, IdentityId};
use core_persona::persona::{Persona, PersonaKey};
use core_persona::store::IdentityStore;
use domain_adapters::{LinearAdapter, SlackAdapter};
use serde::{Deserialize, Serialize};
use tokio::sync::Mutex;
use tower_http::services::{ServeDir, ServeFile};
use tracing::info;
use uuid::Uuid;

type AdapterHandle = Arc<dyn EventAdapter>;

#[derive(Clone)]
struct AppState {
    adapters: Arc<HashMap<Domain, AdapterHandle>>,
    operator_registry: Arc<OperatorRegistry>,
    arrow_store: Arc<ArrowStore>,
    identity_store: Arc<Mutex<IdentityStore>>,
    meta: Arc<MetaSnapshot>,
}

#[derive(Clone, Serialize)]
struct MetaSnapshot {
    slack_mirror_dir: String,
    linear_mirror_dir: String,
    arrow_store_dir: String,
    identity_store_dir: String,
    static_dir: Option<String>,
}

#[derive(Debug, Serialize)]
struct IdentityListItem {
    id: IdentityId,
    canonical_email: String,
    preferred_name: Option<String>,
    avatar: Option<String>,
    personas: Vec<Persona>,
}

pub struct DashboardServerSettings {
    pub slack_mirror_dir: PathBuf,
    pub linear_mirror_dir: PathBuf,
    pub arrow_store_dir: PathBuf,
    pub persona_root_dir: PathBuf,
    pub bind: SocketAddr,
    pub static_dir: Option<PathBuf>,
}

pub async fn run_dashboard_server(settings: DashboardServerSettings) -> Result<()> {
    let adapters = build_adapters(&settings);
    let operator_registry = Arc::new(OperatorRegistry::new());
    let arrow_store = Arc::new(ArrowStore::new(&settings.arrow_store_dir));
    let identity_store = Arc::new(Mutex::new(IdentityStore::load(&settings.persona_root_dir)?));
    let meta = MetaSnapshot {
        slack_mirror_dir: settings.slack_mirror_dir.display().to_string(),
        linear_mirror_dir: settings.linear_mirror_dir.display().to_string(),
        arrow_store_dir: settings.arrow_store_dir.display().to_string(),
        identity_store_dir: settings.persona_root_dir.display().to_string(),
        static_dir: settings
            .static_dir
            .as_ref()
            .map(|p| p.display().to_string()),
    };

    let state = AppState {
        adapters: Arc::new(adapters),
        operator_registry,
        arrow_store,
        identity_store,
        meta: Arc::new(meta),
    };

    let identity_routes = Router::new()
        .route("/", get(list_identities).post(create_identity))
        .route("/lookup", get(lookup_identity))
        .route("/merge", post(merge_identities))
        .route("/:id", get(get_identity))
        .route("/:id/personas", post(attach_persona));

    let api_router = Router::new()
        .route("/domains", get(list_domains))
        .route("/events", get(list_events))
        .route("/operators", get(list_operators))
        .route("/operators/run", post(run_operator))
        .route("/arrows", get(list_arrows))
        .route("/meta", get(fetch_meta))
        .nest("/identities", identity_routes)
        .with_state(state.clone());

    let mut app = Router::new().nest("/api", api_router);

    if let Some(static_dir) = &settings.static_dir {
        let static_service = ServeDir::new(static_dir.clone())
            .not_found_service(ServeFile::new(static_dir.join("index.html")));
        app = app.fallback_service(get_service(static_service));
    } else {
        app = app.fallback(handler_not_found);
    }

    info!("Starting dashboard server on {}", settings.bind);
    let listener = tokio::net::TcpListener::bind(settings.bind).await?;
    axum::serve(listener, app.into_make_service()).await?;
    Ok(())
}

async fn handler_not_found() -> impl IntoResponse {
    (StatusCode::NOT_FOUND, "Not found")
}

async fn list_domains() -> impl IntoResponse {
    Json(builtin_domains())
}

#[derive(Deserialize)]
struct EventsQuery {
    domains: Option<String>,
    from: String,
    to: String,
    limit: Option<usize>,
}

async fn list_events(
    State(state): State<AppState>,
    Query(query): Query<EventsQuery>,
) -> Result<Json<Vec<EventEnvelope>>, AppError> {
    let window = build_window(&query.from, &query.to)?;
    let domains = select_domains(query.domains.as_deref(), &state)?;
    let mut events = Vec::new();
    for domain in domains {
        if let Some(adapter) = state.adapters.get(&domain) {
            let adapter = adapter.clone();
            let window_clone = window.clone();
            let mut loaded =
                tokio::task::spawn_blocking(move || adapter.load_events(&window_clone))
                    .await
                    .map_err(|err| anyhow!("adapter task failed: {}", err))??;
            events.append(&mut loaded);
        }
    }
    events.sort_by(|a, b| b.at.cmp(&a.at));
    if let Some(limit) = query.limit {
        events.truncate(limit);
    }
    resolve_event_entities(&mut events, state.identity_store.clone()).await;
    Ok(Json(events))
}

async fn list_operators(State(state): State<AppState>) -> impl IntoResponse {
    Json(state.operator_registry.descriptors())
}

#[derive(Deserialize)]
struct RunOperatorBody {
    operator_id: String,
    domains: Option<Vec<String>>,
    from: String,
    to: String,
}

#[derive(Serialize)]
struct RunOperatorResponse {
    operator: OperatorDescriptor,
    window: TimeWindow,
    arrows: Vec<Arrow>,
}

#[derive(Debug, Deserialize)]
struct CreateIdentityRequest {
    canonical_email: String,
    preferred_name: Option<String>,
    avatar: Option<String>,
    notes: Option<String>,
}

#[derive(Debug, Deserialize)]
struct AttachPersonaRequest {
    domain: String,
    local_id: String,
    label: Option<String>,
    display_name: Option<String>,
}

#[derive(Debug, Deserialize)]
struct MergeIdentitiesRequest {
    primary: IdentityId,
    secondary: IdentityId,
}

#[derive(Debug, Deserialize)]
struct LookupIdentityQuery {
    email: Option<String>,
    domain: Option<String>,
    local_id: Option<String>,
}

async fn run_operator(
    State(state): State<AppState>,
    Json(body): Json<RunOperatorBody>,
) -> Result<Json<RunOperatorResponse>, AppError> {
    let operator = state
        .operator_registry
        .find(&body.operator_id)
        .ok_or_else(|| anyhow!("operator {} not found", body.operator_id))?;

    let window = build_window(&body.from, &body.to)?;
    let domains = match &body.domains {
        Some(list) if !list.is_empty() => list
            .iter()
            .map(|id| parse_domain_id(id))
            .collect::<Vec<_>>(),
        _ => operator.descriptor().source_domains.clone(),
    };

    let events = load_events_for_domains(&state, &domains, &window).await?;
    let ctx = OperatorContext {
        descriptor: operator.descriptor(),
        window: window.clone(),
        events: &events,
        now: Utc::now(),
    };
    let result = operator.run(&ctx)?;
    let mut arrows = result.arrows;

    if !arrows.is_empty() {
        let store = state.arrow_store.clone();
        let to_store = arrows.clone();
        tokio::task::spawn_blocking(move || store.append(&to_store)).await??;
    }

    resolve_arrow_entities(&mut arrows, state.identity_store.clone()).await;

    Ok(Json(RunOperatorResponse {
        operator: operator.descriptor().clone(),
        window,
        arrows,
    }))
}

#[derive(Deserialize)]
struct ArrowsQuery {
    from: String,
    to: String,
    direction: Option<String>,
    source_domains: Option<String>,
    target_domains: Option<String>,
}

async fn list_arrows(
    State(state): State<AppState>,
    Query(query): Query<ArrowsQuery>,
) -> Result<Json<Vec<Arrow>>, AppError> {
    let window = build_window(&query.from, &query.to)?;
    let direction = query
        .direction
        .as_deref()
        .map(parse_direction)
        .transpose()?;
    let source_filter = parse_domains_csv(query.source_domains.as_deref());
    let target_filter = parse_domains_csv(query.target_domains.as_deref());

    let store = state.arrow_store.clone();
    let mut arrows =
        tokio::task::spawn_blocking(move || store.load_in_window(&window, None)).await??;
    resolve_arrow_entities(&mut arrows, state.identity_store.clone()).await;

    let filtered = arrows
        .into_iter()
        .filter(|arrow| match &direction {
            Some(dir) => arrow.direction == *dir,
            None => true,
        })
        .filter(|arrow| match &source_filter {
            Some(domains) => arrow_source_matches(arrow, domains),
            None => true,
        })
        .filter(|arrow| match &target_filter {
            Some(domains) => arrow_target_matches(arrow, domains),
            None => true,
        })
        .collect();
    Ok(Json(filtered))
}

async fn fetch_meta(State(state): State<AppState>) -> impl IntoResponse {
    Json(state.meta.as_ref().clone())
}

async fn list_identities(
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

async fn get_identity(
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

async fn create_identity(
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

async fn attach_persona(
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

async fn merge_identities(
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

async fn lookup_identity(
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

fn build_adapters(settings: &DashboardServerSettings) -> HashMap<Domain, AdapterHandle> {
    let mut map: HashMap<Domain, AdapterHandle> = HashMap::new();
    map.insert(
        Domain::Slack,
        Arc::new(SlackAdapter::new(&settings.slack_mirror_dir)),
    );
    map.insert(
        Domain::Linear,
        Arc::new(LinearAdapter::new(&settings.linear_mirror_dir)),
    );
    map
}

fn build_window(from: &str, to: &str) -> Result<TimeWindow> {
    let start = parse_time(from)?;
    let end = parse_time(to)?;
    if end <= start {
        anyhow::bail!("`to` must be after `from`");
    }
    Ok(TimeWindow { start, end })
}

fn parse_time(value: &str) -> Result<DateTime<Utc>> {
    DateTime::parse_from_rfc3339(value)
        .map(|dt| dt.with_timezone(&Utc))
        .or_else(|_| {
            DateTime::parse_from_str(value, "%Y-%m-%dT%H:%M:%S").map(|dt| dt.with_timezone(&Utc))
        })
        .with_context(|| format!("failed to parse datetime `{}`", value))
}

fn select_domains(csv: Option<&str>, state: &AppState) -> Result<Vec<Domain>> {
    if let Some(raw) = csv {
        if raw.trim().is_empty() {
            return Ok(state.adapters.keys().cloned().collect());
        }
        return raw
            .split(',')
            .map(|token| {
                let domain = parse_domain_id(token.trim());
                if state.adapters.contains_key(&domain) {
                    Ok(domain)
                } else {
                    Err(anyhow!("domain `{}` is not registered", token))
                }
            })
            .collect();
    }
    Ok(state.adapters.keys().cloned().collect())
}

fn parse_domain_id(raw: &str) -> Domain {
    match raw {
        "slack" => Domain::Slack,
        "linear" => Domain::Linear,
        "github" => Domain::Github,
        "grain" => Domain::Grain,
        "google_calendar" => Domain::GoogleCalendar,
        other => Domain::Other(other.to_string()),
    }
}

fn parse_direction(raw: &str) -> Result<ArrowDirection> {
    match raw {
        "analysis_request" => Ok(ArrowDirection::AnalysisRequest),
        "synthesis_request" => Ok(ArrowDirection::SynthesisRequest),
        "elaboration_request" => Ok(ArrowDirection::ElaborationRequest),
        other => Err(anyhow!("unknown arrow direction `{}`", other)),
    }
}

fn parse_domains_csv(raw: Option<&str>) -> Option<Vec<Domain>> {
    raw.map(|csv| {
        csv.split(',')
            .map(|token| parse_domain_id(token.trim()))
            .collect()
    })
}

async fn load_events_for_domains(
    state: &AppState,
    domains: &[Domain],
    window: &TimeWindow,
) -> Result<Vec<EventEnvelope>> {
    let mut events = Vec::new();
    for domain in domains {
        if let Some(adapter) = state.adapters.get(domain) {
            let adapter = adapter.clone();
            let window_clone = window.clone();
            let mut loaded =
                tokio::task::spawn_blocking(move || adapter.load_events(&window_clone))
                    .await
                    .map_err(|err| anyhow!("adapter task failed: {}", err))??;
            events.append(&mut loaded);
        }
    }
    events.sort_by(|a, b| a.at.cmp(&b.at));
    resolve_event_entities(&mut events, state.identity_store.clone()).await;
    Ok(events)
}

async fn resolve_event_entities(events: &mut [EventEnvelope], store: Arc<Mutex<IdentityStore>>) {
    let store_guard = store.lock().await;
    for event in events {
        if let Some(key) = &event.actor_persona_key {
            if let Some((persona_id, identity_id)) = store_guard.lookup_persona_ids(key) {
                event.actor_persona_id = Some(persona_id);
                event.actor_identity_id = Some(identity_id);
            }
        }
    }
}

async fn resolve_arrow_entities(arrows: &mut [Arrow], store: Arc<Mutex<IdentityStore>>) {
    let store_guard = store.lock().await;
    for arrow in arrows {
        if let Some(key) = &arrow.author_persona_key {
            if let Some((persona_id, identity_id)) = store_guard.lookup_persona_ids(key) {
                arrow.author_persona_id = Some(persona_id);
                arrow.author_identity_id = Some(identity_id);
            }
        }
    }
}

fn arrow_source_matches(arrow: &Arrow, domains: &[Domain]) -> bool {
    domains
        .iter()
        .any(|domain| endpoint_has_domain(&arrow.source, domain))
}

fn arrow_target_matches(arrow: &Arrow, domains: &[Domain]) -> bool {
    domains
        .iter()
        .any(|domain| endpoint_has_domain(&arrow.target, domain))
}

fn endpoint_has_domain(endpoint: &core_model::arrow::ArrowEndpoint, domain: &Domain) -> bool {
    match endpoint {
        core_model::arrow::ArrowEndpoint::Domain { domain: d } => d == domain,
        core_model::arrow::ArrowEndpoint::DomainObject { domain: d, .. } => d == domain,
    }
}

fn parse_identity_id(raw: &str) -> Result<IdentityId> {
    let id = Uuid::parse_str(raw).with_context(|| format!("invalid identity id `{}`", raw))?;
    Ok(IdentityId(id))
}

struct AppError(anyhow::Error);

impl IntoResponse for AppError {
    fn into_response(self) -> Response {
        (StatusCode::BAD_REQUEST, format!("error: {}", self.0)).into_response()
    }
}

impl<E> From<E> for AppError
where
    E: Into<anyhow::Error>,
{
    fn from(err: E) -> Self {
        AppError(err.into())
    }
}
