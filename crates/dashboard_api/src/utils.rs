use std::collections::HashMap;
use std::sync::Arc;

use anyhow::{anyhow, Context, Result};
use chrono::{DateTime, Duration, TimeZone, Utc};
use core_model::arrow::{Arrow, ArrowEndpoint};
use core_model::domain::Domain;
use core_model::event::EventEnvelope;
use core_model::operator::ArrowDirection;
use core_model::time::TimeWindow;
use core_persona::identity::IdentityId;
use core_persona::persona::PersonaId;
use core_persona::store::IdentityStore;
use tokio::sync::Mutex;
use uuid::Uuid;

use crate::state::{AdapterHandle, AppState, DashboardServerSettings};

pub(crate) fn build_adapters(settings: &DashboardServerSettings) -> HashMap<Domain, AdapterHandle> {
    use domain_adapters::{LinearAdapter, SlackAdapter};

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

pub(crate) fn build_window(from: &str, to: &str) -> Result<TimeWindow> {
    let start = parse_time(from)?;
    let end = parse_time(to)?;
    if end <= start {
        anyhow::bail!("`to` must be after `from`");
    }
    Ok(TimeWindow { start, end })
}

pub(crate) fn parse_time(value: &str) -> Result<DateTime<Utc>> {
    DateTime::parse_from_rfc3339(value)
        .map(|dt| dt.with_timezone(&Utc))
        .or_else(|_| {
            DateTime::parse_from_str(value, "%Y-%m-%dT%H:%M:%S").map(|dt| dt.with_timezone(&Utc))
        })
        .with_context(|| format!("failed to parse datetime `{}`", value))
}

pub(crate) fn full_history_window() -> TimeWindow {
    let start = Utc
        .timestamp_opt(0, 0)
        .single()
        .expect("unix epoch should be representable");
    let end = Utc::now() + Duration::days(365 * 100);
    TimeWindow { start, end }
}

pub(crate) fn select_domains(csv: Option<&str>, state: &AppState) -> Result<Vec<Domain>> {
    if let Some(raw) = csv {
        if raw.trim().is_empty() {
            return Ok(state.adapters.keys().cloned().collect());
        }
        let domains: Vec<Domain> = raw
            .split(',')
            .map(|token| parse_domain_id(token.trim()))
            .filter(|domain| state.adapters.contains_key(domain))
            .collect();
        return Ok(domains);
    }
    Ok(state.adapters.keys().cloned().collect())
}

pub(crate) fn parse_domain_id(raw: &str) -> Domain {
    match raw {
        "slack" => Domain::Slack,
        "linear" => Domain::Linear,
        "github" => Domain::Github,
        "grain" => Domain::Grain,
        "google_calendar" => Domain::GoogleCalendar,
        other => Domain::Other(other.to_string()),
    }
}

pub(crate) fn parse_direction(raw: &str) -> Result<ArrowDirection> {
    match raw {
        "analysis_request" => Ok(ArrowDirection::AnalysisRequest),
        "synthesis_request" => Ok(ArrowDirection::SynthesisRequest),
        "elaboration_request" => Ok(ArrowDirection::ElaborationRequest),
        other => Err(anyhow!("unknown arrow direction `{}`", other)),
    }
}

pub(crate) fn parse_domains_csv(raw: Option<&str>) -> Option<Vec<Domain>> {
    raw.map(|csv| {
        csv.split(',')
            .map(|token| parse_domain_id(token.trim()))
            .collect()
    })
}

pub(crate) async fn load_events_for_domains(
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

pub(crate) async fn resolve_event_entities(
    events: &mut [EventEnvelope],
    store: Arc<Mutex<IdentityStore>>,
) {
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

pub(crate) async fn resolve_arrow_entities(arrows: &mut [Arrow], store: Arc<Mutex<IdentityStore>>) {
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

pub(crate) fn arrow_source_matches(arrow: &Arrow, domains: &[Domain]) -> bool {
    domains
        .iter()
        .any(|domain| endpoint_has_domain(&arrow.source, domain))
}

pub(crate) fn arrow_target_matches(arrow: &Arrow, domains: &[Domain]) -> bool {
    domains
        .iter()
        .any(|domain| endpoint_has_domain(&arrow.target, domain))
}

fn endpoint_has_domain(endpoint: &ArrowEndpoint, domain: &Domain) -> bool {
    match endpoint {
        ArrowEndpoint::Domain { domain: d } => d == domain,
        ArrowEndpoint::DomainObject { domain: d, .. } => d == domain,
    }
}

pub(crate) fn parse_identity_id(raw: &str) -> Result<IdentityId> {
    let id = Uuid::parse_str(raw).with_context(|| format!("invalid identity id `{}`", raw))?;
    Ok(IdentityId(id))
}

pub(crate) fn parse_persona_id(raw: &str) -> Result<PersonaId> {
    let id = Uuid::parse_str(raw).with_context(|| format!("invalid persona id `{}`", raw))?;
    Ok(PersonaId(id))
}
