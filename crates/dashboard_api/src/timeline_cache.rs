use std::collections::HashMap;
use std::sync::Arc;
use std::time::Instant;

use anyhow::{anyhow, Result};
use chrono::{Duration, Utc};
use core_model::domain::Domain;
use core_model::event::EventEnvelope;
use core_model::time::TimeWindow;
use core_persona::store::IdentityStore;
use tokio::sync::Mutex;
use tokio::task;

use crate::state::AdapterHandle;
use crate::utils::resolve_event_entities;

// Re-fetch recent events if cached data is older than this threshold
const CACHE_TTL_SECONDS: u64 = 60;
// Only force refresh for events in the last N hours
const REFRESH_RECENT_HOURS: i64 = 24;

#[derive(Clone)]
pub struct TimelineCache {
    adapters: Arc<HashMap<Domain, AdapterHandle>>,
    identity_store: Arc<Mutex<IdentityStore>>,
    state: Arc<Mutex<CacheState>>,
}

#[derive(Default)]
struct CacheState {
    per_domain: HashMap<Domain, DomainCache>,
    version: u64,
}

#[derive(Default)]
struct DomainCache {
    coverage: Option<TimeWindow>,
    entries: Vec<CachedEvent>,
    last_fetch: Option<Instant>,
}

#[derive(Clone)]
struct CachedEvent {
    event: EventEnvelope,
    version: u64,
}

pub struct TimelineSnapshot {
    pub cursor: u64,
    pub window: TimeWindow,
    pub events: Vec<EventEnvelope>,
}

pub struct TimelineDelta {
    pub cursor: u64,
    pub window: TimeWindow,
    pub added: Vec<EventEnvelope>,
    pub removed: Vec<String>,
}

impl TimelineCache {
    pub fn new(
        adapters: Arc<HashMap<Domain, AdapterHandle>>,
        identity_store: Arc<Mutex<IdentityStore>>,
    ) -> Self {
        Self {
            adapters,
            identity_store,
            state: Arc::new(Mutex::new(CacheState::default())),
        }
    }

    pub async fn ensure_window(&self, domains: &[Domain], window: &TimeWindow) -> Result<()> {
        for domain in domains {
            self.ensure_domain_window(domain, window).await?;
        }
        Ok(())
    }

    pub async fn snapshot(
        &self,
        domains: &[Domain],
        window: &TimeWindow,
    ) -> Result<TimelineSnapshot> {
        self.ensure_window(domains, window).await?;
        let state = self.state.lock().await;
        let events = state.collect_events(domains, window);
        Ok(TimelineSnapshot {
            cursor: state.version,
            window: window.clone(),
            events,
        })
    }

    pub async fn delta_since(
        &self,
        domains: &[Domain],
        window: &TimeWindow,
        cursor: u64,
    ) -> Result<TimelineDelta> {
        self.ensure_window(domains, window).await?;
        let state = self.state.lock().await;
        let added = state.collect_added(domains, window, cursor);
        Ok(TimelineDelta {
            cursor: state.version,
            window: window.clone(),
            added,
            removed: Vec::new(),
        })
    }

    async fn ensure_domain_window(&self, domain: &Domain, window: &TimeWindow) -> Result<()> {
        loop {
            let missing = {
                let mut state = self.state.lock().await;
                let cache = state.per_domain.entry(domain.clone()).or_default();
                cache.next_missing_segment(window)
            };

            let segment = match missing {
                Some(segment) => segment,
                None => break,
            };

            let mut events = self.fetch_domain_events(domain.clone(), &segment).await?;
            if !events.is_empty() {
                resolve_event_entities(&mut events, self.identity_store.clone()).await;
            }

            let mut state = self.state.lock().await;
            {
                let cache = state.per_domain.entry(domain.clone()).or_default();
                cache.extend_coverage(&segment);
            }
            if events.is_empty() {
                continue;
            }
            state.version += 1;
            let version = state.version;
            let cache = state.per_domain.entry(domain.clone()).or_default();
            cache.insert_events(events, version);
        }

        Ok(())
    }

    async fn fetch_domain_events(
        &self,
        domain: Domain,
        window: &TimeWindow,
    ) -> Result<Vec<EventEnvelope>> {
        let adapter = self
            .adapters
            .get(&domain)
            .ok_or_else(|| anyhow!("no adapter registered for domain {:?}", domain))?
            .clone();
        let window_clone = window.clone();
        let mut events = task::spawn_blocking(move || adapter.load_events(&window_clone))
            .await
            .map_err(|err| anyhow!("adapter task failed: {}", err))??;
        events.sort_by(|a, b| a.at.cmp(&b.at));
        Ok(events)
    }
}

impl CacheState {
    fn collect_events(&self, domains: &[Domain], window: &TimeWindow) -> Vec<EventEnvelope> {
        let mut combined: Vec<EventEnvelope> = domains
            .iter()
            .filter_map(|domain| self.per_domain.get(domain))
            .flat_map(|cache| {
                cache
                    .entries
                    .iter()
                    .filter(|entry| window.contains(&entry.event.at))
                    .map(|entry| entry.event.clone())
                    .collect::<Vec<_>>()
            })
            .collect();
        combined.sort_by(|a, b| b.at.cmp(&a.at));
        combined
    }

    fn collect_added(
        &self,
        domains: &[Domain],
        window: &TimeWindow,
        cursor: u64,
    ) -> Vec<EventEnvelope> {
        let mut added: Vec<EventEnvelope> = domains
            .iter()
            .filter_map(|domain| self.per_domain.get(domain))
            .flat_map(|cache| {
                cache
                    .entries
                    .iter()
                    .filter(|entry| entry.version > cursor && window.contains(&entry.event.at))
                    .map(|entry| entry.event.clone())
                    .collect::<Vec<_>>()
            })
            .collect();
        added.sort_by(|a, b| b.at.cmp(&a.at));
        added
    }
}

impl DomainCache {
    fn next_missing_segment(&mut self, requested: &TimeWindow) -> Option<TimeWindow> {
        match &self.coverage {
            None => Some(requested.clone()),
            Some(existing) => {
                // Check if cache is stale (older than TTL) and requested window includes recent time
                let should_refresh_recent = if let Some(last_fetch) = self.last_fetch {
                    last_fetch.elapsed().as_secs() > CACHE_TTL_SECONDS
                } else {
                    false
                };

                if should_refresh_recent {
                    let now = Utc::now();
                    let recent_threshold = now - Duration::hours(REFRESH_RECENT_HOURS);

                    // If the requested window includes recent time, force re-fetch of that portion
                    if requested.end >= recent_threshold {
                        let refresh_start = requested.start.max(recent_threshold);
                        if refresh_start < requested.end {
                            // Return recent period to force re-fetch
                            return Some(TimeWindow {
                                start: refresh_start,
                                end: requested.end,
                            });
                        }
                    }
                }

                missing_segment(existing, requested)
            }
        }
    }

    fn extend_coverage(&mut self, segment: &TimeWindow) {
        self.coverage = Some(match &self.coverage {
            Some(existing) => TimeWindow {
                start: existing.start.min(segment.start),
                end: existing.end.max(segment.end),
            },
            None => segment.clone(),
        });
        self.last_fetch = Some(Instant::now());
    }

    fn insert_events(&mut self, mut events: Vec<EventEnvelope>, version: u64) {
        events.sort_by(|a, b| a.at.cmp(&b.at));
        for event in events {
            if let Some(pos) = self
                .entries
                .iter()
                .position(|entry| entry.event.id == event.id)
            {
                self.entries.remove(pos);
            }
            self.entries.push(CachedEvent { event, version });
        }
    }
}

fn missing_segment(existing: &TimeWindow, requested: &TimeWindow) -> Option<TimeWindow> {
    if requested.start >= existing.start && requested.end <= existing.end {
        return None;
    }

    if requested.end <= existing.start {
        return Some(requested.clone());
    }

    if requested.start >= existing.end {
        return Some(requested.clone());
    }

    if requested.start < existing.start {
        let end = existing.start;
        if requested.start < end {
            return Some(TimeWindow {
                start: requested.start,
                end,
            });
        }
    }

    if requested.end > existing.end {
        let start = requested.start.max(existing.end);
        if start < requested.end {
            return Some(TimeWindow {
                start,
                end: requested.end,
            });
        }
    }

    None
}
