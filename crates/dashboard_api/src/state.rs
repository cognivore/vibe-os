use std::collections::HashMap;
use std::net::SocketAddr;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Instant;

use crate::search::SearchService;
use crate::timeline_cache::TimelineCache;
use arrow_store::ArrowStore;
use chrono::{DateTime, Utc};
use core_model::adapters::EventAdapter;
use core_model::domain::Domain;
use core_operators::OperatorRegistry;
use core_persona::store::IdentityStore;
use serde::Serialize;
use tokio::sync::Mutex;

pub type AdapterHandle = Arc<dyn EventAdapter>;

/// Tracks the status of background sync operations
#[derive(Default)]
pub struct SyncState {
    pub last_slack_sync: Option<DateTime<Utc>>,
    pub last_linear_sync: Option<DateTime<Utc>>,
    pub slack_sync_in_progress: AtomicBool,
    pub linear_sync_in_progress: AtomicBool,
    pub last_manual_trigger: Option<Instant>,
}

impl SyncState {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_slack_sync_started(&self) {
        self.slack_sync_in_progress.store(true, Ordering::SeqCst);
    }

    pub fn set_slack_sync_completed(&mut self) {
        self.slack_sync_in_progress.store(false, Ordering::SeqCst);
        self.last_slack_sync = Some(Utc::now());
    }

    pub fn set_linear_sync_started(&self) {
        self.linear_sync_in_progress.store(true, Ordering::SeqCst);
    }

    pub fn set_linear_sync_completed(&mut self) {
        self.linear_sync_in_progress.store(false, Ordering::SeqCst);
        self.last_linear_sync = Some(Utc::now());
    }

    pub fn is_slack_sync_in_progress(&self) -> bool {
        self.slack_sync_in_progress.load(Ordering::SeqCst)
    }

    pub fn is_linear_sync_in_progress(&self) -> bool {
        self.linear_sync_in_progress.load(Ordering::SeqCst)
    }

    pub fn is_any_sync_in_progress(&self) -> bool {
        self.is_slack_sync_in_progress() || self.is_linear_sync_in_progress()
    }

    /// Check if manual trigger is rate-limited (max once per minute)
    pub fn can_manual_trigger(&self) -> bool {
        match self.last_manual_trigger {
            Some(last) => last.elapsed().as_secs() >= 60,
            None => true,
        }
    }

    pub fn record_manual_trigger(&mut self) {
        self.last_manual_trigger = Some(Instant::now());
    }
}

#[derive(Clone)]
pub struct AppState {
    pub(crate) adapters: Arc<HashMap<Domain, AdapterHandle>>,
    pub(crate) operator_registry: Arc<OperatorRegistry>,
    pub(crate) arrow_store: Arc<ArrowStore>,
    pub(crate) identity_store: Arc<Mutex<IdentityStore>>,
    pub(crate) meta: Arc<MetaSnapshot>,
    pub(crate) slack_mirror_dir: Arc<PathBuf>,
    pub(crate) linear_mirror_dir: Arc<PathBuf>,
    pub(crate) slack_token: Option<Arc<String>>,
    pub(crate) linear_api_key: Option<Arc<String>>,
    pub(crate) search: SearchService,
    pub(crate) timeline_cache: Arc<TimelineCache>,
    pub(crate) sync_state: Arc<Mutex<SyncState>>,
}

#[derive(Clone, Serialize)]
pub struct MetaSnapshot {
    pub slack_mirror_dir: String,
    pub linear_mirror_dir: String,
    pub arrow_store_dir: String,
    pub identity_store_dir: String,
    pub static_dir: Option<String>,
}

pub struct DashboardServerSettings {
    pub slack_mirror_dir: PathBuf,
    pub linear_mirror_dir: PathBuf,
    pub arrow_store_dir: PathBuf,
    pub persona_root_dir: PathBuf,
    pub bind: SocketAddr,
    pub static_dir: Option<PathBuf>,
    pub slack_token: Option<String>,
    pub linear_api_key: Option<String>,
    pub search_index_dir: PathBuf,
}
