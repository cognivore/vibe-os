use std::collections::HashMap;
use std::net::SocketAddr;
use std::path::PathBuf;
use std::sync::Arc;

use arrow_store::ArrowStore;
use core_model::adapters::EventAdapter;
use core_model::domain::Domain;
use core_operators::OperatorRegistry;
use core_persona::store::IdentityStore;
use serde::Serialize;
use tokio::sync::Mutex;

pub type AdapterHandle = Arc<dyn EventAdapter>;

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
}
