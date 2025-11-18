use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use anyhow::Result;
use arrow_store::ArrowStore;
use axum::http::StatusCode;
use axum::response::IntoResponse;
use axum::routing::get_service;
use axum::Router;
use core_operators::OperatorRegistry;
use core_persona::store::IdentityStore;
use tokio::sync::Mutex;
use tokio::time::sleep;
use tower_http::services::{ServeDir, ServeFile};
use tower_http::trace::{
    DefaultMakeSpan, DefaultOnFailure, DefaultOnRequest, DefaultOnResponse, TraceLayer,
};
use tracing::{error, info, Level};

mod error;
mod routes;
mod search;
mod state;
mod utils;

pub use state::DashboardServerSettings;

use search::{spawn_periodic_reindex, SearchService};
use state::{AppState, MetaSnapshot};
use utils::build_adapters;

pub async fn run_dashboard_server(settings: DashboardServerSettings) -> Result<()> {
    let adapters = build_adapters(&settings);
    let operator_registry = Arc::new(OperatorRegistry::new());
    let arrow_store = Arc::new(ArrowStore::new(&settings.arrow_store_dir));
    let identity_store = Arc::new(Mutex::new(IdentityStore::load(&settings.persona_root_dir)?));
    let search_service = SearchService::open(&settings.search_index_dir)?;
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

    let slack_token = settings.slack_token.clone().map(|token| {
        info!("Slack token detected; enabling on-demand Slack thread fetch");
        Arc::new(token)
    });

    let state = AppState {
        adapters: Arc::new(adapters),
        operator_registry,
        arrow_store,
        identity_store,
        meta: Arc::new(meta),
        slack_mirror_dir: Arc::new(settings.slack_mirror_dir.clone()),
        linear_mirror_dir: Arc::new(settings.linear_mirror_dir.clone()),
        slack_token,
        search: search_service.clone(),
    };

    spawn_periodic_reindex(state.clone());

    // Start background Slack sync task if token available
    if let Some(token) = settings.slack_token {
        let mirror_dir = settings.slack_mirror_dir.clone();
        tokio::spawn(async move {
            background_slack_sync(token, mirror_dir).await;
        });
    }

    let cors = routes::cors_layer();
    let trace_layer = TraceLayer::new_for_http()
        .make_span_with(DefaultMakeSpan::new().level(Level::DEBUG))
        .on_request(DefaultOnRequest::new().level(Level::DEBUG))
        .on_response(DefaultOnResponse::new().level(Level::DEBUG))
        .on_failure(DefaultOnFailure::new().level(Level::ERROR));

    let api_router = routes::build_api_router()
        .layer(cors)
        .layer(trace_layer.clone())
        .with_state(state.clone());

    let mut app = Router::new().nest("/api", api_router).layer(trace_layer);

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

async fn background_slack_sync(token: String, mirror_dir: PathBuf) {
    info!("Starting background Slack sync (every 60 seconds)");

    loop {
        sleep(Duration::from_secs(60)).await;

        info!("Running background Slack sync...");
        match run_slack_sync(&token, &mirror_dir).await {
            Ok(()) => info!("Background Slack sync completed successfully"),
            Err(e) => error!("Background Slack sync failed: {}", e),
        }
    }
}

async fn run_slack_sync(token: &str, mirror_dir: &Path) -> Result<()> {
    info!("Syncing Slack mirror at {}", mirror_dir.display());

    // Use the vibeos binary from target directory
    // This works because we're running from the workspace root
    use tokio::process::Command;
    let vibeos_path = std::env::current_dir()?.join("target/debug/vibeos");

    let status = Command::new(&vibeos_path)
        .arg("slack")
        .arg("mirror")
        .env("VIBEOS_SLACK_TOKEN", token)
        .env("VIBEOS_SLACK_MIRROR_DIR", mirror_dir.display().to_string())
        .status()
        .await?;

    if !status.success() {
        anyhow::bail!("Slack mirror sync exited with status: {}", status);
    }

    Ok(())
}
