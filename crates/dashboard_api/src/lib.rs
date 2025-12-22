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
use tokio::time::{interval, sleep};
use tower_http::services::{ServeDir, ServeFile};
use tower_http::trace::{
    DefaultMakeSpan, DefaultOnFailure, DefaultOnRequest, DefaultOnResponse, TraceLayer,
};
use tracing::{error, info, Level};

mod error;
mod routes;
mod search;
mod state;
mod timeline_cache;
mod utils;

pub use state::DashboardServerSettings;

use search::{spawn_periodic_reindex, SearchService};
use state::{AppState, MetaSnapshot, SyncState};
use timeline_cache::TimelineCache;
use utils::{build_adapters, default_timeline_window};

pub async fn run_dashboard_server(settings: DashboardServerSettings) -> Result<()> {
    let adapters = Arc::new(build_adapters(&settings));
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
    let linear_api_key = settings.linear_api_key.clone().map(|key| {
        info!("Linear API key detected; enabling Linear background sync");
        Arc::new(key)
    });

    let timeline_cache = Arc::new(TimelineCache::new(adapters.clone(), identity_store.clone()));
    let sync_state = Arc::new(Mutex::new(SyncState::new()));

    let state = AppState {
        adapters: adapters.clone(),
        operator_registry,
        arrow_store,
        identity_store,
        meta: Arc::new(meta),
        slack_mirror_dir: Arc::new(settings.slack_mirror_dir.clone()),
        linear_mirror_dir: Arc::new(settings.linear_mirror_dir.clone()),
        slack_token,
        linear_api_key,
        search: search_service.clone(),
        timeline_cache: timeline_cache.clone(),
        sync_state,
    };

    spawn_periodic_reindex(state.clone());

    let warm_domains: Vec<_> = adapters.keys().cloned().collect();
    let warm_window = default_timeline_window();
    if let Err(err) = timeline_cache
        .ensure_window(&warm_domains, &warm_window)
        .await
    {
        error!("Failed to warm timeline cache: {}", err);
    }

    {
        let refresh_cache = timeline_cache.clone();
        let refresh_domains = warm_domains.clone();
        tokio::spawn(async move {
            let mut ticker = interval(Duration::from_secs(60));
            loop {
                ticker.tick().await;
                let latest_window = default_timeline_window();
                if let Err(err) = refresh_cache
                    .ensure_window(&refresh_domains, &latest_window)
                    .await
                {
                    error!("Failed to refresh timeline cache: {}", err);
                }
            }
        });
    }

    // Start background Slack sync task if token available
    if let Some(token) = settings.slack_token {
        let mirror_dir = settings.slack_mirror_dir.clone();
        let sync_state = state.clone();
        tokio::spawn(async move {
            background_slack_sync(token, mirror_dir, sync_state).await;
        });
    }

    // Start background Linear sync task if API key available
    if let Some(api_key) = settings.linear_api_key {
        let mirror_dir = settings.linear_mirror_dir.clone();
        let sync_state = state.clone();
        tokio::spawn(async move {
            background_linear_sync(api_key, mirror_dir, sync_state).await;
        });
    }

    let cors = routes::cors_layer();
    let trace_layer = TraceLayer::new_for_http()
        .make_span_with(DefaultMakeSpan::new().level(Level::DEBUG))
        .on_request(DefaultOnRequest::new().level(Level::DEBUG))
        .on_response(DefaultOnResponse::new().level(Level::DEBUG))
        .on_failure(DefaultOnFailure::new().level(Level::ERROR));

    let api_router = routes::build_api_router()
        .layer(trace_layer.clone())
        .with_state(state.clone());

    // Apply CORS to the entire app so error responses (including 404) include CORS headers
    let mut app = Router::new()
        .nest("/api", api_router)
        .layer(cors)
        .layer(trace_layer);

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

async fn background_slack_sync(token: String, mirror_dir: PathBuf, state: AppState) {
    // Run every 5 minutes to keep data fresh while respecting Slack rate limits
    const SLACK_SYNC_INTERVAL_SECS: u64 = 5 * 60;
    info!(
        "Starting background Slack sync (every {} minutes, immediate first sync)",
        SLACK_SYNC_INTERVAL_SECS / 60
    );

    loop {
        // Run sync first, then sleep (ensures immediate sync on startup)
        info!("Running background Slack sync...");

        // Mark sync as in progress
        {
            let sync_state = state.sync_state.lock().await;
            sync_state.set_slack_sync_started();
        }

        let result = run_slack_sync(&token, &mirror_dir).await;

        // Mark sync as completed and update timestamp
        {
            let mut sync_state = state.sync_state.lock().await;
            sync_state.set_slack_sync_completed();
        }

        match result {
            Ok(()) => {
                info!("Background Slack sync completed successfully");
                // Invalidate timeline cache for recent window to pick up new data
                state.timeline_cache.invalidate_recent().await;
                // Trigger immediate reindex after successful sync
                match search::rebuild_full_index(&state).await {
                    Ok(count) => info!(count, "Search index rebuilt after Slack sync"),
                    Err(e) => error!("Failed to rebuild search index after Slack sync: {}", e),
                }
            }
            Err(e) => error!("Background Slack sync failed: {}", e),
        }

        sleep(Duration::from_secs(SLACK_SYNC_INTERVAL_SECS)).await;
    }
}

pub async fn run_slack_sync(token: &str, mirror_dir: &Path) -> Result<()> {
    info!("Syncing Slack mirror at {}", mirror_dir.display());

    // Use the vibeos binary from target directory
    // This works because we're running from the workspace root
    use tokio::process::Command;
    let vibeos_path = std::env::current_dir()?.join("target/debug/vibeos");

    let output = Command::new(&vibeos_path)
        .arg("slack")
        .arg("mirror")
        .env("VIBEOS_SLACK_TOKEN", token)
        .env("VIBEOS_SLACK_MIRROR_DIR", mirror_dir.display().to_string())
        .output()
        .await?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        error!("Slack sync stderr: {}", stderr);
        error!("Slack sync stdout (last 500 chars): {}", stdout.chars().rev().take(500).collect::<String>().chars().rev().collect::<String>());
        anyhow::bail!("Slack mirror sync exited with status: {}", output.status);
    }

    Ok(())
}

async fn background_linear_sync(api_key: String, mirror_dir: PathBuf, state: AppState) {
    const LINEAR_SYNC_INTERVAL_SECS: u64 = 60;
    info!(
        "Starting background Linear sync (every {} seconds, immediate first sync)",
        LINEAR_SYNC_INTERVAL_SECS
    );

    loop {
        // Run sync first, then sleep (ensures immediate sync on startup)
        info!("Running background Linear sync...");

        // Mark sync as in progress
        {
            let sync_state = state.sync_state.lock().await;
            sync_state.set_linear_sync_started();
        }

        let result = run_linear_sync(&api_key, &mirror_dir).await;

        // Mark sync as completed and update timestamp
        {
            let mut sync_state = state.sync_state.lock().await;
            sync_state.set_linear_sync_completed();
        }

        match result {
            Ok(()) => {
                info!("Background Linear sync completed successfully");
                // Invalidate timeline cache for recent window to pick up new data
                state.timeline_cache.invalidate_recent().await;
                // Trigger immediate reindex after successful sync
                match search::rebuild_full_index(&state).await {
                    Ok(count) => info!(count, "Search index rebuilt after Linear sync"),
                    Err(e) => error!("Failed to rebuild search index after Linear sync: {}", e),
                }
            }
            Err(e) => error!("Background Linear sync failed: {}", e),
        }

        sleep(Duration::from_secs(LINEAR_SYNC_INTERVAL_SECS)).await;
    }
}

pub async fn run_linear_sync(api_key: &str, mirror_dir: &Path) -> Result<()> {
    info!("Syncing Linear mirror at {}", mirror_dir.display());

    use tokio::process::Command;
    let vibeos_path = std::env::current_dir()?.join("target/debug/vibeos");

    let mirror_str = mirror_dir.display().to_string();
    let status = Command::new(&vibeos_path)
        .arg("linear")
        .arg("sync")
        .arg("--output-dir")
        .arg(&mirror_str)
        .env("VIBEOS_LINEAR_API_KEY", api_key)
        .env("LINEAR_MIRROR_DIR", &mirror_str)
        .env("VIBEOS_LINEAR_MIRROR_DIR", &mirror_str)
        .status()
        .await?;

    if !status.success() {
        anyhow::bail!("Linear mirror sync exited with status: {}", status);
    }

    Ok(())
}
