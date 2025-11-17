use std::sync::Arc;

use anyhow::Result;
use arrow_store::ArrowStore;
use axum::http::StatusCode;
use axum::response::IntoResponse;
use axum::routing::get_service;
use axum::Router;
use core_operators::OperatorRegistry;
use core_persona::store::IdentityStore;
use tokio::sync::Mutex;
use tower_http::services::{ServeDir, ServeFile};
use tower_http::trace::{
    DefaultMakeSpan, DefaultOnFailure, DefaultOnRequest, DefaultOnResponse, TraceLayer,
};
use tracing::{info, Level};

mod error;
mod routes;
mod state;
mod utils;

pub use state::DashboardServerSettings;

use state::{AppState, MetaSnapshot};
use utils::build_adapters;

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

    let slack_token = settings.slack_token.map(|token| {
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
    };

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
