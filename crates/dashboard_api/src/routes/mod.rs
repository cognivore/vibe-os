use axum::routing::{get, post};
use axum::Router;
use tower_http::cors::{Any, CorsLayer};

use crate::state::AppState;

mod arrows;
mod events;
mod identities;
mod operators;
mod slack;

pub fn build_api_router() -> Router<AppState> {
    Router::new()
        .route("/domains", get(events::list_domains))
        .route("/events", get(events::list_events))
        .route("/providers/personas", get(events::list_provider_personas))
        .route("/operators", get(operators::list_operators))
        .route("/operators/run", post(operators::run_operator))
        .route("/arrows", get(arrows::list_arrows))
        .route(
            "/slack/threads/:channel/:thread_ts",
            get(slack::get_slack_thread),
        )
        .route("/meta", get(events::fetch_meta))
        .nest("/identities", identities::router())
}

pub fn cors_layer() -> CorsLayer {
    CorsLayer::new()
        .allow_origin(Any)
        .allow_methods(Any)
        .allow_headers(Any)
}
