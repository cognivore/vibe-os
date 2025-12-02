use axum::routing::{get, post};
use axum::Router;
use tower_http::cors::{Any, CorsLayer};

use crate::state::AppState;

mod arrows;
mod events;
mod identities;
mod linear;
mod operators;
mod search;
mod slack;

pub fn build_api_router() -> Router<AppState> {
    Router::new()
        .route("/domains", get(events::list_domains))
        .route("/events", get(events::list_events))
        .route("/providers/personas", get(events::list_provider_personas))
        .route("/operators", get(operators::list_operators))
        .route("/operators/run", post(operators::run_operator))
        .route("/arrows", get(arrows::list_arrows))
        .route("/search", get(search::execute_search))
        .route("/search/reindex", post(search::reindex))
        .route("/search/thread-titles", post(search::get_thread_titles))
        .route("/linear/issues/:issue_ref", get(linear::get_linear_issue))
        .route("/linear/snapshot", get(linear::list_linear_issues))
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
