use axum::extract::State;
use axum::http::StatusCode;
use axum::Json;
use serde::{Deserialize, Serialize};

use crate::error::AppError;
use crate::state::AppState;

#[derive(Serialize)]
pub struct SyncStatusResponse {
    pub slack: DomainSyncStatus,
    pub linear: DomainSyncStatus,
    pub any_in_progress: bool,
}

#[derive(Serialize)]
pub struct DomainSyncStatus {
    pub last_sync: Option<String>,
    pub in_progress: bool,
    pub enabled: bool,
}

/// GET /api/sync/status - Returns the current sync status for all domains
pub async fn get_sync_status(State(state): State<AppState>) -> Json<SyncStatusResponse> {
    let sync_state = state.sync_state.lock().await;

    let slack = DomainSyncStatus {
        last_sync: sync_state.last_slack_sync.map(|dt| dt.to_rfc3339()),
        in_progress: sync_state.is_slack_sync_in_progress(),
        enabled: state.slack_token.is_some(),
    };

    let linear = DomainSyncStatus {
        last_sync: sync_state.last_linear_sync.map(|dt| dt.to_rfc3339()),
        in_progress: sync_state.is_linear_sync_in_progress(),
        enabled: state.linear_api_key.is_some(),
    };

    let any_in_progress = sync_state.is_any_sync_in_progress();

    Json(SyncStatusResponse {
        slack,
        linear,
        any_in_progress,
    })
}

#[derive(Deserialize)]
pub struct TriggerSyncRequest {
    /// Which domains to sync: "slack", "linear", or "all" (default)
    #[serde(default = "default_domains")]
    pub domains: String,
}

fn default_domains() -> String {
    "all".to_string()
}

#[derive(Serialize)]
pub struct TriggerSyncResponse {
    pub message: String,
    pub triggered: Vec<String>,
}

/// POST /api/sync/trigger - Manually trigger a sync for specified domains
/// Rate limited to once per minute
pub async fn trigger_sync(
    State(state): State<AppState>,
    Json(request): Json<TriggerSyncRequest>,
) -> Result<(StatusCode, Json<TriggerSyncResponse>), AppError> {
    let mut sync_state = state.sync_state.lock().await;

    // Check rate limit
    if !sync_state.can_manual_trigger() {
        return Err(AppError(anyhow::anyhow!(
            "Rate limited: manual sync can only be triggered once per minute"
        )));
    }

    // Check if sync is already in progress
    if sync_state.is_any_sync_in_progress() {
        return Err(AppError(anyhow::anyhow!(
            "A sync is already in progress"
        )));
    }

    let mut triggered = Vec::new();
    let domains = request.domains.to_lowercase();

    // Record the manual trigger for rate limiting
    sync_state.record_manual_trigger();

    // Drop the lock before spawning tasks
    drop(sync_state);

    // Trigger Slack sync if requested and enabled
    if (domains == "all" || domains == "slack") && state.slack_token.is_some() {
        triggered.push("slack".to_string());
        let state_clone = state.clone();
        tokio::spawn(async move {
            trigger_slack_sync(state_clone).await;
        });
    }

    // Trigger Linear sync if requested and enabled
    if (domains == "all" || domains == "linear") && state.linear_api_key.is_some() {
        triggered.push("linear".to_string());
        let state_clone = state.clone();
        tokio::spawn(async move {
            trigger_linear_sync(state_clone).await;
        });
    }

    if triggered.is_empty() {
        return Err(AppError(anyhow::anyhow!(
            "No domains available to sync. Check that VIBEOS_SLACK_TOKEN and/or VIBEOS_LINEAR_API_KEY are set."
        )));
    }

    Ok((
        StatusCode::ACCEPTED,
        Json(TriggerSyncResponse {
            message: format!("Sync triggered for: {}", triggered.join(", ")),
            triggered,
        }),
    ))
}

async fn trigger_slack_sync(state: AppState) {
    use crate::search;
    use tracing::{error, info};

    let token = match &state.slack_token {
        Some(t) => t.as_ref().clone(),
        None => return,
    };
    let mirror_dir = state.slack_mirror_dir.as_ref().clone();

    info!("Manual Slack sync triggered");

    // Mark sync as in progress
    {
        let sync_state = state.sync_state.lock().await;
        sync_state.set_slack_sync_started();
    }

    let result = crate::run_slack_sync(&token, &mirror_dir).await;

    // Mark sync as completed
    {
        let mut sync_state = state.sync_state.lock().await;
        sync_state.set_slack_sync_completed();
    }

    match result {
        Ok(()) => {
            info!("Manual Slack sync completed successfully");
            state.timeline_cache.invalidate_recent().await;
            if let Err(e) = search::rebuild_full_index(&state).await {
                error!("Failed to rebuild search index after manual Slack sync: {}", e);
            }
        }
        Err(e) => error!("Manual Slack sync failed: {}", e),
    }
}

async fn trigger_linear_sync(state: AppState) {
    use crate::search;
    use tracing::{error, info};

    let api_key = match &state.linear_api_key {
        Some(k) => k.as_ref().clone(),
        None => return,
    };
    let mirror_dir = state.linear_mirror_dir.as_ref().clone();

    info!("Manual Linear sync triggered");

    // Mark sync as in progress
    {
        let sync_state = state.sync_state.lock().await;
        sync_state.set_linear_sync_started();
    }

    let result = crate::run_linear_sync(&api_key, &mirror_dir).await;

    // Mark sync as completed
    {
        let mut sync_state = state.sync_state.lock().await;
        sync_state.set_linear_sync_completed();
    }

    match result {
        Ok(()) => {
            info!("Manual Linear sync completed successfully");
            state.timeline_cache.invalidate_recent().await;
            if let Err(e) = search::rebuild_full_index(&state).await {
                error!("Failed to rebuild search index after manual Linear sync: {}", e);
            }
        }
        Err(e) => error!("Manual Linear sync failed: {}", e),
    }
}
