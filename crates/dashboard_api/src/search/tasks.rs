use std::time::Duration;

use anyhow::Result;
use core_model::domain::Domain;
use tokio::time;
use tracing::{error, info};

use crate::state::AppState;
use crate::utils::{full_history_window, load_events_for_domains};

const REINDEX_INTERVAL_SECS: u64 = 300;

pub async fn rebuild_full_index(state: &AppState) -> Result<usize> {
    let window = full_history_window();
    let domains: Vec<Domain> = state.adapters.keys().cloned().collect();
    let events = load_events_for_domains(state, &domains, &window).await?;
    let count = events.len();
    state.search.replace_all(events).await?;
    Ok(count)
}

pub fn spawn_periodic_reindex(state: AppState) {
    tokio::spawn(async move {
        if let Err(err) = rebuild_full_index(&state).await {
            error!(error = %err, "initial search reindex failed");
        } else {
            info!("search index primed");
        }

        let mut tick = time::interval(Duration::from_secs(REINDEX_INTERVAL_SECS));
        loop {
            tick.tick().await;
            match rebuild_full_index(&state).await {
                Ok(count) => info!(count, "search index refreshed"),
                Err(err) => error!(error = %err, "periodic search reindex failed"),
            }
        }
    });
}
