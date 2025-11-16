use crate::domain::Domain;
use crate::event::EventEnvelope;
use crate::time::TimeWindow;

pub trait EventAdapter: Send + Sync {
    fn domain(&self) -> Domain;

    /// Load events in the given window, ordered by time.
    fn load_events(&self, window: &TimeWindow) -> anyhow::Result<Vec<EventEnvelope>>;
}
