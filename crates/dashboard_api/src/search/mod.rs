mod schema;
mod service;
mod tasks;
mod text;

pub use service::{SearchRequest, SearchResult, SearchService};
pub use tasks::{rebuild_full_index, spawn_periodic_reindex};
