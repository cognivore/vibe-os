use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

use anyhow::{Context, Result};
use core_model::event::EventEnvelope;
use core_model::time::TimeWindow;
use core_types::Domain;
use serde_json::Value as JsonValue;
use tantivy::collector::{Count, TopDocs};
use tantivy::directory::MmapDirectory;
use tantivy::query::{AllQuery, BooleanQuery, Occur, Query, QueryParser, RangeQuery, TermQuery};
use tantivy::schema::{IndexRecordOption, Value};
use tantivy::{Index, IndexReader, IndexWriter, ReloadPolicy, TantivyDocument, Term};
use tokio::sync::Mutex;

use super::schema::SearchSchema;

const MAX_LIMIT: usize = 200;

#[derive(Clone)]
pub struct SearchService {
    inner: Arc<SearchInner>,
}

struct SearchInner {
    schema: SearchSchema,
    index: Index,
    reader: IndexReader,
    writer: Mutex<IndexWriter>,
}

#[derive(Debug, Clone)]
pub struct SearchRequest {
    pub query: String,
    pub limit: usize,
    pub offset: usize,
    pub domains: Vec<Domain>,
    pub window: Option<TimeWindow>,
}

impl Default for SearchRequest {
    fn default() -> Self {
        Self {
            query: String::new(),
            limit: 50,
            offset: 0,
            domains: Vec::new(),
            window: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SearchHit {
    #[allow(dead_code)]
    pub score: f32,
    pub event: EventEnvelope,
    pub thread_name: Option<String>,
}

#[derive(Debug, Clone)]
pub struct SearchResult {
    pub total: usize,
    pub hits: Vec<SearchHit>,
}

impl SearchService {
    pub fn open(index_path: impl AsRef<Path>) -> Result<Self> {
        std::fs::create_dir_all(index_path.as_ref()).with_context(|| {
            format!(
                "failed to create search index directory {}",
                index_path.as_ref().display()
            )
        })?;

        let schema = SearchSchema::new();
        let directory = MmapDirectory::open(index_path.as_ref()).with_context(|| {
            format!(
                "failed to open search index at {}",
                index_path.as_ref().display()
            )
        })?;
        let index = Index::open_or_create(directory, schema.schema())
            .context("failed to open or create search index")?;
        let reader = index
            .reader_builder()
            .reload_policy(ReloadPolicy::OnCommitWithDelay)
            .try_into()
            .context("failed to initialize search index reader")?;
        let writer = index
            .writer(50_000_000)
            .context("failed to initialize search index writer")?;

        Ok(Self {
            inner: Arc::new(SearchInner {
                schema,
                index,
                reader,
                writer: Mutex::new(writer),
            }),
        })
    }

    pub async fn replace_all(&self, events: Vec<EventEnvelope>) -> Result<()> {
        let mut writer = self.inner.writer.lock().await;
        writer
            .delete_all_documents()
            .context("failed to reset search index")?;
        self.add_documents(&mut writer, events)?;
        writer.commit().context("failed to commit search index")?;
        self.inner
            .reader
            .reload()
            .context("failed to reload search reader")?;
        Ok(())
    }

    #[allow(dead_code)]
    pub async fn add_events(&self, events: Vec<EventEnvelope>) -> Result<()> {
        if events.is_empty() {
            return Ok(());
        }
        let mut writer = self.inner.writer.lock().await;
        self.add_documents(&mut writer, events)?;
        writer.commit().context("failed to commit search index")?;
        self.inner
            .reader
            .reload()
            .context("failed to reload search reader")?;
        Ok(())
    }

    pub fn search(&self, request: SearchRequest) -> Result<SearchResult> {
        let query = self.build_query(&request)?;
        let searcher = self.inner.reader.searcher();
        // Fetch more results to properly sort by thread
        let fetch_limit = (request.limit + request.offset).clamp(1, MAX_LIMIT * 2);
        let (top_docs, total) = searcher
            .search(query.as_ref(), &(TopDocs::with_limit(fetch_limit), Count))
            .context("failed to run search query")?;

        let mut raw_hits = Vec::with_capacity(top_docs.len());
        for (score, doc_address) in top_docs {
            let doc = searcher
                .doc(doc_address)
                .context("failed to load search hit document")?;
            let event = self.deserialize_event(&doc)?;
            raw_hits.push(SearchHit {
                score,
                event,
                thread_name: None,
            });
        }

        // Annotate thread metadata and sort by thread recency
        let hits = sort_and_label_threads(raw_hits);

        // Apply pagination after thread sorting
        let start = request.offset.min(hits.len());
        let end = (request.offset + request.limit).min(hits.len());
        let paged_hits = hits[start..end].to_vec();

        Ok(SearchResult {
            total,
            hits: paged_hits,
        })
    }

    fn add_documents(&self, writer: &mut IndexWriter, events: Vec<EventEnvelope>) -> Result<()> {
        for event in events {
            let doc = self
                .inner
                .schema
                .document(&event)
                .context("failed to build search document")?;
            writer.add_document(doc)?;
        }
        Ok(())
    }

    fn build_query(&self, request: &SearchRequest) -> Result<Box<dyn Query>> {
        let mut parser =
            QueryParser::for_index(&self.inner.index, self.inner.schema.default_fields());
        parser.set_conjunction_by_default();

        let mut clauses = Vec::new();
        if request.query.trim().is_empty() {
            clauses.push((Occur::Must, Box::new(AllQuery) as Box<dyn Query>));
        } else {
            let text_query = parser
                .parse_query(&request.query)
                .context("failed to parse search query text")?;
            clauses.push((Occur::Must, text_query));
        }

        if !request.domains.is_empty() {
            let domain_terms: Vec<(Occur, Box<dyn Query>)> = request
                .domains
                .iter()
                .map(|domain| {
                    let token = SearchSchema::encode_domain(domain);
                    let term = Term::from_field_text(self.inner.schema.domain, token.as_str());
                    (
                        Occur::Should,
                        Box::new(TermQuery::new(term, IndexRecordOption::Basic)) as Box<dyn Query>,
                    )
                })
                .collect();
            if !domain_terms.is_empty() {
                clauses.push((Occur::Must, Box::new(BooleanQuery::new(domain_terms))));
            }
        }

        if let Some(window) = &request.window {
            let start = window.start.timestamp_millis();
            let end = window.end.timestamp_millis();
            let schema = self.inner.schema.schema();
            let field_name = schema.get_field_name(self.inner.schema.at);
            let range = RangeQuery::new_i64(field_name.to_string(), start..end);
            clauses.push((Occur::Must, Box::new(range)));
        }

        if clauses.is_empty() {
            clauses.push((Occur::Must, Box::new(AllQuery)));
        }

        Ok(Box::new(BooleanQuery::new(clauses)))
    }

    fn deserialize_event(&self, doc: &TantivyDocument) -> Result<EventEnvelope> {
        let raw_value = doc
            .get_first(self.inner.schema.raw_event)
            .context("search document missing raw event field")?;
        let bytes = raw_value
            .as_bytes()
            .context("search document raw event is not bytes")?
            .to_vec();
        serde_json::from_slice::<EventEnvelope>(&bytes)
            .context("failed to deserialize search hit payload")
    }
}

fn sort_and_label_threads(hits: Vec<SearchHit>) -> Vec<SearchHit> {
    if hits.is_empty() {
        return hits;
    }

    let mut thread_groups: HashMap<String, Vec<SearchHit>> = HashMap::new();

    for hit in hits {
        let thread_key = thread_identifier(&hit.event);
        thread_groups.entry(thread_key).or_default().push(hit);
    }

    let mut thread_latest: Vec<(String, chrono::DateTime<chrono::Utc>)> = thread_groups
        .iter()
        .map(|(thread_key, thread_hits)| {
            let latest = thread_hits
                .iter()
                .map(|hit| hit.event.at)
                .max()
                .unwrap_or(chrono::DateTime::<chrono::Utc>::MIN_UTC);
            (thread_key.clone(), latest)
        })
        .collect();

    thread_latest.sort_by(|a, b| b.1.cmp(&a.1));

    let mut sorted_hits = Vec::new();
    for (thread_key, _) in thread_latest {
        if let Some(mut thread_hits) = thread_groups.remove(&thread_key) {
            // Ensure chronological ordering to determine root message
            thread_hits.sort_by(|a, b| a.event.at.cmp(&b.event.at));
            let thread_name = compute_thread_name(&thread_hits);
            if let Some(name) = &thread_name {
                for hit in thread_hits.iter_mut() {
                    hit.thread_name = Some(name.clone());
                }
            }
            // Present hits newest-first within each thread
            thread_hits.sort_by(|a, b| b.event.at.cmp(&a.event.at));
            sorted_hits.extend(thread_hits);
        }
    }

    sorted_hits
}

fn thread_identifier(event: &EventEnvelope) -> String {
    event.entity_id.clone().unwrap_or_else(|| event.id.clone())
}

fn compute_thread_name(thread_hits: &[SearchHit]) -> Option<String> {
    let first = thread_hits.first()?;
    match first.event.domain {
        Domain::Slack if is_slack_thread(&first.event) => {
            // For Slack threads, entity_id format is "channel_id:thread_ts"
            // The thread_ts is the timestamp of the ROOT message
            let entity_id = first.event.entity_id.as_ref()?;
            let thread_ts = entity_id.split(':').nth(1)?;

            // Find the root message by matching ts field with thread_ts
            for hit in thread_hits.iter() {
                if let Some(text) = slack_root_message_text(&hit.event, thread_ts) {
                    return Some(text);
                }
            }

            // Fallback: use the earliest message's text
            thread_hits
                .iter()
                .min_by(|a, b| a.event.at.cmp(&b.event.at))
                .and_then(|hit| slack_message_text(&hit.event))
        }
        Domain::Linear => {
            let title = thread_hits
                .iter()
                .find_map(|hit| linear_issue_label(&hit.event));
            if let Some(label) = title {
                Some(label)
            } else {
                thread_hits
                    .iter()
                    .min_by(|a, b| a.event.at.cmp(&b.event.at))
                    .map(|hit| hit.event.summary.clone())
            }
        }
        _ => None,
    }
}

fn is_slack_thread(event: &EventEnvelope) -> bool {
    matches!(event.domain, Domain::Slack)
        && event
            .entity_id
            .as_ref()
            .map(|id| id.contains(':'))
            .unwrap_or(false)
}

fn slack_root_message_text(event: &EventEnvelope, thread_ts: &str) -> Option<String> {
    // Check if this event's ts matches the thread_ts (making it the root message)
    match &event.data {
        JsonValue::Object(map) => {
            let ts = map.get("ts")?.as_str()?;
            // Normalize comparison - thread_ts might have underscores instead of dots
            let normalized_thread_ts = thread_ts.replace('_', ".");
            if ts == normalized_thread_ts || ts.replace('.', "_") == thread_ts {
                // This is the root message, extract its text
                map.get("text")
                    .and_then(|value| value.as_str())
                    .map(|s| s.trim())
                    .filter(|s| !s.is_empty())
                    .map(|s| s.to_string())
            } else {
                None
            }
        }
        _ => None,
    }
}

fn slack_message_text(event: &EventEnvelope) -> Option<String> {
    match &event.data {
        JsonValue::Object(map) => map
            .get("text")
            .and_then(|value| value.as_str())
            .map(|s| s.trim())
            .filter(|s| !s.is_empty())
            .map(|s| s.to_string()),
        _ => None,
    }
    .or_else(|| {
        let summary = event.summary.trim();
        if summary.is_empty() {
            None
        } else {
            Some(summary.to_string())
        }
    })
}

fn linear_issue_label(event: &EventEnvelope) -> Option<String> {
    match &event.data {
        JsonValue::Object(map) => {
            if let Some(JsonValue::String(title)) =
                map.get("issue_title").filter(|value| value.is_string())
            {
                let trimmed = title.trim();
                if !trimmed.is_empty() {
                    return Some(trimmed.to_string());
                }
            }
            if let Some(JsonValue::String(identifier)) = map
                .get("issue_identifier")
                .filter(|value| value.is_string())
            {
                let trimmed = identifier.trim();
                if !trimmed.is_empty() {
                    return Some(trimmed.to_string());
                }
            }
            None
        }
        _ => None,
    }
}
