use std::path::Path;
use std::sync::Arc;

use anyhow::{Context, Result};
use core_model::event::EventEnvelope;
use core_model::time::TimeWindow;
use core_types::Domain;
use serde_json;
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
        let limit = request.limit.clamp(1, MAX_LIMIT);
        let offset = request.offset;
        let (top_docs, total) = searcher
            .search(
                query.as_ref(),
                &(TopDocs::with_limit(limit).and_offset(offset), Count),
            )
            .context("failed to run search query")?;

        let mut hits = Vec::with_capacity(top_docs.len());
        for (score, doc_address) in top_docs {
            let doc = searcher
                .doc(doc_address)
                .context("failed to load search hit document")?;
            let event = self.deserialize_event(&doc)?;
            hits.push(SearchHit { score, event });
        }

        Ok(SearchResult { total, hits })
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
