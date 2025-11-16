use std::fs::{self, File, OpenOptions};
use std::io::{BufRead, BufReader, Write};
use std::path::PathBuf;

use anyhow::{Context, Result};

use core_model::arrow::Arrow;
use core_model::domain::Domain;
use core_model::time::TimeWindow;

const DEFAULT_FILE: &str = "arrows.jsonl";

pub struct ArrowStore {
    root: PathBuf,
    file_name: String,
}

impl ArrowStore {
    pub fn new(root: impl Into<PathBuf>) -> Self {
        Self {
            root: root.into(),
            file_name: DEFAULT_FILE.into(),
        }
    }

    pub fn with_file_name(mut self, name: impl Into<String>) -> Self {
        self.file_name = name.into();
        self
    }

    pub fn append(&self, arrows: &[Arrow]) -> Result<()> {
        if arrows.is_empty() {
            return Ok(());
        }
        fs::create_dir_all(&self.root).with_context(|| {
            format!(
                "failed to create arrow store directory {}",
                self.root.display()
            )
        })?;
        let path = self.file_path();
        let mut file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&path)
            .with_context(|| format!("failed to open arrow log {}", path.display()))?;

        for arrow in arrows {
            let line = serde_json::to_string(arrow)?;
            file.write_all(line.as_bytes())?;
            file.write_all(b"\n")?;
        }
        Ok(())
    }

    pub fn load_in_window(
        &self,
        window: &TimeWindow,
        filter_domains: Option<&[Domain]>,
    ) -> Result<Vec<Arrow>> {
        let path = self.file_path();
        if !path.exists() {
            return Ok(Vec::new());
        }
        let file =
            File::open(&path).with_context(|| format!("failed to read {}", path.display()))?;
        let reader = BufReader::new(file);
        let mut arrows = Vec::new();
        for (idx, line) in reader.lines().enumerate() {
            let line = line.with_context(|| {
                format!(
                    "failed to read arrow json line {} from {}",
                    idx + 1,
                    path.display()
                )
            })?;
            if line.trim().is_empty() {
                continue;
            }
            let arrow: Arrow = serde_json::from_str(&line).with_context(|| {
                format!(
                    "failed to parse arrow json on line {} in {}",
                    idx + 1,
                    path.display()
                )
            })?;
            if !window.contains(&arrow.created_at) {
                continue;
            }
            if let Some(domains) = filter_domains {
                if !arrow_matches_domains(&arrow, domains) {
                    continue;
                }
            }
            arrows.push(arrow);
        }
        arrows.sort_by(|a, b| b.created_at.cmp(&a.created_at));
        Ok(arrows)
    }

    fn file_path(&self) -> PathBuf {
        self.root.join(&self.file_name)
    }
}

fn arrow_matches_domains(arrow: &Arrow, domains: &[Domain]) -> bool {
    domains.iter().any(|domain| {
        endpoint_has_domain(&arrow.source, domain) || endpoint_has_domain(&arrow.target, domain)
    })
}

fn endpoint_has_domain(endpoint: &core_model::arrow::ArrowEndpoint, domain: &Domain) -> bool {
    match endpoint {
        core_model::arrow::ArrowEndpoint::Domain { domain: d } => d == domain,
        core_model::arrow::ArrowEndpoint::DomainObject { domain: d, .. } => d == domain,
    }
}
