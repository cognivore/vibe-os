use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};

use crate::linear_sync::{LinearIssueEvent, LinearIssueSnapshot};

const LINEAR_EVENTS_PREFIX: &str = "events";
const LINEAR_EVENTS_EXTENSION: &str = "jsonl";

pub fn load_issues(mirror_dir: &Path) -> Result<Vec<LinearIssueSnapshot>> {
    let path = mirror_dir.join("issues.jsonl");
    let file = File::open(&path)
        .with_context(|| format!("Unable to open Linear issues mirror at {}", path.display()))?;
    let reader = BufReader::new(file);

    reader
        .lines()
        .enumerate()
        .filter_map(|(idx, line)| match line {
            Ok(content) if content.trim().is_empty() => None,
            Ok(content) => Some(
                serde_json::from_str::<LinearIssueSnapshot>(&content).with_context(|| {
                    format!(
                        "Failed to parse Linear issue on line {} in {}",
                        idx + 1,
                        path.display()
                    )
                }),
            ),
            Err(err) => Some(Err(err).with_context(|| {
                format!(
                    "Failed to read Linear issue line {} from {}",
                    idx + 1,
                    path.display()
                )
            })),
        })
        .collect()
}

pub fn load_events(mirror_dir: &Path) -> Result<Vec<LinearIssueEvent>> {
    let mut log_paths = linear_event_logs(mirror_dir)?;
    if log_paths.is_empty() {
        let legacy = mirror_dir.join(format!(
            "{}.{}",
            LINEAR_EVENTS_PREFIX, LINEAR_EVENTS_EXTENSION
        ));
        if legacy.exists() {
            log_paths.push(legacy);
        }
    }

    let mut events = Vec::new();
    for path in log_paths {
        let file = File::open(&path).with_context(|| {
            format!("Unable to open Linear events mirror at {}", path.display())
        })?;
        let reader = BufReader::new(file);

        for (idx, line) in reader.lines().enumerate() {
            let line = line.with_context(|| {
                format!(
                    "Failed to read Linear event line {} from {}",
                    idx + 1,
                    path.display()
                )
            })?;
            if line.trim().is_empty() {
                continue;
            }
            let event: LinearIssueEvent = serde_json::from_str(&line).with_context(|| {
                format!(
                    "Failed to parse Linear event on line {} in {}",
                    idx + 1,
                    path.display()
                )
            })?;
            events.push(event);
        }
    }

    Ok(events)
}

fn linear_event_logs(mirror_dir: &Path) -> Result<Vec<PathBuf>> {
    let mut logs: Vec<(u32, PathBuf)> = Vec::new();

    if mirror_dir.exists() {
        for entry in std::fs::read_dir(mirror_dir).with_context(|| {
            format!(
                "Unable to enumerate mirror directory {}",
                mirror_dir.display()
            )
        })? {
            let path = entry?.path();
            if let Some(file_name) = path.file_name().and_then(|s| s.to_str()) {
                if let Some(number) = linear_event_log_number(file_name) {
                    logs.push((number, path));
                }
            }
        }
    }

    logs.sort_by(|(left, _), (right, _)| right.cmp(left));
    Ok(logs.into_iter().map(|(_, path)| path).collect())
}

fn linear_event_log_number(name: &str) -> Option<u32> {
    let legacy = format!("{LINEAR_EVENTS_PREFIX}.{LINEAR_EVENTS_EXTENSION}");
    if name == legacy {
        return Some(0);
    }

    let prefix = format!("{LINEAR_EVENTS_PREFIX}.");
    let suffix = format!(".{LINEAR_EVENTS_EXTENSION}");
    if name.starts_with(&prefix) && name.ends_with(&suffix) {
        let number_part = &name[prefix.len()..name.len() - suffix.len()];
        return number_part.parse().ok();
    }
    None
}
