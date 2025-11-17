use std::fs;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};

use crate::config;

pub fn resolve_output_dir(arg: Option<PathBuf>) -> Result<PathBuf> {
    arg.map_or_else(config::mirror_dir, Ok)
}

pub fn ensure_directory(dir: &Path) -> Result<()> {
    fs::create_dir_all(dir).with_context(|| format!("Failed to create output directory {:?}", dir))
}
