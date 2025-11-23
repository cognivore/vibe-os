use std::path::Path;

use anyhow::{Context, Result};
use chrono::Utc;

use crate::linear::LinearClient;

mod models;
mod queries;
mod storage;
mod transform;

pub use models::{
    LinearCommentRecord, LinearIssueEvent, LinearIssueSnapshot, LinearMirrorMeta, LinearUserNode,
    LinearUserSnapshot,
};

use queries::{IssuesQueryData, UsersQueryData, ISSUES_QUERY, USERS_QUERY};
use storage::{
    append_events_with_rotation, read_existing_comments, read_existing_event_ids,
    read_existing_issues, read_last_sync, store_user_snapshot, write_comments, write_issues,
    write_meta,
};
use transform::collect_new_events;

pub struct LinearSync<'a> {
    client: &'a LinearClient,
}

impl<'a> LinearSync<'a> {
    pub fn new(client: &'a LinearClient) -> Self {
        Self { client }
    }

    pub async fn full_sync(&self, output_dir: &Path) -> Result<()> {
        std::fs::create_dir_all(output_dir).with_context(|| {
            format!(
                "unable to create Linear mirror dir {}",
                output_dir.display()
            )
        })?;

        let issues_path = output_dir.join("issues.jsonl");
        let comments_path = output_dir.join("comments.jsonl");
        let meta_path = output_dir.join("meta.json");

        let workspace_result = async {
            let mut issues_by_id = read_existing_issues(&issues_path)?;
            let mut comments_by_issue = read_existing_comments(&comments_path).unwrap_or_default();

            let last_sync_at = read_last_sync(meta_path.as_path())?;

            let mut seen_event_ids = read_existing_event_ids(output_dir)?;
            let mut pending_events = Vec::new();
            let mut after: Option<String> = None;
            let mut workspace_name: Option<String> = None;

            loop {
                let data: IssuesQueryData = self
                    .client
                    .graphql_query(
                        ISSUES_QUERY,
                        serde_json::json!({
                            "after": after,
                            "updatedSince": last_sync_at
                        }),
                    )
                    .await
                    .context("failed to fetch issues page from Linear")?;

                if workspace_name.is_none() {
                    workspace_name = data
                        .viewer
                        .and_then(|viewer| viewer.organization.map(|org| org.name));
                }

                for node in data.issues.nodes.into_iter() {
                    let snapshot = LinearIssueSnapshot::from_node(&node);
                    issues_by_id.insert(snapshot.id.clone(), snapshot.clone());

                    pending_events.extend(collect_new_events(&node, &mut seen_event_ids));
                    for comment in LinearCommentRecord::from_issue_node(&node) {
                        let entry = comments_by_issue
                            .entry(comment.issue_id.clone())
                            .or_default();
                        if let Some(existing) =
                            entry.iter_mut().find(|existing| existing.id == comment.id)
                        {
                            *existing = comment;
                        } else {
                            entry.push(comment);
                        }
                    }
                }

                if !data.issues.page_info.has_next_page {
                    break;
                }

                after = data.issues.page_info.end_cursor;
            }

            write_issues(&issues_path, &issues_by_id)?;
            write_comments(&comments_path, &comments_by_issue)?;
            append_events_with_rotation(output_dir, &pending_events)?;

            let meta = LinearMirrorMeta {
                last_full_sync_at: Some(Utc::now()),
                workspace_name,
            };

            write_meta(&meta_path, &meta)?;
            Ok::<(), anyhow::Error>(())
        }
        .await;

        let user_result = self.sync_users(output_dir).await;

        match (workspace_result, user_result) {
            (Ok(()), Ok(())) => Ok(()),
            (Err(err), Ok(())) => Err(err),
            (Ok(()), Err(err)) => Err(err),
            (Err(sync_err), Err(user_err)) => {
                let user_msg = user_err.to_string();
                Err::<(), anyhow::Error>(sync_err).context(format!(
                    "Linear user directory sync also failed: {}",
                    user_msg
                ))
            }
        }
    }

    async fn sync_users(&self, output_dir: &Path) -> Result<()> {
        let users_dir = output_dir.join(storage::USERS_DIR);
        std::fs::create_dir_all(&users_dir).with_context(|| {
            format!("unable to create Linear users dir {}", users_dir.display())
        })?;

        let mut after: Option<String> = None;
        let mut processed = 0usize;
        let mut updated = 0usize;

        loop {
            let data: UsersQueryData = self
                .client
                .graphql_query(
                    USERS_QUERY,
                    serde_json::json!({
                        "after": after,
                    }),
                )
                .await
                .context("failed to fetch Linear users page")?;

            for user in data.users.nodes.into_iter() {
                processed += 1;
                if store_user_snapshot(&users_dir, user.clone())? {
                    updated += 1;
                }
            }

            if !data.users.page_info.has_next_page {
                break;
            }

            after = data.users.page_info.end_cursor;
        }

        println!(
            "Linear user directory synced ({} processed, {} snapshots updated)",
            processed, updated
        );

        Ok(())
    }
}
