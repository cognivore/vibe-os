use std::time::Duration;

use anyhow::{Context, Result};
use serde::{de::DeserializeOwned, Deserialize, Serialize};

const LINEAR_API_URL: &str = "https://api.linear.app/graphql";
/// Default timeout for Linear API requests (60 seconds)
const REQUEST_TIMEOUT_SECS: u64 = 60;
/// Default timeout for establishing connections (30 seconds)
const CONNECT_TIMEOUT_SECS: u64 = 30;
const ISSUE_CREATE_MUTATION: &str = r#"
    mutation IssueCreate($input: IssueCreateInput!) {
        issueCreate(input: $input) {
            success
            issue {
                id
                identifier
                title
                url
            }
        }
    }
"#;

const ISSUE_RELATION_CREATE_MUTATION: &str = r#"
    mutation IssueRelationCreate($input: IssueRelationCreateInput!) {
        issueRelationCreate(input: $input) {
            success
            issueRelation {
                id
                type
            }
        }
    }
"#;

#[derive(Debug, Deserialize, Serialize)]
pub struct LinearIssue {
    pub id: String,
    pub identifier: String,
    pub title: String,
    pub url: Option<String>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct LinearComment {
    pub id: String,
    pub body: String,
    pub url: Option<String>,
}

#[derive(Debug, Serialize)]
struct GraphQLRequest {
    query: &'static str,
    variables: serde_json::Value,
}

#[derive(Debug, Deserialize)]
struct GraphQLResponse<T> {
    data: Option<T>,
    errors: Option<Vec<GraphQLError>>,
}

#[derive(Debug, Deserialize)]
struct GraphQLError {
    message: String,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct IssueCreateResponse {
    issue_create: IssueCreateResult,
}

#[derive(Debug, Deserialize)]
struct IssueCreateResult {
    success: bool,
    issue: Option<LinearIssueData>,
}

#[derive(Debug, Deserialize)]
struct LinearIssueData {
    id: String,
    identifier: String,
    title: String,
    url: Option<String>,
}

pub struct LinearClient {
    http: reqwest::Client,
    api_key: String,
}

impl LinearClient {
    pub fn new(api_key: String) -> Self {
        let http = reqwest::Client::builder()
            .timeout(Duration::from_secs(REQUEST_TIMEOUT_SECS))
            .connect_timeout(Duration::from_secs(CONNECT_TIMEOUT_SECS))
            .build()
            .expect("Failed to build HTTP client");

        Self { http, api_key }
    }

    pub async fn update_issue(
        &self,
        issue_id: &str,
        title: Option<&str>,
        description: Option<&str>,
    ) -> Result<LinearIssue> {
        let mut input = serde_json::json!({});

        if let Some(t) = title {
            input["title"] = serde_json::json!(t);
        }

        if let Some(d) = description {
            input["description"] = serde_json::json!(d);
        }

        const QUERY: &str = r#"mutation IssueUpdate($id: String!, $input: IssueUpdateInput!) {
            issueUpdate(id: $id, input: $input) {
                issue {
                    id
                    identifier
                    title
                    url
                }
            }
        }"#;

        let data: serde_json::Value = self
            .graphql_query(QUERY, serde_json::json!({ "id": issue_id, "input": input }))
            .await?;

        let issue = data["issueUpdate"]["issue"]
            .as_object()
            .context("Missing issue in response")?;

        Ok(LinearIssue {
            id: issue["id"].as_str().unwrap_or_default().to_string(),
            identifier: issue["identifier"].as_str().unwrap_or_default().to_string(),
            title: issue["title"].as_str().unwrap_or_default().to_string(),
            url: issue["url"].as_str().map(|s| s.to_string()),
        })
    }

    /// Update issue state, priority, or other mutable fields
    /// state_id: The ID of the workflow state to move the issue to
    /// priority: 0 = No priority, 1 = Urgent, 2 = High, 3 = Normal, 4 = Low
    /// assignee_id: The ID of the user to assign the issue to (or None to unassign)
    pub async fn edit_issue(
        &self,
        issue_id: &str,
        state_id: Option<&str>,
        priority: Option<i32>,
        assignee_id: Option<&str>,
    ) -> Result<LinearIssue> {
        let mut input = serde_json::json!({});

        if let Some(s) = state_id {
            input["stateId"] = serde_json::json!(s);
        }

        if let Some(p) = priority {
            input["priority"] = serde_json::json!(p);
        }

        if let Some(a) = assignee_id {
            input["assigneeId"] = serde_json::json!(a);
        }

        const QUERY: &str = r#"mutation IssueUpdate($id: String!, $input: IssueUpdateInput!) {
            issueUpdate(id: $id, input: $input) {
                issue {
                    id
                    identifier
                    title
                    url
                    state { name }
                    priority
                    assignee { name }
                }
            }
        }"#;

        let data: serde_json::Value = self
            .graphql_query(QUERY, serde_json::json!({ "id": issue_id, "input": input }))
            .await?;

        let issue = data["issueUpdate"]["issue"]
            .as_object()
            .context("Missing issue in response")?;

        Ok(LinearIssue {
            id: issue["id"].as_str().unwrap_or_default().to_string(),
            identifier: issue["identifier"].as_str().unwrap_or_default().to_string(),
            title: issue["title"].as_str().unwrap_or_default().to_string(),
            url: issue["url"].as_str().map(|s| s.to_string()),
        })
    }

    /// Create an issue relation (dependency)
    /// When `issue_id` depends on `related_issue_id`, create a "blocked_by" relation
    pub async fn create_issue_relation(
        &self,
        issue_id: &str,
        related_issue_id: &str,
    ) -> Result<()> {
        self.create_issue_relation_typed(issue_id, related_issue_id, "blocks")
            .await
    }

    /// Create an issue relation with a specific type
    ///
    /// Supported relation types:
    /// - "blocks" - issue_id blocks related_issue_id
    /// - "duplicate" - issue_id is a duplicate of related_issue_id
    /// - "related" - issue_id is related to related_issue_id (bidirectional)
    pub async fn create_issue_relation_typed(
        &self,
        issue_id: &str,
        related_issue_id: &str,
        relation_type: &str,
    ) -> Result<()> {
        let input = serde_json::json!({
            "issueId": issue_id,
            "relatedIssueId": related_issue_id,
            "type": relation_type
        });

        let data: serde_json::Value = self
            .graphql_query(
                ISSUE_RELATION_CREATE_MUTATION,
                serde_json::json!({ "input": input }),
            )
            .await?;

        let success = data["issueRelationCreate"]["success"]
            .as_bool()
            .unwrap_or(false);

        if !success {
            anyhow::bail!(
                "issueRelationCreate returned success=false for type '{}'",
                relation_type
            );
        }

        Ok(())
    }

    pub async fn create_issue(
        &self,
        team_id: &str,
        title: &str,
        description: &str,
        priority: Option<i32>,
        assignee_id: Option<&str>,
        cycle_id: Option<&str>,
    ) -> Result<LinearIssue> {
        let mut input = serde_json::json!({
            "teamId": team_id,
            "title": title,
            "description": description,
        });

        if let Some(prio) = priority {
            input["priority"] = serde_json::json!(prio);
        }

        if let Some(assignee) = assignee_id {
            input["assigneeId"] = serde_json::json!(assignee);
        }

        if let Some(cycle) = cycle_id {
            input["cycleId"] = serde_json::json!(cycle);
        }

        let data: IssueCreateResponse = self
            .graphql_query(
                ISSUE_CREATE_MUTATION,
                serde_json::json!({
                    "input": input
                }),
            )
            .await?;

        if !data.issue_create.success {
            anyhow::bail!("issueCreate returned success=false");
        }

        let issue_data = data
            .issue_create
            .issue
            .context("issueCreate did not return an issue")?;

        Ok(LinearIssue {
            id: issue_data.id,
            identifier: issue_data.identifier,
            title: issue_data.title,
            url: issue_data.url,
        })
    }

    /// Create a comment on an issue
    pub async fn create_comment(&self, issue_id: &str, body: &str) -> Result<LinearComment> {
        const QUERY: &str = r#"mutation CommentCreate($input: CommentCreateInput!) {
            commentCreate(input: $input) {
                success
                comment {
                    id
                    body
                    url
                }
            }
        }"#;

        let input = serde_json::json!({
            "issueId": issue_id,
            "body": body,
        });

        let data: serde_json::Value = self
            .graphql_query(QUERY, serde_json::json!({ "input": input }))
            .await?;

        let success = data["commentCreate"]["success"]
            .as_bool()
            .unwrap_or(false);

        if !success {
            anyhow::bail!("commentCreate returned success=false");
        }

        let comment = data["commentCreate"]["comment"]
            .as_object()
            .context("Missing comment in response")?;

        Ok(LinearComment {
            id: comment["id"].as_str().unwrap_or_default().to_string(),
            body: comment["body"].as_str().unwrap_or_default().to_string(),
            url: comment["url"].as_str().map(|s| s.to_string()),
        })
    }

    /// Delete an issue by its ID
    pub async fn delete_issue(&self, issue_id: &str) -> Result<()> {
        const QUERY: &str = r#"mutation IssueDelete($id: String!) {
            issueDelete(id: $id) {
                success
            }
        }"#;

        let data: serde_json::Value = self
            .graphql_query(QUERY, serde_json::json!({ "id": issue_id }))
            .await?;

        let success = data["issueDelete"]["success"].as_bool().unwrap_or(false);

        if !success {
            anyhow::bail!("issueDelete returned success=false");
        }

        Ok(())
    }

    pub async fn graphql_query<T>(
        &self,
        query: &'static str,
        variables: serde_json::Value,
    ) -> Result<T>
    where
        T: DeserializeOwned,
    {
        let request = GraphQLRequest { query, variables };

        let response = self
            .http
            .post(LINEAR_API_URL)
            .header("Authorization", &self.api_key)
            .header("Content-Type", "application/json")
            .json(&request)
            .send()
            .await
            .context("Failed to send GraphQL request")?;

        let status = response.status();
        let body_text = response
            .text()
            .await
            .context("Failed to read response body")?;

        if !status.is_success() {
            anyhow::bail!(
                "Linear API returned status: {}. Body: {}",
                status,
                body_text
            );
        }

        let resp: GraphQLResponse<T> =
            serde_json::from_str(&body_text).context("Failed to parse GraphQL response")?;

        if let Some(errors) = resp.errors {
            let error_messages: Vec<String> = errors.into_iter().map(|e| e.message).collect();
            anyhow::bail!("GraphQL errors: {}", error_messages.join(", "));
        }

        resp.data.context("GraphQL response missing data")
    }
}
