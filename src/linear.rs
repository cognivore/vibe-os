use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize, Serialize)]
pub struct LinearIssue {
    pub id: String,
    pub identifier: String,
    pub title: String,
    pub url: Option<String>,
}

#[derive(Debug, Serialize)]
struct GraphQLRequest {
    query: String,
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
        Self {
            http: reqwest::Client::new(),
            api_key,
        }
    }

    pub async fn create_issue(
        &self,
        team_id: &str,
        title: &str,
        description: &str,
        priority: Option<i32>,
    ) -> Result<LinearIssue> {
        let mutation = r#"
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

        let mut input = serde_json::json!({
            "teamId": team_id,
            "title": title,
            "description": description,
        });

        if let Some(prio) = priority {
            input["priority"] = serde_json::json!(prio);
        }

        let request = GraphQLRequest {
            query: mutation.to_string(),
            variables: serde_json::json!({
                "input": input
            }),
        };

        let response = self
            .http
            .post("https://api.linear.app/graphql")
            .header("Authorization", self.api_key.clone())
            .header("Content-Type", "application/json")
            .json(&request)
            .send()
            .await
            .context("Failed to send GraphQL request")?;

        let status = response.status();
        if !status.is_success() {
            anyhow::bail!("Linear API returned status: {}", status);
        }

        let resp: GraphQLResponse<IssueCreateResponse> = response
            .json()
            .await
            .context("Failed to parse GraphQL response")?;

        if let Some(errors) = resp.errors {
            let error_messages: Vec<String> = errors
                .into_iter()
                .map(|e| e.message)
                .collect();
            anyhow::bail!("GraphQL errors: {}", error_messages.join(", "));
        }

        let data = resp.data
            .context("GraphQL response missing data")?;

        if !data.issue_create.success {
            anyhow::bail!("issueCreate returned success=false");
        }

        let issue_data = data.issue_create.issue
            .context("issueCreate did not return an issue")?;

        Ok(LinearIssue {
            id: issue_data.id,
            identifier: issue_data.identifier,
            title: issue_data.title,
            url: issue_data.url,
        })
    }
}

