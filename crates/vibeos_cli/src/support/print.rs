use chrono::{DateTime, Utc};

use crate::linear_analysis;
use crate::linear_sync;
use crate::llm;

pub fn print_window_summary(context: &linear_analysis::LinearWindowContext) {
    println!(
        "Linear activity from {} to {}",
        context.window_start, context.window_end
    );
    println!(
        "Created: {} | Completed: {} | Reopened: {} | Archived: {}",
        context.issues_created,
        context.issues_completed,
        context.issues_reopened,
        context.issues_archived
    );
    println!(
        "State transitions: {} | Comments: {}",
        context.transitions, context.comments_added
    );

    if context.top_active_issues.is_empty() {
        println!("No active issues in this window.");
    } else {
        println!("\nTop active issues:");
        for issue in &context.top_active_issues {
            print_issue_summary(issue);
        }
    }
}

pub fn print_stale_issues(
    stale_days: i64,
    issues: &[linear_analysis::LinearIssueSummary],
    as_of: DateTime<Utc>,
) {
    println!(
        "{} stale issues (> {} days without updates) as of {}",
        issues.len(),
        stale_days,
        as_of
    );

    for issue in issues {
        let age_days = (as_of - issue.updated_at).num_days().max(0);
        let assignee = issue
            .assignee_name
            .as_deref()
            .unwrap_or("Unassigned")
            .to_string();
        println!(
            "- {} [{}] {} (assignee: {}, updated {} days ago)",
            issue.identifier,
            issue
                .state_name
                .as_deref()
                .or(issue.state_type.as_deref())
                .unwrap_or("Unknown"),
            issue.title,
            assignee,
            age_days
        );
        if let Some(url) = &issue.url {
            println!("  {}", url);
        }
    }
}

pub fn print_browse_results(issues: &[&linear_sync::LinearIssueSnapshot]) {
    if issues.is_empty() {
        println!("No issues match the provided filters.");
        return;
    }

    for issue in issues {
        let assignee = issue
            .assignee_name
            .as_deref()
            .unwrap_or("Unassigned")
            .to_string();
        let team = issue
            .team_key
            .as_deref()
            .or(issue.team_name.as_deref())
            .unwrap_or("Unknown");
        println!(
            "{} [{} | team {}] {} (assignee: {}, updated: {})",
            issue.identifier,
            issue
                .state_name
                .as_deref()
                .or(issue.state_type.as_deref())
                .unwrap_or("Unknown"),
            team,
            issue.title,
            assignee,
            issue.updated_at
        );
        if let Some(url) = &issue.url {
            println!("  {}", url);
        }
        if !issue.labels.is_empty() {
            println!("  labels: {}", issue.labels.join(", "));
        }
    }
}

pub fn print_issue_summary(issue: &linear_analysis::LinearIssueSummary) {
    let assignee = issue
        .assignee_name
        .as_deref()
        .unwrap_or("Unassigned")
        .to_string();
    println!(
        "- {} [{}] {} (assignee: {}, updated: {})",
        issue.identifier,
        issue
            .state_name
            .as_deref()
            .or(issue.state_type.as_deref())
            .unwrap_or("Unknown"),
        issue.title,
        assignee,
        issue.updated_at
    );
    if let Some(url) = &issue.url {
        println!("  {}", url);
    }
}

pub fn print_llm_window_summary(summary: &llm::LlmLinearWindowSummary) {
    println!(
        "Linear summary for {} to {}",
        summary.window_start, summary.window_end
    );
    println!("--------------------------------------------------");
    println!("{}", summary.summary_markdown);
    println!("\nKey metrics:\n{}", summary.key_metrics_markdown);

    if summary.suggested_slack_threads.is_empty() {
        println!("\nNo Slack thread suggestions.");
    } else {
        println!("\nSuggested Slack threads:");
        for (idx, suggestion) in summary.suggested_slack_threads.iter().enumerate() {
            println!(
                "{}. {} (channel: {})",
                idx + 1,
                suggestion.title,
                suggestion
                    .channel_hint
                    .as_deref()
                    .unwrap_or("<unspecified>")
            );
            if !suggestion.related_issue_identifiers.is_empty() {
                println!(
                    "   Issues: {}",
                    suggestion.related_issue_identifiers.join(", ")
                );
            }
            println!("   Message:\n{}\n", suggestion.message_markdown);
        }
    }
}
