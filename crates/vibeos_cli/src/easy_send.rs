use anyhow::{bail, Context, Result};
use serde::Deserialize;
use std::io::Read;

use crate::linear_sync::LinearIssueSnapshot;

#[derive(Debug, Deserialize)]
pub struct EasySendYaml {
    pub who: String,
    #[serde(rename = "where")]
    pub where_: Option<String>,
    pub title: Option<String>,
    pub why: String,
    pub what: String,
    pub priority: Option<i32>,
}

#[derive(Debug)]
pub struct EasySendRequest {
    pub assignee: String,
    pub team: String,
    pub title: Option<String>,
    pub why: String,
    pub what: String,
    pub priority: Option<i32>,
}

/// Parse input from either YAML file or stdin pipe format
pub fn parse_easy_send_input(input: &str) -> Result<EasySendRequest> {
    // Try YAML format first
    if let Ok(yaml) = serde_yaml::from_str::<EasySendYaml>(input) {
        // Support shorthand: "who: user@TEAM" or separate "who: user" + "where: TEAM"
        let (assignee, team) = if yaml.who.contains('@') {
            let parts: Vec<&str> = yaml.who.splitn(2, '@').collect();
            if parts.len() != 2 {
                bail!("Invalid who format with @, expected: user@TEAM");
            }
            let assignee = parts[0].trim().to_string();
            let team_from_at = parts[1].trim().to_string();

            // If "where" is also specified, prefer "where" but warn about conflict
            if let Some(where_team) = yaml.where_ {
                if where_team != team_from_at {
                    eprintln!("Warning: both '@' notation ({}) and 'where' field ({}) specified, using 'where' field", team_from_at, where_team);
                }
                (assignee, where_team)
            } else {
                (assignee, team_from_at)
            }
        } else {
            // No @ in who, expect separate where field
            match yaml.where_ {
                Some(team) => (yaml.who, team),
                None => bail!("Missing 'where' field and no @TEAM in 'who' field"),
            }
        };

        return Ok(EasySendRequest {
            assignee,
            team,
            title: yaml.title,
            why: yaml.why,
            what: yaml.what,
            priority: yaml.priority,
        });
    }

    // Try pipe format: "user@TEAM" header + markdown body
    parse_pipe_format(input)
}

fn parse_pipe_format(input: &str) -> Result<EasySendRequest> {
    let lines: Vec<&str> = input.lines().collect();

    if lines.is_empty() {
        bail!("Empty input");
    }

    // First non-empty line should be "user@TEAM"
    let header = lines
        .iter()
        .find(|l| !l.trim().is_empty())
        .context("No header found")?
        .trim();

    if !header.contains('@') {
        bail!("Expected format: user@TEAM on first line");
    }

    let parts: Vec<&str> = header.splitn(2, '@').collect();
    if parts.len() != 2 {
        bail!("Invalid header format, expected: user@TEAM");
    }

    let assignee = parts[0].trim().to_string();
    let team = parts[1].trim().to_string();

    // Parse markdown body looking for # Why and # What sections
    let body = lines
        .iter()
        .skip_while(|l| !l.trim().is_empty() || l.contains('@'))
        .skip(1) // skip the header line
        .copied()
        .collect::<Vec<_>>()
        .join("\n");

    let (why, what) = parse_markdown_sections(&body)?;

    Ok(EasySendRequest {
        assignee,
        team,
        title: None,
        why,
        what,
        priority: None,
    })
}

fn parse_markdown_sections(body: &str) -> Result<(String, String)> {
    let lines: Vec<&str> = body.lines().collect();
    let mut why_lines = Vec::new();
    let mut what_lines = Vec::new();
    let mut current_section = None;

    for line in lines {
        let trimmed = line.trim();
        if trimmed.starts_with("# Why") || trimmed.starts_with("#Why") {
            current_section = Some("why");
            continue;
        } else if trimmed.starts_with("# What") || trimmed.starts_with("#What") {
            current_section = Some("what");
            continue;
        }

        match current_section {
            Some("why") => {
                if trimmed.starts_with('#') && !trimmed.starts_with("##") {
                    current_section = None;
                } else {
                    why_lines.push(line);
                }
            }
            Some("what") => {
                if trimmed.starts_with('#') && !trimmed.starts_with("##") {
                    current_section = None;
                } else {
                    what_lines.push(line);
                }
            }
            _ => {}
        }
    }

    let why = why_lines.join("\n").trim().to_string();
    let what = what_lines.join("\n").trim().to_string();

    if why.is_empty() {
        bail!("Missing '# Why' section");
    }
    if what.is_empty() {
        bail!("Missing '# What' section");
    }

    Ok((why, what))
}

/// Match assignee name to ID from Linear mirror
pub fn match_assignee(name: &str, issues: &[LinearIssueSnapshot]) -> Result<String> {
    let needle = name.to_lowercase();
    let mut matches: Vec<(&str, &str)> = Vec::new();

    // Collect unique assignee name -> ID mappings
    for issue in issues {
        if let (Some(assignee_name), Some(assignee_id)) = (&issue.assignee_name, &issue.assignee_id)
        {
            if assignee_name.to_lowercase().contains(&needle)
                && !matches.iter().any(|(_, id)| id == assignee_id)
            {
                matches.push((assignee_name.as_str(), assignee_id.as_str()));
            }
        }
    }

    if matches.is_empty() {
        bail!("No assignee found matching '{}'", name);
    }

    if matches.len() > 1 {
        let names: Vec<String> = matches.iter().map(|(n, _)| n.to_string()).collect();
        bail!(
            "Ambiguous assignee '{}'. Matches: {}",
            name,
            names.join(", ")
        );
    }

    Ok(matches[0].1.to_string())
}

/// Match team key/name to team ID from Linear mirror
pub fn match_team(team_key: &str, issues: &[LinearIssueSnapshot]) -> Result<String> {
    let needle = team_key.to_uppercase();
    let mut matches: Vec<(&str, &str)> = Vec::new();

    // Collect unique team_key -> team_id mappings
    for issue in issues {
        if let (Some(key), Some(id)) = (&issue.team_key, &issue.team_id) {
            if key.to_uppercase() == needle && !matches.iter().any(|(_, tid)| tid == id) {
                matches.push((key.as_str(), id.as_str()));
            }
        }
    }

    if matches.is_empty() {
        bail!("No team found matching '{}'", team_key);
    }

    if matches.len() > 1 {
        let keys: Vec<String> = matches.iter().map(|(k, _)| k.to_string()).collect();
        bail!(
            "Ambiguous team '{}'. Matches: {}",
            team_key,
            keys.join(", ")
        );
    }

    Ok(matches[0].1.to_string())
}

pub fn read_input_from_file_or_stdin(file: Option<&std::path::Path>) -> Result<String> {
    if let Some(path) = file {
        std::fs::read_to_string(path).context("Failed to read file")
    } else {
        let mut buffer = String::new();
        std::io::stdin()
            .read_to_string(&mut buffer)
            .context("Failed to read from stdin")?;
        Ok(buffer)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_yaml_format() {
        let yaml = r#"
who: manu
where: RAD
why: |
  Bloodmoney API key and base URL are hard-coded in frontend code.
what: |
  - [ ] Move API_BASE_URL and API_KEY to backend/worker
  - [ ] Create /api/bloodmoney/* endpoints in worker
"#;

        let result = parse_easy_send_input(yaml).unwrap();
        assert_eq!(result.assignee, "manu");
        assert_eq!(result.team, "RAD");
        assert!(result.why.contains("Bloodmoney"));
        assert!(result.what.contains("Move API_BASE_URL"));
    }

    #[test]
    fn test_parse_pipe_format() {
        let pipe = r#"manu@RAD

# Why

Bloodmoney API key and base URL are hard-coded in frontend code.

# What

- [ ] Move API_BASE_URL and API_KEY to backend/worker
- [ ] Create /api/bloodmoney/* endpoints in worker
"#;

        let result = parse_easy_send_input(pipe).unwrap();
        assert_eq!(result.assignee, "manu");
        assert_eq!(result.team, "RAD");
        assert!(result.why.contains("Bloodmoney"));
        assert!(result.what.contains("Move API_BASE_URL"));
    }

    #[test]
    fn test_parse_yaml_with_at_shorthand() {
        let yaml = r#"
who: cognivore@NIN
why: |
  Explore feature needs LLM-suggested dimensions for predictive caching.
what: |
  - [ ] Create /suggest endpoint in bloodmoney API
  - [ ] Implement LLM-based dimension suggestions
  - [ ] Deploy endpoint to production
"#;

        let result = parse_easy_send_input(yaml).unwrap();
        assert_eq!(result.assignee, "cognivore");
        assert_eq!(result.team, "NIN");
        assert!(result.why.contains("Explore feature"));
        assert!(result.what.contains("Create /suggest endpoint"));
    }

    #[test]
    fn test_parse_yaml_with_at_and_where_prefers_where() {
        let yaml = r#"
who: cognivore@RAD
where: NIN
why: |
  Test conflict resolution.
what: |
  - [ ] Test task
"#;

        let result = parse_easy_send_input(yaml).unwrap();
        assert_eq!(result.assignee, "cognivore");
        assert_eq!(result.team, "NIN"); // Should prefer 'where' field
    }

    #[test]
    fn test_parse_yaml_missing_team_fails() {
        let yaml = r#"
who: cognivore
why: |
  Missing team.
what: |
  - [ ] Test task
"#;

        let result = parse_easy_send_input(yaml);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Missing 'where' field"));
    }

    #[test]
    fn test_parse_yaml_with_priority() {
        let yaml = r#"
who: alice@ENG
priority: 2
why: |
  High priority task requiring immediate attention.
what: |
  - [ ] Address critical security issue
  - [ ] Deploy hotfix
"#;

        let result = parse_easy_send_input(yaml).unwrap();
        assert_eq!(result.assignee, "alice");
        assert_eq!(result.team, "ENG");
        assert_eq!(result.priority, Some(2));
    }

    #[test]
    fn test_parse_yaml_without_priority() {
        let yaml = r#"
who: bob@OPS
why: |
  Regular task with no specific priority.
what: |
  - [ ] Update documentation
"#;

        let result = parse_easy_send_input(yaml).unwrap();
        assert_eq!(result.assignee, "bob");
        assert_eq!(result.team, "OPS");
        assert_eq!(result.priority, None);
    }

    #[test]
    fn test_parse_yaml_with_title() {
        let yaml = r#"
who: cognivore@NIN
title: CloudFront cache testing
why: |
  We need to validate CloudFront respects IOCAINE traffic-class cache rules.
  Without this, we risk serving wrong content to users.
what: |
  - [ ] Implement automated cache tests
  - [ ] Verify HIT/MISS behavior
"#;

        let result = parse_easy_send_input(yaml).unwrap();
        assert_eq!(result.assignee, "cognivore");
        assert_eq!(result.team, "NIN");
        assert_eq!(result.title, Some("CloudFront cache testing".to_string()));
    }

    #[test]
    fn test_parse_yaml_without_title_autogenerates() {
        let yaml = r#"
who: alice@ENG
why: |
  This is the first line that should become the title.
  This is the second line.
what: |
  - [ ] Do something
"#;

        let result = parse_easy_send_input(yaml).unwrap();
        assert_eq!(result.assignee, "alice");
        assert_eq!(result.team, "ENG");
        assert_eq!(result.title, None); // Parser doesn't auto-generate, command does
    }
}
