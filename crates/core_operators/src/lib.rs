use std::collections::HashMap;

use anyhow::Result;
use chrono::{DateTime, Duration, Utc};
use core_model::arrow::{Arrow, ArrowEndpoint};
use core_model::domain::Domain;
use core_model::event::{EventEnvelope, EventRef};
use core_model::operator::{ArrowDirection, OperatorDescriptor, OperatorKind};
use core_model::time::TimeWindow;
use uuid::Uuid;

pub struct OperatorContext<'a> {
    pub descriptor: &'a OperatorDescriptor,
    pub window: TimeWindow,
    pub events: &'a [EventEnvelope],
    pub now: DateTime<Utc>,
}

pub struct OperatorResult {
    pub arrows: Vec<Arrow>,
}

pub trait Operator: Send + Sync {
    fn descriptor(&self) -> &OperatorDescriptor;
    fn run(&self, ctx: &OperatorContext) -> Result<OperatorResult>;
}

pub struct OperatorRegistry {
    ops: Vec<Box<dyn Operator>>,
}

impl OperatorRegistry {
    pub fn new() -> Self {
        let mut registry = Self { ops: Vec::new() };
        registry.register(SlackLinearAnalysisOperator::new());
        registry.register(LinearSlackSynthesisOperator::new());
        registry.register(LinearStaleDetector::new());
        registry
    }

    pub fn register<O>(&mut self, operator: O)
    where
        O: Operator + 'static,
    {
        self.ops.push(Box::new(operator));
    }

    pub fn descriptors(&self) -> Vec<OperatorDescriptor> {
        self.ops.iter().map(|op| op.descriptor().clone()).collect()
    }

    pub fn find(&self, id: &str) -> Option<&(dyn Operator)> {
        self.ops
            .iter()
            .find(|op| op.descriptor().id == id)
            .map(|op| op.as_ref())
    }
}

impl Default for OperatorRegistry {
    fn default() -> Self {
        Self::new()
    }
}

struct SlackLinearAnalysisOperator {
    descriptor: OperatorDescriptor,
}

impl SlackLinearAnalysisOperator {
    fn new() -> Self {
        Self {
            descriptor: OperatorDescriptor {
                id: "slack_to_linear.issue_analysis".into(),
                name: "Slack → Linear analysis".into(),
                description: "Scan Slack discussions for actionable work and propose Linear tasks"
                    .into(),
                direction: ArrowDirection::AnalysisRequest,
                source_domains: vec![Domain::Slack],
                target_domains: vec![Domain::Linear],
                kind: OperatorKind::Llm,
            },
        }
    }
}

impl Operator for SlackLinearAnalysisOperator {
    fn descriptor(&self) -> &OperatorDescriptor {
        &self.descriptor
    }

    fn run(&self, ctx: &OperatorContext) -> Result<OperatorResult> {
        let mut arrows = Vec::new();
        for event in ctx.events.iter().filter(|e| e.domain == Domain::Slack) {
            if !is_actionable_slack_event(event) {
                continue;
            }
            let arrow = Arrow {
                id: Uuid::new_v4().to_string(),
                created_at: ctx.now,
                operator_id: self.descriptor.id.clone(),
                direction: ArrowDirection::AnalysisRequest,
                source: ArrowEndpoint::DomainObject {
                    domain: Domain::Slack,
                    object_id: event.entity_id.clone().unwrap_or_else(|| event.id.clone()),
                },
                target: ArrowEndpoint::Domain {
                    domain: Domain::Linear,
                },
                title: format!("Triage Slack discussion {}", short_id(&event.id)),
                detail_markdown: format!(
                    "Possible follow-up from Slack:\n\n- **Summary:** {}\n- **Kind:** {}\n- **Window:** {} to {}",
                    event.summary,
                    event.kind,
                    ctx.window.start,
                    ctx.window.end
                ),
                evidence: vec![EventRef {
                    domain: Domain::Slack,
                    local_id: event.id.clone(),
                }],
                author_persona_key: event.actor_persona_key.clone(),
                author_persona_id: event.actor_persona_id,
                author_identity_id: event.actor_identity_id,
            };
            arrows.push(arrow);
        }
        Ok(OperatorResult { arrows })
    }
}

struct LinearSlackSynthesisOperator {
    descriptor: OperatorDescriptor,
}

impl LinearSlackSynthesisOperator {
    fn new() -> Self {
        Self {
            descriptor: OperatorDescriptor {
                id: "linear_to_slack.synthesis".into(),
                name: "Linear → Slack synthesis".into(),
                description: "Identify Linear churn (reopens, high-priority changes) that deserve Slack discussion".into(),
                direction: ArrowDirection::SynthesisRequest,
                source_domains: vec![Domain::Linear],
                target_domains: vec![Domain::Slack],
                kind: OperatorKind::Llm,
            },
        }
    }
}

impl Operator for LinearSlackSynthesisOperator {
    fn descriptor(&self) -> &OperatorDescriptor {
        &self.descriptor
    }

    fn run(&self, ctx: &OperatorContext) -> Result<OperatorResult> {
        let mut arrows = Vec::new();
        for event in ctx
            .events
            .iter()
            .filter(|e| e.domain == Domain::Linear && is_synthesis_candidate(e))
        {
            let issue = event.entity_id.clone().unwrap_or_else(|| "issue".into());
            let title = format!("Discuss {} in Slack", issue);
            let detail_markdown = format!(
                "Linear recorded **{}** for `{issue}` at {}.\nConsider a Slack recap to unblock the issue.",
                event.kind,
                event.at
            );
            arrows.push(Arrow {
                id: Uuid::new_v4().to_string(),
                created_at: ctx.now,
                operator_id: self.descriptor.id.clone(),
                direction: ArrowDirection::SynthesisRequest,
                source: ArrowEndpoint::DomainObject {
                    domain: Domain::Linear,
                    object_id: issue.clone(),
                },
                target: ArrowEndpoint::Domain {
                    domain: Domain::Slack,
                },
                title,
                detail_markdown,
                evidence: vec![EventRef {
                    domain: Domain::Linear,
                    local_id: event.id.clone(),
                }],
                author_persona_key: event.actor_persona_key.clone(),
                author_persona_id: event.actor_persona_id,
                author_identity_id: event.actor_identity_id,
            });
        }
        Ok(OperatorResult { arrows })
    }
}

struct LinearStaleDetector {
    descriptor: OperatorDescriptor,
    threshold: Duration,
}

impl LinearStaleDetector {
    fn new() -> Self {
        Self {
            descriptor: OperatorDescriptor {
                id: "linear.stale_detector".into(),
                name: "Linear stale detector".into(),
                description:
                    "Flag Linear issues that have not seen activity within the configured window"
                        .into(),
                direction: ArrowDirection::AnalysisRequest,
                source_domains: vec![Domain::Linear],
                target_domains: vec![Domain::Linear],
                kind: OperatorKind::Pure,
            },
            threshold: Duration::days(7),
        }
    }
}

impl Operator for LinearStaleDetector {
    fn descriptor(&self) -> &OperatorDescriptor {
        &self.descriptor
    }

    fn run(&self, ctx: &OperatorContext) -> Result<OperatorResult> {
        let mut latest_per_issue: HashMap<String, DateTime<Utc>> = HashMap::new();
        for event in ctx.events.iter().filter(|e| e.domain == Domain::Linear) {
            if let Some(issue) = &event.entity_id {
                latest_per_issue
                    .entry(issue.clone())
                    .and_modify(|ts| {
                        if event.at > *ts {
                            *ts = event.at;
                        }
                    })
                    .or_insert(event.at);
            }
        }

        let mut arrows = Vec::new();
        for (issue, last_ts) in latest_per_issue {
            if ctx.now - last_ts < self.threshold {
                continue;
            }
            arrows.push(Arrow {
                id: Uuid::new_v4().to_string(),
                created_at: ctx.now,
                operator_id: self.descriptor.id.clone(),
                direction: ArrowDirection::AnalysisRequest,
                source: ArrowEndpoint::DomainObject {
                    domain: Domain::Linear,
                    object_id: issue.clone(),
                },
                target: ArrowEndpoint::DomainObject {
                    domain: Domain::Linear,
                    object_id: issue.clone(),
                },
                title: format!("{} may be stale", issue),
                detail_markdown: format!(
                    "No Linear activity observed for `{issue}` since {} (>{} days)",
                    last_ts,
                    self.threshold.num_days()
                ),
                evidence: Vec::new(),
                author_persona_key: None,
                author_persona_id: None,
                author_identity_id: None,
            });
        }

        Ok(OperatorResult { arrows })
    }
}

fn is_actionable_slack_event(event: &EventEnvelope) -> bool {
    let text = event.summary.to_lowercase();
    const KEYWORDS: [&str; 6] = ["todo", "fix", "bug", "issue", "action", "follow up"];
    KEYWORDS.iter().any(|kw| text.contains(kw))
}

fn is_synthesis_candidate(event: &EventEnvelope) -> bool {
    matches!(
        event.kind.as_str(),
        "linear.reopened" | "linear.priority_changed" | "linear.state_changed"
    )
}

fn short_id(id: &str) -> String {
    id.rsplit(':').next().unwrap_or(id).to_string()
}
