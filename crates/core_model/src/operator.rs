use serde::{Deserialize, Serialize};

use crate::domain::Domain;

/// Class of arrow: high-level semantics.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum ArrowDirection {
    /// From conversation/brainstorm domains → task/issue domains.
    AnalysisRequest,
    /// From task/issue domains → conversation/calendar domains.
    SynthesisRequest,
    /// Refinements, clarifications, etc.
    ElaborationRequest,
}

/// Implementation style.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum OperatorKind {
    Pure,
    Llm,
}

/// Metadata for an operator; purely descriptive.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OperatorDescriptor {
    pub id: String, // e.g. "slack_to_linear.issue_analysis"
    pub name: String,
    pub description: String,
    pub direction: ArrowDirection,
    /// Which domains it consumes events from.
    pub source_domains: Vec<Domain>,
    /// Which domains it “points toward” (target of arrows).
    pub target_domains: Vec<Domain>,
    pub kind: OperatorKind,
}
