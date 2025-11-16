export type ArrowDirection =
  | "analysis_request"
  | "synthesis_request"
  | "elaboration_request";

export interface DomainDescriptor {
  id: string;
  human_name: string;
  description: string;
}

export interface PersonaKey {
  domain: string;
  local_id: string;
}

export interface Persona {
  id: string;
  key: PersonaKey;
  label?: string | null;
  display_name?: string | null;
}

export interface Identity {
  id: string;
  canonical_email: string;
  preferred_name?: string | null;
  avatar?: string | null;
  notes?: string | null;
  personas: Persona[];
}

export interface EventEnvelope {
  id: string;
  domain: string;
  at: string;
  kind: string;
  summary: string;
  entity_id?: string | null;
  data: Record<string, unknown>;
  actor_persona_key?: PersonaKey | null;
  actor_persona_id?: string | null;
  actor_identity_id?: string | null;
}

export interface ArrowEndpointDomain {
  kind: "domain";
  domain: string;
}

export interface ArrowEndpointDomainObject {
  kind: "domain_object";
  domain: string;
  object_id: string;
}

export type ArrowEndpoint = ArrowEndpointDomain | ArrowEndpointDomainObject;

export interface Arrow {
  id: string;
  created_at: string;
  operator_id: string;
  direction: ArrowDirection;
  source: ArrowEndpoint;
  target: ArrowEndpoint;
  title: string;
  detail_markdown: string;
  evidence: { domain: string; local_id: string }[];
  author_persona_key?: PersonaKey | null;
  author_persona_id?: string | null;
  author_identity_id?: string | null;
}

export interface OperatorDescriptor {
  id: string;
  name: string;
  description: string;
  direction: ArrowDirection;
  source_domains: string[];
  target_domains: string[];
  kind: "pure" | "llm";
}

export interface RunOperatorResponse {
  operator: OperatorDescriptor;
  window: { start: string; end: string };
  arrows: Arrow[];
}

