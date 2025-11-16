export type ArrowDirection =
  | "analysis_request"
  | "synthesis_request"
  | "elaboration_request"
  | "implementation_request";

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

export interface SlackProviderPersona {
  user_id: string;
  team_id?: string | null;
  username?: string | null;
  real_name?: string | null;
  display_name?: string | null;
  email?: string | null;
  title?: string | null;
  phone?: string | null;
  image_url?: string | null;
  is_bot?: boolean | null;
  updated?: number | null;
  profile: Record<string, unknown>;
}

export interface LinearProviderPersona {
  user_id: string;
  name?: string | null;
  display_name?: string | null;
  email?: string | null;
  active: boolean;
  avatar_url?: string | null;
  created_at: string;
  updated_at: string;
}

export interface ProviderPersonasPayload {
  slack: SlackProviderPersona[];
  linear: LinearProviderPersona[];
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

