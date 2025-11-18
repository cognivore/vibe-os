import type {
  Arrow,
  ArrowDirection,
  DomainDescriptor,
  EventEnvelope,
  Identity,
  OperatorDescriptor,
  Persona,
  ProviderPersonasPayload,
  RunOperatorResponse,
} from "../types/core";

const DEFAULT_BASE = "http://localhost:3000";
const API_BASE =
  (import.meta.env.VITE_API_BASE as string | undefined) ?? DEFAULT_BASE;
export const API_ROOT = API_BASE.replace(/\/$/, "");

async function request<T>(
  path: string,
  init?: RequestInit,
): Promise<T> {
  const response = await fetch(`${API_ROOT}${path}`, {
    headers: { "Content-Type": "application/json", ...(init?.headers ?? {}) },
    ...init,
  });

  if (!response.ok) {
    const message = await response.text();
    throw new Error(message || `Request failed (${response.status})`);
  }
  return response.json() as Promise<T>;
}

export async function getDomains(): Promise<DomainDescriptor[]> {
  return request("/api/domains");
}

export interface EventsQuery {
  domains?: string[];
  from: string;
  to: string;
  limit?: number;
  identityId?: string;
}

export async function getEvents(params: EventsQuery): Promise<EventEnvelope[]> {
  const url = new URL(`${API_ROOT}/api/events`);
  if (params.domains && params.domains.length > 0) {
    url.searchParams.set("domains", params.domains.join(","));
  }
  url.searchParams.set("from", params.from);
  url.searchParams.set("to", params.to);
  if (params.limit) {
    url.searchParams.set("limit", params.limit.toString());
  }
  if (params.identityId) {
    url.searchParams.set("identity_id", params.identityId);
  }
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(await response.text());
  }
  return (await response.json()) as EventEnvelope[];
}

export async function getOperators(): Promise<OperatorDescriptor[]> {
  return request("/api/operators");
}

export interface RunOperatorParams {
  operatorId: string;
  domains?: string[];
  from: string;
  to: string;
}

export async function runOperator(
  params: RunOperatorParams,
): Promise<RunOperatorResponse> {
  return request("/api/operators/run", {
    method: "POST",
    body: JSON.stringify({
      operator_id: params.operatorId,
      domains: params.domains,
      from: params.from,
      to: params.to,
    }),
  });
}

export interface ArrowsQuery {
  from: string;
  to: string;
  direction?: ArrowDirection;
  sourceDomains?: string[];
  targetDomains?: string[];
}

export async function getArrows(params: ArrowsQuery): Promise<Arrow[]> {
  const url = new URL(`${API_ROOT}/api/arrows`);
  url.searchParams.set("from", params.from);
  url.searchParams.set("to", params.to);
  if (params.direction) {
    url.searchParams.set("direction", params.direction);
  }
  if (params.sourceDomains?.length) {
    url.searchParams.set("source_domains", params.sourceDomains.join(","));
  }
  if (params.targetDomains?.length) {
    url.searchParams.set("target_domains", params.targetDomains.join(","));
  }
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(await response.text());
  }
  return (await response.json()) as Arrow[];
}

export interface MetaSnapshot {
  slack_mirror_dir: string;
  linear_mirror_dir: string;
  arrow_store_dir: string;
  identity_store_dir: string;
  static_dir?: string | null;
}

export async function getMeta(): Promise<MetaSnapshot> {
  return request("/api/meta");
}

export async function listIdentities(): Promise<Identity[]> {
  return request("/api/identities");
}

export async function getProviderPersonas(): Promise<ProviderPersonasPayload> {
  return request("/api/providers/personas");
}

export async function getIdentity(id: string): Promise<Identity> {
  return request(`/api/identities/${id}`);
}

export interface CreateIdentityInput {
  canonical_email: string;
  preferred_name?: string;
  avatar?: string;
  notes?: string;
}

export async function createIdentity(
  input: CreateIdentityInput,
): Promise<Identity> {
  return request("/api/identities", {
    method: "POST",
    body: JSON.stringify(input),
  });
}

export interface AttachPersonaInput {
  domain: string;
  local_id: string;
  label?: string;
  display_name?: string;
}

export async function attachPersona(
  identityId: string,
  payload: AttachPersonaInput,
): Promise<Identity> {
  return request(`/api/identities/${identityId}/personas`, {
    method: "POST",
    body: JSON.stringify(payload),
  });
}

export async function detachPersona(
  identityId: string,
  personaId: string,
): Promise<Identity> {
  return request(`/api/identities/${identityId}/personas/${personaId}`, {
    method: "DELETE",
  });
}

export async function mergeIdentities(
  primaryId: string,
  secondaryId: string,
): Promise<Identity> {
  return request("/api/identities/merge", {
    method: "POST",
    body: JSON.stringify({ primary: primaryId, secondary: secondaryId }),
  });
}

export async function lookupIdentity(params: {
  email?: string;
  domain?: string;
  local_id?: string;
}): Promise<Identity | null> {
  const url = new URL(`${API_ROOT}/api/identities/lookup`);
  if (params.email) {
    url.searchParams.set("email", params.email);
  }
  if (params.domain) {
    url.searchParams.set("domain", params.domain);
  }
  if (params.local_id) {
    url.searchParams.set("local_id", params.local_id);
  }
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(await response.text());
  }
  return (await response.json()) as Identity | null;
}

export interface SlackThreadResponse {
  thread_id: string;
  channel_id: string;
  root: EventEnvelope;
  replies: EventEnvelope[];
}

export async function getSlackThread(
  channelId: string,
  threadTs: string,
): Promise<SlackThreadResponse> {
  return request(
    `/api/slack/threads/${encodeURIComponent(channelId)}/${encodeURIComponent(threadTs)}`,
  );
}

export interface SearchQuery {
  query: string;
  domains?: string[];
  from?: string;
  to?: string;
  limit?: number;
  offset?: number;
}

export interface SearchResponse {
  events: EventEnvelope[];
  total: number;
}

export async function searchTimeline(params: SearchQuery): Promise<SearchResponse> {
  const url = new URL(`${API_ROOT}/api/search`);
  url.searchParams.set("q", params.query);
  if (params.domains?.length) {
    url.searchParams.set("domains", params.domains.join(","));
  }
  if (params.from) {
    url.searchParams.set("from", params.from);
  }
  if (params.to) {
    url.searchParams.set("to", params.to);
  }
  if (params.limit) {
    url.searchParams.set("limit", params.limit.toString());
  }
  if (params.offset) {
    url.searchParams.set("offset", params.offset.toString());
  }
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(await response.text());
  }
  return (await response.json()) as SearchResponse;
}

export async function reindexSearch(): Promise<void> {
  return request("/api/search/reindex", { method: "POST" });
}

