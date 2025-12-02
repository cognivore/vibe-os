import type {
  DomainDescriptor,
  EventEnvelope,
  Identity,
  ProviderPersonasPayload,
} from "../types/core";

const DEFAULT_BASE = "http://localhost:3000";
const API_BASE =
  (import.meta.env.VITE_API_BASE as string | undefined) ?? DEFAULT_BASE;
export const API_ROOT = API_BASE.replace(/\/$/, "");

async function request<T>(
  path: string,
  init?: RequestInit,
): Promise<T> {
  // Only set Content-Type for requests with a body to avoid unnecessary CORS preflight
  const headers: HeadersInit = init?.body
    ? { "Content-Type": "application/json", ...(init?.headers ?? {}) }
    : { ...(init?.headers ?? {}) };

  const response = await fetch(`${API_ROOT}${path}`, {
    headers,
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
  cursor?: number;
}

export interface TimelineWindowResponse {
  from: string;
  to: string;
}

export interface TimelineSnapshotResponse {
  mode: "snapshot";
  cursor: number;
  window: TimelineWindowResponse;
  events: EventEnvelope[];
}

export interface TimelineDeltaResponse {
  mode: "delta";
  cursor: number;
  window: TimelineWindowResponse;
  added: EventEnvelope[];
  removed: string[];
}

export type TimelineEventsResponse = TimelineSnapshotResponse | TimelineDeltaResponse;

export async function getEvents(params: EventsQuery): Promise<TimelineEventsResponse> {
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
  if (typeof params.cursor === "number") {
    url.searchParams.set("cursor", params.cursor.toString());
  }
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(await response.text());
  }
  return (await response.json()) as TimelineEventsResponse;
}

// Deprecated: Operators and Arrows API have been removed.
// See Linear Dashboard for cycle-based reporting.

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

export interface LinearIssueResponse {
  issue_id: string;
  issue_identifier?: string | null;
  issue_title?: string | null;
  issue_url?: string | null;
  issue_description?: string | null;
  events: EventEnvelope[];
  comments: EventEnvelope[];
}

export async function getLinearIssue(
  issueRef: string,
): Promise<LinearIssueResponse> {
  return request(`/api/linear/issues/${encodeURIComponent(issueRef)}`);
}

export interface SearchQuery {
  query: string;
  domains?: string[];
  from?: string;
  to?: string;
  limit?: number;
  offset?: number;
}

export interface SearchHit {
  event: EventEnvelope;
  thread_name: string | null;
}

export interface SearchResponse {
  hits: SearchHit[];
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

// Linear Dashboard API

export interface CycleHistoryEntry {
  at: string;
  from_cycle_number: number | null;
  to_cycle_number: number | null;
  to_cycle_starts_at: string | null;
}

export interface LinearIssueSnapshot {
  id: string;
  identifier: string;
  title: string;
  description: string | null;
  url: string | null;
  team_id: string | null;
  team_key: string | null;
  team_name: string | null;
  state_id: string | null;
  state_name: string | null;
  state_type: string | null;
  assignee_id: string | null;
  assignee_name: string | null;
  labels: string[];
  priority: number | null;
  estimate: number | null;
  created_at: string;
  updated_at: string;
  completed_at: string | null;
  canceled_at: string | null;
  archived_at: string | null;
  cycle_id: string | null;
  cycle_number: number | null;
  cycle_name: string | null;
  cycle_starts_at: string | null;
  cycle_ends_at: string | null;
  cycle_history: CycleHistoryEntry[];
}

export interface LinearIssuesResponse {
  issues: LinearIssueSnapshot[];
  last_sync_at: string | null;
  workspace_name: string | null;
}

export async function getLinearIssues(): Promise<LinearIssuesResponse> {
  return request("/api/linear/snapshot");
}

export interface ThreadTitlesRequest {
  thread_ids: string[];
}

export interface ThreadTitlesResponse {
  titles: Record<string, string>;
}

export async function getThreadTitles(threadIds: string[]): Promise<Record<string, string>> {
  const response = await request<ThreadTitlesResponse>("/api/search/thread-titles", {
    method: "POST",
    body: JSON.stringify({ thread_ids: threadIds }),
  });
  return response.titles;
}

