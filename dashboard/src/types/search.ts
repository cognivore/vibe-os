import type { EventEnvelope } from "./core";

export interface SearchRequest {
  query: string;
  domains?: string[];
  from?: string;
  to?: string;
  limit?: number;
  offset?: number;
}

export interface SearchResult {
  events: EventEnvelope[];
  total: number;
}

