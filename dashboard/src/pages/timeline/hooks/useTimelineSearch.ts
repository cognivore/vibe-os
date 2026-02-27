import { useEffect, useState, useCallback, useRef } from "react";
import { streamSearch, type SearchQuery, type SearchHit } from "../../../api/client";
import type { EventEnvelope } from "../../../types/core";

export interface EventEnvelopeWithThread extends EventEnvelope {
  thread_name?: string | null;
}

export interface TimelineSearchState {
  results: EventEnvelopeWithThread[];
  total: number;
  loading: boolean;
  loadingMore: boolean;
  error: string | null;
  hasMore: boolean;
}

export interface UseTimelineSearchResult extends TimelineSearchState {
  search: (query: SearchQuery) => void;
  loadMore: () => void;
  clear: () => void;
}

const SEARCH_DEBOUNCE_MS = 350;

function enrichHits(hits: SearchHit[]): EventEnvelopeWithThread[] {
  return hits.map((hit) => ({
    ...hit.event,
    thread_name: hit.thread_name,
  }));
}

export function useTimelineSearch(): UseTimelineSearchResult {
  const [results, setResults] = useState<EventEnvelopeWithThread[]>([]);
  const [total, setTotal] = useState(0);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [streaming, setStreaming] = useState(false);

  const debounceRef = useRef<ReturnType<typeof setTimeout> | null>(null);
  const closeRef = useRef<(() => void) | null>(null);
  const lastQueryRef = useRef<string>("");

  const executeSearch = useCallback((query: SearchQuery) => {
    if (closeRef.current) {
      closeRef.current();
      closeRef.current = null;
    }

    setLoading(true);
    setStreaming(true);
    setError(null);
    setResults([]);
    setTotal(0);
    let receivedFirstPage = false;

    closeRef.current = streamSearch(
      {
        query: query.query,
        domains: query.domains,
        from: query.from,
        to: query.to,
        limit: query.limit,
      },
      (hits) => {
        setResults((prev) => [...prev, ...enrichHits(hits)]);
        if (!receivedFirstPage) {
          receivedFirstPage = true;
          setLoading(false);
        }
      },
      (meta) => {
        setTotal(meta.total);
        setStreaming(false);
        closeRef.current = null;
      },
      (err) => {
        setError(err.message);
        setLoading(false);
        setStreaming(false);
        closeRef.current = null;
      },
    );
  }, []);

  const search = useCallback((query: SearchQuery) => {
    const queryKey = JSON.stringify(query);

    if (queryKey === lastQueryRef.current) {
      return;
    }

    if (debounceRef.current) {
      clearTimeout(debounceRef.current);
      debounceRef.current = null;
    }

    setLoading(true);

    debounceRef.current = setTimeout(() => {
      lastQueryRef.current = queryKey;
      executeSearch(query);
      debounceRef.current = null;
    }, SEARCH_DEBOUNCE_MS);
  }, [executeSearch]);

  const loadMore = useCallback(() => {
    // No-op: SSE streaming delivers all results automatically
  }, []);

  const clear = useCallback(() => {
    if (debounceRef.current) {
      clearTimeout(debounceRef.current);
      debounceRef.current = null;
    }

    if (closeRef.current) {
      closeRef.current();
      closeRef.current = null;
    }

    lastQueryRef.current = "";
    setResults([]);
    setTotal(0);
    setError(null);
    setLoading(false);
    setStreaming(false);
  }, []);

  useEffect(() => {
    return () => {
      if (debounceRef.current) {
        clearTimeout(debounceRef.current);
      }
      if (closeRef.current) {
        closeRef.current();
      }
    };
  }, []);

  return {
    results,
    total,
    loading,
    loadingMore: streaming,
    error,
    hasMore: streaming,
    search,
    loadMore,
    clear,
  };
}
