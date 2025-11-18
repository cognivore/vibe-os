import { useEffect, useState, useCallback } from "react";
import { searchTimeline, type SearchQuery, type SearchHit } from "../../../api/client";
import type { EventEnvelope } from "../../../types/core";

export interface EventEnvelopeWithThread extends EventEnvelope {
  thread_name?: string | null;
}

export interface TimelineSearchState {
  results: EventEnvelopeWithThread[];
  total: number;
  loading: boolean;
  error: string | null;
}

export interface UseTimelineSearchResult extends TimelineSearchState {
  search: (query: SearchQuery) => void;
  clear: () => void;
}

export function useTimelineSearch(): UseTimelineSearchResult {
  const [results, setResults] = useState<EventEnvelopeWithThread[]>([]);
  const [total, setTotal] = useState(0);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [currentQuery, setCurrentQuery] = useState<SearchQuery | null>(null);

  const search = useCallback((query: SearchQuery) => {
    setCurrentQuery(query);
  }, []);

  const clear = useCallback(() => {
    setResults([]);
    setTotal(0);
    setError(null);
    setLoading(false);
    setCurrentQuery(null);
  }, []);

  useEffect(() => {
    if (!currentQuery) {
      return;
    }

    let cancelled = false;
    setLoading(true);
    setError(null);

    searchTimeline(currentQuery)
      .then((response) => {
        if (cancelled) return;
        // Enrich events with thread_name from search hits
        const enrichedEvents = response.hits.map((hit: SearchHit) => ({
          ...hit.event,
          thread_name: hit.thread_name,
        }));
        setResults(enrichedEvents);
        setTotal(response.total);
      })
      .catch((err) => {
        if (cancelled) return;
        setError(err?.message ?? "Failed to search timeline");
      })
      .finally(() => {
        if (cancelled) return;
        setLoading(false);
      });

    return () => {
      cancelled = true;
    };
  }, [currentQuery]);

  return {
    results,
    total,
    loading,
    error,
    search,
    clear,
  };
}

