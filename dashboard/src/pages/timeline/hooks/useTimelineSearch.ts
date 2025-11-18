import { useEffect, useState, useCallback } from "react";
import { searchTimeline, type SearchQuery } from "../../../api/client";
import type { EventEnvelope } from "../../../types/core";

export interface TimelineSearchState {
  results: EventEnvelope[];
  total: number;
  loading: boolean;
  error: string | null;
}

export interface UseTimelineSearchResult extends TimelineSearchState {
  search: (query: SearchQuery) => void;
  clear: () => void;
}

export function useTimelineSearch(): UseTimelineSearchResult {
  const [results, setResults] = useState<EventEnvelope[]>([]);
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
        setResults(response.events);
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

