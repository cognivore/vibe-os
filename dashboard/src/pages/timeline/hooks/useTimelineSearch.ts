import { useEffect, useState, useCallback, useRef } from "react";
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

const SEARCH_DEBOUNCE_MS = 350;

export function useTimelineSearch(): UseTimelineSearchResult {
  const [results, setResults] = useState<EventEnvelopeWithThread[]>([]);
  const [total, setTotal] = useState(0);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  // Use refs to track debounce state and abort controller
  const debounceRef = useRef<ReturnType<typeof setTimeout> | null>(null);
  const abortRef = useRef<AbortController | null>(null);
  const lastQueryRef = useRef<string>("");

  const executeSearch = useCallback(async (query: SearchQuery) => {
    // Abort any in-flight request
    if (abortRef.current) {
      abortRef.current.abort();
    }

    const controller = new AbortController();
    abortRef.current = controller;

    setLoading(true);
    setError(null);

    try {
      const response = await searchTimeline(query);

      // Check if this request was aborted
      if (controller.signal.aborted) return;

      // Enrich events with thread_name from search hits
      const enrichedEvents = response.hits.map((hit: SearchHit) => ({
        ...hit.event,
        thread_name: hit.thread_name,
      }));
      setResults(enrichedEvents);
      setTotal(response.total);
    } catch (err) {
      // Ignore aborted requests
      if (controller.signal.aborted) return;
      setError(err instanceof Error ? err.message : "Failed to search timeline");
    } finally {
      if (!controller.signal.aborted) {
        setLoading(false);
      }
    }
  }, []);

  const search = useCallback((query: SearchQuery) => {
    // Create a stable key for this query to detect duplicates
    const queryKey = JSON.stringify(query);

    // Skip if identical to last query
    if (queryKey === lastQueryRef.current) {
      return;
    }

    // Clear any pending debounce
    if (debounceRef.current) {
      clearTimeout(debounceRef.current);
      debounceRef.current = null;
    }

    // Show loading state immediately
    setLoading(true);

    // Debounce the actual search
    debounceRef.current = setTimeout(() => {
      lastQueryRef.current = queryKey;
      executeSearch(query);
      debounceRef.current = null;
    }, SEARCH_DEBOUNCE_MS);
  }, [executeSearch]);

  const clear = useCallback(() => {
    // Cancel any pending debounce
    if (debounceRef.current) {
      clearTimeout(debounceRef.current);
      debounceRef.current = null;
    }

    // Abort any in-flight request
    if (abortRef.current) {
      abortRef.current.abort();
      abortRef.current = null;
    }

    lastQueryRef.current = "";
    setResults([]);
    setTotal(0);
    setError(null);
    setLoading(false);
  }, []);

  // Cleanup on unmount
  useEffect(() => {
    return () => {
      if (debounceRef.current) {
        clearTimeout(debounceRef.current);
      }
      if (abortRef.current) {
        abortRef.current.abort();
      }
    };
  }, []);

  return {
    results,
    total,
    loading,
    error,
    search,
    clear,
  };
}

