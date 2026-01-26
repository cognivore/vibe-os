import { useEffect, useMemo, useRef, useState } from "react";
import type {
  ThreadAdapterRegistry,
  ThreadEntry,
  TimelineDataResult,
  TimelineDataSource,
  TimelineWindow,
} from "../adapters";
import type { TimelineEntry } from "../../../components/timeline/types";
import type {
  Arrow,
  DomainDescriptor,
  Identity,
  Persona,
  ProviderPersonasPayload,
} from "../../../types/core";
import { getProviderPersonas } from "../../../api/client";
import {
  cacheMatchesDomains,
  commitRangeEvents,
  commitTimelineDelta,
  commitTimelineSnapshot,
  computeMissingRanges,
  filterEventsToWindow,
  rangesContainWindow,
  selectionKeyForDomains,
  useTimelineCacheStore,
} from "../state/useTimelineCache";

export interface TimelineDataState extends TimelineDataResult {
  loading: boolean;
  error: string | null;
}

interface TimelineDataParams {
  dataSource: TimelineDataSource;
  window: TimelineWindow;
  selectedDomains: string[];
  threadAdapters: ThreadAdapterRegistry;
  domains: DomainDescriptor[];
}

export function useTimelineData({
  dataSource,
  window,
  selectedDomains,
  threadAdapters,
  domains,
}: TimelineDataParams): TimelineDataState {
  const [arrows, setArrows] = useState<Arrow[]>([]);
  const [identities, setIdentities] = useState<Identity[]>([]);
  const [providerPersonaLabels, setProviderPersonaLabels] = useState<Record<string, string>>({});
  const [eventsLoading, setEventsLoading] = useState(false);
  const [threadBuildingLoading, setThreadBuildingLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const renderCountRef = useRef(0);
  const lastRenderTimeRef = useRef(Date.now());

  useEffect(() => {
    const now = Date.now();
    const elapsed = now - lastRenderTimeRef.current;
    if (elapsed < 100) {
      renderCountRef.current += 1;
      if (renderCountRef.current > 10) {
        setError("Timeline render loop detected - please refresh the page");
        throw new Error("Infinite render loop detected in useTimelineData");
      }
    } else {
      renderCountRef.current = 0;
    }
    lastRenderTimeRef.current = now;
  });

  const timelineCache = useTimelineCacheStore();
  const selectedDomainsKey = useMemo(
    () => selectionKeyForDomains(selectedDomains),
    [selectedDomains],
  );
  const stableDomains = useMemo(
    () => [...selectedDomains],
    [selectedDomainsKey],
  );

  // Check if domains match (cache is usable at all)
  const domainsMatch = stableDomains.length > 0 && cacheMatchesDomains(timelineCache, stableDomains);

  // Compute missing ranges based on what's already cached
  const missingRanges = useMemo(() => {
    if (!domainsMatch || timelineCache.cachedRanges.length === 0) {
      return [window]; // Need full fetch
    }
    return computeMissingRanges(window, timelineCache.cachedRanges);
  }, [domainsMatch, window.from, window.to, timelineCache.cachedRanges]);

  // Check if cache fully covers the requested window
  const cacheCoversWindow = domainsMatch && rangesContainWindow(timelineCache.cachedRanges, window);

  // Filter cached events to the requested window for display
  const cachedEvents = useMemo(() => {
    if (!domainsMatch) return [];
    return filterEventsToWindow(timelineCache.events, window);
  }, [domainsMatch, timelineCache.eventsFingerprint, window.from, window.to]);

  const cacheCursor = domainsMatch ? timelineCache.cursor : null;

  // Track fetching state for incremental range fetches
  const rangesFetchedRef = useRef<Set<string>>(new Set());
  const rangesInFlightRef = useRef<Set<string>>(new Set());
  const providerPersonasFetchedRef = useRef(false);

  // Reset fetch tracking when domains change
  useEffect(() => {
    if (!domainsMatch) {
      rangesFetchedRef.current.clear();
      rangesInFlightRef.current.clear();
    }
  }, [domainsMatch]);

  useEffect(() => {
    if (!stableDomains.length) {
      setEventsLoading(false);
      setArrows([]);
      setError(null);
    }
  }, [stableDomains.length]);

  useEffect(() => {
    let cancelled = false;
    dataSource
      .fetchIdentities()
      .then((records) => {
        if (!cancelled) {
          setIdentities(records);
        }
      })
      .catch((err) => {
        if (!cancelled) {
          setError((prev) => prev ?? err?.message ?? "Failed to load identities");
        }
      });
    return () => {
      cancelled = true;
    };
  }, [dataSource]);

  useEffect(() => {
    if (providerPersonasFetchedRef.current) return;
    let cancelled = false;
    providerPersonasFetchedRef.current = true;
    getProviderPersonas()
      .then((payload) => {
        if (!cancelled) {
          setProviderPersonaLabels(buildProviderPersonaLabels(payload));
        }
      })
      .catch((err) => {
        console.warn("Failed to load provider personas", err);
      });
    return () => {
      cancelled = true;
    };
  }, []);

  useEffect(() => {
    if (!stableDomains.length) return;
    let cancelled = false;
    dataSource
      .fetchArrows(window)
      .then((arrowData) => {
        if (!cancelled) {
          setArrows(arrowData);
        }
      })
      .catch((err) => {
        if (!cancelled) {
          setError((prev) => prev ?? err?.message ?? "Failed to load arrows");
        }
      });
    return () => {
      cancelled = true;
    };
  }, [dataSource, stableDomains, window.from, window.to]);

  // Fetch missing ranges incrementally
  useEffect(() => {
    if (!stableDomains.length) {
      setEventsLoading(false);
      return;
    }

    // If cache fully covers the window, no fetch needed
    if (cacheCoversWindow) {
      setEventsLoading(false);
      return;
    }

    // If domains don't match, we need a full snapshot for the requested window
    if (!domainsMatch) {
      const snapshotKey = `snapshot:${selectedDomainsKey}:${window.from}:${window.to}`;
      if (rangesInFlightRef.current.has(snapshotKey)) return;

      let cancelled = false;
      rangesInFlightRef.current.add(snapshotKey);
      setEventsLoading(true);
      setError(null);

      dataSource
        .fetchEvents({
          domains: stableDomains,
          window,
        })
        .then((response) => {
          if (cancelled) return;
          if (response.mode !== "snapshot") {
            throw new Error("Expected snapshot response");
          }
          commitTimelineSnapshot(stableDomains, response.window, response.cursor, response.events);
          rangesFetchedRef.current.add(snapshotKey);
        })
        .catch((err) => {
          if (!cancelled) {
            setError(err instanceof Error ? err.message : String(err));
          }
        })
        .finally(() => {
          if (!cancelled) {
            setEventsLoading(false);
          }
          rangesInFlightRef.current.delete(snapshotKey);
        });

      return () => {
        cancelled = true;
      };
    }

    // Domains match - fetch only missing ranges
    if (missingRanges.length === 0) {
      setEventsLoading(false);
      return;
    }

    let cancelled = false;
    const pendingRanges: string[] = [];

    // Filter out ranges already fetched or in flight
    const rangesToFetch = missingRanges.filter((range) => {
      const rangeKey = `range:${range.from}:${range.to}`;
      if (rangesFetchedRef.current.has(rangeKey) || rangesInFlightRef.current.has(rangeKey)) {
        return false;
      }
      pendingRanges.push(rangeKey);
      return true;
    });

    if (rangesToFetch.length === 0) {
      setEventsLoading(false);
      return;
    }

    // Mark ranges as in-flight
    pendingRanges.forEach((key) => rangesInFlightRef.current.add(key));
    setEventsLoading(true);
    setError(null);

    // Fetch all missing ranges in parallel
    Promise.all(
      rangesToFetch.map(async (range, idx) => {
        const rangeKey = pendingRanges[idx];
        try {
          const response = await dataSource.fetchEvents({
            domains: stableDomains,
            window: range,
          });

          if (cancelled) return;

          if (response.mode === "snapshot") {
            // Commit as a range addition (not replacing the whole cache)
            commitRangeEvents(stableDomains, range, response.events);
          }
          rangesFetchedRef.current.add(rangeKey);
        } catch (err) {
          if (!cancelled) {
            setError((prev) => prev ?? (err instanceof Error ? err.message : String(err)));
          }
        } finally {
          rangesInFlightRef.current.delete(rangeKey);
        }
      })
    ).finally(() => {
      if (!cancelled) {
        setEventsLoading(false);
      }
    });

    return () => {
      cancelled = true;
    };
  }, [
    dataSource,
    stableDomains,
    selectedDomainsKey,
    domainsMatch,
    cacheCoversWindow,
    missingRanges,
    window.from,
    window.to,
  ]);

  // Delta updates for real-time changes (only when cursor is available)
  useEffect(() => {
    if (!stableDomains.length) return;
    if (!domainsMatch) return;
    if (cacheCursor == null) return;

    const deltaKey = `delta:${cacheCursor}`;
    if (rangesFetchedRef.current.has(deltaKey) || rangesInFlightRef.current.has(deltaKey)) {
      return;
    }

    let cancelled = false;
    rangesInFlightRef.current.add(deltaKey);

    dataSource
      .fetchEvents({
        domains: stableDomains,
        window,
        cursor: cacheCursor,
      })
      .then((response) => {
        if (cancelled) return;
        if (response.mode === "delta") {
          commitTimelineDelta(
            stableDomains,
            response.window,
            response.cursor,
            response.added,
            response.removed,
          );
        }
        rangesFetchedRef.current.add(deltaKey);
      })
      .catch((err) => {
        if (!cancelled) {
          console.warn("Delta fetch failed:", err);
        }
      })
      .finally(() => {
        rangesInFlightRef.current.delete(deltaKey);
      });

    return () => {
      cancelled = true;
    };
  }, [cacheCursor, dataSource, domainsMatch, stableDomains, window.from, window.to]);

  const [threadEntries, setThreadEntries] = useState<ThreadEntry[]>([]);

  useEffect(() => {
    let cancelled = false;

    // Skip loading state if there are no events to process
    if (cachedEvents.length === 0) {
      setThreadEntries([]);
      setThreadBuildingLoading(false);
      return;
    }

    setThreadBuildingLoading(true);

    const buildThreads = async () => {
      const results = await Promise.all(
        threadAdapters.map(async (adapter) => {
          const entries = adapter.buildEntries(cachedEvents);
          return entries instanceof Promise ? await entries : entries;
        })
      );

      if (!cancelled) {
        setThreadEntries(results.flat());
        setThreadBuildingLoading(false);
      }
    };

    buildThreads().catch((err) => {
      if (!cancelled) {
        console.error("Failed to build thread entries:", err);
        setThreadBuildingLoading(false);
      }
    });

    return () => {
      cancelled = true;
    };
  }, [cachedEvents, threadAdapters]);

  const entries = useMemo<TimelineEntry[]>(() => {
    // Only show arrows and threads - no individual events
    // The unified timeline displays threads only, never standalone messages
    const arrowEntries: TimelineEntry[] = arrows
      .filter((arrow) => arrow.direction !== "analysis_request")
      .map((arrow) => ({
      type: "arrow" as const,
      at: arrow.created_at,
      arrow,
      }));
    return [...arrowEntries, ...threadEntries].sort(
      (a, b) => new Date(b.at).getTime() - new Date(a.at).getTime(),
    );
  }, [arrows, threadEntries]);

  const identityLookup = useMemo(
    () =>
      Object.fromEntries(
        identities.map((identity) => [identity.id, identity]),
      ),
    [identities],
  );

  const domainLookup = useMemo(
    () => Object.fromEntries(domains.map((domain) => [domain.id, domain.human_name])),
    [domains],
  );

  const personaLookup = useMemo(() => {
    const map: Record<string, { persona: Persona; identityId: string }> = {};
    identities.forEach((identity) => {
      identity.personas.forEach((persona) => {
        map[persona.id] = { persona, identityId: identity.id };
      });
    });
    return map;
  }, [identities]);

  const loading = eventsLoading || threadBuildingLoading;

  return {
    loading,
    error,
    entries,
    threadEntries,
    identities,
    domainLookup,
    identityLookup,
    personaLookup,
    providerPersonaLabels,
  };
}

function buildProviderPersonaLabels(
  payload: ProviderPersonasPayload,
): Record<string, string> {
  const map: Record<string, string> = {};
  payload.slack.forEach((persona) => {
    const label =
      persona.real_name ??
      persona.display_name ??
      persona.username ??
      persona.email ??
      persona.user_id;
    map[`slack:${persona.user_id}`] = label;
  });
  payload.linear.forEach((persona) => {
    const label =
      persona.name ??
      persona.display_name ??
      persona.email ??
      persona.user_id;
    map[`linear:${persona.user_id}`] = label;
  });
  return map;
}

