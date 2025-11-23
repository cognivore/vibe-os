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
  cacheMatchesSelection,
  commitTimelineDelta,
  commitTimelineSnapshot,
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
  const [loading, setLoading] = useState(false);
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
  const snapshotKey = `${selectedDomainsKey}:${window.from}:${window.to}`;
  const matchesSelection =
    stableDomains.length > 0 && cacheMatchesSelection(timelineCache, stableDomains, window);
  const cachedEvents = useMemo(
    () => (matchesSelection ? timelineCache.events : []),
    [matchesSelection, timelineCache.eventsFingerprint],
  );
  const cacheCursor = matchesSelection ? timelineCache.cursor : null;

  const snapshotFetchedRef = useRef<string | null>(null);
  const snapshotInFlightRef = useRef<string | null>(null);
  const deltaFetchedRef = useRef<string | null>(null);
  const deltaInFlightRef = useRef<string | null>(null);
  const providerPersonasFetchedRef = useRef(false);

  useEffect(() => {
    if (!matchesSelection) {
      snapshotFetchedRef.current = null;
      snapshotInFlightRef.current = null;
      deltaFetchedRef.current = null;
      deltaInFlightRef.current = null;
    } else {
      snapshotFetchedRef.current = snapshotKey;
    }
  }, [matchesSelection, snapshotKey]);

  useEffect(() => {
    if (!stableDomains.length) {
      setLoading(false);
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

  useEffect(() => {
    if (!stableDomains.length) {
      setLoading(false);
      return;
    }
    if (matchesSelection) {
      setLoading(false);
      return;
    }
    if (snapshotInFlightRef.current === snapshotKey) return;

    let cancelled = false;
    snapshotInFlightRef.current = snapshotKey;
    setLoading(true);
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
        snapshotFetchedRef.current = snapshotKey;
        deltaFetchedRef.current = `${snapshotKey}:${response.cursor}`;
      })
      .catch((err) => {
        if (!cancelled) {
          setError(err instanceof Error ? err.message : String(err));
        }
      })
      .finally(() => {
        if (!cancelled) {
          setLoading(false);
        }
        snapshotInFlightRef.current = null;
      });

    return () => {
      cancelled = true;
    };
  }, [dataSource, matchesSelection, snapshotKey, stableDomains, window.from, window.to]);

  useEffect(() => {
    if (!stableDomains.length) {
      setLoading(false);
      return;
    }
    if (!matchesSelection) {
      return;
    }
    if (cacheCursor == null) {
      setLoading(false);
      return;
    }
    const deltaKey = `${snapshotKey}:${cacheCursor}`;
    if (deltaFetchedRef.current === deltaKey || deltaInFlightRef.current === deltaKey) {
      setLoading(false);
      return;
    }

    let cancelled = false;
    deltaInFlightRef.current = deltaKey;
    setLoading(true);
    setError(null);

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
          deltaFetchedRef.current = `${snapshotKey}:${response.cursor}`;
        } else {
          commitTimelineSnapshot(stableDomains, response.window, response.cursor, response.events);
          snapshotFetchedRef.current = snapshotKey;
          deltaFetchedRef.current = `${snapshotKey}:${response.cursor}`;
        }
      })
      .catch((err) => {
        if (!cancelled) {
          setError(err?.message ?? "Failed to refresh timeline data");
        }
      })
      .finally(() => {
        if (!cancelled) {
          setLoading(false);
        }
        deltaInFlightRef.current = null;
      });

    return () => {
      cancelled = true;
    };
  }, [cacheCursor, dataSource, matchesSelection, snapshotKey, stableDomains, window.from, window.to]);

  const [threadEntries, setThreadEntries] = useState<ThreadEntry[]>([]);

  useEffect(() => {
    let cancelled = false;

    const buildThreads = async () => {
      const results = await Promise.all(
        threadAdapters.map(async (adapter) => {
          const entries = adapter.buildEntries(cachedEvents);
          return entries instanceof Promise ? await entries : entries;
        })
      );

      if (!cancelled) {
        setThreadEntries(results.flat());
      }
    };

    buildThreads();

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

