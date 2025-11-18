import { useEffect, useMemo, useState } from "react";
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
  EventEnvelope,
  Identity,
  Persona,
} from "../../../types/core";

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
  const [events, setEvents] = useState<EventEnvelope[]>([]);
  const [arrows, setArrows] = useState<Arrow[]>([]);
  const [identities, setIdentities] = useState<Identity[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

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
    if (!selectedDomains.length) return;
    let cancelled = false;
    setLoading(true);
    setError(null);
    Promise.all([
      dataSource.fetchEvents({
        domains: selectedDomains,
        window,
      }),
      dataSource.fetchArrows(window),
    ])
      .then(([eventData, arrowData]) => {
        if (cancelled) return;
        setEvents(eventData);
        setArrows(arrowData);
      })
      .catch((err) => {
        if (!cancelled) {
          setError(err?.message ?? "Failed to load timeline data");
        }
      })
      .finally(() => {
        if (!cancelled) {
          setLoading(false);
        }
      });
    return () => {
      cancelled = true;
    };
  }, [dataSource, selectedDomains, window]);

  const [threadEntries, setThreadEntries] = useState<ThreadEntry[]>([]);

  useEffect(() => {
    let cancelled = false;

    const buildThreads = async () => {
      const results = await Promise.all(
        threadAdapters.map(async (adapter) => {
          const entries = adapter.buildEntries(events);
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
  }, [events, threadAdapters]);

  const entries = useMemo<TimelineEntry[]>(() => {
    // Build a set of all entity_ids that are represented as threads
    const threadEntityIds = new Set<string>();
    threadEntries.forEach((thread) => {
      if (thread.type === "slack_thread") {
        threadEntityIds.add(thread.threadId);
      } else if (thread.type === "linear_thread") {
        threadEntityIds.add(thread.issueId);
      }
    });

    // Filter out events that are part of a thread
    const nonThreadEvents = events.filter((event) => {
      // For Slack: filter out if entity_id matches a thread
      if (event.domain === "slack" && event.entity_id) {
        return !threadEntityIds.has(event.entity_id);
      }
      // For Linear: filter out if entity_id matches an issue thread
      if (event.domain === "linear" && event.entity_id) {
        return !threadEntityIds.has(event.entity_id);
      }
      // Keep all other events
      return true;
    });

    const eventEntries: TimelineEntry[] = nonThreadEvents.map((event) => ({
      type: "event" as const,
      at: event.at,
      event,
    }));
    const arrowEntries: TimelineEntry[] = arrows.map((arrow) => ({
      type: "arrow" as const,
      at: arrow.created_at,
      arrow,
    }));
    return [...eventEntries, ...arrowEntries, ...threadEntries].sort(
      (a, b) => new Date(b.at).getTime() - new Date(a.at).getTime(),
    );
  }, [arrows, events, threadEntries]);

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
  };
}

