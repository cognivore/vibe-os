import { useSyncExternalStore } from "react";
import type { EventEnvelope } from "../../../types/core";
import type { TimelineWindow } from "../adapters";
import type { TimelineWindowResponse } from "../../../api/client";

export interface TimelineCacheState {
  cursor: number | null;
  window: TimelineWindow | null;
  domainsKey: string | null;
  events: EventEnvelope[];
  eventsFingerprint: string;
}

type TimelineCacheUpdater = (prev: TimelineCacheState) => TimelineCacheState;

const listeners = new Set<() => void>();

const emptyState = (): TimelineCacheState => ({
  cursor: null,
  window: null,
  domainsKey: null,
  events: [],
  eventsFingerprint: "",
});

let state: TimelineCacheState = emptyState();

const subscribe = (listener: () => void) => {
  listeners.add(listener);
  return () => listeners.delete(listener);
};

const getSnapshot = () => state;

const setState = (updater: TimelineCacheUpdater) => {
  const next = updater(state);
  if (next === state) {
    return;
  }
  state = next;
  listeners.forEach((listener) => listener());
};

export function useTimelineCacheStore(): TimelineCacheState {
  return useSyncExternalStore(subscribe, getSnapshot, getSnapshot);
}

export function clearTimelineCache(): void {
  setState(() => emptyState());
}

const sortEventsDesc = (events: EventEnvelope[]): EventEnvelope[] =>
  [...events].sort((a, b) => new Date(b.at).getTime() - new Date(a.at).getTime());

const computeFingerprint = (events: EventEnvelope[]): string => {
  if (!events.length) return "";
  return events.map((e) => e.id).join(",");
};

const mergeEvents = (base: EventEnvelope[], additions: EventEnvelope[]): EventEnvelope[] => {
  if (!additions.length) {
    return base;
  }
  const merged = new Map(base.map((event) => [event.id, event]));
  for (const event of additions) {
    merged.set(event.id, event);
  }
  return sortEventsDesc([...merged.values()]);
};

export const selectionKeyForDomains = (domains: string[]): string =>
  [...domains].sort().join(",");

const timestamp = (value: string) => new Date(value).getTime();

const windowsEqual = (a: TimelineWindow | null, b: TimelineWindow | null) => {
  if (!a || !b) {
    return false;
  }
  return timestamp(a.from) === timestamp(b.from) && timestamp(a.to) === timestamp(b.to);
};

export const cacheMatchesSelection = (
  cache: TimelineCacheState,
  domains: string[],
  window: TimelineWindow,
): boolean => {
  return (
    cache.domainsKey === selectionKeyForDomains(domains) &&
    windowsEqual(cache.window, window)
  );
};

const toTimelineWindow = (responseWindow: TimelineWindowResponse): TimelineWindow => ({
  from: new Date(responseWindow.from).toISOString(),
  to: new Date(responseWindow.to).toISOString(),
});

export function commitTimelineSnapshot(
  domains: string[],
  window: TimelineWindowResponse,
  cursor: number,
  events: EventEnvelope[],
): void {
  const nextWindow = toTimelineWindow(window);
  const domainsKey = selectionKeyForDomains(domains);
  const sortedEvents = sortEventsDesc(events);
  const eventsFingerprint = computeFingerprint(sortedEvents);
  setState(() => ({
    cursor,
    window: nextWindow,
    domainsKey,
    events: sortedEvents,
    eventsFingerprint,
  }));
}

export function commitTimelineDelta(
  domains: string[],
  window: TimelineWindowResponse,
  cursor: number,
  added: EventEnvelope[],
  removed: string[],
): void {
  const nextWindow = toTimelineWindow(window);
  const domainsKey = selectionKeyForDomains(domains);
  setState((prev) => {
    if (prev.domainsKey !== domainsKey || !windowsEqual(prev.window, nextWindow)) {
      return prev;
    }
    const nextBase =
      removed.length === 0
        ? prev.events
        : prev.events.filter((event) => !removed.includes(event.id));
    const nextEvents = mergeEvents(nextBase, added);
    const eventsFingerprint = computeFingerprint(nextEvents);
    return {
      cursor,
      window: nextWindow,
      domainsKey,
      events: nextEvents,
      eventsFingerprint,
    };
  });
}

