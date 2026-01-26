import { useSyncExternalStore } from "react";
import type { EventEnvelope } from "../../../types/core";
import type { TimelineWindow } from "../adapters";
import type { TimelineWindowResponse } from "../../../api/client";

// Represents a time range that has been fetched and cached
export interface CachedRange {
  from: string;      // ISO timestamp
  to: string;        // ISO timestamp
  fetchedAt: number; // Unix ms - for TTL/staleness tracking
}

export interface TimelineCacheState {
  cursor: number | null;
  window: TimelineWindow | null;
  domainsKey: string | null;
  events: EventEnvelope[];
  eventsFingerprint: string;
  cachedRanges: CachedRange[];  // Tracks which time ranges we have data for
}

type TimelineCacheUpdater = (prev: TimelineCacheState) => TimelineCacheState;

const listeners = new Set<() => void>();

const emptyState = (): TimelineCacheState => ({
  cursor: null,
  window: null,
  domainsKey: null,
  events: [],
  eventsFingerprint: "",
  cachedRanges: [],
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

// ============================================================================
// Range computation utilities for incremental fetching
// ============================================================================

/**
 * Sort ranges by start time ascending
 */
const sortRangesByStart = (ranges: CachedRange[]): CachedRange[] =>
  [...ranges].sort((a, b) => timestamp(a.from) - timestamp(b.from));

/**
 * Merge overlapping or adjacent ranges into a minimal set of non-overlapping ranges.
 * Two ranges are considered adjacent if they touch (end of one = start of another).
 */
export const mergeRanges = (ranges: CachedRange[]): CachedRange[] => {
  if (ranges.length <= 1) return ranges;

  const sorted = sortRangesByStart(ranges);
  const merged: CachedRange[] = [];

  let current = { ...sorted[0] };

  for (let i = 1; i < sorted.length; i++) {
    const next = sorted[i];
    const currentEnd = timestamp(current.to);
    const nextStart = timestamp(next.from);

    // If ranges overlap or are adjacent, merge them
    if (nextStart <= currentEnd) {
      const nextEnd = timestamp(next.to);
      if (nextEnd > currentEnd) {
        current.to = next.to;
      }
      // Keep the most recent fetchedAt
      current.fetchedAt = Math.max(current.fetchedAt, next.fetchedAt);
    } else {
      // No overlap - push current and start new range
      merged.push(current);
      current = { ...next };
    }
  }

  merged.push(current);
  return merged;
};

/**
 * Compute the missing ranges needed to cover a requested window.
 * Returns an array of ranges that need to be fetched.
 *
 * Example:
 *   Requested:  |-------- Jul -------- Jan --------|
 *   Cached:               |-- Aug -------- Jan ---|
 *   Missing:    |-- Jul --|
 */
export const computeMissingRanges = (
  requested: TimelineWindow,
  cachedRanges: CachedRange[],
): TimelineWindow[] => {
  if (cachedRanges.length === 0) {
    return [requested];
  }

  const reqFrom = timestamp(requested.from);
  const reqTo = timestamp(requested.to);

  // Merge cached ranges first to simplify computation
  const merged = mergeRanges(cachedRanges);

  // Find ranges within the requested window
  const relevantRanges = merged.filter((range) => {
    const rangeFrom = timestamp(range.from);
    const rangeTo = timestamp(range.to);
    // Range overlaps with requested window
    return rangeFrom < reqTo && rangeTo > reqFrom;
  });

  if (relevantRanges.length === 0) {
    return [requested];
  }

  const missing: TimelineWindow[] = [];
  let currentStart = reqFrom;

  for (const range of sortRangesByStart(relevantRanges)) {
    const rangeFrom = timestamp(range.from);
    const rangeTo = timestamp(range.to);

    // If there's a gap before this range
    if (rangeFrom > currentStart) {
      missing.push({
        from: new Date(currentStart).toISOString(),
        to: new Date(Math.min(rangeFrom, reqTo)).toISOString(),
      });
    }

    // Move past this range
    currentStart = Math.max(currentStart, rangeTo);

    // If we've covered the entire requested range, stop
    if (currentStart >= reqTo) {
      break;
    }
  }

  // If there's still a gap after the last range
  if (currentStart < reqTo) {
    missing.push({
      from: new Date(currentStart).toISOString(),
      to: new Date(reqTo).toISOString(),
    });
  }

  return missing;
};

/**
 * Check if the cached ranges fully cover the requested window.
 */
export const rangesContainWindow = (
  cachedRanges: CachedRange[],
  window: TimelineWindow,
): boolean => {
  const missing = computeMissingRanges(window, cachedRanges);
  return missing.length === 0;
};

/**
 * Get stale ranges that need refreshing based on TTL.
 * Returns ranges that were fetched more than ttlMs ago.
 */
export const getStaleRanges = (
  cachedRanges: CachedRange[],
  ttlMs: number,
): CachedRange[] => {
  const now = Date.now();
  return cachedRanges.filter((range) => now - range.fetchedAt > ttlMs);
};

/**
 * Filter events to only those within the requested window.
 */
export const filterEventsToWindow = (
  events: EventEnvelope[],
  window: TimelineWindow,
): EventEnvelope[] => {
  const from = timestamp(window.from);
  const to = timestamp(window.to);
  return events.filter((event) => {
    const eventTime = timestamp(event.at);
    return eventTime >= from && eventTime <= to;
  });
};

/**
 * Check if domains match the cache (for determining if cache is valid).
 */
export const cacheMatchesDomains = (
  cache: TimelineCacheState,
  domains: string[],
): boolean => {
  return cache.domainsKey === selectionKeyForDomains(domains);
};

/**
 * Legacy function - checks exact window match.
 * Use rangesContainWindow for incremental fetching support.
 */
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

/**
 * Commit a full snapshot - replaces all cached data.
 * Use this when domains change or for initial load.
 */
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
  const cachedRange: CachedRange = {
    from: nextWindow.from,
    to: nextWindow.to,
    fetchedAt: Date.now(),
  };
  setState(() => ({
    cursor,
    window: nextWindow,
    domainsKey,
    events: sortedEvents,
    eventsFingerprint,
    cachedRanges: [cachedRange],
  }));
}

/**
 * Commit a delta update within the same window.
 */
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
      ...prev,
      cursor,
      window: nextWindow,
      events: nextEvents,
      eventsFingerprint,
    };
  });
}

/**
 * Commit events for an additional range (incremental fetch).
 * Merges with existing events and updates cached ranges.
 */
export function commitRangeEvents(
  domains: string[],
  range: TimelineWindow,
  events: EventEnvelope[],
): void {
  const domainsKey = selectionKeyForDomains(domains);
  setState((prev) => {
    // If domains changed, ignore this update
    if (prev.domainsKey !== domainsKey) {
      return prev;
    }

    const newRange: CachedRange = {
      from: range.from,
      to: range.to,
      fetchedAt: Date.now(),
    };

    const nextEvents = mergeEvents(prev.events, events);
    const eventsFingerprint = computeFingerprint(nextEvents);
    const nextRanges = mergeRanges([...prev.cachedRanges, newRange]);

    // Update the window to encompass all cached ranges
    const allFrom = Math.min(...nextRanges.map((r) => timestamp(r.from)));
    const allTo = Math.max(...nextRanges.map((r) => timestamp(r.to)));

    return {
      ...prev,
      window: {
        from: new Date(allFrom).toISOString(),
        to: new Date(allTo).toISOString(),
      },
      events: nextEvents,
      eventsFingerprint,
      cachedRanges: nextRanges,
    };
  });
}

/**
 * Refresh a range (update fetchedAt timestamp) without changing events.
 * Used after background refresh completes.
 */
export function refreshRangeTimestamp(range: TimelineWindow): void {
  const rangeFrom = timestamp(range.from);
  const rangeTo = timestamp(range.to);
  const now = Date.now();

  setState((prev) => {
    const updatedRanges = prev.cachedRanges.map((r) => {
      const from = timestamp(r.from);
      const to = timestamp(r.to);
      // If this range overlaps with the refreshed range, update fetchedAt
      if (from < rangeTo && to > rangeFrom) {
        return { ...r, fetchedAt: now };
      }
      return r;
    });

    return {
      ...prev,
      cachedRanges: updatedRanges,
    };
  });
}

/**
 * Get the current cache state (for reading outside of React).
 */
export function getTimelineCacheState(): TimelineCacheState {
  return state;
}