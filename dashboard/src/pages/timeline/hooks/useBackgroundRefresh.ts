import { useEffect, useRef } from "react";
import type { TimelineDataSource, TimelineWindow } from "../adapters";
import {
  commitRangeEvents,
  getStaleRanges,
  getTimelineCacheState,
  refreshRangeTimestamp,
} from "../state/useTimelineCache";

// Default TTL: 5 minutes
const DEFAULT_TTL_MS = 5 * 60 * 1000;

// Check interval: 1 minute
const CHECK_INTERVAL_MS = 60 * 1000;

export interface BackgroundRefreshOptions {
  /** Time-to-live in milliseconds before a range is considered stale */
  ttlMs?: number;
  /** Interval in milliseconds between stale checks */
  checkIntervalMs?: number;
  /** Whether background refresh is enabled */
  enabled?: boolean;
}

/**
 * Hook that manages background refresh of stale cached ranges.
 *
 * This hook periodically checks for stale ranges and refreshes them
 * in the background without blocking the UI.
 */
export function useBackgroundRefresh(
  dataSource: TimelineDataSource,
  domains: string[],
  options: BackgroundRefreshOptions = {},
): void {
  const {
    ttlMs = DEFAULT_TTL_MS,
    checkIntervalMs = CHECK_INTERVAL_MS,
    enabled = true,
  } = options;

  const domainsRef = useRef(domains);
  domainsRef.current = domains;

  const refreshInProgressRef = useRef<Set<string>>(new Set());

  useEffect(() => {
    if (!enabled || domains.length === 0) {
      return;
    }

    const refreshStaleRanges = async () => {
      const cache = getTimelineCacheState();

      // Skip if domains don't match
      const currentDomains = domainsRef.current;
      const domainsKey = [...currentDomains].sort().join(",");
      if (cache.domainsKey !== domainsKey) {
        return;
      }

      // Find stale ranges
      const staleRanges = getStaleRanges(cache.cachedRanges, ttlMs);
      if (staleRanges.length === 0) {
        return;
      }

      // Refresh each stale range
      for (const range of staleRanges) {
        const rangeKey = `${range.from}:${range.to}`;

        // Skip if already refreshing this range
        if (refreshInProgressRef.current.has(rangeKey)) {
          continue;
        }

        refreshInProgressRef.current.add(rangeKey);

        try {
          const window: TimelineWindow = {
            from: range.from,
            to: range.to,
          };

          const response = await dataSource.fetchEvents({
            domains: currentDomains,
            window,
          });

          // Re-check that domains haven't changed during fetch
          const currentCache = getTimelineCacheState();
          if (currentCache.domainsKey !== domainsKey) {
            continue;
          }

          if (response.mode === "snapshot") {
            // Update events and refresh timestamp
            commitRangeEvents(currentDomains, window, response.events);
          }

          // Update the fetchedAt timestamp for this range
          refreshRangeTimestamp(window);

          console.debug(
            `[BackgroundRefresh] Refreshed range ${range.from} to ${range.to}`
          );
        } catch (err) {
          console.warn(
            `[BackgroundRefresh] Failed to refresh range ${range.from} to ${range.to}:`,
            err
          );
        } finally {
          refreshInProgressRef.current.delete(rangeKey);
        }
      }
    };

    // Run initial check after a short delay
    const initialTimeout = setTimeout(refreshStaleRanges, 5000);

    // Set up periodic checks
    const intervalId = setInterval(refreshStaleRanges, checkIntervalMs);

    return () => {
      clearTimeout(initialTimeout);
      clearInterval(intervalId);
    };
  }, [dataSource, domains.length, enabled, ttlMs, checkIntervalMs]);
}

/**
 * Get the current TTL configuration.
 * Useful for displaying cache age information in the UI.
 */
export function getDefaultTtlMs(): number {
  return DEFAULT_TTL_MS;
}
