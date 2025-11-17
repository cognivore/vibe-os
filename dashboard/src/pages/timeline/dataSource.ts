import {
  getArrows,
  getDomains,
  getEvents,
  getSlackThread,
  listIdentities,
} from "../../api/client";
import type {
  TimelineDataQuery,
  TimelineDataSource,
  TimelineWindow,
} from "./adapters";

export function createTimelineApiDataSource(): TimelineDataSource {
  return {
    fetchDomains: () => getDomains(),
    fetchIdentities: () => listIdentities(),
    fetchEvents: (query: TimelineDataQuery) =>
      getEvents({
        domains: query.domains,
        from: query.window.from,
        to: query.window.to,
        limit: query.limit,
      }),
    fetchArrows: (window: TimelineWindow) =>
      getArrows({
        from: window.from,
        to: window.to,
      }),
    fetchSlackThread: (channelId: string, threadTs: string) =>
      getSlackThread(channelId, threadTs),
  };
}

export const timelineApiDataSource = createTimelineApiDataSource();

