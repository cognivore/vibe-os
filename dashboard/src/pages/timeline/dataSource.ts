import {
  getDomains,
  getEvents,
  getLinearIssue,
  getSlackThread,
  listIdentities,
} from "../../api/client";
import type {
  TimelineDataQuery,
  TimelineDataSource,
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
        cursor: query.cursor,
      }),
    // Arrows are deprecated - return empty array
    fetchArrows: () => Promise.resolve([]),
    fetchSlackThread: (channelId: string, threadTs: string) =>
      getSlackThread(channelId, threadTs),
    fetchLinearIssue: (issueRef: string) => getLinearIssue(issueRef),
  };
}

export const timelineApiDataSource = createTimelineApiDataSource();

