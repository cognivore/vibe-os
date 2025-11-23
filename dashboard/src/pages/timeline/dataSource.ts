import {
  getArrows,
  getDomains,
  getEvents,
  getLinearIssue,
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
        cursor: query.cursor,
      }),
    fetchArrows: (window: TimelineWindow) =>
      getArrows({
        from: window.from,
        to: window.to,
      }),
    fetchSlackThread: (channelId: string, threadTs: string) =>
      getSlackThread(channelId, threadTs),
    fetchLinearIssue: (issueRef: string) => getLinearIssue(issueRef),
  };
}

export const timelineApiDataSource = createTimelineApiDataSource();

