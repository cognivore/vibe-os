import type { ThreadEntry } from "./adapters";

export function threadKey(thread: ThreadEntry) {
  return thread.type === "slack_thread"
    ? `slack:${thread.threadId}`
    : `linear:${thread.issueId}`;
}

export function parseSlackThreadKey(threadId: string) {
  const [channelId, ...rest] = threadId.split(":");
  if (!channelId || rest.length === 0) {
    return null;
  }
  return { channelId, threadTs: rest.join(":") };
}

