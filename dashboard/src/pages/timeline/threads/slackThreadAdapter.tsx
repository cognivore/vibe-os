import { useMemo } from "react";
import { Button } from "../../../components/ui/button";
import {
  SlackEventBody,
  SlackMessageLinks,
  type SlackEventData,
  type SlackThreadEntry,
  type PersonaClickTarget,
} from "../../../components/timeline/TimelineEntryList";
import type { EventEnvelope, Identity, Persona } from "../../../types/core";
import type {
  ThreadAdapter,
  ThreadPanelProps,
  TimelineDataSource,
} from "../adapters";
import { parseSlackThreadKey } from "../threadUtils";

export const slackThreadAdapter: ThreadAdapter<SlackThreadEntry> = {
  kind: "slack_thread",
  domains: ["slack"],
  buildEntries: buildSlackThreadEntries,
  hydrate: hydrateSlackThread,
  matches: (entry): entry is SlackThreadEntry => entry.type === "slack_thread",
  Panel: SlackThreadPanel,
};

function buildSlackThreadEntries(events: EventEnvelope[]): SlackThreadEntry[] {
  const map = new Map<string, { channelId?: string; messages: EventEnvelope[] }>();
  events
    .filter((event) => event.domain === "slack")
    .forEach((event) => {
      const data = event.data as SlackEventData;
      const ts =
        typeof data.ts === "string"
          ? data.ts
          : event.data && (event.data as { ts?: string }).ts;
      const threadTs =
        typeof data.thread_ts === "string" && data.thread_ts
          ? data.thread_ts
          : ts ?? event.id;
      const channelId =
        (typeof data.channel === "string" && data.channel) ||
        event.entity_id ||
        "channel";
      const threadKey = `${channelId}:${threadTs}`;
      if (!map.has(threadKey)) {
        map.set(threadKey, { channelId, messages: [] });
      }
      map.get(threadKey)!.messages.push(event);
    });

  return Array.from(map.entries())
    .filter(([, { messages }]) => messages.length > 0)
    .map(([threadId, { channelId, messages }]) => {
      const sorted = [...messages].sort(
        (a, b) => new Date(a.at).getTime() - new Date(b.at).getTime(),
      );
      const threadTs = threadId.split(":").slice(-1)[0];
      const root =
        sorted.find((msg) => {
          const data = msg.data as SlackEventData;
          const ts = typeof data.ts === "string" ? data.ts : undefined;
          const thread =
            typeof data.thread_ts === "string" && data.thread_ts
              ? data.thread_ts
              : ts;
          return thread === threadTs;
        }) ?? sorted[0];
      const replies = sorted.filter((msg) => msg !== root);
      const latestAt = sorted[sorted.length - 1]?.at ?? root.at;
      return {
        type: "slack_thread" as const,
        threadId,
        channelId,
        root,
        replies,
        at: latestAt,
      };
    });
}

async function hydrateSlackThread(
  entry: SlackThreadEntry,
  dataSource: TimelineDataSource,
): Promise<SlackThreadEntry> {
  const parsed = parseSlackThreadKey(entry.threadId);
  if (!parsed) {
    throw new Error("Invalid Slack thread identifier");
  }
  const response = await dataSource.fetchSlackThread(parsed.channelId, parsed.threadTs);
  const latestAt =
    response.replies.length > 0
      ? response.replies[response.replies.length - 1].at
      : response.root.at;
  return {
    ...entry,
    channelId: response.channel_id ?? entry.channelId,
    root: response.root,
    replies: response.replies,
    at: latestAt,
  };
}

function SlackThreadPanel({
  entry: thread,
  identityLookup,
  personaLookup,
  onClose,
  loading,
  error,
}: ThreadPanelProps<SlackThreadEntry>) {
  const messages = useMemo(
    () =>
      [thread.root, ...thread.replies].sort(
        (a, b) => new Date(a.at).getTime() - new Date(b.at).getTime(),
      ),
    [thread],
  );

  const rootData = thread.root.data as SlackEventData & { reply_count?: number };
  const totalReplyCount =
    typeof rootData.reply_count === "number" ? rootData.reply_count : null;
  const hasMoreMessages =
    totalReplyCount !== null && thread.replies.length < totalReplyCount;

  return (
    <div className="flex h-full flex-col gap-4">
      <div className="flex items-start justify-between gap-2">
        <div>
          <p className="text-sm font-semibold">
            Slack thread {thread.channelId ? `· #${thread.channelId}` : ""}
          </p>
          <p className="text-xs text-muted-foreground">
            {messages.length} message{messages.length === 1 ? "" : "s"}
            {hasMoreMessages && ` (${totalReplyCount! + 1} total in Slack)`}
          </p>
          {hasMoreMessages && (
            <p className="text-xs text-yellow-600 dark:text-yellow-500">
              Some messages are outside the current time window
            </p>
          )}
        </div>
        <Button variant="ghost" size="sm" onClick={onClose}>
          Close
        </Button>
      </div>
      {loading ? (
        <p className="text-xs text-muted-foreground">Refreshing full thread…</p>
      ) : null}
      {error ? (
        <p className="text-xs text-destructive-foreground">{error}</p>
      ) : null}
      <div className="flex-1 space-y-3 overflow-y-auto pr-2">
        {messages.map((message) => (
          <SlackThreadMessage
            key={message.id}
            event={message}
            identityLookup={identityLookup}
            personaLookup={personaLookup}
          />
        ))}
      </div>
    </div>
  );
}

function SlackThreadMessage({
  event,
  identityLookup,
  personaLookup,
}: {
  event: EventEnvelope;
  identityLookup: Record<string, Identity>;
  personaLookup: Record<string, { persona: Persona; identityId: string }>;
}) {
  const actorLabel = resolveSlackActor(event, identityLookup, personaLookup);
  return (
    <div className="rounded-lg border border-border bg-background/80 p-3 shadow-sm">
      <div className="flex flex-wrap items-center justify-between gap-2 text-xs text-muted-foreground">
        <div className="font-medium text-foreground">{actorLabel}</div>
        <div className="flex flex-col items-end gap-1 text-right">
          <p>{new Date(event.at).toLocaleString()}</p>
          <SlackMessageLinks event={event} className="justify-end" />
        </div>
      </div>
      <div className="mt-2 text-sm">
        <SlackEventBody event={event} />
      </div>
    </div>
  );
}

function resolveSlackActor(
  event: EventEnvelope,
  identityLookup: Record<string, Identity>,
  personaLookup: Record<string, { persona: Persona; identityId: string }>,
) {
  if (event.actor_identity_id && identityLookup[event.actor_identity_id]) {
    const identity = identityLookup[event.actor_identity_id];
    return identity.preferred_name ?? identity.canonical_email;
  }
  if (event.actor_persona_id && personaLookup[event.actor_persona_id]) {
    const info = personaLookup[event.actor_persona_id];
    return info.persona.label ?? `@${info.persona.key.local_id}`;
  }
  if (event.actor_persona_key) {
    return `@${event.actor_persona_key.local_id}`;
  }
  const data = event.data as SlackEventData;
  if (data.user) {
    return `@${data.user}`;
  }
  return "Unknown actor";
}

