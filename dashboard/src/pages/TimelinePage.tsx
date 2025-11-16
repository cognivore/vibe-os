import { useEffect, useMemo, useState } from "react";
import { Link, useNavigate } from "react-router-dom";
import {
  getArrows,
  getDomains,
  getEvents,
  listIdentities,
} from "../api/client";
import type {
  Arrow,
  DomainDescriptor,
  EventEnvelope,
  Identity,
  Persona,
  PersonaKey,
} from "../types/core";
import { Button } from "../components/ui/button";
import { Card, CardContent, CardHeader, CardTitle } from "../components/ui/card";
import { ScrollArea } from "../components/ui/scroll-area";
import { format } from "date-fns";
import {
  TimelineEntryList,
  type TimelineEntry,
  type PersonaClickTarget,
  type SlackThreadEntry,
  type LinearThreadEntry,
  SlackEventBody,
  SlackMessageLinks,
  type SlackEventData,
  type LinearEventData,
} from "../components/timeline/TimelineEntryList";

type ThreadEntry = SlackThreadEntry | LinearThreadEntry;

const ISO_FORMAT = "yyyy-MM-dd'T'HH:mm:ss";
const DAY_IN_MS = 1000 * 60 * 60 * 24;

function defaultWindow() {
  const end = new Date();
  const start = new Date(end.getTime());
  start.setMonth(start.getMonth() - 3);
  return {
    from: start.toISOString(),
    to: end.toISOString(),
  };
}

function last24hWindow() {
  const end = new Date();
  const start = new Date(end.getTime() - DAY_IN_MS);
  return {
    from: start.toISOString(),
    to: end.toISOString(),
  };
}

export default function TimelinePage() {
  const navigate = useNavigate();
  const [{ from, to }, setWindow] = useState(defaultWindow);
  const [domains, setDomains] = useState<DomainDescriptor[]>([]);
  const [selectedDomains, setSelectedDomains] = useState<string[]>([]);
  const [events, setEvents] = useState<EventEnvelope[]>([]);
  const [arrows, setArrows] = useState<Arrow[]>([]);
  const [identities, setIdentities] = useState<Identity[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [selectedThread, setSelectedThread] = useState<ThreadEntry | null>(null);

  const loadIdentities = () => {
    listIdentities()
      .then(setIdentities)
      .catch((err) => setError((prev) => prev ?? err.message));
  };

  useEffect(() => {
    getDomains()
      .then((available) => {
        setDomains(available);
        if (selectedDomains.length === 0) {
          setSelectedDomains(available.map((d) => d.id));
        }
      })
      .catch((err) => setError(err.message));
    loadIdentities();
  }, []);

  useEffect(() => {
    if (!selectedDomains.length) return;
    setLoading(true);
    setError(null);
    Promise.all([
      getEvents({ domains: selectedDomains, from, to, limit: 200 }),
      getArrows({ from, to }),
    ])
      .then(([eventData, arrowData]) => {
        setEvents(eventData);
        setArrows(arrowData);
      })
      .catch((err) => setError(err.message))
      .finally(() => setLoading(false));
  }, [from, to, selectedDomains]);

  const handlePersonaClick = (target: PersonaClickTarget) => {
    if (target.identityId) {
      navigate(`/identities/${target.identityId}`);
      return;
    }
    if (target.persona) {
      // Navigate to Identities page with prepopulated persona data
      const params = new URLSearchParams({
        provider: target.persona.domain,
        local_id: target.persona.local_id,
      });
      if (target.label) {
        params.set("label", target.label);
      }
      navigate(`/identities?${params.toString()}`);
    }
  };


  const slackThreadEntries = useMemo(
    () => buildSlackThreadEntries(events.filter((event) => event.domain === "slack")),
    [events],
  );
  const linearThreadEntries = useMemo(
    () => buildLinearThreadEntries(events.filter((event) => event.domain === "linear")),
    [events],
  );

  useEffect(() => {
    if (!selectedThread) return;
    if (selectedThread.type === "slack_thread") {
      const updated = slackThreadEntries.find(
        (thread) => thread.threadId === selectedThread.threadId,
      );
      if (updated && updated !== selectedThread) {
        setSelectedThread(updated);
      }
    } else if (selectedThread.type === "linear_thread") {
      const updated = linearThreadEntries.find(
        (thread) => thread.issueId === selectedThread.issueId,
      );
      if (updated && updated !== selectedThread) {
        setSelectedThread(updated);
      }
    }
  }, [slackThreadEntries, linearThreadEntries, selectedThread]);

  const timeline = useMemo<TimelineEntry[]>(() => {
    const nonThreadEvents = events.filter(
      (event) => event.domain !== "slack" && event.domain !== "linear",
    );
    const entries: TimelineEntry[] = [
      ...nonThreadEvents.map((event) => ({
        type: "event" as const,
        at: event.at,
        event,
      })),
      ...arrows.map((arrow) => ({
        type: "arrow" as const,
        at: arrow.created_at,
        arrow,
      })),
      ...slackThreadEntries,
      ...linearThreadEntries,
    ];
    return entries.sort(
      (a, b) => new Date(b.at).getTime() - new Date(a.at).getTime(),
    );
  }, [events, arrows, slackThreadEntries, linearThreadEntries]);

  const domainLookup = useMemo(
    () =>
      Object.fromEntries(
        domains.map((domain) => [domain.id, domain.human_name]),
      ),
    [domains],
  );

  const identityLookup = useMemo(
    () =>
      Object.fromEntries(
        identities.map((identity) => [identity.id, identity]),
      ),
    [identities],
  );

  const personaLookup = useMemo(() => {
    const map: Record<
      string,
      { persona: Persona; identityId: string }
    > = {};
    identities.forEach((identity) => {
      identity.personas.forEach((persona) => {
        map[persona.id] = { persona, identityId: identity.id };
      });
    });
    return map;
  }, [identities]);

  return (
    <div className="space-y-6">
      <div className="flex flex-col gap-4 md:flex-row md:items-end md:justify-between">
        <div>
          <h2 className="text-2xl font-semibold">Unified timeline</h2>
          <p className="text-sm text-muted-foreground">
            Events and arrows across all domains
          </p>
        </div>
        <div className="flex flex-wrap gap-2">
          <label className="flex flex-col text-xs">
            From
            <input
              type="datetime-local"
              value={format(new Date(from), ISO_FORMAT)}
              onChange={(e) =>
                setWindow((prev) => ({
                  ...prev,
                  from: new Date(e.target.value).toISOString(),
                }))
              }
              className="rounded-md border border-input bg-background px-2 py-1 text-sm"
            />
          </label>
          <label className="flex flex-col text-xs">
            To
            <input
              type="datetime-local"
              value={format(new Date(to), ISO_FORMAT)}
              onChange={(e) =>
                setWindow((prev) => ({
                  ...prev,
                  to: new Date(e.target.value).toISOString(),
                }))
              }
              className="rounded-md border border-input bg-background px-2 py-1 text-sm"
            />
          </label>
          <Button variant="secondary" onClick={() => setWindow(last24hWindow())}>
            Last 24h
          </Button>
        </div>
      </div>

      <Card>
        <CardHeader className="flex flex-row items-center justify-between">
          <CardTitle>Domains</CardTitle>
          <div className="flex flex-wrap gap-2">
            {domains.map((domain) => {
              const active = selectedDomains.includes(domain.id);
              return (
                <label
                  key={domain.id}
                  className="flex items-center gap-1 text-sm"
                >
                  <input
                    type="checkbox"
                    checked={active}
                    onChange={(e) => {
                      setSelectedDomains((prev) =>
                        e.target.checked
                          ? [...new Set([...prev, domain.id])]
                          : prev.filter((d) => d !== domain.id),
                      );
                    }}
                  />
                  {domain.human_name}
                </label>
              );
            })}
          </div>
        </CardHeader>
        <CardContent>
          <div className="flex flex-col gap-1 text-sm text-muted-foreground">
            {error ? (
              <p className="text-destructive-foreground">{error}</p>
            ) : (
              <p>
                Showing {timeline.length} entries between{" "}
                {new Date(from).toLocaleString()} and{" "}
                {new Date(to).toLocaleString()}
              </p>
            )}
            <Link to="/identities" className="text-xs underline">
              Manage identities
            </Link>
          </div>
        </CardContent>
      </Card>

      <div className="grid gap-4 lg:grid-cols-[minmax(0,2fr)_minmax(320px,1fr)]">
        <ScrollArea className="h-[65vh] rounded-lg border border-border bg-card">
          {loading ? (
            <div className="p-6 text-sm text-muted-foreground">Loading...</div>
          ) : (
            <TimelineEntryList
              entries={timeline}
              domainLookup={domainLookup}
              identityLookup={identityLookup}
              personaLookup={personaLookup}
              onPersonaClick={handlePersonaClick}
              onThreadSelect={(thread) => setSelectedThread(thread)}
              activeThreadKey={
                selectedThread ? threadKey(selectedThread) : undefined
              }
            />
          )}
        </ScrollArea>
        <div
          className={`rounded-lg border border-border bg-card p-4 ${
            selectedThread ? "block" : "hidden lg:block"
          }`}
        >
          {selectedThread ? (
            selectedThread.type === "slack_thread" ? (
              <SlackThreadPanel
                thread={selectedThread}
                identityLookup={identityLookup}
                personaLookup={personaLookup}
                onClose={() => setSelectedThread(null)}
              />
            ) : (
              <LinearThreadPanel
                thread={selectedThread}
                identityLookup={identityLookup}
                personaLookup={personaLookup}
                onPersonaClick={handlePersonaClick}
                onClose={() => setSelectedThread(null)}
              />
            )
          ) : (
            <div className="text-sm text-muted-foreground">
              Select a Slack or Linear thread to inspect the conversation.
            </div>
          )}
        </div>
      </div>
    </div>
  );
}

function buildSlackThreadEntries(events: EventEnvelope[]): SlackThreadEntry[] {
  const map = new Map<string, { channelId?: string; messages: EventEnvelope[] }>();
  events.forEach((event) => {
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
    .map(([threadId, { channelId, messages }]): SlackThreadEntry => {
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
function buildLinearThreadEntries(events: EventEnvelope[]): LinearThreadEntry[] {
  const map = new Map<
    string,
    {
      issueId: string;
      issueIdentifier?: string;
      issueTitle?: string;
      issueUrl?: string;
      issueDescription?: string;
      comments: EventEnvelope[];
      events: EventEnvelope[];
      at: string;
    }
  >();

  events.forEach((event) => {
    const data = event.data as LinearEventData;
    const issueId = getLinearIssueId(event);
    if (!issueId) {
      return;
    }
    if (!map.has(issueId)) {
      map.set(issueId, {
        issueId,
        issueIdentifier: data.issue_identifier ?? undefined,
        issueTitle: data.issue_title ?? event.summary ?? undefined,
        issueUrl: data.issue_url ?? undefined,
        issueDescription: data.issue_description ?? undefined,
        comments: [],
        events: [],
        at: event.at,
      });
    }
    const entry = map.get(issueId)!;
    entry.issueIdentifier ??= data.issue_identifier ?? undefined;
    entry.issueTitle ??= data.issue_title ?? event.summary ?? undefined;
    entry.issueUrl ??= data.issue_url ?? undefined;
    entry.issueDescription ??= data.issue_description ?? undefined;
    if (new Date(event.at).getTime() > new Date(entry.at).getTime()) {
      entry.at = event.at;
    }
    if (event.kind === "linear.comment") {
      entry.comments.push(event);
    } else {
      entry.events.push(event);
    }
  });

  return Array.from(map.values())
    .map((entry) => ({
      type: "linear_thread" as const,
      issueId: entry.issueId,
      issueIdentifier: entry.issueIdentifier,
      issueTitle: entry.issueTitle,
      issueUrl: entry.issueUrl,
      issueDescription: entry.issueDescription,
      comments: entry.comments.sort(
        (a, b) => new Date(a.at).getTime() - new Date(b.at).getTime(),
      ),
      events: entry.events.sort(
        (a, b) => new Date(a.at).getTime() - new Date(b.at).getTime(),
      ),
      at: entry.at,
    }))
    .sort((a, b) => new Date(b.at).getTime() - new Date(a.at).getTime());
}

function SlackThreadPanel({
  thread,
  identityLookup,
  personaLookup,
  onClose,
}: {
  thread: SlackThreadEntry;
  identityLookup: Record<string, Identity>;
  personaLookup: Record<string, { persona: Persona; identityId: string }>;
  onClose: () => void;
}) {
  const messages = useMemo(
    () =>
      [thread.root, ...thread.replies].sort(
        (a, b) => new Date(a.at).getTime() - new Date(b.at).getTime(),
      ),
    [thread],
  );

  // Extract reply_count from root message if available
  const rootData = thread.root.data as SlackEventData & { reply_count?: number };
  const totalReplyCount = typeof rootData.reply_count === "number" ? rootData.reply_count : null;
  const hasMoreMessages = totalReplyCount !== null && thread.replies.length < totalReplyCount;

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

function resolveLinearActor(
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
  const data = event.data as LinearEventData;
  if (data.actor_display_name) {
    return data.actor_display_name;
  }
  if (data.actor_name) {
    return data.actor_name;
  }
  const body = getLinearCommentBody(event);
  return body ? "Linear commenter" : "Unknown actor";
}

function getLinearIssueId(event: EventEnvelope) {
  const data = event.data as LinearEventData;
  return (
    (typeof data.issue_id === "string" && data.issue_id) ??
    event.entity_id ??
    undefined
  );
}

function getLinearCommentBody(event: EventEnvelope) {
  const data = event.data as LinearEventData;
  if (typeof data.comment_body === "string" && data.comment_body.trim().length) {
    return data.comment_body.trim();
  }
  if (
    typeof data.issue_description === "string" &&
    data.issue_description.trim().length
  ) {
    return data.issue_description.trim();
  }
  return event.summary ?? "";
}

function threadKey(thread: ThreadEntry) {
  return thread.type === "slack_thread"
    ? `slack:${thread.threadId}`
    : `linear:${thread.issueId}`;
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

function LinearThreadPanel({
  thread,
  identityLookup,
  personaLookup,
  onPersonaClick,
  onClose,
}: {
  thread: LinearThreadEntry;
  identityLookup: Record<string, Identity>;
  personaLookup: Record<string, { persona: Persona; identityId: string }>;
  onPersonaClick: (target: PersonaClickTarget) => void;
  onClose: () => void;
}) {
  const comments = useMemo(
    () =>
      [...thread.comments].sort(
        (a, b) => new Date(a.at).getTime() - new Date(b.at).getTime(),
      ),
    [thread],
  );
  const issueTitle =
    thread.issueTitle ?? thread.issueIdentifier ?? "Linear issue";
  return (
    <div className="flex h-full flex-col gap-4">
      <div className="flex items-start justify-between gap-2">
        <div>
          <p className="text-sm font-semibold">
            {thread.issueUrl ? (
              <a
                href={thread.issueUrl}
                target="_blank"
                rel="noreferrer"
                className="underline"
              >
                {issueTitle}
              </a>
            ) : (
              issueTitle
            )}
          </p>
          {thread.issueIdentifier ? (
            <p className="text-xs text-muted-foreground">
              {thread.issueIdentifier}
            </p>
          ) : null}
          <p className="text-xs text-muted-foreground">
            {comments.length} comment{comments.length === 1 ? "" : "s"}
          </p>
        </div>
        <Button variant="ghost" size="sm" onClick={onClose}>
          Close
        </Button>
      </div>
      {thread.issueDescription ? (
        <p className="whitespace-pre-wrap text-sm text-muted-foreground">
          {thread.issueDescription}
        </p>
      ) : null}
      <div className="flex-1 space-y-3 overflow-y-auto pr-2">
        {comments.length ? (
          comments.map((comment) => (
            <LinearCommentMessage
              key={comment.id}
              event={comment}
              identityLookup={identityLookup}
              personaLookup={personaLookup}
            />
          ))
        ) : (
          <div className="text-sm text-muted-foreground">
            No comments yet. Check back after teammates respond.
          </div>
        )}
      </div>
    </div>
  );
}

function LinearCommentMessage({
  event,
  identityLookup,
  personaLookup,
}: {
  event: EventEnvelope;
  identityLookup: Record<string, Identity>;
  personaLookup: Record<string, { persona: Persona; identityId: string }>;
}) {
  const actorLabel = resolveLinearActor(event, identityLookup, personaLookup);
  const commentBody = getLinearCommentBody(event);
  const data = event.data as LinearEventData;
  return (
    <div className="rounded-lg border border-border bg-background/80 p-3 shadow-sm">
      <div className="flex flex-wrap items-center justify-between gap-2 text-xs text-muted-foreground">
        <div className="font-medium">{actorLabel}</div>
        <p>{new Date(event.at).toLocaleString()}</p>
      </div>
      <div className="mt-2 text-sm whitespace-pre-wrap">{commentBody}</div>
      {data.comment_url ? (
        <a
          href={data.comment_url}
          target="_blank"
          rel="noreferrer"
          className="text-xs text-primary underline"
        >
          Open in Linear
        </a>
      ) : null}
    </div>
  );
}
