import type {
  EventEnvelope,
  Identity,
  Persona,
} from "../../../types/core";
import { Badge } from "../../ui/badge";
import type {
  LinearEventData,
  LinearThreadEntry,
  PersonaClickTarget,
  SlackEventData,
  SlackLinkTargets,
  SlackThreadEntry,
} from "../types";
import { ActorChip } from "./ActorChip";

interface EventEntryProps {
  event: EventEnvelope & { thread_name?: string | null };
  identityLookup: Record<string, Identity>;
  personaLookup: Record<string, { persona: Persona; identityId: string }>;
  providerPersonaLabels?: Record<string, string>;
  onPersonaClick?: (target: PersonaClickTarget) => void;
  onThreadClick?: (thread: SlackThreadEntry | LinearThreadEntry) => void;
}

export function EventEntry({
  event,
  identityLookup,
  personaLookup,
  providerPersonaLabels = {},
  onPersonaClick,
  onThreadClick,
}: EventEntryProps) {
  const threadName = (event as { thread_name?: string | null }).thread_name;
  const threadEntry = extractThreadEntry(event);
  const canNavigateToThread = onThreadClick && threadEntry;

  const handleThreadClick = () => {
    if (canNavigateToThread) {
      onThreadClick(threadEntry);
    }
  };

  const userLookup = buildSlackUserLookup(
    Object.values(identityLookup),
    providerPersonaLabels,
  );

  return (
    <li className="p-4">
      <div className="flex flex-wrap items-center justify-between gap-2">
        <div className="space-y-1">
          <Badge variant="secondary">{event.domain}</Badge>
          <p className="text-sm font-medium">{event.summary}</p>
          {threadName && (
            <button
              type="button"
              onClick={handleThreadClick}
              disabled={!canNavigateToThread}
              className={`text-xs italic text-left ${canNavigateToThread
                  ? "text-primary hover:underline cursor-pointer"
                  : "text-muted-foreground"
                }`}
            >
              ↪ {threadName} {canNavigateToThread && "→"}
            </button>
          )}
          {!threadName && canNavigateToThread && (
            <button
              type="button"
              onClick={handleThreadClick}
              className="text-xs text-primary hover:underline cursor-pointer"
            >
              View thread →
            </button>
          )}
          <p className="text-xs text-muted-foreground">{event.kind}</p>
          {event.domain === "slack" ? (
            <div className="space-y-2">
              <SlackEventBody event={event} userLookup={userLookup} />
              <SlackMessageLinks event={event} />
            </div>
          ) : event.domain === "linear" ? (
            <LinearEventBody event={event} />
          ) : null}
          <ActorChip
            event={event}
            identities={identityLookup}
            personas={personaLookup}
            providerPersonaLabels={providerPersonaLabels}
            onPersonaClick={onPersonaClick}
          />
        </div>
        <p className="text-xs text-muted-foreground">
          {new Date(event.at).toLocaleString()}
        </p>
      </div>
    </li>
  );
}

/**
 * Extract a minimal thread entry from an event for navigation purposes.
 * The thread will be hydrated with full data when selected.
 */
function extractThreadEntry(
  event: EventEnvelope,
): SlackThreadEntry | LinearThreadEntry | null {
  if (event.domain === "slack") {
    return extractSlackThreadEntry(event);
  }
  if (event.domain === "linear") {
    return extractLinearThreadEntry(event);
  }
  return null;
}

function extractSlackThreadEntry(event: EventEnvelope): SlackThreadEntry | null {
  const data = event.data as SlackEventData;
  const channel =
    (typeof data.channel === "string" && data.channel.trim()) ||
    (typeof event.entity_id === "string" && event.entity_id.split(":")[0]?.trim()) ||
    null;
  // Use thread_ts if available (message is part of a thread), otherwise use ts (message itself)
  const threadTs =
    (typeof data.thread_ts === "string" && data.thread_ts.trim()) ||
    (typeof data.ts === "string" && data.ts.trim()) ||
    null;

  if (!channel || !threadTs) {
    return null;
  }

  const threadId = `${channel}:${threadTs}`;
  return {
    type: "slack_thread",
    threadId,
    channelId: channel,
    at: event.at,
    // Minimal entry - will be hydrated when selected
    root: event,
    replies: [],
  };
}

function extractLinearThreadEntry(event: EventEnvelope): LinearThreadEntry | null {
  const data = event.data as LinearEventData;
  const issueId = data.issue_id || null;

  if (!issueId) {
    return null;
  }

  return {
    type: "linear_thread",
    issueId,
    issueIdentifier: data.issue_identifier ?? undefined,
    issueTitle: data.issue_title ?? undefined,
    issueUrl: data.issue_url ?? undefined,
    issueDescription: data.issue_description ?? undefined,
    at: event.at,
    // Minimal entry - will be hydrated when selected
    comments: [],
    events: [event],
  };
}

export function SlackEventBody({
  event,
  userLookup,
}: {
  event: EventEnvelope;
  userLookup?: Record<string, string>;
}) {
  const data = event.data as SlackEventData;
  const text = typeof data.text === "string" ? data.text : null;
  const attachments = Array.isArray(data.attachments) ? data.attachments : [];
  const files = Array.isArray(data.files) ? data.files : [];

  if (!text && attachments.length === 0 && files.length === 0) {
    return null;
  }

  return (
    <div className="space-y-2 pt-1 text-sm text-foreground">
      {text ? (
        <p className="whitespace-pre-wrap text-sm">
          {normalizeSlackMarkup(text, userLookup)}
        </p>
      ) : null}
      {attachments.length ? (
        <div className="space-y-1 text-xs text-muted-foreground">
          {attachments.map((attachment, idx) => {
            const title = attachment.title ?? attachment.fallback;
            const url = attachment.title_link;
            if (!title) return null;
            return (
              <a
                key={`${attachment.title ?? attachment.fallback ?? idx}`}
                href={url ?? undefined}
                target="_blank"
                rel="noreferrer"
                className={url ? "underline" : undefined}
              >
                {title}
              </a>
            );
          })}
        </div>
      ) : null}
      {files.length ? (
        <div className="space-y-1 text-xs text-muted-foreground">
          {files.map((file) => (
            <a
              key={file.id ?? file.name}
              href={file.permalink ?? file.url_private ?? undefined}
              target="_blank"
              rel="noreferrer"
              className="underline"
            >
              {file.name ?? file.id ?? "View file"}
            </a>
          ))}
        </div>
      ) : null}
    </div>
  );
}

export function SlackMessageLinks({
  event,
  className = "",
}: {
  event: EventEnvelope;
  className?: string;
}) {
  const targets = buildSlackLinkTargets(event);
  if (!targets) {
    return null;
  }
  return (
    <div
      className={`flex flex-wrap items-center gap-2 text-[11px] font-medium text-primary ${className}`}
    >
      <span className="text-muted-foreground">Open:</span>
      <a
        href={targets.webUrl}
        target="_blank"
        rel="noreferrer"
        className="underline decoration-dotted underline-offset-2"
      >
        Web
      </a>
      <span className="text-muted-foreground">/</span>
      <a
        href={targets.desktopUrl}
        className="underline decoration-dotted underline-offset-2"
      >
        App
      </a>
    </div>
  );
}

function buildSlackLinkTargets(event: EventEnvelope): SlackLinkTargets | null {
  const data = event.data as SlackEventData;
  const team =
    (typeof data.team === "string" && data.team.trim()) ||
    (typeof data.team_id === "string" && data.team_id.trim()) ||
    null;
  const channel =
    (typeof data.channel === "string" && data.channel.trim()) ||
    (typeof event.entity_id === "string" &&
      event.entity_id.split(":")[0]?.trim()) ||
    null;
  const threadTs =
    typeof data.thread_ts === "string" && data.thread_ts.trim().length
      ? data.thread_ts.trim()
      : typeof data.ts === "string"
        ? data.ts.trim()
        : null;
  if (!team || !channel || !threadTs) {
    return null;
  }
  return {
    webUrl: `https://app.slack.com/client/${team}/${channel}/${threadTs}`,
    desktopUrl: `slack://channel?team=${encodeURIComponent(team)}&id=${encodeURIComponent(
      channel,
    )}`,
  };
}

export function LinearEventBody({ event }: { event: EventEnvelope }) {
  const data = event.data as LinearEventData;
  const title =
    data.issue_title ??
    data.issue_identifier ??
    data.issue_id ??
    event.summary ??
    null;
  const description =
    typeof data.issue_description === "string" &&
      data.issue_description.trim().length
      ? data.issue_description.trim()
      : extractLinearNote(data.extra);
  const stateChange =
    data.from_state || data.to_state
      ? `State: ${data.from_state ?? "—"} → ${data.to_state ?? "—"}`
      : null;
  const priorityChange =
    typeof data.from_priority === "number" ||
      typeof data.to_priority === "number"
      ? `Priority: ${formatLinearPriority(data.from_priority)} → ${formatLinearPriority(
        data.to_priority,
      )}`
      : null;

  if (!title && !description && !stateChange && !priorityChange) {
    return null;
  }

  return (
    <div className="space-y-2 pt-1 text-sm text-foreground">
      {title ? (
        data.issue_url ? (
          <a
            href={data.issue_url}
            target="_blank"
            rel="noreferrer"
            className="font-semibold underline"
          >
            {title}
          </a>
        ) : (
          <p className="font-semibold">{title}</p>
        )
      ) : null}
      {description ? (
        <p className="whitespace-pre-wrap text-sm text-muted-foreground">
          {description}
        </p>
      ) : null}
      <div className="space-y-1 text-xs text-muted-foreground">
        {stateChange ? <p>{stateChange}</p> : null}
        {priorityChange ? <p>{priorityChange}</p> : null}
        {data.actor_name ? <p>Actor: {data.actor_name}</p> : null}
      </div>
    </div>
  );
}

function formatLinearPriority(value?: number | null) {
  if (value === null || value === undefined) {
    return "—";
  }
  return value.toString();
}

function extractLinearNote(extra?: Record<string, unknown>) {
  if (!extra) return null;
  const note = extra["note"];
  if (typeof note === "string" && note.trim().length) {
    return note.trim();
  }
  const description = extra["description"];
  if (typeof description === "string" && description.trim().length) {
    return description.trim();
  }
  return null;
}

export function extractLinearCommentSnippet(event: EventEnvelope) {
  const data = event.data as LinearEventData;
  const body =
    (typeof data.comment_body === "string" && data.comment_body) ||
    (typeof data.issue_description === "string" && data.issue_description) ||
    event.summary;
  return body ? body.slice(0, 160) : "Linear update";
}

/**
 * Builds a lookup map for resolving Slack user IDs to display names.
 * Priority: identity.preferred_name > persona.display_name > persona.label > providerPersonaLabels
 */
export function buildSlackUserLookup(
  identities: Identity[],
  providerPersonaLabels: Record<string, string>,
): Record<string, string> {
  const lookup: Record<string, string> = { ...providerPersonaLabels };

  // Override with identity-linked persona names (higher priority)
  for (const identity of identities) {
    for (const persona of identity.personas) {
      if (persona.key.domain === "slack") {
        const key = `slack:${persona.key.local_id}`;
        const resolved =
          identity.preferred_name ??
          persona.display_name ??
          persona.label ??
          lookup[key];
        if (resolved) {
          lookup[key] = resolved;
        }
      }
    }
  }

  return lookup;
}

export function normalizeSlackMarkup(
  text: string,
  userLookup?: Record<string, string>,
) {
  return text
    .replace(/<@([^>|]+)>/g, (_match, userId) => {
      const resolved = userLookup?.[`slack:${userId}`];
      return resolved ? `@${resolved}` : `@${userId}`;
    })
    .replace(/<!([^>|]+)(?:\|([^>]+))?>/g, (_match, command, label) =>
      label ? label : command,
    )
    .replace(/<([^>|]+)\|([^>]+)>/g, (_match, url, label) => `${label} (${url})`)
    .replace(/<([^>]+)>/g, (_match, value) => value)
    .replace(/:[a-z0-9_+-]+:/gi, "");
}

