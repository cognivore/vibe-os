import type {
  EventEnvelope,
  Identity,
  Persona,
} from "../../../types/core";
import { Badge } from "../../ui/badge";
import type {
  LinearEventData,
  PersonaClickTarget,
  SlackEventData,
  SlackLinkTargets,
} from "../types";
import { ActorChip } from "./ActorChip";

interface EventEntryProps {
  event: EventEnvelope;
  identityLookup: Record<string, Identity>;
  personaLookup: Record<string, { persona: Persona; identityId: string }>;
  onPersonaClick?: (target: PersonaClickTarget) => void;
}

export function EventEntry({
  event,
  identityLookup,
  personaLookup,
  onPersonaClick,
}: EventEntryProps) {
  return (
    <li className="p-4">
      <div className="flex flex-wrap items-center justify-between gap-2">
        <div className="space-y-1">
          <Badge variant="secondary">{event.domain}</Badge>
          <p className="text-sm font-medium">{event.summary}</p>
          <p className="text-xs text-muted-foreground">{event.kind}</p>
          {event.domain === "slack" ? (
            <div className="space-y-2">
              <SlackEventBody event={event} />
              <SlackMessageLinks event={event} />
            </div>
          ) : event.domain === "linear" ? (
            <LinearEventBody event={event} />
          ) : null}
          <ActorChip
            event={event}
            identities={identityLookup}
            personas={personaLookup}
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

export function SlackEventBody({ event }: { event: EventEnvelope }) {
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
          {normalizeSlackMarkup(text)}
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

function LinearEventBody({ event }: { event: EventEnvelope }) {
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

function normalizeSlackMarkup(text: string) {
  return text
    .replace(/<@([^>|]+)>/g, (_match, userId) => `@${userId}`)
    .replace(/<!([^>|]+)(?:\|([^>]+))?>/g, (_match, command, label) =>
      label ? label : command,
    )
    .replace(/<([^>|]+)\|([^>]+)>/g, (_match, url, label) => `${label} (${url})`)
    .replace(/<([^>]+)>/g, (_match, value) => value)
    .replace(/:[a-z0-9_+-]+:/gi, "");
}

