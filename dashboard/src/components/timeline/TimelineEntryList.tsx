import { type ReactNode } from "react";
import type {
  Arrow,
  EventEnvelope,
  Identity,
  Persona,
  PersonaKey,
} from "../../types/core";
import { Badge } from "../ui/badge";
import { Button } from "../ui/button";

export interface SlackThreadEntry {
  type: "slack_thread";
  threadId: string;
  channelId?: string;
  at: string;
  root: EventEnvelope;
  replies: EventEnvelope[];
}

export type TimelineEntry =
  | { type: "event"; at: string; event: EventEnvelope }
  | { type: "arrow"; at: string; arrow: Arrow }
  | SlackThreadEntry
  | LinearThreadEntry;

export interface LinearThreadEntry {
  type: "linear_thread";
  issueId: string;
  issueIdentifier?: string;
  issueTitle?: string;
  issueUrl?: string;
  issueDescription?: string;
  comments: EventEnvelope[];
  events: EventEnvelope[];
  at: string;
}

export interface PersonaClickTarget {
  identityId?: string;
  persona?: PersonaKey;
  label?: string;
}

interface TimelineEntryListProps {
  entries: TimelineEntry[];
  domainLookup?: Record<string, string>;
  identityLookup: Record<string, Identity>;
  personaLookup: Record<string, { persona: Persona; identityId: string }>;
  onPersonaClick?: (target: PersonaClickTarget) => void;
  onThreadSelect?: (thread: SlackThreadEntry | LinearThreadEntry) => void;
  activeThreadKey?: string;
}

export function TimelineEntryList({
  entries,
  domainLookup = {},
  identityLookup,
  personaLookup,
  onPersonaClick,
  onThreadSelect,
  activeThreadKey,
}: TimelineEntryListProps) {
  return (
    <ul className="divide-y divide-border">
      {entries.map((entry) => {
        if (entry.type === "event" && entry.event) {
          const event = entry.event;
          return (
            <li key={`event-${event.id}`} className="p-4">
              <div className="flex flex-wrap items-center justify-between gap-2">
                <div className="space-y-1">
                  <Badge variant="secondary">{event.domain}</Badge>
                  <p className="text-sm font-medium">{event.summary}</p>
                  <p className="text-xs text-muted-foreground">{event.kind}</p>
                  {renderEventBody(event)}
                  {renderActorChip(
                    event,
                    identityLookup,
                    personaLookup,
                    onPersonaClick,
                  )}
                </div>
                <p className="text-xs text-muted-foreground">
                  {new Date(event.at).toLocaleString()}
                </p>
              </div>
            </li>
          );
        }

        if (entry.type === "slack_thread") {
          return renderSlackThreadPreview(
            entry,
            domainLookup,
            identityLookup,
            personaLookup,
            onPersonaClick,
            onThreadSelect,
            activeThreadKey === `slack:${entry.threadId}`,
          );
        }

        if (entry.type === "linear_thread") {
          return renderLinearThreadPreview(
            entry,
            identityLookup,
            personaLookup,
            onPersonaClick,
            onThreadSelect,
            activeThreadKey === `linear:${entry.issueId}`,
          );
        }

        if (entry.type === "arrow" && entry.arrow) {
          const arrow = entry.arrow;
          return (
            <li key={`arrow-${arrow.id}`} className="bg-muted/20 p-4">
              <div className="flex flex-wrap items-center justify-between gap-2">
                <div className="space-y-1">
                  <Badge>{arrow.direction.replace("_", " ")}</Badge>
                  <p className="text-sm font-semibold">{arrow.title}</p>
                  <p className="text-xs text-muted-foreground">
                    {arrow.source.kind === "domain_object"
                      ? `${domainLookup[arrow.source.domain] ?? arrow.source.domain} → ${
                          domainLookup[arrow.target.domain] ?? arrow.target.domain
                        }`
                      : `${arrow.source.domain} → ${arrow.target.domain}`}
                  </p>
                  {renderArrowAuthorChip(
                    arrow,
                    identityLookup,
                    personaLookup,
                    onPersonaClick,
                  )}
                </div>
                <p className="text-xs text-muted-foreground">
                  {new Date(arrow.created_at).toLocaleString()}
                </p>
              </div>
              <p className="mt-2 text-sm text-muted-foreground">
                {arrow.detail_markdown}
              </p>
            </li>
          );
        }
        return null;
      })}
    </ul>
  );
}

function renderActorChip(
  event: EventEnvelope,
  identities: Record<string, Identity>,
  personas: Record<string, { persona: Persona; identityId: string }>,
  onPersonaClick?: (target: PersonaClickTarget) => void,
) {
  if (event.actor_identity_id && identities[event.actor_identity_id]) {
    const identity = identities[event.actor_identity_id];
    const personaInfo = event.actor_persona_id
      ? personas[event.actor_persona_id]
      : undefined;
    const personaKey: PersonaKey | undefined =
      personaInfo?.persona.key ?? normalizePersonaKey(event.actor_persona_key);
    return (
      <Badge
        variant="outline"
        className="cursor-pointer"
        onClick={() =>
          onPersonaClick?.({
            identityId: identity.id,
            persona: personaKey,
            label:
              identity.preferred_name ?? identity.canonical_email ?? undefined,
          })
        }
      >
        {identity.preferred_name ?? identity.canonical_email}
        {personaInfo
          ? ` (${personaInfo.persona.key.domain}:${personaInfo.persona.key.local_id})`
          : null}
      </Badge>
    );
  }
  if (event.actor_persona_key) {
    const personaKey = event.actor_persona_key;
    return (
      <Badge
        variant="outline"
        className="cursor-pointer"
        onClick={() =>
          onPersonaClick?.({ persona: personaKey, label: undefined })
        }
      >
        @{personaKey.local_id} ({personaKey.domain})
      </Badge>
    );
  }
  return null;
}

function renderArrowAuthorChip(
  arrow: Arrow,
  identities: Record<string, Identity>,
  personas: Record<string, { persona: Persona; identityId: string }>,
  onPersonaClick?: (target: PersonaClickTarget) => void,
) {
  if (arrow.author_identity_id && identities[arrow.author_identity_id]) {
    const identity = identities[arrow.author_identity_id];
    const personaInfo = arrow.author_persona_id
      ? personas[arrow.author_persona_id]
      : undefined;
    const personaKey: PersonaKey | undefined =
      personaInfo?.persona.key ??
      normalizePersonaKey(arrow.author_persona_key);
    return (
      <Badge
        variant="outline"
        className="cursor-pointer"
        onClick={() =>
          onPersonaClick?.({
            identityId: identity.id,
            persona: personaKey,
            label:
              identity.preferred_name ?? identity.canonical_email ?? undefined,
          })
        }
      >
        {identity.preferred_name ?? identity.canonical_email}
        {personaInfo
          ? ` (${personaInfo.persona.key.domain}:${personaInfo.persona.key.local_id})`
          : null}
      </Badge>
    );
  }
  if (arrow.author_persona_key) {
    const personaKey = arrow.author_persona_key;
    return (
      <Badge
        variant="outline"
        className="cursor-pointer"
        onClick={() =>
          onPersonaClick?.({
            persona: personaKey,
            label: undefined,
          })
        }
      >
        {personaKey.domain}:{personaKey.local_id}
      </Badge>
    );
  }
  return null;
}

function renderEventBody(event: EventEnvelope): ReactNode {
  if (event.domain === "slack") {
    return (
      <div className="space-y-2">
        <SlackEventBody event={event} />
        <SlackMessageLinks event={event} />
      </div>
    );
  }
  if (event.domain === "linear") {
    return <LinearEventBody event={event} />;
  }
  return null;
}

export type SlackAttachment = {
  title?: string;
  title_link?: string;
  fallback?: string;
  text?: string;
};

export type SlackFile = {
  id?: string;
  name?: string;
  permalink?: string;
  url_private?: string;
};

export type SlackEventData = {
  text?: string;
  attachments?: SlackAttachment[];
  files?: SlackFile[];
  ts?: string;
  thread_ts?: string;
  channel?: string;
  user?: string;
  team?: string;
  team_id?: string;
};

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

type SlackLinkTargets = {
  webUrl: string;
  desktopUrl: string;
};

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

export type LinearEventData = {
  issue_identifier?: string;
  issue_id?: string;
  issue_title?: string | null;
  issue_description?: string | null;
  issue_url?: string | null;
  comment_body?: string | null;
  comment_url?: string | null;
  event_kind?: string;
  actor_name?: string | null;
  actor_display_name?: string | null;
  from_state?: string | null;
  to_state?: string | null;
  from_priority?: number | null;
  to_priority?: number | null;
  extra?: Record<string, unknown>;
};

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

function extractLinearCommentSnippet(event: EventEnvelope) {
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
    .replace(/<([^>]+)>/g, (_match, value) => value);
}

function normalizePersonaKey(
  key?: PersonaKey | null,
): PersonaKey | undefined {
  return key ?? undefined;
}

function renderSlackThreadPreview(
  entry: SlackThreadEntry,
  domainLookup: Record<string, string>,
  identityLookup: Record<string, Identity>,
  personaLookup: Record<string, { persona: Persona; identityId: string }>,
  onPersonaClick?: (target: PersonaClickTarget) => void,
  onThreadSelect?: (thread: SlackThreadEntry) => void,
  isActive?: boolean,
) {
  const replyCount = entry.replies.length;
  const channelLabel =
    domainLookup["slack"] && entry.channelId
      ? `${domainLookup["slack"]} · #${entry.channelId}`
      : entry.channelId
        ? `#${entry.channelId}`
        : "Slack thread";
  const latestTime = new Date(entry.at).toLocaleString();
  return (
    <li
      key={`slack-thread-${entry.threadId}`}
      className={`p-4 ${isActive ? "bg-primary/10" : "bg-muted/10"}`}
    >
      <div className="flex flex-wrap items-center justify-between gap-2">
        <div className="space-y-2">
          <div className="flex flex-wrap items-center gap-2">
            <Badge variant="secondary">Slack thread</Badge>
            <p className="text-sm font-medium">{channelLabel}</p>
          </div>
          <SlackEventBody event={entry.root} />
          <SlackMessageLinks event={entry.root} />
          <div className="text-xs text-muted-foreground">
            {replyCount > 0
              ? `${replyCount} repl${replyCount === 1 ? "y" : "ies"}`
              : "No replies yet"}
          </div>
          <div className="flex flex-wrap gap-2 text-xs text-muted-foreground">
            {renderActorChip(
              entry.root,
              identityLookup,
              personaLookup,
              onPersonaClick,
            )}
          </div>
        </div>
        <div className="flex flex-col items-end gap-2 text-xs text-muted-foreground">
          <p>{latestTime}</p>
          <Button
            size="sm"
            variant="outline"
            onClick={() => onThreadSelect?.(entry)}
          >
            View thread
          </Button>
        </div>
      </div>
    </li>
  );
}

function renderLinearThreadPreview(
  entry: LinearThreadEntry,
  identities: Record<string, Identity>,
  personas: Record<string, { persona: Persona; identityId: string }>,
  onPersonaClick?: (target: PersonaClickTarget) => void,
  onThreadSelect?: (thread: LinearThreadEntry) => void,
  isActive?: boolean,
) {
  const commentCount = entry.comments.length;
  const latestComment = entry.comments[entry.comments.length - 1];
  const issueTitle =
    entry.issueTitle ??
    entry.issueIdentifier ??
    entry.events[0]?.summary ??
    "Linear issue";
  const previewText =
    (latestComment && extractLinearCommentSnippet(latestComment)) ||
    entry.issueDescription ||
    "No comments yet";
  const latestTime = new Date(entry.at).toLocaleString();

  return (
    <li
      key={`linear-thread-${entry.issueId}`}
      className={`p-4 ${isActive ? "bg-primary/10" : "bg-muted/10"}`}
    >
      <div className="flex flex-wrap items-center justify-between gap-2">
        <div className="space-y-2">
          <div className="flex flex-wrap items-center gap-2">
            <Badge variant="secondary">
              Linear issue {entry.issueIdentifier ? `· ${entry.issueIdentifier}` : ""}
            </Badge>
            {entry.issueUrl ? (
              <a
                href={entry.issueUrl}
                target="_blank"
                rel="noreferrer"
                className="text-sm font-medium underline"
              >
                {issueTitle}
              </a>
            ) : (
              <p className="text-sm font-medium">{issueTitle}</p>
            )}
          </div>
          <p className="text-sm text-muted-foreground line-clamp-2">{previewText}</p>
          <div className="text-xs text-muted-foreground">
            {commentCount > 0
              ? `${commentCount} comment${commentCount === 1 ? "" : "s"}`
              : "No comments yet"}
          </div>
          {latestComment
            ? renderActorChip(
                latestComment,
                identities,
                personas,
                onPersonaClick,
              )
            : null}
        </div>
        <div className="flex flex-col items-end gap-2 text-xs text-muted-foreground">
          <p>{latestTime}</p>
          <Button
            size="sm"
            variant="outline"
            onClick={() => onThreadSelect?.(entry)}
          >
            View issue
          </Button>
        </div>
      </div>
    </li>
  );
}

