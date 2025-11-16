import { type ReactNode } from "react";
import type {
  Arrow,
  EventEnvelope,
  Identity,
  Persona,
  PersonaKey,
} from "../../types/core";
import { Badge } from "../ui/badge";

export type TimelineEntry =
  | { type: "event"; at: string; event: EventEnvelope }
  | { type: "arrow"; at: string; arrow: Arrow };

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
}

export function TimelineEntryList({
  entries,
  domainLookup = {},
  identityLookup,
  personaLookup,
  onPersonaClick,
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
    return <SlackEventBody event={event} />;
  }
  if (event.domain === "linear") {
    return <LinearEventBody event={event} />;
  }
  return null;
}

type SlackAttachment = {
  title?: string;
  title_link?: string;
  fallback?: string;
  text?: string;
};

type SlackFile = {
  id?: string;
  name?: string;
  permalink?: string;
  url_private?: string;
};

type SlackEventData = {
  text?: string;
  attachments?: SlackAttachment[];
  files?: SlackFile[];
};

function SlackEventBody({ event }: { event: EventEnvelope }) {
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

type LinearEventData = {
  issue_identifier?: string;
  issue_id?: string;
  event_kind?: string;
  actor_name?: string | null;
  from_state?: string | null;
  to_state?: string | null;
  from_priority?: number | null;
  to_priority?: number | null;
  extra?: Record<string, unknown>;
};

function LinearEventBody({ event }: { event: EventEnvelope }) {
  const data = event.data as LinearEventData;
  const rows: string[] = [];
  if (data.issue_identifier) {
    rows.push(`Issue ${data.issue_identifier}`);
  } else if (data.issue_id) {
    rows.push(`Issue ${data.issue_id}`);
  }
  if (data.from_state || data.to_state) {
    rows.push(`State: ${data.from_state ?? "—"} → ${data.to_state ?? "—"}`);
  }
  if (
    typeof data.from_priority === "number" ||
    typeof data.to_priority === "number"
  ) {
    rows.push(
      `Priority: ${formatLinearPriority(data.from_priority)} → ${formatLinearPriority(
        data.to_priority,
      )}`,
    );
  }
  if (data.actor_name) {
    rows.push(`Actor: ${data.actor_name}`);
  }
  const extraNote = extractLinearNote(data.extra);
  if (extraNote) {
    rows.push(extraNote);
  }

  if (!rows.length) {
    return null;
  }

  return (
    <div className="space-y-1 pt-1 text-xs text-muted-foreground">
      {rows.map((row, idx) => (
        <p key={`${event.id}-row-${idx}`}>{row}</p>
      ))}
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

