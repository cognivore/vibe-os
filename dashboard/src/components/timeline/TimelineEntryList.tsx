import type { Arrow, Identity, Persona } from "../../types/core";
import { Badge } from "../ui/badge";
import type {
  LinearThreadEntry,
  PersonaClickTarget,
  SlackThreadEntry,
  TimelineEntry,
} from "./types";
import { ArrowAuthorChip } from "./entries/ActorChip";
import { EventEntry } from "./entries/EventEntry";
import { LinearThreadPreview } from "./entries/LinearThreadPreview";
import { SlackThreadPreview } from "./entries/SlackThreadPreview";

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
          return (
            <EventEntry
              key={`event-${entry.event.id}`}
              event={entry.event}
              identityLookup={identityLookup}
              personaLookup={personaLookup}
              onPersonaClick={onPersonaClick}
            />
          );
        }

        if (entry.type === "slack_thread") {
          return (
            <SlackThreadPreview
              key={`slack-thread-${entry.threadId}`}
              entry={entry}
              domainLookup={domainLookup}
              identityLookup={identityLookup}
              personaLookup={personaLookup}
              onPersonaClick={onPersonaClick}
              onThreadSelect={onThreadSelect}
              isActive={activeThreadKey === `slack:${entry.threadId}`}
            />
          );
        }

        if (entry.type === "linear_thread") {
          return (
            <LinearThreadPreview
              key={`linear-thread-${entry.issueId}`}
              entry={entry}
              identityLookup={identityLookup}
              personaLookup={personaLookup}
              onPersonaClick={onPersonaClick}
              onThreadSelect={onThreadSelect}
              isActive={activeThreadKey === `linear:${entry.issueId}`}
            />
          );
        }

        if (entry.type === "arrow" && entry.arrow) {
          return (
            <ArrowEntry
              key={`arrow-${entry.arrow.id}`}
              arrow={entry.arrow}
              domainLookup={domainLookup}
              identityLookup={identityLookup}
              personaLookup={personaLookup}
              onPersonaClick={onPersonaClick}
            />
          );
        }
        return null;
      })}
    </ul>
  );
}

interface ArrowEntryProps {
  arrow: Arrow;
  domainLookup: Record<string, string>;
  identityLookup: Record<string, Identity>;
  personaLookup: Record<string, { persona: Persona; identityId: string }>;
  onPersonaClick?: (target: PersonaClickTarget) => void;
}

function ArrowEntry({
  arrow,
  domainLookup,
  identityLookup,
  personaLookup,
  onPersonaClick,
}: ArrowEntryProps) {
  const sourceLabel =
    arrow.source.kind === "domain_object"
      ? domainLookup[arrow.source.domain] ?? arrow.source.domain
      : arrow.source.domain;
  const targetLabel =
    arrow.target.kind === "domain_object"
      ? domainLookup[arrow.target.domain] ?? arrow.target.domain
      : arrow.target.domain;

  return (
    <li className="bg-muted/20 p-4">
      <div className="flex flex-wrap items-center justify-between gap-2">
        <div className="space-y-1">
          <Badge>{arrow.direction.replace("_", " ")}</Badge>
          <p className="text-sm font-semibold">{arrow.title}</p>
          <p className="text-xs text-muted-foreground">
            {sourceLabel} → {targetLabel}
          </p>
          <ArrowAuthorChip
            arrow={arrow}
            identities={identityLookup}
            personas={personaLookup}
            onPersonaClick={onPersonaClick}
          />
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

