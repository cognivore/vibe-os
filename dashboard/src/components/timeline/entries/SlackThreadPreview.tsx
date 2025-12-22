import { useMemo } from "react";
import type {
  Identity,
  Persona,
} from "../../../types/core";
import { Badge } from "../../ui/badge";
import { Button } from "../../ui/button";
import { ActorChip } from "./ActorChip";
import {
  SlackEventBody,
  SlackMessageLinks,
  buildSlackUserLookup,
  normalizeSlackMarkup,
} from "./EventEntry";
import type {
  PersonaClickTarget,
  SlackThreadEntry,
} from "../types";

interface SlackThreadPreviewProps {
  entry: SlackThreadEntry;
  domainLookup: Record<string, string>;
  identityLookup: Record<string, Identity>;
  personaLookup: Record<string, { persona: Persona; identityId: string }>;
  providerPersonaLabels: Record<string, string>;
  onPersonaClick?: (target: PersonaClickTarget) => void;
  onThreadSelect?: (thread: SlackThreadEntry) => void;
  isActive?: boolean;
  isSearchMode?: boolean;
}

export function SlackThreadPreview({
  entry,
  domainLookup,
  identityLookup,
  personaLookup,
  providerPersonaLabels,
  onPersonaClick,
  onThreadSelect,
  isActive,
  isSearchMode = false,
}: SlackThreadPreviewProps) {
  const replyCount = entry.replies.length;

  const userLookup = useMemo(
    () => buildSlackUserLookup(Object.values(identityLookup), providerPersonaLabels),
    [identityLookup, providerPersonaLabels],
  );

  // Use fetched thread title, fallback to channel ID
  const threadTitle = entry.threadTitle
    ? normalizeSlackMarkup(entry.threadTitle, userLookup)
    : (entry.channelId ? `#${entry.channelId}` : "Slack thread");

  const latestTime = new Date(entry.at).toLocaleString();
  return (
    <li className={`p-4 ${isActive ? "bg-primary/10" : "bg-muted/10"}`}>
      <div className="flex flex-wrap items-center justify-between gap-2">
        <div className="space-y-2">
          <div className="flex flex-wrap items-center gap-2">
            <Badge variant="secondary">Slack thread</Badge>
            <p className="text-sm font-medium">{threadTitle}</p>
          </div>
          {isSearchMode && <SlackEventBody event={entry.root} userLookup={userLookup} />}
          <SlackMessageLinks event={entry.root} />
          <div className="text-xs text-muted-foreground">
            {replyCount > 0
              ? `${replyCount} repl${replyCount === 1 ? "y" : "ies"}`
              : "No replies yet"}
          </div>
          <div className="flex flex-wrap gap-2 text-xs text-muted-foreground">
            <ActorChip
              event={entry.root}
              identities={identityLookup}
              personas={personaLookup}
              providerPersonaLabels={providerPersonaLabels}
              onPersonaClick={onPersonaClick}
            />
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

