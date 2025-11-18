import { useMemo } from "react";
import type { EventEnvelope, Identity, Persona } from "../../../types/core";
import { Badge } from "../../ui/badge";
import { Button } from "../../ui/button";
import { ActorChip } from "./ActorChip";
import { extractLinearCommentSnippet } from "./EventEntry";
import type {
  LinearEventData,
  LinearThreadEntry,
  PersonaClickTarget,
} from "../types";

interface LinearThreadPreviewProps {
  entry: LinearThreadEntry;
  identityLookup: Record<string, Identity>;
  personaLookup: Record<string, { persona: Persona; identityId: string }>;
  onPersonaClick?: (target: PersonaClickTarget) => void;
  onThreadSelect?: (thread: LinearThreadEntry) => void;
  isActive?: boolean;
}

export function LinearThreadPreview({
  entry,
  identityLookup,
  personaLookup,
  onPersonaClick,
  onThreadSelect,
  isActive,
}: LinearThreadPreviewProps) {
  const issueTitle =
    entry.issueTitle ??
    entry.issueIdentifier ??
    entry.events[0]?.summary ??
    "Linear issue";

  const latestActivity = useMemo(() => {
    const combined = [...entry.events, ...entry.comments];
    if (!combined.length) {
      return null;
    }
    return combined.sort(
      (a, b) => new Date(a.at).getTime() - new Date(b.at).getTime(),
    )[combined.length - 1];
  }, [entry.comments, entry.events]);

  const previewText = latestActivity
    ? latestActivity.kind === "linear.comment"
      ? extractLinearCommentSnippet(latestActivity)
      : buildLinearEventPreview(latestActivity)
    : entry.issueDescription ?? "No activity yet";

  const latestTime = latestActivity
    ? new Date(latestActivity.at).toLocaleString()
    : new Date(entry.at).toLocaleString();

  const actorChip = latestActivity ? (
    <ActorChip
      event={latestActivity}
      identities={identityLookup}
      personas={personaLookup}
      onPersonaClick={onPersonaClick}
    />
  ) : null;

  return (
    <li className={`p-4 ${isActive ? "bg-primary/10" : "bg-muted/10"}`}>
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
          <div className="text-xs text-muted-foreground flex flex-col gap-1">
            <span>{latestTime}</span>
            {actorChip}
          </div>
        </div>
        <div className="flex flex-col items-end gap-2 text-xs text-muted-foreground">
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

function buildLinearEventPreview(event: EventEnvelope) {
  if (event.summary?.trim().length) {
    return event.summary;
  }
  const data = event.data as LinearEventData;
  if (data.to_state || data.from_state) {
    return `State: ${data.from_state ?? "—"} → ${data.to_state ?? "—"}`;
  }
  if (
    typeof data.to_priority === "number" ||
    typeof data.from_priority === "number"
  ) {
    return `Priority: ${formatPriority(data.from_priority)} → ${formatPriority(
      data.to_priority,
    )}`;
  }
  return "Issue updated";
}

function formatPriority(value?: number | null) {
  if (value === null || value === undefined) {
    return "—";
  }
  return value.toString();
}

