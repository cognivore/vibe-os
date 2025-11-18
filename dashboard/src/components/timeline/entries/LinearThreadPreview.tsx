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
  const issueTitle = entry.issueTitle || entry.issueIdentifier || "Linear issue";

  // Extract metadata from the first event or comment
  const issueMetadata = useMemo(() => {
    const firstEvent = entry.events[0] || entry.comments[0];
    if (!firstEvent) return null;
    const data = firstEvent.data as LinearEventData;
    return {
      assignee: data.issue_assignee,
      state: data.issue_state,
      state_type: data.issue_state_type,
      team: data.issue_team,
      priority: data.issue_priority,
      labels: data.issue_labels || [],
    };
  }, [entry.events, entry.comments]);

  const latestActivity = useMemo(() => {
    const combined = [...entry.events, ...entry.comments];
    if (!combined.length) {
      return null;
    }
    return combined.sort(
      (a, b) => new Date(a.at).getTime() - new Date(b.at).getTime(),
    )[combined.length - 1];
  }, [entry.comments, entry.events]);

  const latestTime = latestActivity
    ? new Date(latestActivity.at).toLocaleString()
    : new Date(entry.at).toLocaleString();

  return (
    <li className={`p-4 ${isActive ? "bg-primary/10" : "bg-muted/10"}`}>
      <div className="flex flex-wrap items-center justify-between gap-2">
        <div className="flex-1 space-y-2">
          <div className="flex flex-wrap items-center gap-2">
            <Badge variant="secondary">
              Linear issue {entry.issueIdentifier ? `· ${entry.issueIdentifier}` : ""}
            </Badge>
            {issueMetadata?.state && (
              <Badge variant="outline">{issueMetadata.state}</Badge>
            )}
            {issueMetadata?.priority !== null && issueMetadata?.priority !== undefined && (
              <Badge variant="outline">P{issueMetadata.priority}</Badge>
            )}
          </div>
          <div>
            {entry.issueUrl ? (
              <a
                href={entry.issueUrl}
                target="_blank"
                rel="noreferrer"
                className="text-base font-semibold underline"
              >
                {issueTitle}
              </a>
            ) : (
              <p className="text-base font-semibold">{issueTitle}</p>
            )}
          </div>
          {entry.issueDescription && (
            <p className="text-sm text-muted-foreground whitespace-pre-wrap">
              {entry.issueDescription}
            </p>
          )}
          <div className="flex flex-wrap items-center gap-3 text-xs text-muted-foreground">
            {issueMetadata?.assignee && (
              <span>👤 {issueMetadata.assignee}</span>
            )}
            {issueMetadata?.team && (
              <span>📁 {issueMetadata.team}</span>
            )}
            {issueMetadata?.labels && issueMetadata.labels.length > 0 && (
              <span>🏷️ {issueMetadata.labels.join(", ")}</span>
            )}
          </div>
          <div className="text-xs text-muted-foreground">
            <span>Last activity: {latestTime}</span>
          </div>
        </div>
        <div className="flex flex-col items-end gap-2">
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

