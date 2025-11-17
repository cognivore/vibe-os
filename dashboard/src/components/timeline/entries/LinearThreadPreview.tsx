import type {
  Identity,
  Persona,
} from "../../../types/core";
import { Badge } from "../../ui/badge";
import { Button } from "../../ui/button";
import { ActorChip } from "./ActorChip";
import { extractLinearCommentSnippet } from "./EventEntry";
import type {
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
          <div className="text-xs text-muted-foreground">
            {commentCount > 0
              ? `${commentCount} comment${commentCount === 1 ? "" : "s"}`
              : "No comments yet"}
          </div>
          {latestComment ? (
            <ActorChip
              event={latestComment}
              identities={identityLookup}
              personas={personaLookup}
              onPersonaClick={onPersonaClick}
            />
          ) : null}
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

