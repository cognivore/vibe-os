import { useMemo } from "react";
import { Button } from "../../../components/ui/button";
import {
  type LinearEventData,
  type LinearThreadEntry,
} from "../../../components/timeline/TimelineEntryList";
import type { EventEnvelope, Identity, Persona } from "../../../types/core";
import type { ThreadAdapter, ThreadPanelProps } from "../adapters";

export const linearThreadAdapter: ThreadAdapter<LinearThreadEntry> = {
  kind: "linear_thread",
  domains: ["linear"],
  buildEntries: buildLinearThreadEntries,
  matches: (entry): entry is LinearThreadEntry => entry.type === "linear_thread",
  Panel: LinearThreadPanel,
};

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

  events
    .filter((event) => event.domain === "linear")
    .forEach((event) => {
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

function LinearThreadPanel({
  entry: thread,
  identityLookup,
  personaLookup,
  onPersonaClick,
  onClose,
}: ThreadPanelProps<LinearThreadEntry>) {
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

