import { useMemo, useState } from "react";
import { Button } from "../../../components/ui/button";
import { ActorChip } from "../../../components/timeline/entries/ActorChip";
import { LinearEventBody } from "../../../components/timeline/entries/EventEntry";
import type {
  LinearEventData,
  LinearThreadEntry,
} from "../../../components/timeline/types";
import type { EventEnvelope, Identity, Persona } from "../../../types/core";
import type {
  ThreadAdapter,
  ThreadPanelProps,
  TimelineDataSource,
} from "../adapters";
import { exportLinearThreadToMarkdown, downloadMarkdown, copyToClipboard } from "./exportUtils";

export const linearThreadAdapter: ThreadAdapter<LinearThreadEntry> = {
  kind: "linear_thread",
  domains: ["linear"],
  buildEntries: buildLinearThreadEntries,
  hydrate: hydrateLinearThread,
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
          issueTitle: data.issue_title ?? undefined,
          issueUrl: data.issue_url ?? undefined,
          issueDescription: data.issue_description ?? undefined,
          comments: [],
          events: [],
          at: event.at,
        });
      }
      const entry = map.get(issueId)!;
      // Accumulate metadata from any event that has it (prefer non-null values)
      if (data.issue_identifier && !entry.issueIdentifier) {
        entry.issueIdentifier = data.issue_identifier;
      }
      if (data.issue_title && !entry.issueTitle) {
        entry.issueTitle = data.issue_title;
      }
      if (data.issue_url && !entry.issueUrl) {
        entry.issueUrl = data.issue_url;
      }
      if (data.issue_description && !entry.issueDescription) {
        entry.issueDescription = data.issue_description;
      }
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
  providerPersonaLabels: _providerPersonaLabels,
  onPersonaClick,
  onClose,
  loading,
  error,
}: ThreadPanelProps<LinearThreadEntry>) {
  const [exportStatus, setExportStatus] = useState<string | null>(null);

  const comments = useMemo(
    () =>
      [...thread.comments].sort(
        (a, b) => new Date(a.at).getTime() - new Date(b.at).getTime(),
      ),
    [thread.comments],
  );
  const events = useMemo(
    () =>
      [...thread.events].sort(
        (a, b) => new Date(a.at).getTime() - new Date(b.at).getTime(),
      ),
    [thread.events],
  );
  const timeline = useMemo(
    () =>
      [
        ...events.map((event) => ({ kind: "event" as const, event })),
        ...comments.map((event) => ({ kind: "comment" as const, event })),
      ].sort(
        (a, b) => new Date(a.event.at).getTime() - new Date(b.event.at).getTime(),
      ),
    [comments, events],
  );
  const issueTitle =
    thread.issueTitle ?? thread.issueIdentifier ?? "Linear issue";

  const handleExportMarkdown = () => {
    const markdown = exportLinearThreadToMarkdown(thread, identityLookup, personaLookup);
    const filename = `linear-issue-${thread.issueIdentifier ?? thread.issueId}-${new Date().toISOString().split('T')[0]}.md`;
    downloadMarkdown(markdown, filename);
    setExportStatus("Downloaded!");
    setTimeout(() => setExportStatus(null), 2000);
  };

  const handleCopyMarkdown = async () => {
    const markdown = exportLinearThreadToMarkdown(thread, identityLookup, personaLookup);
    await copyToClipboard(markdown);
    setExportStatus("Copied!");
    setTimeout(() => setExportStatus(null), 2000);
  };

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
            {timeline.length} activity item{timeline.length === 1 ? "" : "s"}
          </p>
        </div>
        <div className="flex gap-2">
          <Button variant="outline" size="sm" onClick={handleCopyMarkdown}>
            {exportStatus === "Copied!" ? "Copied!" : "Copy MD"}
          </Button>
          <Button variant="outline" size="sm" onClick={handleExportMarkdown}>
            {exportStatus === "Downloaded!" ? "Downloaded!" : "Export MD"}
          </Button>
          <Button variant="ghost" size="sm" onClick={onClose}>
            Close
          </Button>
        </div>
      </div>
      {thread.issueDescription ? (
        <p className="whitespace-pre-wrap text-sm text-muted-foreground">
          {thread.issueDescription}
        </p>
      ) : null}
      {loading ? (
        <p className="text-xs text-muted-foreground">
          Refreshing latest issue activity…
        </p>
      ) : null}
      {error ? (
        <p className="text-xs text-destructive-foreground">{error}</p>
      ) : null}
      <div className="flex-1 space-y-3 overflow-y-auto pr-2">
        {timeline.length ? (
          timeline.map((item) =>
            item.kind === "comment" ? (
              <LinearCommentMessage
                key={item.event.id}
                event={item.event}
                identityLookup={identityLookup}
                personaLookup={personaLookup}
                onPersonaClick={onPersonaClick}
              />
            ) : (
              <LinearEventUpdate
                key={item.event.id}
                event={item.event}
                identityLookup={identityLookup}
                personaLookup={personaLookup}
                onPersonaClick={onPersonaClick}
              />
            ),
          )
        ) : (
          <div className="text-sm text-muted-foreground">
            No historical activity available for this issue.
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
  onPersonaClick,
}: {
  event: EventEnvelope;
  identityLookup: Record<string, Identity>;
  personaLookup: Record<string, { persona: Persona; identityId: string }>;
  onPersonaClick?: ThreadPanelProps<LinearThreadEntry>["onPersonaClick"];
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
      <div className="mt-2">
        <ActorChip
          event={event}
          identities={identityLookup}
          personas={personaLookup}
          onPersonaClick={onPersonaClick}
        />
      </div>
    </div>
  );
}

function LinearEventUpdate({
  event,
  identityLookup,
  personaLookup,
  onPersonaClick,
}: {
  event: EventEnvelope;
  identityLookup: Record<string, Identity>;
  personaLookup: Record<string, { persona: Persona; identityId: string }>;
  onPersonaClick: ThreadPanelProps<LinearThreadEntry>["onPersonaClick"];
}) {
  const actorLabel = resolveLinearActor(event, identityLookup, personaLookup);
  const data = event.data as LinearEventData;

  const stateChange =
    data.from_state || data.to_state
      ? `State: ${data.from_state ?? "—"} → ${data.to_state ?? "—"}`
      : null;
  const priorityChange =
    typeof data.from_priority === "number" ||
      typeof data.to_priority === "number"
      ? `Priority: ${formatPriority(data.from_priority)} → ${formatPriority(
        data.to_priority,
      )}`
      : null;

  return (
    <div className="rounded-lg border border-border bg-background/80 p-3 shadow-sm">
      <div className="flex flex-wrap items-center justify-between gap-2 text-xs text-muted-foreground">
        <div className="font-medium">{actorLabel}</div>
        <p>{new Date(event.at).toLocaleString()}</p>
      </div>
      <div className="mt-2 space-y-1 text-sm text-foreground">
        {stateChange ? <p>{stateChange}</p> : null}
        {priorityChange ? <p>{priorityChange}</p> : null}
        {data.actor_name ? <p className="text-xs text-muted-foreground">Actor: {data.actor_name}</p> : null}
        {!stateChange && !priorityChange && event.summary ? (
          <p className="text-sm text-muted-foreground">{event.summary}</p>
        ) : null}
      </div>
      <div className="mt-2">
        <ActorChip
          event={event}
          identities={identityLookup}
          personas={personaLookup}
          onPersonaClick={onPersonaClick}
        />
      </div>
    </div>
  );
}

function formatPriority(value?: number | null) {
  if (value === null || value === undefined) {
    return "—";
  }
  return value.toString();
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

async function hydrateLinearThread(
  entry: LinearThreadEntry,
  dataSource: TimelineDataSource,
): Promise<LinearThreadEntry> {
  const response = await dataSource.fetchLinearIssue(entry.issueId);
  const comments = [...response.comments].sort(
    (a, b) => new Date(a.at).getTime() - new Date(b.at).getTime(),
  );
  const events = [...response.events].sort(
    (a, b) => new Date(a.at).getTime() - new Date(b.at).getTime(),
  );
  const latest =
    [...comments, ...events].sort(
      (a, b) => new Date(b.at).getTime() - new Date(a.at).getTime(),
    )[0]?.at ?? entry.at;

  return {
    ...entry,
    issueIdentifier: response.issue_identifier ?? entry.issueIdentifier,
    issueTitle: response.issue_title ?? entry.issueTitle,
    issueUrl: response.issue_url ?? entry.issueUrl,
    issueDescription: response.issue_description ?? entry.issueDescription,
    comments,
    events,
    at: latest,
  };
}

