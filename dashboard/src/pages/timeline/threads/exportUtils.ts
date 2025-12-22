import type { EventEnvelope, Identity, Persona } from "../../../types/core";
import type { SlackThreadEntry, LinearThreadEntry, SlackEventData, LinearEventData } from "../../../components/timeline/types";
import { normalizeSlackMarkup } from "../../../components/timeline/entries/EventEntry";

/**
 * Exports a Slack thread to well-formatted Markdown
 */
export function exportSlackThreadToMarkdown(
  thread: SlackThreadEntry,
  identityLookup: Record<string, Identity>,
  personaLookup: Record<string, { persona: Persona; identityId: string }>,
  userLookup?: Record<string, string>,
): string {
  const messages = [thread.root, ...thread.replies].sort(
    (a, b) => new Date(a.at).getTime() - new Date(b.at).getTime(),
  );

  const threadTitle = thread.threadTitle
    ? normalizeSlackMarkup(thread.threadTitle, userLookup)
    : (thread.channelId ? `#${thread.channelId}` : "Slack Thread");

  const lines: string[] = [];

  // Header
  lines.push(`# Slack Thread: ${threadTitle}`);
  lines.push("");
  lines.push(`**Channel:** ${thread.channelId ?? "Unknown"}`);
  lines.push(`**Thread ID:** ${thread.threadId}`);
  lines.push(`**Messages:** ${messages.length}`);
  lines.push(`**Latest Activity:** ${new Date(thread.at).toLocaleString()}`);
  lines.push("");
  lines.push("---");
  lines.push("");

  // Messages
  for (const message of messages) {
    const actor = resolveSlackActor(message, identityLookup, personaLookup);
    const timestamp = new Date(message.at).toLocaleString();
    const data = message.data as SlackEventData;
    const text = typeof data.text === "string" ? normalizeSlackMarkup(data.text, userLookup) : "";

    lines.push(`## ${actor}`);
    lines.push(`*${timestamp}*`);
    lines.push("");

    if (text) {
      lines.push(text);
      lines.push("");
    }

    // Attachments
    const attachments = Array.isArray(data.attachments) ? data.attachments : [];
    if (attachments.length > 0) {
      lines.push("**Attachments:**");
      for (const attachment of attachments) {
        const title = attachment.title ?? attachment.fallback;
        const url = attachment.title_link;
        if (title) {
          if (url) {
            lines.push(`- [${title}](${url})`);
          } else {
            lines.push(`- ${title}`);
          }
        }
      }
      lines.push("");
    }

    // Files
    const files = Array.isArray(data.files) ? data.files : [];
    if (files.length > 0) {
      lines.push("**Files:**");
      for (const file of files) {
        const name = file.name ?? file.id ?? "file";
        const url = file.permalink ?? file.url_private;
        if (url) {
          lines.push(`- [${name}](${url})`);
        } else {
          lines.push(`- ${name}`);
        }
      }
      lines.push("");
    }

    lines.push("---");
    lines.push("");
  }

  return lines.join("\n");
}

/**
 * Exports a Linear thread to well-formatted Markdown
 */
export function exportLinearThreadToMarkdown(
  thread: LinearThreadEntry,
  identityLookup: Record<string, Identity>,
  personaLookup: Record<string, { persona: Persona; identityId: string }>,
): string {
  const timeline = [
    ...thread.events.map((event) => ({ kind: "event" as const, event })),
    ...thread.comments.map((event) => ({ kind: "comment" as const, event })),
  ].sort(
    (a, b) => new Date(a.event.at).getTime() - new Date(b.event.at).getTime(),
  );

  const issueTitle = thread.issueTitle ?? thread.issueIdentifier ?? "Linear Issue";
  const lines: string[] = [];

  // Header
  lines.push(`# Linear Issue: ${issueTitle}`);
  lines.push("");

  if (thread.issueIdentifier) {
    lines.push(`**Issue ID:** ${thread.issueIdentifier}`);
  }
  if (thread.issueUrl) {
    lines.push(`**URL:** ${thread.issueUrl}`);
  }
  lines.push(`**Activity Items:** ${timeline.length}`);
  lines.push(`**Latest Activity:** ${new Date(thread.at).toLocaleString()}`);
  lines.push("");

  // Issue Description
  if (thread.issueDescription) {
    lines.push("## Description");
    lines.push("");
    lines.push(thread.issueDescription);
    lines.push("");
  }

  lines.push("---");
  lines.push("");

  // Timeline
  if (timeline.length > 0) {
    lines.push("## Activity Timeline");
    lines.push("");

    for (const item of timeline) {
      const actor = resolveLinearActor(item.event, identityLookup, personaLookup);
      const timestamp = new Date(item.event.at).toLocaleString();
      const data = item.event.data as LinearEventData;

      if (item.kind === "comment") {
        const commentBody = getLinearCommentBody(item.event);

        lines.push(`### 💬 Comment by ${actor}`);
        lines.push(`*${timestamp}*`);
        lines.push("");
        lines.push(commentBody);
        lines.push("");

        if (data.comment_url) {
          lines.push(`[View in Linear](${data.comment_url})`);
          lines.push("");
        }
      } else {
        lines.push(`### 📝 ${item.event.kind} by ${actor}`);
        lines.push(`*${timestamp}*`);
        lines.push("");

        const changes: string[] = [];

        if (data.from_state || data.to_state) {
          changes.push(`**State:** ${data.from_state ?? "—"} → ${data.to_state ?? "—"}`);
        }

        if (typeof data.from_priority === "number" || typeof data.to_priority === "number") {
          changes.push(`**Priority:** ${formatPriority(data.from_priority)} → ${formatPriority(data.to_priority)}`);
        }

        if (changes.length > 0) {
          for (const change of changes) {
            lines.push(change);
          }
          lines.push("");
        } else if (item.event.summary) {
          lines.push(item.event.summary);
          lines.push("");
        }
      }

      lines.push("---");
      lines.push("");
    }
  }

  return lines.join("\n");
}

/**
 * Downloads markdown content as a file
 */
export function downloadMarkdown(content: string, filename: string): void {
  const blob = new Blob([content], { type: "text/markdown;charset=utf-8" });
  const url = URL.createObjectURL(blob);
  const link = document.createElement("a");
  link.href = url;
  link.download = filename;
  document.body.appendChild(link);
  link.click();
  document.body.removeChild(link);
  URL.revokeObjectURL(url);
}

/**
 * Copies markdown content to clipboard
 */
export async function copyToClipboard(content: string): Promise<void> {
  await navigator.clipboard.writeText(content);
}

function resolveSlackActor(
  event: EventEnvelope,
  identityLookup: Record<string, Identity>,
  personaLookup: Record<string, { persona: Persona; identityId: string }>,
): string {
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
  const data = event.data as SlackEventData;
  if (data.user) {
    return `@${data.user}`;
  }
  return "Unknown actor";
}

function resolveLinearActor(
  event: EventEnvelope,
  identityLookup: Record<string, Identity>,
  personaLookup: Record<string, { persona: Persona; identityId: string }>,
): string {
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
  return "Unknown actor";
}

function getLinearCommentBody(event: EventEnvelope): string {
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

function formatPriority(value?: number | null): string {
  if (value === null || value === undefined) {
    return "—";
  }
  return value.toString();
}


