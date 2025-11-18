import type {
  Arrow,
  EventEnvelope,
  PersonaKey,
} from "../../types/core";

export interface SlackThreadEntry {
  type: "slack_thread";
  threadId: string;
  channelId?: string;
  threadTitle?: string;
  at: string;
  root: EventEnvelope;
  replies: EventEnvelope[];
}

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

export type TimelineEntry =
  | { type: "event"; at: string; event: EventEnvelope }
  | { type: "arrow"; at: string; arrow: Arrow }
  | SlackThreadEntry
  | LinearThreadEntry;

export interface PersonaClickTarget {
  identityId?: string;
  persona?: PersonaKey;
  label?: string;
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

export type SlackLinkTargets = {
  webUrl: string;
  desktopUrl: string;
};

export type LinearEventData = {
  issue_identifier?: string;
  issue_id?: string;
  issue_title?: string | null;
  issue_description?: string | null;
  issue_url?: string | null;
  issue_assignee?: string | null;
  issue_state?: string | null;
  issue_state_type?: string | null;
  issue_team?: string | null;
  issue_priority?: number | null;
  issue_labels?: string[];
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

