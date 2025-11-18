import type { ComponentType } from "react";
import type {
  Arrow,
  DomainDescriptor,
  EventEnvelope,
  Identity,
  Persona,
} from "../../types/core";
import type {
  LinearThreadEntry,
  PersonaClickTarget,
  SlackThreadEntry,
  TimelineEntry,
} from "../../components/timeline/types";
import type {
  LinearIssueResponse,
  SlackThreadResponse,
} from "../../api/client";

export interface TimelineWindow {
  from: string;
  to: string;
}

export interface TimelineDataQuery {
  domains: string[];
  window: TimelineWindow;
  limit?: number;
}

export interface TimelineDataSource {
  fetchDomains(): Promise<DomainDescriptor[]>;
  fetchIdentities(): Promise<Identity[]>;
  fetchEvents(query: TimelineDataQuery): Promise<EventEnvelope[]>;
  fetchArrows(window: TimelineWindow): Promise<Arrow[]>;
  fetchSlackThread(channelId: string, threadTs: string): Promise<SlackThreadResponse>;
  fetchLinearIssue(issueRef: string): Promise<LinearIssueResponse>;
}

export type ThreadEntry = SlackThreadEntry | LinearThreadEntry;

export interface ThreadPanelProps<TEntry extends ThreadEntry> {
  entry: TEntry;
  identityLookup: Record<string, Identity>;
  personaLookup: Record<string, { persona: Persona; identityId: string }>;
  onPersonaClick: (target: PersonaClickTarget) => void;
  onClose: () => void;
  loading?: boolean;
  error?: string | null;
}

export interface ThreadAdapter<TEntry extends ThreadEntry = ThreadEntry> {
  readonly kind: TEntry["type"];
  readonly domains: string[];
  buildEntries(events: EventEnvelope[]): TEntry[] | Promise<TEntry[]>;
  hydrate?(entry: TEntry, dataSource: TimelineDataSource): Promise<TEntry>;
  matches(entry: ThreadEntry): entry is TEntry;
  Panel: ComponentType<ThreadPanelProps<TEntry>>;
}

export type ThreadAdapterRegistry = ThreadAdapter<any>[];

export interface TimelineDataResult {
  entries: TimelineEntry[];
  threadEntries: ThreadEntry[];
  identities: Identity[];
  domainLookup: Record<string, string>;
  identityLookup: Record<string, Identity>;
  personaLookup: Record<string, { persona: Persona; identityId: string }>;
}

