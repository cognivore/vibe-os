import { useEffect, useMemo, useState } from "react";
import { Link } from "react-router-dom";
import { getArrows, getDomains, getEvents, listIdentities } from "../api/client";
import type {
  Arrow,
  DomainDescriptor,
  EventEnvelope,
  Identity,
  Persona,
} from "../types/core";
import { Button } from "../components/ui/button";
import { Card, CardContent, CardHeader, CardTitle } from "../components/ui/card";
import { Badge } from "../components/ui/badge";
import { ScrollArea } from "../components/ui/scroll-area";
import { format } from "date-fns";

interface TimelineEntry {
  type: "event" | "arrow";
  at: string;
  event?: EventEnvelope;
  arrow?: Arrow;
}

const ISO_FORMAT = "yyyy-MM-dd'T'HH:mm:ss";

function defaultWindow() {
  const end = new Date();
  const start = new Date(end.getTime() - 1000 * 60 * 60 * 24);
  return {
    from: start.toISOString(),
    to: end.toISOString(),
  };
}

export default function TimelinePage() {
  const [{ from, to }, setWindow] = useState(defaultWindow);
  const [domains, setDomains] = useState<DomainDescriptor[]>([]);
  const [selectedDomains, setSelectedDomains] = useState<string[]>([]);
  const [events, setEvents] = useState<EventEnvelope[]>([]);
  const [arrows, setArrows] = useState<Arrow[]>([]);
  const [identities, setIdentities] = useState<Identity[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    getDomains()
      .then((available) => {
        setDomains(available);
        if (selectedDomains.length === 0) {
          setSelectedDomains(available.map((d) => d.id));
        }
      })
      .catch((err) => setError(err.message));
    listIdentities()
      .then(setIdentities)
      .catch((err) => setError((prev) => prev ?? err.message));
  }, []);

  useEffect(() => {
    if (!selectedDomains.length) return;
    setLoading(true);
    setError(null);
    Promise.all([
      getEvents({ domains: selectedDomains, from, to, limit: 200 }),
      getArrows({ from, to }),
    ])
      .then(([eventData, arrowData]) => {
        setEvents(eventData);
        setArrows(arrowData);
      })
      .catch((err) => setError(err.message))
      .finally(() => setLoading(false));
  }, [from, to, selectedDomains]);

  const timeline = useMemo<TimelineEntry[]>(() => {
    const entries: TimelineEntry[] = [
      ...events.map((event) => ({
        type: "event" as const,
        at: event.at,
        event,
      })),
      ...arrows.map((arrow) => ({
        type: "arrow" as const,
        at: arrow.created_at,
        arrow,
      })),
    ];
    return entries.sort(
      (a, b) => new Date(b.at).getTime() - new Date(a.at).getTime(),
    );
  }, [events, arrows]);

  const domainLookup = useMemo(
    () =>
      Object.fromEntries(
        domains.map((domain) => [domain.id, domain.human_name]),
      ),
    [domains],
  );

  const identityLookup = useMemo(
    () =>
      Object.fromEntries(
        identities.map((identity) => [identity.id, identity]),
      ),
    [identities],
  );

  const personaLookup = useMemo(() => {
    const map: Record<
      string,
      { persona: Persona; identityId: string }
    > = {};
    identities.forEach((identity) => {
      identity.personas.forEach((persona) => {
        map[persona.id] = { persona, identityId: identity.id };
      });
    });
    return map;
  }, [identities]);

  return (
    <div className="space-y-6">
      <div className="flex flex-col gap-4 md:flex-row md:items-end md:justify-between">
        <div>
          <h2 className="text-2xl font-semibold">Unified timeline</h2>
          <p className="text-sm text-muted-foreground">
            Events and arrows across all domains
          </p>
        </div>
        <div className="flex flex-wrap gap-2">
          <label className="flex flex-col text-xs">
            From
            <input
              type="datetime-local"
              value={format(new Date(from), ISO_FORMAT)}
              onChange={(e) =>
                setWindow((prev) => ({
                  ...prev,
                  from: new Date(e.target.value).toISOString(),
                }))
              }
              className="rounded-md border border-input bg-background px-2 py-1 text-sm"
            />
          </label>
          <label className="flex flex-col text-xs">
            To
            <input
              type="datetime-local"
              value={format(new Date(to), ISO_FORMAT)}
              onChange={(e) =>
                setWindow((prev) => ({
                  ...prev,
                  to: new Date(e.target.value).toISOString(),
                }))
              }
              className="rounded-md border border-input bg-background px-2 py-1 text-sm"
            />
          </label>
          <Button
            variant="secondary"
            onClick={() => setWindow(defaultWindow())}
          >
            Last 24h
          </Button>
        </div>
      </div>

      <Card>
        <CardHeader className="flex flex-row items-center justify-between">
          <CardTitle>Domains</CardTitle>
          <div className="flex flex-wrap gap-2">
            {domains.map((domain) => {
              const active = selectedDomains.includes(domain.id);
              return (
                <label
                  key={domain.id}
                  className="flex items-center gap-1 text-sm"
                >
                  <input
                    type="checkbox"
                    checked={active}
                    onChange={(e) => {
                      setSelectedDomains((prev) =>
                        e.target.checked
                          ? [...new Set([...prev, domain.id])]
                          : prev.filter((d) => d !== domain.id),
                      );
                    }}
                  />
                  {domain.human_name}
                </label>
              );
            })}
          </div>
        </CardHeader>
        <CardContent>
          <div className="flex flex-col gap-1 text-sm text-muted-foreground">
            {error ? (
              <p className="text-destructive-foreground">{error}</p>
            ) : (
              <p>
                Showing {timeline.length} entries between{" "}
                {new Date(from).toLocaleString()} and{" "}
                {new Date(to).toLocaleString()}
              </p>
            )}
            <Link to="/identities" className="text-xs underline">
              Manage identities
            </Link>
          </div>
        </CardContent>
      </Card>

      <ScrollArea className="h-[65vh] rounded-lg border border-border bg-card">
        {loading ? (
          <div className="p-6 text-sm text-muted-foreground">Loading...</div>
        ) : (
          <ul className="divide-y divide-border">
            {timeline.map((entry) => {
              if (entry.type === "event" && entry.event) {
                const event = entry.event;
                return (
                  <li key={`event-${event.id}`} className="p-4">
                    <div className="flex flex-wrap items-center justify-between gap-2">
                      <div className="space-y-1">
                        <Badge variant="secondary">{event.domain}</Badge>
                        <p className="text-sm font-medium">{event.summary}</p>
                        <p className="text-xs text-muted-foreground">
                          {event.kind}
                        </p>
                        {renderActorChip(event, identityLookup, personaLookup)}
                      </div>
                      <p className="text-xs text-muted-foreground">
                        {new Date(event.at).toLocaleString()}
                      </p>
                    </div>
                  </li>
                );
              }
              if (entry.arrow && entry.type === "arrow") {
                const arrow = entry.arrow;
                return (
                  <li key={`arrow-${arrow.id}`} className="p-4 bg-muted/20">
                    <div className="flex flex-wrap items-center justify-between gap-2">
                      <div className="space-y-1">
                        <Badge>{arrow.direction.replace("_", " ")}</Badge>
                        <p className="text-sm font-semibold">{arrow.title}</p>
                        <p className="text-xs text-muted-foreground">
                          {arrow.source.kind === "domain_object"
                            ? `${domainLookup[arrow.source.domain] ?? arrow.source.domain} → ${
                                domainLookup[arrow.target.domain] ??
                                arrow.target.domain
                              }`
                            : `${arrow.source.domain} → ${arrow.target.domain}`}
                        </p>
                        {renderArrowAuthorChip(arrow, identityLookup, personaLookup)}
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
              return null;
            })}
          </ul>
        )}
      </ScrollArea>
    </div>
  );
}

function renderActorChip(
  event: EventEnvelope,
  identities: Record<string, Identity>,
  personas: Record<string, { persona: Persona; identityId: string }>,
) {
  if (event.actor_identity_id && identities[event.actor_identity_id]) {
    const identity = identities[event.actor_identity_id];
    const personaInfo =
      event.actor_persona_id && personas[event.actor_persona_id];
    return (
      <Badge variant="outline">
        {identity.preferred_name ?? identity.canonical_email}
        {personaInfo
          ? ` (${personaInfo.persona.key.domain}:${personaInfo.persona.key.local_id})`
          : null}
      </Badge>
    );
  }
  if (event.actor_persona_key) {
    return (
      <Badge variant="outline">
        @{event.actor_persona_key.local_id} ({event.actor_persona_key.domain})
      </Badge>
    );
  }
  return null;
}

function renderArrowAuthorChip(
  arrow: Arrow,
  identities: Record<string, Identity>,
  personas: Record<string, { persona: Persona; identityId: string }>,
) {
  if (arrow.author_identity_id && identities[arrow.author_identity_id]) {
    const identity = identities[arrow.author_identity_id];
    const personaInfo =
      arrow.author_persona_id && personas[arrow.author_persona_id];
    return (
      <Badge variant="outline">
        {identity.preferred_name ?? identity.canonical_email}
        {personaInfo
          ? ` (${personaInfo.persona.key.domain}:${personaInfo.persona.key.local_id})`
          : null}
      </Badge>
    );
  }
  if (arrow.author_persona_key) {
    return (
      <Badge variant="outline">
        {arrow.author_persona_key.domain}:{arrow.author_persona_key.local_id}
      </Badge>
    );
  }
  return null;
}

