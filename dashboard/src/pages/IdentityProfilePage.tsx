import { useEffect, useMemo, useState } from "react";
import { Link, useNavigate, useParams } from "react-router-dom";
import {
  getDomains,
  getEvents,
  getIdentity,
  type EventsQuery,
} from "../api/client";
import type {
  DomainDescriptor,
  EventEnvelope,
  Identity,
  Persona,
} from "../types/core";
import { Button } from "../components/ui/button";
import { Card, CardContent, CardHeader, CardTitle } from "../components/ui/card";
import { Badge } from "../components/ui/badge";
import { ScrollArea } from "../components/ui/scroll-area";
import { TimelineEntryList } from "../components/timeline/TimelineEntryList";
import type {
  PersonaClickTarget,
  TimelineEntry,
} from "../components/timeline/types";
import { format } from "date-fns";

const ISO_FORMAT = "yyyy-MM-dd'T'HH:mm:ss";

function defaultWindow() {
  const end = new Date();
  const start = new Date(end.getTime() - 7 * 24 * 60 * 60 * 1000);
  return { from: start.toISOString(), to: end.toISOString() };
}

export default function IdentityProfilePage() {
  const navigate = useNavigate();
  const { id } = useParams<{ id: string }>();
  const [{ from, to }, setWindow] = useState(defaultWindow);
  const [identity, setIdentity] = useState<Identity | null>(null);
  const [events, setEvents] = useState<EventEnvelope[]>([]);
  const [domains, setDomains] = useState<DomainDescriptor[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    getDomains()
      .then(setDomains)
      .catch((err) => setError((prev) => prev ?? err.message));
  }, []);

  useEffect(() => {
    if (!id) return;
    setLoading(true);
    setError(null);
    const query: EventsQuery = { from, to, limit: 200, identityId: id };
    Promise.all([getIdentity(id), getEvents(query)])
      .then(([identityData, eventResponse]) => {
        setIdentity(identityData);
        const normalizedEvents =
          eventResponse.mode === "snapshot" ? eventResponse.events : eventResponse.added;
        setEvents(normalizedEvents);
      })
      .catch((err) => setError(err.message))
      .finally(() => setLoading(false));
  }, [id, from, to]);

  const domainLookup = useMemo(
    () =>
      Object.fromEntries(
        domains.map((domain) => [domain.id, domain.human_name]),
      ),
    [domains],
  );

  const identityLookup = useMemo(
    () => (identity ? { [identity.id]: identity } : {}),
    [identity],
  );

  const personaLookup = useMemo(() => {
    const map: Record<string, { persona: Persona; identityId: string }> = {};
    if (identity) {
      identity.personas.forEach((persona) => {
        map[persona.id] = { persona, identityId: identity.id };
      });
    }
    return map;
  }, [identity]);

  const entries = useMemo<TimelineEntry[]>(
    () =>
      events
        .map((event) => ({
          type: "event" as const,
          at: event.at,
          event,
        }))
        .sort((a, b) => new Date(b.at).getTime() - new Date(a.at).getTime()),
    [events],
  );

  const missingDomains = useMemo(() => {
    if (!identity) return [];
    const supported = new Set(["slack", "linear"]);
    const present = new Set(identity.personas.map((persona) => persona.key.domain));
    return domains.filter(
      (domain) => supported.has(domain.id) && !present.has(domain.id),
    );
  }, [domains, identity]);

  const handlePersonaClick = (target: PersonaClickTarget) => {
    if (target.identityId) {
      navigate(`/identities/${target.identityId}`);
    }
  };

  if (!id) {
    return (
      <div className="space-y-4">
        <p className="text-sm text-muted-foreground">
          Identity identifier missing from URL.
        </p>
        <Button variant="secondary" onClick={() => navigate("/identities")}>
          Back to identities
        </Button>
      </div>
    );
  }

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <Button variant="ghost" onClick={() => navigate(-1)}>
          ← Back
        </Button>
        <Link to="/identities" className="text-sm underline">
          Manage identities
        </Link>
      </div>

      {error ? (
        <p className="text-sm text-destructive-foreground">{error}</p>
      ) : null}

      <Card>
        <CardHeader>
          <CardTitle className="text-2xl">
            {identity?.preferred_name ?? identity?.canonical_email ?? "Loading..."}
          </CardTitle>
          <p className="text-sm text-muted-foreground">
            {identity?.canonical_email ?? ""}
          </p>
        </CardHeader>
        <CardContent className="space-y-2 text-sm">
          {identity ? (
            <>
              <div className="flex flex-wrap gap-2">
                {identity.personas.map((persona) => (
                  <Badge key={persona.id} variant="outline">
                    {persona.label ?? persona.key.local_id} ({persona.key.domain})
                  </Badge>
                ))}
                {identity.personas.length === 0 ? (
                  <p className="text-muted-foreground">
                    No personas attached yet.
                  </p>
                ) : null}
              </div>
              {missingDomains.length ? (
                <div className="text-xs text-muted-foreground">
                  Missing personas:
                  {missingDomains.map((domain) => (
                    <Link
                      key={`${identity.id}-${domain.id}`}
                      to={`/identities?provider=${domain.id}&identity=${identity.id}`}
                      className="ml-1 text-primary underline"
                    >
                      {domain.human_name}
                    </Link>
                  ))}
                </div>
              ) : null}
            </>
          ) : (
            <p className="text-sm text-muted-foreground">Loading identity…</p>
          )}
        </CardContent>
      </Card>

      <Card>
        <CardHeader className="flex flex-col gap-3 md:flex-row md:items-end md:justify-between">
          <div>
            <CardTitle>Activity</CardTitle>
            <p className="text-sm text-muted-foreground">
              Combined timeline based on known personas.
            </p>
          </div>
          <div className="flex flex-wrap gap-2 text-xs">
            <label className="flex flex-col gap-1">
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
            <label className="flex flex-col gap-1">
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
            <Button variant="secondary" onClick={() => setWindow(defaultWindow())}>
              Last 7 days
            </Button>
          </div>
        </CardHeader>
        <CardContent>
          {loading ? (
            <p className="text-sm text-muted-foreground">Loading activity…</p>
          ) : entries.length === 0 ? (
            <p className="text-sm text-muted-foreground">
              No events captured for this identity in the selected window.
            </p>
          ) : (
            <ScrollArea className="h-[55vh] rounded border border-border">
              <TimelineEntryList
                entries={entries}
                domainLookup={domainLookup}
                identityLookup={identityLookup}
                personaLookup={personaLookup}
                onPersonaClick={handlePersonaClick}
              />
            </ScrollArea>
          )}
        </CardContent>
      </Card>
    </div>
  );
}

