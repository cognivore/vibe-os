import { useEffect, useMemo, useState } from "react";
import { Link, useNavigate } from "react-router-dom";
import {
  attachPersona,
  createIdentity,
  getArrows,
  getDomains,
  getEvents,
  listIdentities,
  lookupIdentity,
} from "../api/client";
import type {
  Arrow,
  DomainDescriptor,
  EventEnvelope,
  Identity,
  Persona,
  PersonaKey,
} from "../types/core";
import { Button } from "../components/ui/button";
import { Card, CardContent, CardHeader, CardTitle } from "../components/ui/card";
import { ScrollArea } from "../components/ui/scroll-area";
import { format } from "date-fns";
import {
  TimelineEntryList,
  type TimelineEntry,
  type PersonaClickTarget,
} from "../components/timeline/TimelineEntryList";

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
  const navigate = useNavigate();
  const [{ from, to }, setWindow] = useState(defaultWindow);
  const [domains, setDomains] = useState<DomainDescriptor[]>([]);
  const [selectedDomains, setSelectedDomains] = useState<string[]>([]);
  const [events, setEvents] = useState<EventEnvelope[]>([]);
  const [arrows, setArrows] = useState<Arrow[]>([]);
  const [identities, setIdentities] = useState<Identity[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [personaPrompt, setPersonaPrompt] = useState<PersonaPromptState | null>(
    null,
  );
  const [personaEmail, setPersonaEmail] = useState("");
  const [personaName, setPersonaName] = useState("");
  const [personaBusy, setPersonaBusy] = useState(false);
  const [personaError, setPersonaError] = useState<string | null>(null);

  const loadIdentities = () => {
    listIdentities()
      .then(setIdentities)
      .catch((err) => setError((prev) => prev ?? err.message));
  };

  useEffect(() => {
    getDomains()
      .then((available) => {
        setDomains(available);
        if (selectedDomains.length === 0) {
          setSelectedDomains(available.map((d) => d.id));
        }
      })
      .catch((err) => setError(err.message));
    loadIdentities();
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

  const handlePersonaClick = (target: PersonaClickTarget) => {
    if (target.identityId) {
      navigate(`/identities/${target.identityId}`);
      return;
    }
    if (target.persona) {
      setPersonaPrompt({ persona: target.persona, label: target.label });
      setPersonaEmail("");
      setPersonaName("");
      setPersonaError(null);
    }
  };

  async function handlePersonaAttach() {
    if (!personaPrompt || !personaEmail.trim()) {
      setPersonaError("Email is required");
      return;
    }
    setPersonaBusy(true);
    setPersonaError(null);
    try {
      const email = personaEmail.trim();
      const preferredName = personaName.trim();
      let identity =
        (await lookupIdentity({ email })) ??
        (await createIdentity({
          canonical_email: email,
          preferred_name: preferredName || undefined,
        }));
      await attachPersona(identity.id, {
        domain: personaPrompt.persona.domain,
        local_id: personaPrompt.persona.local_id,
        label: personaPrompt.label,
        display_name: preferredName || undefined,
      });
      loadIdentities();
      setPersonaPrompt(null);
      setPersonaEmail("");
      setPersonaName("");
    } catch (err) {
      setPersonaError((err as Error).message);
    } finally {
      setPersonaBusy(false);
    }
  }

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
          <TimelineEntryList
            entries={timeline}
            domainLookup={domainLookup}
            identityLookup={identityLookup}
            personaLookup={personaLookup}
            onPersonaClick={handlePersonaClick}
          />
        )}
      </ScrollArea>

      {personaPrompt ? (
        <Card className="border border-dashed border-primary">
          <CardHeader>
            <CardTitle className="text-base">
              Link persona @{personaPrompt.persona.local_id} (
              {personaPrompt.persona.domain})
            </CardTitle>
            <p className="text-xs text-muted-foreground">
              Attach an email to convert this persona into a full identity.
            </p>
          </CardHeader>
          <CardContent className="space-y-3 text-sm">
            <label className="flex flex-col gap-1">
              Canonical email
              <input
                className="rounded-md border border-input bg-background px-3 py-2"
                placeholder="person@example.com"
                value={personaEmail}
                onChange={(e) => setPersonaEmail(e.target.value)}
              />
            </label>
            <label className="flex flex-col gap-1">
              Preferred name (optional)
              <input
                className="rounded-md border border-input bg-background px-3 py-2"
                placeholder="Ada Lovelace"
                value={personaName}
                onChange={(e) => setPersonaName(e.target.value)}
              />
            </label>
            {personaError ? (
              <p className="text-xs text-destructive-foreground">
                {personaError}
              </p>
            ) : null}
            <div className="flex flex-wrap gap-2">
              <Button
                onClick={handlePersonaAttach}
                disabled={personaBusy}
                className="text-sm"
              >
                {personaBusy ? "Linking..." : "Attach persona"}
              </Button>
              <Button
                variant="ghost"
                type="button"
                onClick={() => setPersonaPrompt(null)}
              >
                Cancel
              </Button>
            </div>
          </CardContent>
        </Card>
      ) : null}
    </div>
  );
}

interface PersonaPromptState {
  persona: PersonaKey;
  label?: string;
}

