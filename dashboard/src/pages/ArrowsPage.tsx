import { useEffect, useMemo, useState } from "react";
import { Link } from "react-router-dom";
import { getArrows, getDomains, listIdentities } from "../api/client";
import type {
  Arrow,
  ArrowDirection,
  DomainDescriptor,
  Identity,
  Persona,
} from "../types/core";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "../components/ui/table";
import { Button } from "../components/ui/button";
import { Badge } from "../components/ui/badge";

function toIsoDate(offsetDays: number) {
  const d = new Date();
  d.setDate(d.getDate() + offsetDays);
  return d.toISOString();
}

export default function ArrowsPage() {
  const [from, setFrom] = useState(() => toIsoDate(-7));
  const [to, setTo] = useState(() => toIsoDate(0));
  const [direction, setDirection] = useState<ArrowDirection | undefined>();
  const [sourceDomains, setSourceDomains] = useState<string[]>([]);
  const [targetDomains, setTargetDomains] = useState<string[]>([]);
  const [domains, setDomains] = useState<DomainDescriptor[]>([]);
  const [arrows, setArrows] = useState<Arrow[]>([]);
  const [identities, setIdentities] = useState<Identity[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    getDomains()
      .then((list) => setDomains(list))
      .catch((err) => setError(err.message));
    listIdentities()
      .then(setIdentities)
      .catch((err) => setError((prev) => prev ?? err.message));
  }, []);

  useEffect(() => {
    setLoading(true);
    setError(null);
    getArrows({
      from,
      to,
      direction,
      sourceDomains: sourceDomains.length ? sourceDomains : undefined,
      targetDomains: targetDomains.length ? targetDomains : undefined,
    })
      .then(setArrows)
      .catch((err) => setError(err.message))
      .finally(() => setLoading(false));
  }, [from, to, direction, sourceDomains, targetDomains]);

  const directionOptions: { label: string; value?: ArrowDirection }[] = [
    { label: "All" },
    { label: "Analysis", value: "analysis_request" },
    { label: "Synthesis", value: "synthesis_request" },
    { label: "Elaboration", value: "elaboration_request" },
  ];

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
    identities.forEach((identity) =>
      identity.personas.forEach((persona) => {
        map[persona.id] = { persona, identityId: identity.id };
      }),
    );
    return map;
  }, [identities]);

  return (
    <div className="space-y-6">
      <div className="flex flex-col gap-4 md:flex-row md:items-end md:justify-between">
        <div>
          <h2 className="text-2xl font-semibold">Arrows</h2>
          <p className="text-sm text-muted-foreground">
            Typed suggestions flowing between domains
          </p>
        </div>
        <div className="flex items-center gap-2">
          <Button
            variant="secondary"
            onClick={() => {
              setFrom(toIsoDate(-7));
              setTo(toIsoDate(0));
            }}
          >
            Last 7 days
          </Button>
        </div>
      </div>

      <div className="grid gap-4 md:grid-cols-2">
        <div className="space-y-2 rounded-lg border border-border p-4">
          <label className="flex flex-col text-xs">
            From
            <input
              type="datetime-local"
              value={from.slice(0, 16)}
              onChange={(e) => setFrom(new Date(e.target.value).toISOString())}
              className="rounded-md border border-input bg-background px-2 py-1 text-sm"
            />
          </label>
          <label className="flex flex-col text-xs">
            To
            <input
              type="datetime-local"
              value={to.slice(0, 16)}
              onChange={(e) => setTo(new Date(e.target.value).toISOString())}
              className="rounded-md border border-input bg-background px-2 py-1 text-sm"
            />
          </label>
        </div>
        <div className="space-y-4 rounded-lg border border-border p-4">
          <div className="flex flex-wrap gap-2 text-sm">
            {directionOptions.map((option) => (
              <Button
                key={option.label}
                variant={
                  option.value === direction || (!option.value && !direction)
                    ? "default"
                    : "secondary"
                }
                onClick={() => setDirection(option.value)}
              >
                {option.label}
              </Button>
            ))}
          </div>
          <div className="grid grid-cols-2 gap-2">
            <DomainMultiSelect
              label="Source"
              domains={domains}
              selected={sourceDomains}
              onChange={setSourceDomains}
            />
            <DomainMultiSelect
              label="Target"
              domains={domains}
              selected={targetDomains}
              onChange={setTargetDomains}
            />
          </div>
        </div>
      </div>

      {error ? (
        <p className="text-sm text-destructive-foreground">{error}</p>
      ) : null}
      <div className="text-right text-xs">
        <Link to="/identities" className="underline">
          Manage identities
        </Link>
      </div>

      <div className="rounded-lg border border-border">
        <Table>
          <TableHeader>
            <TableRow>
              <TableHead>When</TableHead>
              <TableHead>Direction</TableHead>
              <TableHead>Source → Target</TableHead>
              <TableHead>Title</TableHead>
            </TableRow>
          </TableHeader>
          <TableBody>
            {loading ? (
              <TableRow>
                <TableCell colSpan={4} className="text-center text-sm">
                  Loading...
                </TableCell>
              </TableRow>
            ) : (
              arrows.map((arrow) => (
                <TableRow key={arrow.id}>
                  <TableCell className="text-xs text-muted-foreground">
                    {new Date(arrow.created_at).toLocaleString()}
                  </TableCell>
                  <TableCell>
                    <Badge>{arrow.direction.replace("_", " ")}</Badge>
                  </TableCell>
                  <TableCell className="text-sm">
                    {renderEndpoint(arrow.source, domainLookup)} →{" "}
                    {renderEndpoint(arrow.target, domainLookup)}
                  </TableCell>
                  <TableCell>
                    <p className="font-medium">{arrow.title}</p>
                    <p className="text-xs text-muted-foreground">
                      {arrow.detail_markdown}
                    </p>
                {renderArrowAuthorChip(arrow, identityLookup, personaLookup)}
                  </TableCell>
                </TableRow>
              ))
            )}
          </TableBody>
        </Table>
      </div>
    </div>
  );
}

function renderEndpoint(
  endpoint: Arrow["source"],
  lookup: Record<string, string>,
) {
  if (endpoint.kind === "domain") {
    return lookup[endpoint.domain] ?? endpoint.domain;
  }
  const label = lookup[endpoint.domain] ?? endpoint.domain;
  return `${label}:${endpoint.object_id}`;
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
      <p className="text-xs text-muted-foreground">
        By {identity.preferred_name ?? identity.canonical_email}
        {personaInfo
          ? ` (${personaInfo.persona.key.domain}:${personaInfo.persona.key.local_id})`
          : null}
      </p>
    );
  }
  if (arrow.author_persona_key) {
    return (
      <p className="text-xs text-muted-foreground">
        By {arrow.author_persona_key.domain}:{arrow.author_persona_key.local_id}
      </p>
    );
  }
  return null;
}

interface DomainMultiSelectProps {
  label: string;
  domains: DomainDescriptor[];
  selected: string[];
  onChange: (value: string[]) => void;
}

function DomainMultiSelect({
  label,
  domains,
  selected,
  onChange,
}: DomainMultiSelectProps) {
  return (
    <div className="space-y-2">
      <p className="text-xs uppercase tracking-wide text-muted-foreground">
        {label}
      </p>
      <div className="flex flex-wrap gap-1 text-sm">
        {domains.map((domain) => {
          const active = selected.includes(domain.id);
          return (
            <Button
              key={`${label}-${domain.id}`}
              variant={active ? "default" : "outline"}
              size="sm"
              onClick={() =>
                onChange(
                  active
                    ? selected.filter((id) => id !== domain.id)
                    : [...selected, domain.id],
                )
              }
            >
              {domain.human_name}
            </Button>
          );
        })}
      </div>
    </div>
  );
}

