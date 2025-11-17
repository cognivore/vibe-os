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
import { ArrowFilters } from "./arrows/ArrowFilters";
import { ArrowTable } from "./arrows/ArrowTable";

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
    { label: "Implementation", value: "implementation_request" },
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
      <ArrowFilters
        from={from}
        to={to}
        direction={direction}
        directionOptions={directionOptions}
        domains={domains}
        sourceDomains={sourceDomains}
        targetDomains={targetDomains}
        onFromChange={setFrom}
        onToChange={setTo}
        onDirectionChange={setDirection}
        onSourceDomainsChange={setSourceDomains}
        onTargetDomainsChange={setTargetDomains}
        onResetRange={() => {
          setFrom(toIsoDate(-7));
          setTo(toIsoDate(0));
        }}
      />

      {error ? (
        <p className="text-sm text-destructive-foreground">{error}</p>
      ) : null}
      <div className="text-right text-xs">
        <Link to="/identities" className="underline">
          Manage identities
        </Link>
      </div>

      <ArrowTable
        arrows={arrows}
        loading={loading}
        domainLookup={domainLookup}
        identityLookup={identityLookup}
        personaLookup={personaLookup}
      />
    </div>
  );
}

