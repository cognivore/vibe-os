import { useEffect, useMemo, useState } from "react";
import { Link, useSearchParams } from "react-router-dom";
import {
  attachPersona,
  createIdentity,
  detachPersona,
  getProviderPersonas,
  listIdentities,
  mergeIdentities,
} from "../api/client";
import type {
  Identity,
  LinearProviderPersona,
  ProviderPersonasPayload,
  SlackProviderPersona,
} from "../types/core";
import { Button } from "../components/ui/button";
import { Card, CardContent, CardHeader, CardTitle } from "../components/ui/card";
import { Badge } from "../components/ui/badge";
import { ScrollArea } from "../components/ui/scroll-area";

type ProviderMode = "slack" | "linear";

const PROVIDER_LABELS: Record<ProviderMode, string> = {
  slack: "Slack",
  linear: "Linear",
};

export default function IdentitiesPage() {
  const [searchParams] = useSearchParams();
  const [identities, setIdentities] = useState<Identity[]>([]);
  const [providerData, setProviderData] = useState<ProviderPersonasPayload>({
    slack: [],
    linear: [],
  });
  const [providerMode, setProviderMode] = useState<ProviderMode>("slack");
  const [error, setError] = useState<string | null>(null);

  const [createEmail, setCreateEmail] = useState("");
  const [createName, setCreateName] = useState("");

  const [linkIdentityId, setLinkIdentityId] = useState("");
  const [linkLocalId, setLinkLocalId] = useState("");
  const [linkLabel, setLinkLabel] = useState("");
  const [linkDisplayName, setLinkDisplayName] = useState("");

  const [mergePrimary, setMergePrimary] = useState("");
  const [mergeSecondary, setMergeSecondary] = useState("");

  const [selectedPersonaKey, setSelectedPersonaKey] = useState<string | null>(
    null,
  );

  useEffect(() => {
    Promise.all([listIdentities(), getProviderPersonas()])
      .then(([list, personas]) => {
        setIdentities(list);
        setProviderData(personas);
        if (!linkIdentityId && list.length > 0) {
          setLinkIdentityId(list[0].id);
        }
        if (!mergePrimary && list.length > 0) {
          setMergePrimary(list[0].id);
        }
        if (!mergeSecondary && list.length > 1) {
          setMergeSecondary(list[1].id);
        }
      })
      .catch((err) => setError(err.message));
  }, []);

  useEffect(() => {
    const providerParam = searchParams.get("provider");
    if (providerParam === "slack" || providerParam === "linear") {
      setProviderMode(providerParam);
    }
    const identityParam = searchParams.get("identity");
    if (identityParam) {
      setLinkIdentityId(identityParam);
    }
    const localIdParam = searchParams.get("local_id");
    if (localIdParam) {
      setLinkLocalId(localIdParam);
    }
    const labelParam = searchParams.get("label");
    if (labelParam) {
      setLinkLabel(labelParam);
      setLinkDisplayName(labelParam);
    }
  }, [searchParams]);

  function refreshIdentities() {
    listIdentities()
      .then((list) => {
        setIdentities(list);
        if (!linkIdentityId && list.length > 0) {
          setLinkIdentityId(list[0].id);
        }
        if (!mergePrimary && list.length > 0) {
          setMergePrimary(list[0].id);
        }
        if (!mergeSecondary && list.length > 1) {
          setMergeSecondary(list[1].id);
        }
      })
      .catch((err) => setError(err.message));
  }

  function refreshProviderData() {
    getProviderPersonas()
      .then(setProviderData)
      .catch((err) => setError(err.message));
  }

  const identityOptions = useMemo(
    () =>
      identities.map((identity) => ({
        id: identity.id,
        label: `${identity.preferred_name ?? identity.canonical_email} (${identity.canonical_email})`,
      })),
    [identities],
  );

  const identityByEmail = useMemo(() => {
    const map = new Map<string, string>();
    identities.forEach((identity) => {
      map.set(identity.canonical_email.toLowerCase(), identity.id);
    });
    return map;
  }, [identities]);

  const personaLinks = useMemo(() => {
    const map = new Map<string, Identity>();
    identities.forEach((identity) => {
      identity.personas.forEach((persona) => {
        map.set(
          `${persona.key.domain}:${persona.key.local_id}`,
          identity,
        );
      });
    });
    return map;
  }, [identities]);

  const personasForMode = providerMode === "slack"
    ? providerData.slack
    : providerData.linear;
  const providerLabel = PROVIDER_LABELS[providerMode];
  const providerDomain = providerMode;

  async function handleCreate() {
    try {
      await createIdentity({
        canonical_email: createEmail.trim(),
        preferred_name: createName || undefined,
      });
      setCreateEmail("");
      setCreateName("");
      refreshIdentities();
    } catch (err) {
      setError((err as Error).message);
    }
  }

  async function handleLink() {
    if (!linkIdentityId || !linkLocalId) return;
    try {
      await attachPersona(linkIdentityId, {
        domain: providerDomain,
        local_id: linkLocalId,
        label: linkLabel || undefined,
        display_name: linkDisplayName || undefined,
      });
      setLinkLocalId("");
      setLinkLabel("");
      setLinkDisplayName("");
      setSelectedPersonaKey(null);
      refreshIdentities();
      refreshProviderData();
    } catch (err) {
      setError((err as Error).message);
    }
  }

  async function handleMerge() {
    if (!mergePrimary || !mergeSecondary || mergePrimary === mergeSecondary) {
      return;
    }
    try {
      await mergeIdentities(mergePrimary, mergeSecondary);
      refreshIdentities();
    } catch (err) {
      setError((err as Error).message);
    }
  }

  function handleSlackPersonaPick(persona: SlackProviderPersona) {
    setProviderMode("slack");
    setSelectedPersonaKey(`slack:${persona.user_id}`);
    setLinkLocalId(persona.user_id);
    setLinkLabel(
      persona.username
        ? `@${persona.username}`
        : persona.display_name ?? persona.real_name ?? "",
    );
    setLinkDisplayName(persona.display_name ?? persona.real_name ?? "");
    if (persona.email) {
      const identityId = identityByEmail.get(persona.email.toLowerCase());
      if (identityId) {
        setLinkIdentityId(identityId);
      }
      if (!createEmail) {
        setCreateEmail(persona.email);
      }
    }
  }

  function handleLinearPersonaPick(persona: LinearProviderPersona) {
    setProviderMode("linear");
    setSelectedPersonaKey(`linear:${persona.user_id}`);
    setLinkLocalId(persona.user_id);
    setLinkLabel(persona.display_name ?? persona.name ?? "");
    setLinkDisplayName(persona.display_name ?? persona.name ?? "");
    if (persona.email) {
      const identityId = identityByEmail.get(persona.email.toLowerCase());
      if (identityId) {
        setLinkIdentityId(identityId);
      }
      if (!createEmail) {
        setCreateEmail(persona.email);
      }
    }
  }

  function startLinkFlow(identityId: string, provider: ProviderMode) {
    setProviderMode(provider);
    setLinkIdentityId(identityId);
    setSelectedPersonaKey(null);
    setLinkLocalId("");
    setLinkLabel("");
    setLinkDisplayName("");
  }

  async function handleDetach(identityId: string, personaId: string) {
    if (!confirm("Detach this persona? This action cannot be undone.")) {
      return;
    }
    try {
      await detachPersona(identityId, personaId);
      refreshIdentities();
      refreshProviderData();
    } catch (err) {
      setError((err as Error).message);
    }
  }

  return (
    <div className="space-y-6">
      <div>
        <h2 className="text-2xl font-semibold">Identities</h2>
        <p className="text-sm text-muted-foreground">
          Canonical people (by email) and their provider personas.
        </p>
      </div>

      {error ? (
        <p className="text-sm text-destructive-foreground">{error}</p>
      ) : null}

      <Card>
        <CardHeader className="flex flex-col gap-3 md:flex-row md:items-end md:justify-between">
          <div>
            <CardTitle>{providerLabel} personas</CardTitle>
            <p className="text-sm text-muted-foreground">
              Select a persona to pre-fill the attach form. Already linked
              personas show their identity.
            </p>
          </div>
          <div className="flex gap-2">
            {(["slack", "linear"] as ProviderMode[]).map((provider) => (
              <Button
                key={provider}
                variant={providerMode === provider ? "default" : "outline"}
                onClick={() => {
                  setProviderMode(provider);
                  setSelectedPersonaKey(null);
                }}
              >
                {PROVIDER_LABELS[provider]}
              </Button>
            ))}
          </div>
        </CardHeader>
        <CardContent>
          {personasForMode.length === 0 ? (
            <p className="text-sm text-muted-foreground">
              No personas mirrored yet for {providerLabel}. Run the mirror to
              populate provider directories.
            </p>
          ) : (
            <ScrollArea className="h-80 rounded border border-border">
              <div className="divide-y divide-border">
                {personasForMode.map((persona) => {
                  const personaKey =
                    providerMode === "slack"
                      ? `slack:${persona.user_id}`
                      : `linear:${persona.user_id}`;
                  const linkedIdentity = personaLinks.get(personaKey);
                  const isSelected = selectedPersonaKey === personaKey;
                  const displayTitle =
                    providerMode === "slack"
                      ? (persona as SlackProviderPersona).display_name ??
                        (persona as SlackProviderPersona).real_name ??
                        persona.user_id
                      : (persona as LinearProviderPersona).display_name ??
                        (persona as LinearProviderPersona).name ??
                        persona.user_id;

                  const email =
                    (persona as SlackProviderPersona).email ??
                    (persona as LinearProviderPersona).email ??
                    null;

                  const onPick =
                    providerMode === "slack"
                      ? () =>
                          handleSlackPersonaPick(persona as SlackProviderPersona)
                      : () =>
                          handleLinearPersonaPick(
                            persona as LinearProviderPersona,
                          );

                  return (
                    <div
                      key={personaKey}
                      role="button"
                      tabIndex={0}
                      className={`flex w-full items-center justify-between gap-4 px-4 py-3 text-left transition cursor-pointer ${
                        isSelected
                          ? "bg-primary/10"
                          : "hover:bg-muted/60 focus:bg-muted/60"
                      }`}
                      onClick={onPick}
                      onKeyDown={(e) => {
                        if (e.key === "Enter" || e.key === " ") {
                          e.preventDefault();
                          onPick();
                        }
                      }}
                    >
                      <div>
                        <p className="font-medium">{displayTitle}</p>
                        <p className="text-xs text-muted-foreground">
                          {email ?? "No email"} · {persona.user_id}
                        </p>
                      </div>
                      <div className="flex items-center gap-2">
                        {linkedIdentity ? (
                          <Badge variant="secondary">
                            Linked to{" "}
                            {linkedIdentity.preferred_name ??
                              linkedIdentity.canonical_email}
                          </Badge>
                        ) : (
                          <Badge variant="outline">Unlinked</Badge>
                        )}
                        <Button
                          size="sm"
                          variant="outline"
                          onClick={(e) => {
                            e.stopPropagation();
                            onPick();
                          }}
                        >
                          {linkedIdentity ? "View" : "Use"}
                        </Button>
                      </div>
                    </div>
                  );
                })}
              </div>
            </ScrollArea>
          )}
        </CardContent>
      </Card>

      <Card className={selectedPersonaKey ? "border-primary" : ""}>
        <CardHeader>
          <CardTitle>
            Attach {providerLabel} persona to a canonical identity
          </CardTitle>
          <p className="text-sm text-muted-foreground">
            Pick a persona above or enter one manually.
          </p>
        </CardHeader>
        <CardContent className="grid gap-2 text-sm md:grid-cols-2">
          <label className="flex flex-col gap-1">
            Identity
            <select
              className="rounded-md border border-input bg-background px-3 py-2"
              value={linkIdentityId}
              onChange={(e) => setLinkIdentityId(e.target.value)}
            >
              {identityOptions.map((option) => (
                <option key={option.id} value={option.id}>
                  {option.label}
                </option>
              ))}
            </select>
          </label>
          <label className="flex flex-col gap-1 text-muted-foreground">
            Provider
            <input
              className="rounded-md border border-input bg-muted px-3 py-2 text-foreground"
              value={providerLabel}
              readOnly
            />
          </label>
          <label className="flex flex-col gap-1">
            Remote ID
            <input
              className="rounded-md border border-input bg-background px-3 py-2"
              value={linkLocalId}
              onChange={(e) => setLinkLocalId(e.target.value)}
              placeholder={
                providerMode === "slack" ? "U123456" : "usr_01J..."
              }
              title="The provider's user ID (e.g., Slack user ID or Linear user UUID)"
            />
          </label>
          <label className="flex flex-col gap-1">
            Label / handle
            <input
              className="rounded-md border border-input bg-background px-3 py-2"
              value={linkLabel}
              onChange={(e) => setLinkLabel(e.target.value)}
              placeholder="@handle"
            />
          </label>
          <label className="flex flex-col gap-1">
            Display name (optional)
            <input
              className="rounded-md border border-input bg-background px-3 py-2"
              value={linkDisplayName}
              onChange={(e) => setLinkDisplayName(e.target.value)}
            />
          </label>
          <div className="md:col-span-2 flex gap-2">
            <Button onClick={handleLink} disabled={!linkLocalId}>
              Attach persona
            </Button>
            {selectedPersonaKey && (
              <Button
                variant="outline"
                onClick={() => {
                  setSelectedPersonaKey(null);
                  setLinkLocalId("");
                  setLinkLabel("");
                  setLinkDisplayName("");
                }}
              >
                Clear
              </Button>
            )}
          </div>
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>Create identity</CardTitle>
        </CardHeader>
        <CardContent className="grid gap-2 text-sm md:grid-cols-2">
          <label className="flex flex-col gap-1">
            Canonical email
            <input
              className="rounded-md border border-input bg-background px-3 py-2"
              value={createEmail}
              onChange={(e) => setCreateEmail(e.target.value)}
              placeholder="person@example.com"
            />
          </label>
          <label className="flex flex-col gap-1">
            Preferred name (optional)
            <input
              className="rounded-md border border-input bg-background px-3 py-2"
              value={createName}
              onChange={(e) => setCreateName(e.target.value)}
              placeholder="Ada Lovelace"
            />
          </label>
          <div className="md:col-span-2">
            <Button onClick={handleCreate} disabled={!createEmail.trim()}>
              Create identity
            </Button>
          </div>
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>Merge identities</CardTitle>
        </CardHeader>
        <CardContent className="grid gap-2 text-sm md:grid-cols-2">
          <label className="flex flex-col gap-1">
            Primary identity
            <select
              className="rounded-md border border-input bg-background px-3 py-2"
              value={mergePrimary}
              onChange={(e) => setMergePrimary(e.target.value)}
            >
              {identityOptions.map((option) => (
                <option key={`${option.id}-primary`} value={option.id}>
                  {option.label}
                </option>
              ))}
            </select>
          </label>
          <label className="flex flex-col gap-1">
            Secondary identity
            <select
              className="rounded-md border border-input bg-background px-3 py-2"
              value={mergeSecondary}
              onChange={(e) => setMergeSecondary(e.target.value)}
            >
              {identityOptions.map((option) => (
                <option key={`${option.id}-secondary`} value={option.id}>
                  {option.label}
                </option>
              ))}
            </select>
          </label>
          <div className="md:col-span-2">
            <Button
              onClick={handleMerge}
              disabled={
                !mergePrimary ||
                !mergeSecondary ||
                mergePrimary === mergeSecondary
              }
            >
              Merge identities
            </Button>
          </div>
        </CardContent>
      </Card>

      <div className="grid gap-4 md:grid-cols-2">
        {identities.map((identity) => {
          const missingProviders = (["slack", "linear"] as ProviderMode[]).filter(
            (provider) =>
              !identity.personas.some(
                (persona) => persona.key.domain === provider,
              ),
          );
          return (
            <Card key={identity.id}>
              <CardHeader>
                <CardTitle>
                  {identity.preferred_name ?? identity.canonical_email}
                </CardTitle>
                <p className="text-xs text-muted-foreground">
                  {identity.canonical_email}
                </p>
              </CardHeader>
              <CardContent className="space-y-3 text-sm">
                {identity.personas.length === 0 ? (
                  <p className="text-muted-foreground">No personas attached.</p>
                ) : (
                  <div className="flex flex-wrap gap-2">
                    {identity.personas.map((persona) => (
                      <Badge
                        key={persona.id}
                        className="flex items-center gap-1"
                      >
                        <span>
                          {persona.label ?? persona.key.local_id} (
                          {persona.key.domain})
                        </span>
                        <button
                          onClick={(e) => {
                            e.stopPropagation();
                            handleDetach(identity.id, persona.id);
                          }}
                          className="ml-1 hover:text-destructive"
                          title="Detach persona"
                        >
                          ×
                        </button>
                      </Badge>
                    ))}
                  </div>
                )}
                {missingProviders.length ? (
                  <div className="text-xs text-muted-foreground">
                    Missing personas:
                    {missingProviders.map((provider) => (
                      <button
                        key={`${identity.id}-${provider}`}
                        className="ml-1 text-primary underline"
                        onClick={() => startLinkFlow(identity.id, provider)}
                      >
                        {PROVIDER_LABELS[provider]}
                      </button>
                    ))}
                  </div>
                ) : null}
                <Link
                  to={`/identities/${identity.id}`}
                  className="text-xs underline text-primary"
                >
                  View profile
                </Link>
              </CardContent>
            </Card>
          );
        })}
      </div>
    </div>
  );
}

