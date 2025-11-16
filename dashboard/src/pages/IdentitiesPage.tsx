import { useEffect, useMemo, useState } from "react";
import { Link } from "react-router-dom";
import {
  attachPersona,
  createIdentity,
  listIdentities,
  mergeIdentities,
} from "../api/client";
import type { Identity } from "../types/core";
import { Button } from "../components/ui/button";
import { Card, CardContent, CardHeader, CardTitle } from "../components/ui/card";
import { Badge } from "../components/ui/badge";

export default function IdentitiesPage() {
  const [identities, setIdentities] = useState<Identity[]>([]);
  const [error, setError] = useState<string | null>(null);

  const [createEmail, setCreateEmail] = useState("");
  const [createName, setCreateName] = useState("");

  const [linkIdentityId, setLinkIdentityId] = useState("");
  const [linkDomain, setLinkDomain] = useState("slack");
  const [linkLocalId, setLinkLocalId] = useState("");
  const [linkLabel, setLinkLabel] = useState("");
  const [linkDisplayName, setLinkDisplayName] = useState("");

  const [mergePrimary, setMergePrimary] = useState("");
  const [mergeSecondary, setMergeSecondary] = useState("");

  const identityOptions = useMemo(
    () =>
      identities.map((identity) => ({
        id: identity.id,
        label: `${identity.preferred_name ?? identity.canonical_email} (${identity.canonical_email})`,
      })),
    [identities],
  );

  useEffect(() => {
    refresh();
  }, []);

  function refresh() {
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

  async function handleCreate() {
    try {
      await createIdentity({
        canonical_email: createEmail.trim(),
        preferred_name: createName || undefined,
      });
      setCreateEmail("");
      setCreateName("");
      refresh();
    } catch (err) {
      setError((err as Error).message);
    }
  }

  async function handleLink() {
    if (!linkIdentityId || !linkLocalId) return;
    try {
      await attachPersona(linkIdentityId, {
        domain: linkDomain,
        local_id: linkLocalId,
        label: linkLabel || undefined,
        display_name: linkDisplayName || undefined,
      });
      setLinkLocalId("");
      setLinkLabel("");
      setLinkDisplayName("");
      refresh();
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
      refresh();
    } catch (err) {
      setError((err as Error).message);
    }
  }

  return (
    <div className="space-y-6">
      <div>
        <h2 className="text-2xl font-semibold">Identities</h2>
        <p className="text-sm text-muted-foreground">
          Canonical people (by email) and their domain personas.
        </p>
      </div>

      {error ? (
        <p className="text-sm text-destructive-foreground">{error}</p>
      ) : null}

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
            <Button
              onClick={handleCreate}
              disabled={!createEmail.trim()}
            >
              Create identity
            </Button>
          </div>
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>Attach persona</CardTitle>
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
          <label className="flex flex-col gap-1">
            Domain
            <input
              className="rounded-md border border-input bg-background px-3 py-2"
              value={linkDomain}
              onChange={(e) => setLinkDomain(e.target.value)}
              placeholder="slack"
            />
          </label>
          <label className="flex flex-col gap-1">
            Local ID
            <input
              className="rounded-md border border-input bg-background px-3 py-2"
              value={linkLocalId}
              onChange={(e) => setLinkLocalId(e.target.value)}
              placeholder="U123456"
            />
          </label>
          <label className="flex flex-col gap-1">
            Label
            <input
              className="rounded-md border border-input bg-background px-3 py-2"
              value={linkLabel}
              onChange={(e) => setLinkLabel(e.target.value)}
              placeholder="@ada"
            />
          </label>
          <label className="flex flex-col gap-1">
            Display name
            <input
              className="rounded-md border border-input bg-background px-3 py-2"
              value={linkDisplayName}
              onChange={(e) => setLinkDisplayName(e.target.value)}
            />
          </label>
          <div className="md:col-span-2">
            <Button onClick={handleLink} disabled={!linkLocalId}>
              Attach persona
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
        {identities.map((identity) => (
          <Card key={identity.id}>
            <CardHeader>
              <CardTitle>
                {identity.preferred_name ?? identity.canonical_email}
              </CardTitle>
              <p className="text-xs text-muted-foreground">
                {identity.canonical_email}
              </p>
            </CardHeader>
            <CardContent className="space-y-2 text-sm">
              {identity.personas.length === 0 ? (
                <p className="text-muted-foreground">No personas attached.</p>
              ) : (
                identity.personas.map((persona) => (
                  <Badge key={persona.id}>
                    {persona.label ?? persona.key.local_id} ({persona.key.domain})
                  </Badge>
                ))
              )}
              <Link
                to={`/identities/${identity.id}`}
                className="text-xs underline text-primary"
              >
                View profile
              </Link>
            </CardContent>
          </Card>
        ))}
      </div>
    </div>
  );
}

