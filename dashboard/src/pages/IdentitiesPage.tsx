import { useEffect, useMemo, useState } from "react";
import { useSearchParams } from "react-router-dom";
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
import {
  ProviderPersonaBrowser,
} from "./identities/components/ProviderPersonaBrowser";
import { LinkPersonaForm } from "./identities/components/LinkPersonaForm";
import { CreateIdentityForm } from "./identities/components/CreateIdentityForm";
import { MergeIdentitiesForm } from "./identities/components/MergeIdentitiesForm";
import { IdentityList } from "./identities/components/IdentityList";
import type { IdentityOption, ProviderMode } from "./identities/types";
import { PROVIDER_LABELS } from "./identities/constants";

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

  const identityOptions: IdentityOption[] = useMemo(
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

      <ProviderPersonaBrowser
        providerMode={providerMode}
        providerLabel={providerLabel}
        personas={personasForMode}
        personaLinks={personaLinks}
        selectedPersonaKey={selectedPersonaKey}
        onProviderChange={(mode) => {
          setProviderMode(mode);
          setSelectedPersonaKey(null);
        }}
        onSlackPick={handleSlackPersonaPick}
        onLinearPick={handleLinearPersonaPick}
      />

      <LinkPersonaForm
        providerLabel={providerLabel}
        providerMode={providerMode}
        identityOptions={identityOptions}
        linkIdentityId={linkIdentityId}
        linkLocalId={linkLocalId}
        linkLabel={linkLabel}
        linkDisplayName={linkDisplayName}
        selectedPersonaKey={selectedPersonaKey}
        onIdentityChange={setLinkIdentityId}
        onLocalIdChange={setLinkLocalId}
        onLabelChange={setLinkLabel}
        onDisplayNameChange={setLinkDisplayName}
        onSubmit={handleLink}
        onClearSelection={() => {
          setSelectedPersonaKey(null);
          setLinkLocalId("");
          setLinkLabel("");
          setLinkDisplayName("");
        }}
        canSubmit={Boolean(linkLocalId)}
      />

      <CreateIdentityForm
        email={createEmail}
        name={createName}
        onEmailChange={setCreateEmail}
        onNameChange={setCreateName}
        onSubmit={handleCreate}
      />

      <MergeIdentitiesForm
        identityOptions={identityOptions}
        primary={mergePrimary}
        secondary={mergeSecondary}
        onPrimaryChange={setMergePrimary}
        onSecondaryChange={setMergeSecondary}
        onSubmit={handleMerge}
      />

      <IdentityList
        identities={identities}
        onStartLink={startLinkFlow}
        onDetach={handleDetach}
      />
    </div>
  );
}

