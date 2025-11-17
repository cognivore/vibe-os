import type {
  Arrow,
  EventEnvelope,
  Identity,
  Persona,
  PersonaKey,
} from "../../../types/core";
import { Badge } from "../../ui/badge";
import type { PersonaClickTarget } from "../types";

interface ActorChipProps {
  event: EventEnvelope;
  identities: Record<string, Identity>;
  personas: Record<string, { persona: Persona; identityId: string }>;
  onPersonaClick?: (target: PersonaClickTarget) => void;
}

export function ActorChip({
  event,
  identities,
  personas,
  onPersonaClick,
}: ActorChipProps) {
  if (event.actor_identity_id && identities[event.actor_identity_id]) {
    const identity = identities[event.actor_identity_id];
    const personaInfo = event.actor_persona_id
      ? personas[event.actor_persona_id]
      : undefined;
    const personaKey: PersonaKey | undefined =
      personaInfo?.persona.key ?? normalizePersonaKey(event.actor_persona_key);
    return (
      <Badge
        variant="outline"
        className="cursor-pointer"
        onClick={() =>
          onPersonaClick?.({
            identityId: identity.id,
            persona: personaKey,
            label:
              identity.preferred_name ?? identity.canonical_email ?? undefined,
          })
        }
      >
        {identity.preferred_name ?? identity.canonical_email}
        {personaInfo
          ? ` (${personaInfo.persona.key.domain}:${personaInfo.persona.key.local_id})`
          : null}
      </Badge>
    );
  }
  if (event.actor_persona_key) {
    const personaKey = event.actor_persona_key;
    return (
      <Badge
        variant="outline"
        className="cursor-pointer"
        onClick={() =>
          onPersonaClick?.({ persona: personaKey, label: undefined })
        }
      >
        @{personaKey.local_id} ({personaKey.domain})
      </Badge>
    );
  }
  return null;
}

interface ArrowAuthorChipProps {
  arrow: Arrow;
  identities: Record<string, Identity>;
  personas: Record<string, { persona: Persona; identityId: string }>;
  onPersonaClick?: (target: PersonaClickTarget) => void;
}

export function ArrowAuthorChip({
  arrow,
  identities,
  personas,
  onPersonaClick,
}: ArrowAuthorChipProps) {
  if (arrow.author_identity_id && identities[arrow.author_identity_id]) {
    const identity = identities[arrow.author_identity_id];
    const personaInfo = arrow.author_persona_id
      ? personas[arrow.author_persona_id]
      : undefined;
    const personaKey: PersonaKey | undefined =
      personaInfo?.persona.key ??
      normalizePersonaKey(arrow.author_persona_key);
    return (
      <Badge
        variant="outline"
        className="cursor-pointer"
        onClick={() =>
          onPersonaClick?.({
            identityId: identity.id,
            persona: personaKey,
            label:
              identity.preferred_name ?? identity.canonical_email ?? undefined,
          })
        }
      >
        {identity.preferred_name ?? identity.canonical_email}
        {personaInfo
          ? ` (${personaInfo.persona.key.domain}:${personaInfo.persona.key.local_id})`
          : null}
      </Badge>
    );
  }
  if (arrow.author_persona_key) {
    const personaKey = arrow.author_persona_key;
    return (
      <Badge
        variant="outline"
        className="cursor-pointer"
        onClick={() =>
          onPersonaClick?.({
            persona: personaKey,
            label: undefined,
          })
        }
      >
        {personaKey.domain}:{personaKey.local_id}
      </Badge>
    );
  }
  return null;
}

function normalizePersonaKey(
  key?: PersonaKey | null,
): PersonaKey | undefined {
  return key ?? undefined;
}

