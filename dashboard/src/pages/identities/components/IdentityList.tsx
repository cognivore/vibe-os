import { Link } from "react-router-dom";
import type { Identity } from "../../../types/core";
import { Badge } from "../../../components/ui/badge";
import { Button } from "../../../components/ui/button";
import {
  Card,
  CardContent,
  CardHeader,
  CardTitle,
} from "../../../components/ui/card";
import { PROVIDER_LABELS } from "../constants";
import type { ProviderMode } from "../types";

interface IdentityListProps {
  identities: Identity[];
  onStartLink: (identityId: string, provider: ProviderMode) => void;
  onDetach: (identityId: string, personaId: string) => void;
}

export function IdentityList({
  identities,
  onStartLink,
  onDetach,
}: IdentityListProps) {
  return (
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
                    <Badge key={persona.id} className="flex items-center gap-1">
                      <span>
                        {persona.label ?? persona.key.local_id} ({persona.key.domain})
                      </span>
                      <button
                        onClick={(e) => {
                          e.stopPropagation();
                          onDetach(identity.id, persona.id);
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
                        className="ml-2 underline decoration-dotted underline-offset-2"
                        onClick={() => onStartLink(identity.id, provider)}
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
  );
}

