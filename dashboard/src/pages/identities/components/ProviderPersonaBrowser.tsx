import type {
  Identity,
  LinearProviderPersona,
  SlackProviderPersona,
} from "../../../types/core";
import { Badge } from "../../../components/ui/badge";
import { Button } from "../../../components/ui/button";
import {
  Card,
  CardContent,
  CardHeader,
  CardTitle,
} from "../../../components/ui/card";
import { ScrollArea } from "../../../components/ui/scroll-area";
import { PROVIDER_LABELS } from "../constants";
import type { ProviderMode } from "../types";

interface ProviderPersonaBrowserProps {
  providerMode: ProviderMode;
  personas: SlackProviderPersona[] | LinearProviderPersona[];
  personaLinks: Map<string, Identity>;
  providerLabel: string;
  selectedPersonaKey: string | null;
  onProviderChange: (mode: ProviderMode) => void;
  onSlackPick: (persona: SlackProviderPersona) => void;
  onLinearPick: (persona: LinearProviderPersona) => void;
}

export function ProviderPersonaBrowser({
  providerMode,
  personas,
  personaLinks,
  providerLabel,
  selectedPersonaKey,
  onProviderChange,
  onSlackPick,
  onLinearPick,
}: ProviderPersonaBrowserProps) {
  return (
    <Card>
      <CardHeader className="flex flex-col gap-3 md:flex-row md:items-end md:justify-between">
        <div>
          <CardTitle>{providerLabel} personas</CardTitle>
          <p className="text-sm text-muted-foreground">
            Select a persona to pre-fill the attach form. Already linked personas
            show their identity.
          </p>
        </div>
        <div className="flex gap-2">
          {(["slack", "linear"] as ProviderMode[]).map((provider) => (
            <Button
              key={provider}
              variant={providerMode === provider ? "default" : "outline"}
              onClick={() => onProviderChange(provider)}
            >
              {PROVIDER_LABELS[provider]}
            </Button>
          ))}
        </div>
      </CardHeader>
      <CardContent>
        {personas.length === 0 ? (
          <p className="text-sm text-muted-foreground">
            No personas mirrored yet for {providerLabel}. Run the mirror to populate
            provider directories.
          </p>
        ) : (
          <ScrollArea className="h-80 rounded border border-border">
            <div className="divide-y divide-border">
              {personas.map((persona) => {
                const personaKey =
                  providerMode === "slack"
                    ? `slack:${persona.user_id}`
                    : `linear:${persona.user_id}`;
                const linkedIdentity = personaLinks.get(personaKey);
                const isSelected = selectedPersonaKey === personaKey;
                const displayTitle =
                  providerMode === "slack"
                    ? (persona as SlackProviderPersona).real_name ??
                      (persona as SlackProviderPersona).display_name ??
                      (persona as SlackProviderPersona).username ??
                      (persona as SlackProviderPersona).email ??
                      persona.user_id
                    : (persona as LinearProviderPersona).name ??
                      (persona as LinearProviderPersona).display_name ??
                      (persona as LinearProviderPersona).email ??
                      persona.user_id;

                const email =
                  (persona as SlackProviderPersona).email ??
                  (persona as LinearProviderPersona).email ??
                  null;

                const onPick =
                  providerMode === "slack"
                    ? () => onSlackPick(persona as SlackProviderPersona)
                    : () => onLinearPick(persona as LinearProviderPersona);

                return (
                  <div
                    key={personaKey}
                    role="button"
                    tabIndex={0}
                    className={`flex w-full cursor-pointer items-center justify-between gap-4 px-4 py-3 text-left transition ${
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
  );
}

