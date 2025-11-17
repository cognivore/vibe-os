import { Button } from "../../../components/ui/button";
import {
  Card,
  CardContent,
  CardHeader,
  CardTitle,
} from "../../../components/ui/card";
import type { IdentityOption, ProviderMode } from "../types";

interface LinkPersonaFormProps {
  providerLabel: string;
  providerMode: ProviderMode;
  identityOptions: IdentityOption[];
  linkIdentityId: string;
  linkLocalId: string;
  linkLabel: string;
  linkDisplayName: string;
  selectedPersonaKey: string | null;
  onIdentityChange: (value: string) => void;
  onLocalIdChange: (value: string) => void;
  onLabelChange: (value: string) => void;
  onDisplayNameChange: (value: string) => void;
  onSubmit: () => void;
  onClearSelection: () => void;
  canSubmit: boolean;
}

export function LinkPersonaForm({
  providerLabel,
  providerMode,
  identityOptions,
  linkIdentityId,
  linkLocalId,
  linkLabel,
  linkDisplayName,
  selectedPersonaKey,
  onIdentityChange,
  onLocalIdChange,
  onLabelChange,
  onDisplayNameChange,
  onSubmit,
  onClearSelection,
  canSubmit,
}: LinkPersonaFormProps) {
  return (
    <Card className={selectedPersonaKey ? "border-primary" : ""}>
      <CardHeader>
        <CardTitle>Attach {providerLabel} persona to a canonical identity</CardTitle>
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
            onChange={(e) => onIdentityChange(e.target.value)}
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
            onChange={(e) => onLocalIdChange(e.target.value)}
            placeholder={providerMode === "slack" ? "U123456" : "usr_01J..."}
            title="The provider's user ID (e.g., Slack user ID or Linear user UUID)"
          />
        </label>
        <label className="flex flex-col gap-1">
          Label / handle
          <input
            className="rounded-md border border-input bg-background px-3 py-2"
            value={linkLabel}
            onChange={(e) => onLabelChange(e.target.value)}
            placeholder="@handle"
          />
        </label>
        <label className="flex flex-col gap-1">
          Display name (optional)
          <input
            className="rounded-md border border-input bg-background px-3 py-2"
            value={linkDisplayName}
            onChange={(e) => onDisplayNameChange(e.target.value)}
          />
        </label>
        <div className="md:col-span-2 flex gap-2">
          <Button onClick={onSubmit} disabled={!canSubmit}>
            Attach persona
          </Button>
          {selectedPersonaKey && (
            <Button variant="outline" onClick={onClearSelection}>
              Clear
            </Button>
          )}
        </div>
      </CardContent>
    </Card>
  );
}

