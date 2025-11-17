import { Button } from "../../../components/ui/button";
import {
  Card,
  CardContent,
  CardHeader,
  CardTitle,
} from "../../../components/ui/card";
import type { IdentityOption } from "../types";

interface MergeIdentitiesFormProps {
  identityOptions: IdentityOption[];
  primary: string;
  secondary: string;
  onPrimaryChange: (value: string) => void;
  onSecondaryChange: (value: string) => void;
  onSubmit: () => void;
}

export function MergeIdentitiesForm({
  identityOptions,
  primary,
  secondary,
  onPrimaryChange,
  onSecondaryChange,
  onSubmit,
}: MergeIdentitiesFormProps) {
  const disabled = !primary || !secondary || primary === secondary;

  return (
    <Card>
      <CardHeader>
        <CardTitle>Merge identities</CardTitle>
      </CardHeader>
      <CardContent className="grid gap-2 text-sm md:grid-cols-2">
        <label className="flex flex-col gap-1">
          Primary identity
          <select
            className="rounded-md border border-input bg-background px-3 py-2"
            value={primary}
            onChange={(e) => onPrimaryChange(e.target.value)}
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
            value={secondary}
            onChange={(e) => onSecondaryChange(e.target.value)}
          >
            {identityOptions.map((option) => (
              <option key={`${option.id}-secondary`} value={option.id}>
                {option.label}
              </option>
            ))}
          </select>
        </label>
        <div className="md:col-span-2">
          <Button onClick={onSubmit} disabled={disabled}>
            Merge identities
          </Button>
        </div>
      </CardContent>
    </Card>
  );
}

