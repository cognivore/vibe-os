import { Button } from "../../../components/ui/button";
import {
  Card,
  CardContent,
  CardHeader,
  CardTitle,
} from "../../../components/ui/card";

interface CreateIdentityFormProps {
  email: string;
  name: string;
  onEmailChange: (value: string) => void;
  onNameChange: (value: string) => void;
  onSubmit: () => void;
}

export function CreateIdentityForm({
  email,
  name,
  onEmailChange,
  onNameChange,
  onSubmit,
}: CreateIdentityFormProps) {
  return (
    <Card>
      <CardHeader>
        <CardTitle>Create identity</CardTitle>
      </CardHeader>
      <CardContent className="grid gap-2 text-sm md:grid-cols-2">
        <label className="flex flex-col gap-1">
          Canonical email
          <input
            className="rounded-md border border-input bg-background px-3 py-2"
            value={email}
            onChange={(e) => onEmailChange(e.target.value)}
            placeholder="person@example.com"
          />
        </label>
        <label className="flex flex-col gap-1">
          Preferred name (optional)
          <input
            className="rounded-md border border-input bg-background px-3 py-2"
            value={name}
            onChange={(e) => onNameChange(e.target.value)}
            placeholder="Ada Lovelace"
          />
        </label>
        <div className="md:col-span-2">
          <Button onClick={onSubmit} disabled={!email.trim()}>
            Create identity
          </Button>
        </div>
      </CardContent>
    </Card>
  );
}

