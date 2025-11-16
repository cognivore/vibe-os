import { useEffect, useState } from "react";
import { getDomains, getMeta, type MetaSnapshot } from "../api/client";
import type { DomainDescriptor } from "../types/core";
import { Card, CardContent, CardHeader, CardTitle } from "../components/ui/card";

export default function SettingsPage() {
  const [domains, setDomains] = useState<DomainDescriptor[]>([]);
  const [meta, setMeta] = useState<MetaSnapshot | null>(null);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    Promise.all([getDomains(), getMeta()])
      .then(([list, metaResponse]) => {
        setDomains(list);
        setMeta(metaResponse);
      })
      .catch((err) => setError(err.message));
  }, []);

  return (
    <div className="space-y-6">
      <div>
        <h2 className="text-2xl font-semibold">Settings</h2>
        <p className="text-sm text-muted-foreground">
          Runtime snapshot of the dashboard server.
        </p>
      </div>

      {error ? (
        <p className="text-sm text-destructive-foreground">{error}</p>
      ) : null}

      <Card>
        <CardHeader>
          <CardTitle>Domains</CardTitle>
        </CardHeader>
        <CardContent className="grid gap-2 sm:grid-cols-2">
          {domains.map((domain) => (
            <div
              key={domain.id}
              className="rounded-md border border-border p-3"
            >
              <p className="font-medium">{domain.human_name}</p>
              <p className="text-xs text-muted-foreground">{domain.id}</p>
              <p className="mt-1 text-sm text-muted-foreground">
                {domain.description}
              </p>
            </div>
          ))}
        </CardContent>
      </Card>

      {meta ? (
        <Card>
          <CardHeader>
            <CardTitle>Server paths</CardTitle>
          </CardHeader>
          <CardContent className="space-y-2 text-sm">
            <p>Slack mirror: {meta.slack_mirror_dir}</p>
            <p>Linear mirror: {meta.linear_mirror_dir}</p>
            <p>Arrow store: {meta.arrow_store_dir}</p>
            <p>Identity store: {meta.identity_store_dir}</p>
            <p>Static dir: {meta.static_dir ?? "not configured"}</p>
          </CardContent>
        </Card>
      ) : null}
    </div>
  );
}

