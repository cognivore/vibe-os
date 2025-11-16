import { useEffect, useState } from "react";
import { getDomains, getOperators, runOperator } from "../api/client";
import type { Arrow, OperatorDescriptor, RunOperatorResponse } from "../types/core";
import { Button } from "../components/ui/button";
import { Card, CardContent, CardHeader, CardTitle } from "../components/ui/card";
import { Badge } from "../components/ui/badge";

function defaultWindow(hours: number) {
  const now = new Date();
  const start = new Date(now.getTime() - hours * 60 * 60 * 1000);
  return {
    from: start.toISOString().slice(0, 16),
    to: now.toISOString().slice(0, 16),
  };
}

export default function OperatorsPage() {
  const [operators, setOperators] = useState<OperatorDescriptor[]>([]);
  const [selected, setSelected] = useState<OperatorDescriptor | null>(null);
  const [window, setWindow] = useState(() => defaultWindow(24));
  const [domains, setDomains] = useState<string[]>([]);
  const [result, setResult] = useState<RunOperatorResponse | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    Promise.all([getOperators(), getDomains()])
      .then(([ops, domainDescriptors]) => {
        setOperators(ops);
        if (ops.length > 0) {
          setSelected(ops[0]);
          setDomains(ops[0].source_domains);
        }
        if (domainDescriptors.length > 0 && ops.length === 0) {
          setDomains(domainDescriptors.map((d) => d.id));
        }
      })
      .catch((err) => setError(err.message));
  }, []);

  const handleRun = () => {
    if (!selected) return;
    setLoading(true);
    setError(null);
    runOperator({
      operatorId: selected.id,
      domains,
      from: new Date(window.from).toISOString(),
      to: new Date(window.to).toISOString(),
    })
      .then(setResult)
      .catch((err) => setError(err.message))
      .finally(() => setLoading(false));
  };

  return (
    <div className="space-y-6">
      <div className="flex flex-col gap-4 md:flex-row md:items-end md:justify-between">
        <div>
          <h2 className="text-2xl font-semibold">Operator catalogue</h2>
          <p className="text-sm text-muted-foreground">
            Select an operator and run it against a time window of events.
          </p>
        </div>
        <div className="flex gap-2">
          <label className="flex flex-col text-xs">
            From
            <input
              type="datetime-local"
              value={window.from}
              onChange={(e) =>
                setWindow((prev) => ({ ...prev, from: e.target.value }))
              }
              className="rounded-md border border-input bg-background px-2 py-1 text-sm"
            />
          </label>
          <label className="flex flex-col text-xs">
            To
            <input
              type="datetime-local"
              value={window.to}
              onChange={(e) =>
                setWindow((prev) => ({ ...prev, to: e.target.value }))
              }
              className="rounded-md border border-input bg-background px-2 py-1 text-sm"
            />
          </label>
        </div>
      </div>

      <div className="grid gap-4 md:grid-cols-2">
        {operators.map((operator) => {
          const active = selected?.id === operator.id;
          return (
            <Card
              key={operator.id}
              className={active ? "border-primary" : undefined}
            >
              <CardHeader>
                <CardTitle className="flex items-center justify-between">
                  {operator.name}
                  <Badge variant="secondary">{operator.kind}</Badge>
                </CardTitle>
                <p className="text-sm text-muted-foreground">
                  {operator.description}
                </p>
              </CardHeader>
              <CardContent className="space-y-2 text-sm text-muted-foreground">
                <p>
                  Direction: {operator.direction.replace("_", " ")} | Sources:{" "}
                  {operator.source_domains.join(", ")} → Targets:{" "}
                  {operator.target_domains.join(", ")}
                </p>
                <Button
                  variant={active ? "default" : "secondary"}
                  onClick={() => {
                    setSelected(operator);
                    setDomains(operator.source_domains);
                  }}
                >
                  {active ? "Selected" : "Select"}
                </Button>
              </CardContent>
            </Card>
          );
        })}
      </div>

      <div className="flex items-center gap-2">
        <Button onClick={handleRun} disabled={!selected || loading}>
          {loading ? "Running..." : "Run operator"}
        </Button>
        {error ? (
          <p className="text-sm text-destructive-foreground">{error}</p>
        ) : null}
      </div>

      {result ? (
        <Card>
          <CardHeader>
            <CardTitle>Run output</CardTitle>
            <p className="text-sm text-muted-foreground">
              Produced {result.arrows.length} arrows between{" "}
              {new Date(result.window.start).toLocaleString()} and{" "}
              {new Date(result.window.end).toLocaleString()}
            </p>
          </CardHeader>
          <CardContent className="space-y-4">
            {result.arrows.map((arrow: Arrow) => (
              <div
                key={arrow.id}
                className="rounded-md border border-border p-3"
              >
                <div className="flex items-center justify-between">
                  <p className="font-semibold">{arrow.title}</p>
                  <Badge>{arrow.direction.replace("_", " ")}</Badge>
                </div>
                <p className="text-xs text-muted-foreground">
                  {arrow.detail_markdown}
                </p>
              </div>
            ))}
            {result.arrows.length === 0 ? (
              <p className="text-sm text-muted-foreground">
                No arrows generated.
              </p>
            ) : null}
          </CardContent>
        </Card>
      ) : null}
    </div>
  );
}

