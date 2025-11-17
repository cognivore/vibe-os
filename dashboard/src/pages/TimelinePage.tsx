import { useMemo } from "react";
import { Link, useNavigate } from "react-router-dom";
import { format } from "date-fns";
import { Button } from "../components/ui/button";
import { Card, CardContent, CardHeader, CardTitle } from "../components/ui/card";
import { ScrollArea } from "../components/ui/scroll-area";
import {
  TimelineEntryList,
  type PersonaClickTarget,
} from "../components/timeline/TimelineEntryList";
import { useTimelineWindow, defaultTimelineWindow } from "./timeline/hooks/useTimelineWindow";
import { useDomainSelection } from "./timeline/hooks/useDomainSelection";
import { useTimelineData } from "./timeline/hooks/useTimelineData";
import { useThreadSelection } from "./timeline/hooks/useThreadSelection";
import { timelineApiDataSource } from "./timeline/dataSource";
import {
  type ThreadAdapterRegistry,
  type ThreadEntry,
} from "./timeline/adapters";
import { slackThreadAdapter } from "./timeline/threads/slackThreadAdapter";
import { linearThreadAdapter } from "./timeline/threads/linearThreadAdapter";
import { threadKey } from "./timeline/threadUtils";

const ISO_FORMAT = "yyyy-MM-dd'T'HH:mm:ss";

export default function TimelinePage() {
  const navigate = useNavigate();
  const dataSource = timelineApiDataSource;
  const threadAdapters = useMemo<ThreadAdapterRegistry>(
    () => [slackThreadAdapter, linearThreadAdapter],
    [],
  );

  const windowState = useTimelineWindow(defaultTimelineWindow());
  const { window, setFrom, setTo, last24h } = windowState;

  const domainState = useDomainSelection(dataSource);
  const timelineState = useTimelineData({
    dataSource,
    window,
    selectedDomains: domainState.selectedDomains,
    threadAdapters,
    domains: domainState.domains,
  });

  const threadState = useThreadSelection({
    adapters: threadAdapters,
    dataSource,
    threadEntries: timelineState.threadEntries,
  });

  const handlePersonaClick = (target: PersonaClickTarget) => {
    if (target.identityId) {
      navigate(`/identities/${target.identityId}`);
      return;
    }
    if (target.persona) {
      const params = new URLSearchParams({
        provider: target.persona.domain,
        local_id: target.persona.local_id,
      });
      if (target.label) {
        params.set("label", target.label);
      }
      navigate(`/identities?${params.toString()}`);
    }
  };

  const selectedAdapter = useMemo(() => {
    if (!threadState.selectedThread) return null;
    return threadAdapters.find((adapter) =>
      adapter.matches(threadState.selectedThread as ThreadEntry),
    );
  }, [threadAdapters, threadState.selectedThread]);

  const SelectedPanel = selectedAdapter?.Panel ?? null;
  const activeThreadKey = threadState.selectedThread
    ? threadKey(threadState.selectedThread)
    : undefined;
  const pageError = domainState.error ?? timelineState.error;

  return (
    <div className="space-y-6">
      <div className="flex flex-col gap-4 md:flex-row md:items-end md:justify-between">
        <div>
          <h2 className="text-2xl font-semibold">Unified timeline</h2>
          <p className="text-sm text-muted-foreground">
            Events and arrows across all domains
          </p>
        </div>
        <div className="flex flex-wrap gap-2">
          <label className="flex flex-col text-xs">
            From
            <input
              type="datetime-local"
              value={format(new Date(window.from), ISO_FORMAT)}
              onChange={(e) => setFrom(e.target.value)}
              className="rounded-md border border-input bg-background px-2 py-1 text-sm"
            />
          </label>
          <label className="flex flex-col text-xs">
            To
            <input
              type="datetime-local"
              value={format(new Date(window.to), ISO_FORMAT)}
              onChange={(e) => setTo(e.target.value)}
              className="rounded-md border border-input bg-background px-2 py-1 text-sm"
            />
          </label>
          <Button variant="secondary" onClick={last24h}>
            Last 24h
          </Button>
        </div>
      </div>

      <Card>
        <CardHeader className="flex flex-row items-center justify-between">
          <CardTitle>Domains</CardTitle>
          <div className="flex flex-wrap gap-2">
            {domainState.domains.map((domain) => {
              const active = domainState.selectedDomains.includes(domain.id);
              return (
                <label
                  key={domain.id}
                  className="flex items-center gap-1 text-sm"
                >
                  <input
                    type="checkbox"
                    checked={active}
                    onChange={() => domainState.toggleDomain(domain.id)}
                  />
                  {domain.human_name}
                </label>
              );
            })}
          </div>
        </CardHeader>
        <CardContent>
          <div className="flex flex-col gap-1 text-sm text-muted-foreground">
            {pageError ? (
              <p className="text-destructive-foreground">{pageError}</p>
            ) : (
              <p>
                Showing {timelineState.entries.length} entries between{" "}
                {new Date(window.from).toLocaleString()} and{" "}
                {new Date(window.to).toLocaleString()}
              </p>
            )}
            <Link to="/identities" className="text-xs underline">
              Manage identities
            </Link>
          </div>
        </CardContent>
      </Card>

      <div className="grid gap-4 lg:grid-cols-[minmax(0,2fr)_minmax(320px,1fr)]">
        <ScrollArea className="h-[65vh] rounded-lg border border-border bg-card">
          {timelineState.loading ? (
            <div className="p-6 text-sm text-muted-foreground">Loading...</div>
          ) : (
            <TimelineEntryList
              entries={timelineState.entries}
              domainLookup={timelineState.domainLookup}
              identityLookup={timelineState.identityLookup}
              personaLookup={timelineState.personaLookup}
              onPersonaClick={handlePersonaClick}
              onThreadSelect={threadState.selectThread}
              activeThreadKey={activeThreadKey}
            />
          )}
        </ScrollArea>
        <div
          className={`rounded-lg border border-border bg-card p-4 ${threadState.selectedThread ? "block" : "hidden lg:block"
            }`}
        >
          {threadState.selectedThread && SelectedPanel ? (
            <SelectedPanel
              entry={threadState.selectedThread as ThreadEntry}
              identityLookup={timelineState.identityLookup}
              personaLookup={timelineState.personaLookup}
              onPersonaClick={handlePersonaClick}
              onClose={threadState.clearSelection}
              loading={threadState.loading}
              error={threadState.error}
            />
          ) : (
            <div className="text-sm text-muted-foreground">
              Select a Slack or Linear thread to inspect the conversation.
            </div>
          )}
        </div>
      </div>
    </div>
  );
}
