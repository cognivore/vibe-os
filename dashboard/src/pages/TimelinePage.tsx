import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { Link, useNavigate } from "react-router-dom";
import { format } from "date-fns";
import { Button } from "../components/ui/button";
import { Card, CardContent, CardHeader, CardTitle } from "../components/ui/card";
import { ScrollArea } from "../components/ui/scroll-area";
import { TimelineEntryList } from "../components/timeline/TimelineEntryList";
import type { PersonaClickTarget, TimelineEntry } from "../components/timeline/types";
import { useTimelineWindow, defaultTimelineWindow } from "./timeline/hooks/useTimelineWindow";
import { useDomainSelection } from "./timeline/hooks/useDomainSelection";
import { useTimelineData } from "./timeline/hooks/useTimelineData";
import { useThreadSelection } from "./timeline/hooks/useThreadSelection";
import { useTimelineSearch } from "./timeline/hooks/useTimelineSearch";
import { timelineApiDataSource } from "./timeline/dataSource";
import {
  type ThreadAdapterRegistry,
  type ThreadEntry,
} from "./timeline/adapters";
import { slackThreadAdapter } from "./timeline/threads/slackThreadAdapter";
import { linearThreadAdapter } from "./timeline/threads/linearThreadAdapter";
import { threadKey } from "./timeline/threadUtils";

const ISO_FORMAT = "yyyy-MM-dd'T'HH:mm:ss";
const SEARCH_DEBOUNCE_MS = 700;

export default function TimelinePage() {
  const navigate = useNavigate();
  const dataSource = timelineApiDataSource;
  const threadAdapters = useMemo<ThreadAdapterRegistry>(
    () => [slackThreadAdapter, linearThreadAdapter],
    [],
  );

  const [searchQuery, setSearchQuery] = useState("");
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

  const searchState = useTimelineSearch();
  const { search: performSearch, clear: clearSearch } = searchState;
  const debounceRef = useRef<ReturnType<typeof setTimeout> | null>(null);

  const buildSearchParams = useCallback(
    (queryText: string) => ({
      query: queryText,
      domains: domainState.selectedDomains,
      from: window.from,
      to: window.to,
    }),
    [domainState.selectedDomains, window.from, window.to],
  );

  const runSearch = useCallback(
    (queryText: string) => {
      const trimmed = queryText.trim();
      if (!trimmed) {
        clearSearch();
        return;
      }
      const params = buildSearchParams(trimmed);
      if (!params.domains.length) {
        return;
      }
      performSearch(params);
    },
    [buildSearchParams, clearSearch, performSearch],
  );

  useEffect(() => {
    if (debounceRef.current) {
      clearTimeout(debounceRef.current);
      debounceRef.current = null;
    }

    const trimmed = searchQuery.trim();
    if (!trimmed) {
      clearSearch();
      return;
    }

    const params = buildSearchParams(trimmed);
    if (!params.domains.length) {
      return;
    }

    debounceRef.current = setTimeout(() => {
      performSearch(params);
      debounceRef.current = null;
    }, SEARCH_DEBOUNCE_MS);

    return () => {
      if (debounceRef.current) {
        clearTimeout(debounceRef.current);
        debounceRef.current = null;
      }
    };
  }, [searchQuery, buildSearchParams, clearSearch, performSearch]);

  const handleSearch = () => {
    if (debounceRef.current) {
      clearTimeout(debounceRef.current);
      debounceRef.current = null;
    }
    runSearch(searchQuery);
  };

  const handleClearSearch = () => {
    if (debounceRef.current) {
      clearTimeout(debounceRef.current);
      debounceRef.current = null;
    }
    setSearchQuery("");
    clearSearch();
  };

  const [searchThreadEntries, setSearchThreadEntries] = useState<ThreadEntry[]>([]);

  useEffect(() => {
    let cancelled = false;

    if (!searchState.results.length) {
      setSearchThreadEntries([]);
      return;
    }

    const buildThreads = async () => {
      const results = await Promise.all(
        threadAdapters.map(async (adapter) => {
          const entries = adapter.buildEntries(searchState.results);
          return entries instanceof Promise ? await entries : entries;
        })
      );

      if (!cancelled) {
        setSearchThreadEntries(results.flat());
      }
    };

    buildThreads();

    return () => {
      cancelled = true;
    };
  }, [searchState.results, threadAdapters]);

  const searchEventEntries = useMemo<TimelineEntry[]>(() => {
    if (!searchState.results.length) return [];

    // Build a set of all thread keys
    const slackThreadIds = new Set<string>();
    const linearIssueIds = new Set<string>();

    searchThreadEntries.forEach((thread) => {
      if (thread.type === "slack_thread") {
        slackThreadIds.add(thread.threadId);
      } else if (thread.type === "linear_thread") {
        linearIssueIds.add(thread.issueId);
      }
    });

    return searchState.results
      .filter((event) => {
        // For Linear: ALWAYS filter out (always part of issue thread)
        if (event.domain === "linear") {
          return false;
        }

        // For Slack: construct thread key and check if it matches any thread
        if (event.domain === "slack") {
          const data = event.data as any;
          const ts = typeof data.ts === "string" ? data.ts : undefined;
          const threadTs =
            typeof data.thread_ts === "string" && data.thread_ts
              ? data.thread_ts
              : ts;
          const channelId =
            (typeof data.channel === "string" && data.channel) ||
            event.entity_id ||
            "";

          if (channelId && threadTs) {
            const threadKey = `${channelId}:${threadTs}`;
            return !slackThreadIds.has(threadKey);
          }
        }

        // Keep all other events
        return true;
      })
      .map((event) => ({
        type: "event" as const,
        at: event.at,
        event,
      }));
  }, [searchState.results, searchThreadEntries]);

  const searchEntries = useMemo<TimelineEntry[]>(
    () =>
      [...searchEventEntries, ...searchThreadEntries].sort(
        (a, b) => new Date(b.at).getTime() - new Date(a.at).getTime(),
      ),
    [searchEventEntries, searchThreadEntries],
  );

  const hasSearchQuery = searchQuery.trim().length > 0;
  const isSearchMode = hasSearchQuery || searchState.loading;
  const displayEntries: TimelineEntry[] = isSearchMode ? searchEntries : timelineState.entries;

  const displayThreadEntries = isSearchMode ? searchThreadEntries : timelineState.threadEntries;

  const threadState = useThreadSelection({
    adapters: threadAdapters,
    dataSource,
    threadEntries: displayThreadEntries,
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
        <CardHeader>
          <CardTitle>Search & Filters</CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="flex gap-2">
            <input
              type="text"
              placeholder="Search timeline..."
              value={searchQuery}
              onChange={(e) => setSearchQuery(e.target.value)}
              onKeyDown={(e) => e.key === "Enter" && handleSearch()}
              className="flex-1 rounded-md border border-input bg-background px-3 py-2 text-sm"
            />
            <Button onClick={handleSearch} disabled={!searchQuery.trim()}>
              Search
            </Button>
            {isSearchMode && (
              <Button variant="outline" onClick={handleClearSearch}>
                Clear
              </Button>
            )}
          </div>
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
          <div className="flex flex-col gap-1 text-sm text-muted-foreground">
            {pageError ? (
              <p className="text-destructive-foreground">{pageError}</p>
            ) : isSearchMode ? (
              <p>
                Found {searchState.total} results for "{searchQuery}"
              </p>
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
          {(timelineState.loading || searchState.loading) ? (
            <div className="p-6 text-sm text-muted-foreground">Loading...</div>
          ) : searchState.error ? (
            <div className="p-6 text-sm text-destructive">{searchState.error}</div>
          ) : (
            <TimelineEntryList
              entries={displayEntries}
              domainLookup={timelineState.domainLookup}
              identityLookup={timelineState.identityLookup}
              personaLookup={timelineState.personaLookup}
              onPersonaClick={handlePersonaClick}
              onThreadSelect={threadState.selectThread}
              activeThreadKey={activeThreadKey}
              isSearchMode={isSearchMode}
            />
          )}
        </ScrollArea>
        <ScrollArea
          className={`h-[65vh] rounded-lg border border-border bg-card ${threadState.selectedThread ? "block" : "hidden lg:block"
            }`}
        >
          <div className="p-4">
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
        </ScrollArea>
      </div>
    </div>
  );
}
