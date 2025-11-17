import { useCallback, useEffect, useMemo, useState } from "react";
import type {
  ThreadAdapterRegistry,
  ThreadEntry,
  TimelineDataSource,
} from "../adapters";

interface ThreadSelectionParams {
  adapters: ThreadAdapterRegistry;
  dataSource: TimelineDataSource;
  threadEntries: ThreadEntry[];
}

export interface ThreadSelectionState {
  selectedThread: ThreadEntry | null;
  selectThread: (entry: ThreadEntry) => void;
  clearSelection: () => void;
  loading: boolean;
  error: string | null;
}

export function useThreadSelection({
  adapters,
  dataSource,
  threadEntries,
}: ThreadSelectionParams): ThreadSelectionState {
  const [selectedThread, setSelectedThread] = useState<ThreadEntry | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [selectionNonce, setSelectionNonce] = useState(0);

  const adapterMap = useMemo(
    () =>
      new Map(
        adapters.map((adapter) => [adapter.kind, adapter] as const),
      ),
    [adapters],
  );

  const selectThread = useCallback((entry: ThreadEntry) => {
    setError(null);
    setSelectedThread(entry);
    setSelectionNonce((nonce) => nonce + 1);
  }, []);

  const clearSelection = useCallback(() => {
    setSelectedThread(null);
    setError(null);
    setLoading(false);
  }, []);

  useEffect(() => {
    if (!selectedThread) return;
    const adapter = adapterMap.get(selectedThread.type);
    if (!adapter || !adapter.hydrate) {
      setLoading(false);
      return;
    }

    let cancelled = false;
    setLoading(true);
    adapter
      .hydrate(selectedThread, dataSource)
      .then((hydrated) => {
        if (!cancelled) {
          setSelectedThread(hydrated);
          setError(null);
        }
      })
      .catch((err) => {
        if (!cancelled) {
          setError(err?.message ?? "Failed to load thread");
        }
      })
      .finally(() => {
        if (!cancelled) {
          setLoading(false);
        }
      });

    return () => {
      cancelled = true;
    };
  }, [adapterMap, dataSource, selectedThread, selectionNonce]);

  useEffect(() => {
    if (!selectedThread) return;
    const updated = threadEntries.find((entry) =>
      isSameThreadEntry(entry, selectedThread),
    );
    if (updated && updated !== selectedThread) {
      setSelectedThread(updated);
    }
  }, [selectedThread, threadEntries]);

  return {
    selectedThread,
    selectThread,
    clearSelection,
    loading,
    error,
  };
}

function isSameThreadEntry(a: ThreadEntry, b: ThreadEntry) {
  if (a.type !== b.type) {
    return false;
  }
  if (a.type === "slack_thread" && b.type === "slack_thread") {
    return a.threadId === b.threadId;
  }
  if (a.type === "linear_thread" && b.type === "linear_thread") {
    return a.issueId === b.issueId;
  }
  return false;
}

