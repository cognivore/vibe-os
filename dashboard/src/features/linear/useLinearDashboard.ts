import { useCallback, useEffect, useMemo, useState } from "react";
import { getLinearIssues, LinearIssuesResponse } from "../../api/client";
import { buildLinearSummary } from "./logic";
import type { LinearSummary } from "./types";

export interface UseLinearDashboardResult {
  summary: LinearSummary | null;
  loading: boolean;
  error: string | null;
  refetch: () => void;
}

export function useLinearDashboard(): UseLinearDashboardResult {
  const [data, setData] = useState<LinearIssuesResponse | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const fetchData = useCallback(async () => {
    setLoading(true);
    setError(null);
    try {
      const response = await getLinearIssues();
      setData(response);
    } catch (err) {
      setError(err instanceof Error ? err.message : "Failed to fetch Linear issues");
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    fetchData();
  }, [fetchData]);

  const summary = useMemo(() => {
    if (!data) return null;
    return buildLinearSummary(data.issues, data.last_sync_at, data.workspace_name);
  }, [data]);

  return {
    summary,
    loading,
    error,
    refetch: fetchData,
  };
}


