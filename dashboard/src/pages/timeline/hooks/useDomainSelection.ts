import { useCallback, useEffect, useState } from "react";
import type { DomainDescriptor } from "../../types/core";
import type { TimelineDataSource } from "../adapters";

export interface DomainSelectionState {
  domains: DomainDescriptor[];
  selectedDomains: string[];
  toggleDomain: (domainId: string) => void;
  setSelectedDomains: (domains: string[]) => void;
  error: string | null;
}

export function useDomainSelection(
  dataSource: TimelineDataSource,
): DomainSelectionState {
  const [domains, setDomains] = useState<DomainDescriptor[]>([]);
  const [selectedDomains, setSelectedDomains] = useState<string[]>([]);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    let cancelled = false;
    dataSource
      .fetchDomains()
      .then((available) => {
        if (cancelled) return;
        setDomains(available);
        setSelectedDomains((prev) =>
          prev.length ? prev : available.map((d) => d.id),
        );
      })
      .catch((err) => {
        if (!cancelled) {
          setError(err?.message ?? "Failed to load domains");
        }
      });
    return () => {
      cancelled = true;
    };
  }, [dataSource]);

  const toggleDomain = useCallback((domainId: string) => {
    setSelectedDomains((prev) =>
      prev.includes(domainId)
        ? prev.filter((id) => id !== domainId)
        : [...prev, domainId],
    );
  }, []);

  return {
    domains,
    selectedDomains,
    toggleDomain,
    setSelectedDomains,
    error,
  };
}

