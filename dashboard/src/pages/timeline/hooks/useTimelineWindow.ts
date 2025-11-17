import { useCallback, useMemo, useState } from "react";
import type { TimelineWindow } from "../adapters";

const DAY_IN_MS = 1000 * 60 * 60 * 24;

export function defaultTimelineWindow(): TimelineWindow {
  const end = new Date();
  const start = new Date(end.getTime());
  start.setMonth(start.getMonth() - 3);
  return {
    from: start.toISOString(),
    to: end.toISOString(),
  };
}

export function last24hWindow(): TimelineWindow {
  const end = new Date();
  const start = new Date(end.getTime() - DAY_IN_MS);
  return {
    from: start.toISOString(),
    to: end.toISOString(),
  };
}

type WindowSetter = TimelineWindow | ((prev: TimelineWindow) => TimelineWindow);

export function useTimelineWindow(initialWindow = defaultTimelineWindow()) {
  const [window, setWindow] = useState<TimelineWindow>(initialWindow);

  const updateWindow = useCallback((next: WindowSetter) => {
    setWindow((prev) => (typeof next === "function" ? next(prev) : next));
  }, []);

  const controls = useMemo(
    () => ({
      window,
      setWindow: updateWindow,
      setFrom: (iso: string) =>
        updateWindow((prev) => ({
          ...prev,
          from: new Date(iso).toISOString(),
        })),
      setTo: (iso: string) =>
        updateWindow((prev) => ({
          ...prev,
          to: new Date(iso).toISOString(),
        })),
      reset: () => updateWindow(defaultTimelineWindow()),
      last24h: () => updateWindow(last24hWindow()),
    }),
    [window, updateWindow],
  );

  return controls;
}

