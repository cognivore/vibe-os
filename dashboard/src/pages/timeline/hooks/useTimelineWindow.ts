import { useCallback, useMemo, useState } from "react";
import type { TimelineWindow } from "../adapters";

export type TimelineWindowPreset = "24h" | "72h" | "2w" | "1mo" | "3mo";

const HOURS_IN_MS = 1000 * 60 * 60;
const DAYS_IN_MS = HOURS_IN_MS * 24;

const createWindow = (start: Date, end: Date): TimelineWindow => ({
  from: start.toISOString(),
  to: end.toISOString(),
});

const windowFromPreset = (preset: TimelineWindowPreset): TimelineWindow => {
  const end = new Date();
  const start = new Date(end.getTime());

  switch (preset) {
    case "24h":
      start.setTime(start.getTime() - HOURS_IN_MS * 24);
      break;
    case "72h":
      start.setTime(start.getTime() - HOURS_IN_MS * 72);
      break;
    case "2w":
      start.setTime(start.getTime() - DAYS_IN_MS * 14);
      break;
    case "1mo":
      start.setMonth(start.getMonth() - 1);
      break;
    case "3mo":
    default:
      start.setMonth(start.getMonth() - 3);
      break;
  }

  return createWindow(start, end);
};

const windowsEqual = (a: TimelineWindow, b: TimelineWindow) => a.from === b.from && a.to === b.to;

export function defaultTimelineWindow(): TimelineWindow {
  return windowFromPreset("3mo");
}

type WindowSetter = TimelineWindow | ((prev: TimelineWindow) => TimelineWindow);

export function useTimelineWindow(initialWindow = defaultTimelineWindow()) {
  const [window, setWindowValue] = useState<TimelineWindow>(initialWindow);
  const [preset, setPreset] = useState<TimelineWindowPreset | null>(() => {
    const defaultWindow = defaultTimelineWindow();
    return windowsEqual(initialWindow, defaultWindow) ? "3mo" : null;
  });

  const applyWindow = useCallback((next: WindowSetter, nextPreset: TimelineWindowPreset | null) => {
    setWindowValue((prev) => (typeof next === "function" ? next(prev) : next));
    setPreset(nextPreset);
  }, []);

  const setWindow = useCallback(
    (next: WindowSetter) => applyWindow(next, null),
    [applyWindow],
  );

  const setFrom = useCallback(
    (iso: string) =>
      applyWindow(
        (prev) => ({
          ...prev,
          from: new Date(iso).toISOString(),
        }),
        null,
      ),
    [applyWindow],
  );

  const setTo = useCallback(
    (iso: string) =>
      applyWindow(
        (prev) => ({
          ...prev,
          to: new Date(iso).toISOString(),
        }),
        null,
      ),
    [applyWindow],
  );

  const reset = useCallback(() => applyWindow(defaultTimelineWindow(), "3mo"), [applyWindow]);

  const selectPreset = useCallback(
    (nextPreset: TimelineWindowPreset) => applyWindow(windowFromPreset(nextPreset), nextPreset),
    [applyWindow],
  );

  const controls = useMemo(
    () => ({
      window,
      preset,
      setWindow,
      setFrom,
      setTo,
      reset,
      selectPreset,
    }),
    [window, preset, reset, selectPreset, setFrom, setTo, setWindow],
  );

  return controls;
}

