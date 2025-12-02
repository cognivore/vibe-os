import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { Card, CardContent, CardHeader, CardTitle } from "../components/ui/card";
import { Badge } from "../components/ui/badge";
import { Button } from "../components/ui/button";
import {
  useLinearDashboard,
  TeamReport,
  IssueCategory,
  IssueWithLate,
  getIssuesByCategory,
  getCategoryLabel,
  exportCategoryToText,
} from "../features/linear";

const CATEGORIES: IssueCategory[] = ["planned", "late", "unplanned", "backlog"];

function downloadText(content: string, filename: string) {
  const blob = new Blob([content], { type: "text/plain;charset=utf-8" });
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = filename;
  a.click();
  URL.revokeObjectURL(url);
}

interface TeamPanelProps {
  report: TeamReport;
  focused: boolean;
  onFocus: () => void;
}

function TeamPanel({ report, focused, onFocus }: TeamPanelProps) {
  const [activeCategory, setActiveCategory] = useState<IssueCategory>("planned");
  const [selectedIndex, setSelectedIndex] = useState(0);
  const panelRef = useRef<HTMLDivElement>(null);
  const issues = useMemo(
    () => getIssuesByCategory(report, activeCategory),
    [report, activeCategory]
  );

  // Reset selection when category changes
  useEffect(() => {
    setSelectedIndex(0);
  }, [activeCategory]);

  // Keyboard handling
  useEffect(() => {
    if (!focused) return;

    const handler = (e: KeyboardEvent) => {
      // Tab through categories with 1-4 or [ ]
      if (e.key >= "1" && e.key <= "4") {
        const idx = parseInt(e.key) - 1;
        if (idx < CATEGORIES.length) {
          setActiveCategory(CATEGORIES[idx]);
          e.preventDefault();
        }
      } else if (e.key === "[") {
        const idx = CATEGORIES.indexOf(activeCategory);
        setActiveCategory(CATEGORIES[(idx - 1 + CATEGORIES.length) % CATEGORIES.length]);
        e.preventDefault();
      } else if (e.key === "]") {
        const idx = CATEGORIES.indexOf(activeCategory);
        setActiveCategory(CATEGORIES[(idx + 1) % CATEGORIES.length]);
        e.preventDefault();
      } else if (e.key === "ArrowUp" || e.key === "k") {
        setSelectedIndex((i) => Math.max(0, i - 1));
        e.preventDefault();
      } else if (e.key === "ArrowDown" || e.key === "j") {
        setSelectedIndex((i) => Math.min(issues.length - 1, i + 1));
        e.preventDefault();
      } else if (e.key === "Enter" && issues[selectedIndex]?.url) {
        window.open(issues[selectedIndex].url!, "_blank");
        e.preventDefault();
      } else if (e.key === "P" && e.shiftKey) {
        // Shift+P to export
        const text = exportCategoryToText(report, activeCategory);
        const filename = `${report.team.key}-${activeCategory}-${Date.now()}.txt`;
        downloadText(text, filename);
        e.preventDefault();
      }
    };

    window.addEventListener("keydown", handler);
    return () => window.removeEventListener("keydown", handler);
  }, [focused, activeCategory, issues, selectedIndex, report]);

  // Scroll selected into view
  useEffect(() => {
    const el = panelRef.current?.querySelector(`[data-index="${selectedIndex}"]`);
    el?.scrollIntoView({ block: "nearest" });
  }, [selectedIndex]);

  return (
    <Card
      className={`cursor-pointer transition-all ${focused ? "ring-2 ring-primary" : ""}`}
      onClick={onFocus}
      tabIndex={0}
      onFocus={onFocus}
    >
      <CardHeader className="pb-2">
        <CardTitle className="flex items-center justify-between text-lg">
          <span>{report.team.name}</span>
          <Badge variant="outline" className="text-xs font-normal">
            {report.team.key}
          </Badge>
        </CardTitle>
        <div className="flex gap-2 text-xs text-muted-foreground">
          <span>Planned: {report.totalPlanned}</span>
          <span>|</span>
          <span className="text-yellow-600">Late: {report.totalLateAdditions}</span>
          <span>|</span>
          <span>Unplanned: {report.totalUnplanned}</span>
        </div>
      </CardHeader>
      <CardContent className="pt-0">
        {/* Category tabs */}
        <div className="mb-2 flex gap-1 border-b border-border pb-2">
          {CATEGORIES.map((cat, idx) => {
            const count =
              cat === "planned"
                ? report.totalPlanned - report.totalLateAdditions
                : cat === "late"
                  ? report.totalLateAdditions
                  : cat === "unplanned"
                    ? report.unplannedActive.length
                    : report.unplannedBacklog.length;
            return (
              <button
                key={cat}
                onClick={(e) => {
                  e.stopPropagation();
                  setActiveCategory(cat);
                }}
                className={`rounded px-2 py-1 text-xs transition-colors ${
                  activeCategory === cat
                    ? "bg-primary text-primary-foreground"
                    : "hover:bg-muted"
                }`}
              >
                <span className="opacity-50 mr-1">{idx + 1}</span>
                {getCategoryLabel(cat)} ({count})
              </button>
            );
          })}
        </div>

        {/* Issue list */}
        <div ref={panelRef} className="max-h-64 overflow-y-auto space-y-1">
          {issues.length === 0 ? (
            <p className="text-xs text-muted-foreground py-4 text-center">
              No issues in this category
            </p>
          ) : (
            issues.map((issue, idx) => (
              <IssueRow
                key={issue.id}
                issue={issue}
                selected={focused && idx === selectedIndex}
                index={idx}
              />
            ))
          )}
        </div>

        {/* Footer hint */}
        {focused && (
          <p className="mt-2 text-[10px] text-muted-foreground text-center">
            ↑↓ navigate • Enter open • 1-4 or [] switch tabs • Shift+P export
          </p>
        )}
      </CardContent>
    </Card>
  );
}

interface IssueRowProps {
  issue: IssueWithLate;
  selected: boolean;
  index: number;
}

function IssueRow({ issue, selected, index }: IssueRowProps) {
  return (
    <div
      data-index={index}
      className={`rounded px-2 py-1 text-xs transition-colors ${
        selected ? "bg-accent text-accent-foreground" : "hover:bg-muted/50"
      }`}
    >
      <div className="flex items-center gap-2">
        <span className="font-mono text-muted-foreground">{issue.identifier}</span>
        <span className="truncate flex-1">{issue.title}</span>
        {issue.lateAddedAt && (
          <Badge variant="destructive" className="text-[10px] px-1">
            LATE
          </Badge>
        )}
      </div>
      <div className="flex items-center gap-2 text-muted-foreground mt-0.5">
        <span>{issue.state_name ?? "Unknown"}</span>
        {issue.assignee_name && (
          <>
            <span>•</span>
            <span>{issue.assignee_name}</span>
          </>
        )}
      </div>
    </div>
  );
}

export default function LinearDashboardPage() {
  const { summary, loading, error, refetch } = useLinearDashboard();
  const [focusedTeamIdx, setFocusedTeamIdx] = useState(0);

  // Tab key cycles through teams
  useEffect(() => {
    const handler = (e: KeyboardEvent) => {
      if (e.key === "Tab" && summary && summary.teams.length > 0) {
        e.preventDefault();
        setFocusedTeamIdx((i) =>
          e.shiftKey
            ? (i - 1 + summary.teams.length) % summary.teams.length
            : (i + 1) % summary.teams.length
        );
      }
    };
    window.addEventListener("keydown", handler);
    return () => window.removeEventListener("keydown", handler);
  }, [summary]);

  // Responsive grid columns
  const gridClass = useMemo(() => {
    if (!summary) return "grid-cols-1";
    const count = summary.teams.length;
    if (count <= 2) return "grid-cols-1 md:grid-cols-2";
    if (count <= 4) return "grid-cols-1 md:grid-cols-2 xl:grid-cols-3";
    return "grid-cols-1 md:grid-cols-2 xl:grid-cols-3 2xl:grid-cols-4";
  }, [summary]);

  if (loading) {
    return (
      <div className="flex items-center justify-center h-64">
        <p className="text-muted-foreground">Loading Linear data...</p>
      </div>
    );
  }

  if (error) {
    return (
      <div className="flex flex-col items-center justify-center h-64 gap-4">
        <p className="text-destructive">{error}</p>
        <Button onClick={refetch}>Retry</Button>
      </div>
    );
  }

  if (!summary || summary.teams.length === 0) {
    return (
      <div className="flex items-center justify-center h-64">
        <p className="text-muted-foreground">No Linear data available</p>
      </div>
    );
  }

  return (
    <div className="space-y-4">
      {/* Header */}
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-2xl font-semibold">Linear Overview</h1>
          <p className="text-sm text-muted-foreground">
            {summary.workspaceName && <span>{summary.workspaceName} • </span>}
            {summary.totalIssues} issues across {summary.teams.length} teams
            {summary.lastSyncAt && (
              <span className="ml-2">
                • Synced {summary.lastSyncAt.toLocaleString()}
              </span>
            )}
          </p>
        </div>
        <Button variant="outline" size="sm" onClick={refetch}>
          Refresh
        </Button>
      </div>

      {/* Team grid */}
      <div className={`grid gap-4 ${gridClass}`}>
        {summary.teams.map((report, idx) => (
          <TeamPanel
            key={report.team.id}
            report={report}
            focused={idx === focusedTeamIdx}
            onFocus={() => setFocusedTeamIdx(idx)}
          />
        ))}
      </div>

      {/* Global hints */}
      <p className="text-xs text-muted-foreground text-center">
        Press Tab to cycle between team panels
      </p>
    </div>
  );
}


