/**
 * Pure functions for computing Linear team reports, cycle detection, and issue classification.
 * This mirrors the semantics of priv/scripts/linear-report.py but optimized for the browser.
 */

import type { LinearIssueSnapshot, CycleHistoryEntry } from "../../api/client";
import type {
  Team,
  Cycle,
  IssueWithLate,
  CycleWithIssues,
  TeamReport,
  LinearSummary,
  IssueCategory,
} from "./types";

// ============================================================================
// Date utilities
// ============================================================================

function parseDate(s: string | null | undefined): Date | null {
  if (!s) return null;
  const d = new Date(s);
  return isNaN(d.getTime()) ? null : d;
}

function today(): Date {
  const d = new Date();
  d.setHours(0, 0, 0, 0);
  return d;
}

// ============================================================================
// Team extraction
// ============================================================================

export function extractTeams(issues: LinearIssueSnapshot[]): Team[] {
  const seen = new Map<string, Team>();
  for (const issue of issues) {
    if (issue.team_id && issue.team_key && issue.team_name && !seen.has(issue.team_id)) {
      seen.set(issue.team_id, {
        id: issue.team_id,
        key: issue.team_key,
        name: issue.team_name,
      });
    }
  }
  return Array.from(seen.values()).sort((a, b) => a.name.localeCompare(b.name));
}

// ============================================================================
// Cycle extraction and detection (per-team)
// ============================================================================

interface CycleInfo {
  number: number;
  name: string | null;
  startsAt: Date | null;
  endsAt: Date | null;
}

function extractTeamCycles(issues: LinearIssueSnapshot[], teamId: string): CycleInfo[] {
  const cycleMap = new Map<number, CycleInfo>();
  for (const issue of issues) {
    if (issue.team_id !== teamId || issue.cycle_number == null) continue;
    if (!cycleMap.has(issue.cycle_number)) {
      cycleMap.set(issue.cycle_number, {
        number: issue.cycle_number,
        name: issue.cycle_name ?? null,
        startsAt: parseDate(issue.cycle_starts_at),
        endsAt: parseDate(issue.cycle_ends_at),
      });
    }
  }
  return Array.from(cycleMap.values()).sort((a, b) => a.number - b.number);
}

interface CycleClassification {
  previous: Cycle | null;
  current: Cycle | null;
  upcoming: Cycle[];
}

function classifyCycles(cycles: CycleInfo[], now: Date): CycleClassification {
  let previous: Cycle | null = null;
  let current: Cycle | null = null;
  const upcoming: Cycle[] = [];

  for (const c of cycles) {
    const startsAt = c.startsAt;
    const endsAt = c.endsAt;

    if (startsAt && endsAt) {
      if (now >= startsAt && now <= endsAt) {
        current = c;
      } else if (endsAt < now) {
        // Past cycle - keep track of most recent
        if (!previous || c.number > previous.number) {
          previous = c;
        }
      } else if (startsAt > now) {
        upcoming.push(c);
      }
    } else {
      // No dates - treat as upcoming if cycle number is higher than current
      if (current && c.number > current.number) {
        upcoming.push(c);
      } else if (!current && !previous) {
        // No current or previous, treat as potentially current or upcoming
        upcoming.push(c);
      }
    }
  }

  // If no current cycle found but we have a previous, check if it just ended
  if (!current && previous) {
    // Keep previous as is
  }

  // Sort upcoming by number
  upcoming.sort((a, b) => a.number - b.number);

  return { previous, current, upcoming };
}

// ============================================================================
// Late addition detection
// ============================================================================

function computeLateAddedAt(
  issue: LinearIssueSnapshot,
  cycle: Cycle
): Date | null {
  if (!cycle.startsAt) return null;

  // Find the earliest addition to this cycle number
  const additions = issue.cycle_history
    .filter((h) => h.to_cycle_number === cycle.number && h.at)
    .map((h) => parseDate(h.at))
    .filter((d): d is Date => d !== null)
    .sort((a, b) => a.getTime() - b.getTime());

  if (additions.length === 0) return null;

  const addedAt = additions[0];
  // If added after cycle started, it's a late addition
  if (addedAt > cycle.startsAt) {
    return addedAt;
  }
  return null;
}

function enrichWithLate(
  issues: LinearIssueSnapshot[],
  cycle: Cycle | null
): IssueWithLate[] {
  return issues.map((issue) => ({
    ...issue,
    lateAddedAt: cycle ? computeLateAddedAt(issue, cycle) : null,
  }));
}

// ============================================================================
// Issue classification
// ============================================================================

function isActiveState(stateType: string | null): boolean {
  return stateType === "started" || stateType === "unstarted";
}

function isBacklogState(stateType: string | null): boolean {
  return stateType === "backlog" || stateType === "triage";
}

function isDoneState(stateType: string | null): boolean {
  return stateType === "completed" || stateType === "canceled";
}

// ============================================================================
// Backlog activity scoring
// ============================================================================

export function scoreBacklogActivity(issue: LinearIssueSnapshot): number {
  const updated = parseDate(issue.updated_at);
  const created = parseDate(issue.created_at);
  if (!updated) return 0;

  const now = new Date();
  const daysSinceUpdate = (now.getTime() - updated.getTime()) / (1000 * 60 * 60 * 24);

  // Recent activity in backlog is suspicious
  let score = 0;
  if (daysSinceUpdate < 7) score += 50;
  else if (daysSinceUpdate < 30) score += 20;
  else if (daysSinceUpdate < 90) score += 5;

  // Comments and history activity
  if (issue.cycle_history.length > 0) score += 10 * issue.cycle_history.length;

  // Priority boost
  if (issue.priority !== null && issue.priority > 0) {
    score += (5 - issue.priority) * 5; // Higher priority = lower number
  }

  return score;
}

// ============================================================================
// Team report building
// ============================================================================

export function buildTeamReport(
  team: Team,
  allIssues: LinearIssueSnapshot[],
  now: Date = today()
): TeamReport {
  // Filter issues for this team
  const teamIssues = allIssues.filter((i) => i.team_id === team.id);

  // Extract and classify cycles for this team
  const cycles = extractTeamCycles(allIssues, team.id);
  const { previous, current, upcoming } = classifyCycles(cycles, now);

  // Build cycle buckets
  function buildCycleWithIssues(cycle: Cycle | null): CycleWithIssues | null {
    if (!cycle) return null;
    const cycleIssues = teamIssues.filter((i) => i.cycle_number === cycle.number);
    const enriched = enrichWithLate(cycleIssues, cycle);
    return {
      cycle,
      issues: enriched,
      lateCount: enriched.filter((i) => i.lateAddedAt !== null).length,
    };
  }

  const previousCycle = buildCycleWithIssues(previous);
  const currentCycle = buildCycleWithIssues(current);
  const upcomingCycles = upcoming.map((c) => buildCycleWithIssues(c)!).filter(Boolean);

  // Unplanned: issues without a cycle
  const unplannedIssues = teamIssues.filter((i) => i.cycle_number == null);
  const unplannedActive = unplannedIssues
    .filter((i) => isActiveState(i.state_type))
    .map((i) => ({ ...i, lateAddedAt: null }));
  const unplannedBacklog = unplannedIssues
    .filter((i) => isBacklogState(i.state_type))
    .map((i) => ({ ...i, lateAddedAt: null }))
    .sort((a, b) => scoreBacklogActivity(b) - scoreBacklogActivity(a));

  // Compute totals
  const plannedCycles = [previousCycle, currentCycle, ...upcomingCycles].filter(Boolean);
  const totalPlanned = plannedCycles.reduce((sum, c) => sum + c!.issues.length, 0);
  const totalLateAdditions = plannedCycles.reduce((sum, c) => sum + c!.lateCount, 0);

  return {
    team,
    previousCycle,
    currentCycle,
    upcomingCycles,
    unplannedActive,
    unplannedBacklog,
    totalPlanned,
    totalLateAdditions,
    totalUnplanned: unplannedActive.length + unplannedBacklog.length,
  };
}

// ============================================================================
// Full summary building
// ============================================================================

export function buildLinearSummary(
  issues: LinearIssueSnapshot[],
  lastSyncAt: string | null,
  workspaceName: string | null
): LinearSummary {
  const teams = extractTeams(issues);
  const now = today();
  const teamReports = teams.map((team) => buildTeamReport(team, issues, now));

  return {
    teams: teamReports,
    lastSyncAt: parseDate(lastSyncAt),
    workspaceName,
    totalIssues: issues.length,
  };
}

// ============================================================================
// Category helpers
// ============================================================================

export function getIssuesByCategory(
  report: TeamReport,
  category: IssueCategory
): IssueWithLate[] {
  switch (category) {
    case "planned": {
      const all: IssueWithLate[] = [];
      for (const cwi of [report.previousCycle, report.currentCycle, ...report.upcomingCycles]) {
        if (cwi) all.push(...cwi.issues.filter((i) => !i.lateAddedAt));
      }
      return all;
    }
    case "late": {
      const all: IssueWithLate[] = [];
      for (const cwi of [report.previousCycle, report.currentCycle, ...report.upcomingCycles]) {
        if (cwi) all.push(...cwi.issues.filter((i) => i.lateAddedAt));
      }
      return all;
    }
    case "unplanned":
      return report.unplannedActive;
    case "backlog":
      return report.unplannedBacklog;
  }
}

export function getCategoryLabel(category: IssueCategory): string {
  switch (category) {
    case "planned":
      return "Planned";
    case "late":
      return "Late Additions";
    case "unplanned":
      return "Unplanned Active";
    case "backlog":
      return "Backlog";
  }
}

// ============================================================================
// Export helpers
// ============================================================================

export function exportCategoryToText(report: TeamReport, category: IssueCategory): string {
  const issues = getIssuesByCategory(report, category);
  const lines: string[] = [
    `${report.team.name} - ${getCategoryLabel(category)}`,
    `Generated: ${new Date().toISOString()}`,
    "=".repeat(60),
    "",
  ];

  if (issues.length === 0) {
    lines.push("No issues in this category.");
  } else {
    for (const issue of issues) {
      lines.push(`[${issue.identifier}] ${issue.title}`);
      lines.push(`  State: ${issue.state_name ?? "Unknown"}`);
      if (issue.assignee_name) lines.push(`  Assignee: ${issue.assignee_name}`);
      if (issue.lateAddedAt) {
        lines.push(`  Late Added: ${issue.lateAddedAt.toISOString()}`);
      }
      if (issue.url) lines.push(`  ${issue.url}`);
      lines.push("");
    }
  }

  return lines.join("\n");
}


