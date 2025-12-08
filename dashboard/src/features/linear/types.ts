import type { CycleHistoryEntry, LinearIssueSnapshot } from "../../api/client";

/** Re-export for convenience */
export type { CycleHistoryEntry, LinearIssueSnapshot };

/** Team descriptor extracted from issues */
export interface Team {
  id: string;
  key: string;
  name: string;
}

/** Cycle metadata */
export interface Cycle {
  number: number;
  name: string | null;
  startsAt: Date | null;
  endsAt: Date | null;
}

/** Issue with computed late-addition timestamp */
export interface IssueWithLate extends LinearIssueSnapshot {
  lateAddedAt: Date | null;
}

/** Issues grouped by cycle with computed late counts */
export interface CycleWithIssues {
  cycle: Cycle;
  issues: IssueWithLate[];
  lateCount: number;
}

/** Report for a single team */
export interface TeamReport {
  team: Team;
  previousCycle: CycleWithIssues | null;
  currentCycle: CycleWithIssues | null;
  upcomingCycles: CycleWithIssues[];
  unplannedActive: IssueWithLate[];
  unplannedBacklog: IssueWithLate[];
  totalPlanned: number;
  totalLateAdditions: number;
  totalUnplanned: number;
}

/** Category for issue classification */
export type IssueCategory = "planned" | "late" | "unplanned" | "backlog";

/** Summary across all teams */
export interface LinearSummary {
  teams: TeamReport[];
  lastSyncAt: Date | null;
  workspaceName: string | null;
  totalIssues: number;
}





