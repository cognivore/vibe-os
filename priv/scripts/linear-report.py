#!/usr/bin/env python3
"""Linear Cycle Report - Interactive TUI with textual

Usage:
    ./linear-report.py                    # Interactive TUI
    ./linear-report.py --print-all        # Full report to file
    ./linear-report.py --format md        # Output as markdown
"""

from __future__ import annotations

import argparse
import os
import sys
import webbrowser
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Optional

import requests

# ═══════════════════════════════════════════════════════════════════════════════
# Configuration
# ═══════════════════════════════════════════════════════════════════════════════

LINEAR_API_URL = "https://api.linear.app/graphql"
PROJECT_ROOT = Path(__file__).parent.parent.parent
ENV_FILE = PROJECT_ROOT / ".env"


def load_api_key() -> str:
    """Load Linear API key from environment or .env file."""
    key = os.environ.get("VIBEOS_LINEAR_API_KEY") or os.environ.get("LINEAR_API_KEY")
    if key:
        return key

    if ENV_FILE.exists():
        for line in ENV_FILE.read_text().splitlines():
            if line.startswith("VIBEOS_LINEAR_API_KEY="):
                return line.split("=", 1)[1].strip()
            if line.startswith("LINEAR_API_KEY="):
                return line.split("=", 1)[1].strip()

    print("Error: VIBEOS_LINEAR_API_KEY not set. Run 'vibeos setup' or set it in .env")
    sys.exit(1)


# ═══════════════════════════════════════════════════════════════════════════════
# Data Classes
# ═══════════════════════════════════════════════════════════════════════════════


@dataclass
class Team:
    key: str
    name: str


@dataclass
class Cycle:
    id: str
    number: int
    name: Optional[str]
    starts_at: Optional[datetime]
    ends_at: Optional[datetime]
    team_name: str

    @property
    def status(self) -> str:
        now = datetime.now(timezone.utc)
        if self.ends_at and self.ends_at < now:
            return "ended"
        if self.starts_at and self.starts_at > now:
            return "upcoming"
        return "active"

    @property
    def date_range(self) -> str:
        if self.starts_at and self.ends_at:
            return (
                f"{self.starts_at.strftime('%b %d')} - {self.ends_at.strftime('%b %d')}"
            )
        return ""


@dataclass
class Issue:
    id: str
    identifier: str
    title: str
    url: str
    team_key: str
    team_name: str
    state_name: str
    state_type: str
    assignee_name: Optional[str]
    labels: list[str]
    priority: int
    cycle_number: Optional[int]
    cycle_starts_at: Optional[datetime]
    cycle_ends_at: Optional[datetime]
    created_at: datetime
    updated_at: datetime
    completed_at: Optional[datetime]

    @property
    def is_planned(self) -> bool:
        return self.cycle_number is not None

    @property
    def is_backlog(self) -> bool:
        return self.state_type == "backlog"

    @property
    def is_active(self) -> bool:
        return self.state_type in ("started", "unstarted")

    @property
    def is_done(self) -> bool:
        return self.state_type in ("completed", "canceled")

    @property
    def state_icon(self) -> str:
        icons = {
            "completed": "✓",
            "canceled": "✗",
            "started": "→",
            "unstarted": "○",
            "backlog": "◌",
        }
        return icons.get(self.state_type, "?")


@dataclass
class BacklogItem:
    issue: Issue
    activity_score: int
    is_sus: bool


@dataclass
class CycleWithIssues:
    """A cycle with its associated issues and stats."""

    cycle: Cycle
    issues: list[Issue] = field(default_factory=list)

    @property
    def done_count(self) -> int:
        return sum(1 for i in self.issues if i.state_type == "completed")

    @property
    def in_progress_count(self) -> int:
        return sum(1 for i in self.issues if i.state_type == "started")

    @property
    def todo_count(self) -> int:
        return sum(1 for i in self.issues if i.state_type in ("unstarted", "backlog"))

    @property
    def canceled_count(self) -> int:
        return sum(1 for i in self.issues if i.state_type == "canceled")

    @property
    def total(self) -> int:
        return len(self.issues)

    @property
    def completion_pct(self) -> int:
        return (self.done_count * 100 // self.total) if self.total > 0 else 0


@dataclass
class TeamReport:
    """Per-team report with their cycles and issues."""

    team: Team
    previous_cycle: Optional[CycleWithIssues]  # Most recent ended
    current_cycle: Optional[CycleWithIssues]  # Currently active
    upcoming_cycles: list[CycleWithIssues]  # Future cycles
    unplanned_active: list[Issue]  # Not in cycle, in progress/todo
    unplanned_backlog: list[BacklogItem]  # Not in cycle, backlog
    unplanned_done: list[Issue]  # Not in cycle, done/canceled

    @property
    def total_planned(self) -> int:
        total = 0
        if self.previous_cycle:
            total += self.previous_cycle.total
        if self.current_cycle:
            total += self.current_cycle.total
        for c in self.upcoming_cycles:
            total += c.total
        return total

    @property
    def total_unplanned(self) -> int:
        return (
            len(self.unplanned_active)
            + len(self.unplanned_backlog)
            + len(self.unplanned_done)
        )


# ═══════════════════════════════════════════════════════════════════════════════
# Linear API Client
# ═══════════════════════════════════════════════════════════════════════════════


class LinearClient:
    def __init__(self, api_key: str):
        self.api_key = api_key
        self.session = requests.Session()
        self.session.headers.update(
            {"Authorization": api_key, "Content-Type": "application/json"}
        )

    def graphql(self, query: str, variables: dict = None) -> dict:
        resp = self.session.post(
            LINEAR_API_URL, json={"query": query, "variables": variables or {}}
        )
        resp.raise_for_status()
        data = resp.json()
        if "errors" in data:
            raise RuntimeError(f"GraphQL errors: {data['errors']}")
        return data["data"]

    def fetch_workspace_name(self) -> str:
        query = "{ viewer { organization { name } } }"
        data = self.graphql(query)
        return data["viewer"]["organization"]["name"]

    def fetch_teams(self) -> list[Team]:
        query = "{ teams { nodes { key name } } }"
        data = self.graphql(query)
        return [Team(key=t["key"], name=t["name"]) for t in data["teams"]["nodes"]]

    def fetch_cycles(self) -> list[Cycle]:
        query = """
        { cycles(first: 100) { nodes {
            id number name startsAt endsAt team { name }
        } } }
        """
        data = self.graphql(query)
        cycles = []
        for c in data["cycles"]["nodes"]:
            cycles.append(
                Cycle(
                    id=c["id"],
                    number=c["number"],
                    name=c.get("name"),
                    starts_at=parse_datetime(c.get("startsAt")),
                    ends_at=parse_datetime(c.get("endsAt")),
                    team_name=c["team"]["name"] if c.get("team") else "Unknown",
                )
            )
        return cycles

    def fetch_all_issues(self) -> list[Issue]:
        query = """
        query($after: String) {
            issues(first: 100, after: $after) {
                nodes {
                    id identifier title url
                    team { key name }
                    state { name type }
                    assignee { name }
                    labels { nodes { name } }
                    priority
                    cycle { number startsAt endsAt }
                    createdAt updatedAt completedAt
                }
                pageInfo { hasNextPage endCursor }
            }
        }
        """
        issues = []
        after = None
        while True:
            data = self.graphql(query, {"after": after})
            for node in data["issues"]["nodes"]:
                cycle = node.get("cycle")
                issues.append(
                    Issue(
                        id=node["id"],
                        identifier=node["identifier"],
                        title=node["title"],
                        url=node.get("url")
                        or f"https://linear.app/issue/{node['identifier']}",
                        team_key=node["team"]["key"] if node.get("team") else "???",
                        team_name=(
                            node["team"]["name"] if node.get("team") else "Unknown"
                        ),
                        state_name=(
                            node["state"]["name"] if node.get("state") else "Unknown"
                        ),
                        state_type=(
                            node["state"]["type"] if node.get("state") else "unknown"
                        ),
                        assignee_name=(
                            node["assignee"]["name"] if node.get("assignee") else None
                        ),
                        labels=[
                            lbl["name"]
                            for lbl in node.get("labels", {}).get("nodes", [])
                        ],
                        priority=node.get("priority") or 0,
                        cycle_number=cycle["number"] if cycle else None,
                        cycle_starts_at=(
                            parse_datetime(cycle.get("startsAt")) if cycle else None
                        ),
                        cycle_ends_at=(
                            parse_datetime(cycle.get("endsAt")) if cycle else None
                        ),
                        created_at=parse_datetime(node["createdAt"]),
                        updated_at=parse_datetime(node["updatedAt"]),
                        completed_at=parse_datetime(node.get("completedAt")),
                    )
                )
            if not data["issues"]["pageInfo"]["hasNextPage"]:
                break
            after = data["issues"]["pageInfo"]["endCursor"]
        return issues


# ═══════════════════════════════════════════════════════════════════════════════
# Data Processing
# ═══════════════════════════════════════════════════════════════════════════════


def parse_datetime(value: Optional[str]) -> Optional[datetime]:
    if not value:
        return None
    try:
        return datetime.fromisoformat(value.replace("Z", "+00:00"))
    except ValueError:
        return None


def get_team_cycles(
    team_name: str, all_cycles: list[Cycle], now: datetime
) -> tuple[Optional[Cycle], Optional[Cycle], list[Cycle]]:
    """Get previous, current, and upcoming cycles for a specific team."""
    team_cycles = [c for c in all_cycles if c.team_name == team_name]

    ended = [c for c in team_cycles if c.ends_at and c.ends_at < now]
    active = [
        c
        for c in team_cycles
        if c.starts_at and c.starts_at <= now and (not c.ends_at or c.ends_at >= now)
    ]
    upcoming = [c for c in team_cycles if c.starts_at and c.starts_at > now]

    previous = max(ended, key=lambda c: c.ends_at) if ended else None
    current = active[0] if active else None
    upcoming_sorted = sorted(upcoming, key=lambda c: c.starts_at)

    return previous, current, upcoming_sorted


def build_team_reports(
    teams: list[Team],
    cycles: list[Cycle],
    issues: list[Issue],
) -> list[TeamReport]:
    """Build per-team reports with cycles and issues."""
    now = datetime.now(timezone.utc)
    reports = []

    for team in teams:
        team_issues = [i for i in issues if i.team_name == team.name]
        previous_cycle, current_cycle, upcoming_cycles = get_team_cycles(
            team.name, cycles, now
        )

        # Build CycleWithIssues for each cycle
        def make_cycle_with_issues(cycle: Optional[Cycle]) -> Optional[CycleWithIssues]:
            if not cycle:
                return None
            cycle_issues = [i for i in team_issues if i.cycle_number == cycle.number]
            return CycleWithIssues(cycle=cycle, issues=cycle_issues)

        prev_cwi = make_cycle_with_issues(previous_cycle)
        curr_cwi = make_cycle_with_issues(current_cycle)
        upcoming_cwi = [make_cycle_with_issues(c) for c in upcoming_cycles]
        upcoming_cwi = [c for c in upcoming_cwi if c]  # Filter None

        # Classify unplanned issues
        unplanned = [i for i in team_issues if not i.is_planned]
        unplanned_active = [i for i in unplanned if i.is_active]
        unplanned_done = [i for i in unplanned if i.is_done]
        unplanned_backlog_raw = [i for i in unplanned if i.is_backlog]

        # Score backlog items
        backlog_items = []
        for issue in unplanned_backlog_raw:
            days_since_created = (now - issue.created_at).days
            days_since_updated = (now - issue.updated_at).days
            activity_score = max(0, days_since_created - days_since_updated)
            is_sus = activity_score >= 3 and days_since_updated < 7
            backlog_items.append(BacklogItem(issue, activity_score, is_sus))

        # Sort backlog by activity (most active first)
        backlog_items.sort(key=lambda b: (-b.activity_score, b.issue.updated_at))

        reports.append(
            TeamReport(
                team=team,
                previous_cycle=prev_cwi,
                current_cycle=curr_cwi,
                upcoming_cycles=upcoming_cwi,
                unplanned_active=unplanned_active,
                unplanned_backlog=backlog_items,
                unplanned_done=unplanned_done,
            )
        )

    # Sort teams by total planned issues (descending)
    reports.sort(key=lambda r: r.total_planned, reverse=True)
    return reports


# ═══════════════════════════════════════════════════════════════════════════════
# Textual TUI Application
# ═══════════════════════════════════════════════════════════════════════════════


# Only import textual if we're running TUI mode
def run_tui(reports: list[TeamReport], workspace_name: str):
    """Run the interactive TUI."""
    from textual.app import App, ComposeResult
    from textual.containers import Container, Horizontal, Vertical, VerticalScroll
    from textual.widgets import Footer, Header, Static, ListItem, ListView, Label, Rule
    from textual.binding import Binding
    from textual.reactive import reactive
    import traceback

    # Debug logging to file
    DEBUG_LOG = Path("/tmp/linear-tui-debug.log")

    def debug(msg: str):
        with open(DEBUG_LOG, "a") as f:
            f.write(f"{datetime.now().isoformat()} | {msg}\n")

    # Clear previous log
    DEBUG_LOG.write_text(
        f"=== TUI Debug Log Started {datetime.now().isoformat()} ===\n"
    )
    debug(f"Reports count: {len(reports)}")
    for r in reports[:3]:
        debug(f"  Team: {r.team.name}, planned: {r.total_planned}")

    class IssueItem(Static):
        """A single issue display."""

        def __init__(self, issue: Issue, **kwargs):
            super().__init__(**kwargs)
            self.issue = issue

        def compose(self) -> ComposeResult:
            assignee = self.issue.assignee_name or "Unassigned"
            yield Static(
                f"{self.issue.state_icon} [{self.issue.identifier}] {self.issue.title[:50]}{'...' if len(self.issue.title) > 50 else ''}\n"
                f"   {assignee} | {self.issue.state_name}",
                classes="issue-item",
            )

    class CyclePanel(Static):
        """Display a cycle with its issues."""

        def __init__(self, cwi: CycleWithIssues, **kwargs):
            super().__init__(**kwargs)
            self.cwi = cwi

        def compose(self) -> ComposeResult:
            c = self.cwi.cycle
            status_icon = {"ended": "✅", "active": "🔄", "upcoming": "📅"}.get(
                c.status, ""
            )
            yield Static(
                f"[bold]Cycle {c.number}[/bold] {status_icon} {c.status.title()} ({c.date_range})\n"
                f"  {self.cwi.completion_pct}% complete | "
                f"✓{self.cwi.done_count} →{self.cwi.in_progress_count} ○{self.cwi.todo_count}",
                classes="cycle-header",
            )
            for issue in self.cwi.issues[:15]:  # Limit display
                yield IssueItem(issue)
            if len(self.cwi.issues) > 15:
                yield Static(
                    f"   ... and {len(self.cwi.issues) - 15} more", classes="more-items"
                )

    class TeamDetailPanel(VerticalScroll):
        """Right panel showing team details - uses Static.update() for content."""

        def compose(self) -> ComposeResult:
            debug("TeamDetailPanel.compose() called")
            yield Static("Select a team from the sidebar", id="detail-content")

        def show_report(self, report: Optional[TeamReport]) -> None:
            """Update the detail panel with a team report."""
            debug(f"show_report() called with: {report.team.name if report else None}")
            try:
                content = self.query_one("#detail-content", Static)

                if not report:
                    content.update("Select a team from the sidebar")
                    return

                # Build content as Rich markup string
                lines = []
                lines.append(f"[bold underline]{report.team.name}[/bold underline]")
                lines.append(
                    f"Planned: {report.total_planned} | Unplanned: {report.total_unplanned}"
                )
                lines.append("─" * 40)

                def render_cycle(cwi: CycleWithIssues, label: str, style: str):
                    c = cwi.cycle
                    status_icon = {"ended": "✅", "active": "🔄", "upcoming": "📅"}.get(
                        c.status, ""
                    )
                    lines.append(f"\n[{style}]{label}[/{style}]")
                    lines.append(
                        f"[bold]Cycle {c.number}[/bold] {status_icon} {c.status.title()} ({c.date_range})"
                    )
                    lines.append(
                        f"  {cwi.completion_pct}% complete | ✓{cwi.done_count} →{cwi.in_progress_count} ○{cwi.todo_count}"
                    )
                    for issue in cwi.issues[:10]:
                        assignee = issue.assignee_name or "Unassigned"
                        lines.append(
                            f"  {issue.state_icon} [{issue.identifier}] {issue.title[:40]}{'...' if len(issue.title) > 40 else ''}"
                        )
                    if len(cwi.issues) > 10:
                        lines.append(f"  ... and {len(cwi.issues) - 10} more")
                    lines.append("─" * 40)

                # Previous cycle
                if report.previous_cycle and report.previous_cycle.total > 0:
                    render_cycle(report.previous_cycle, "Previous Cycle", "dim")

                # Current cycle
                if report.current_cycle and report.current_cycle.total > 0:
                    render_cycle(report.current_cycle, "Current Cycle", "bold green")

                # Upcoming cycles
                for cwi in report.upcoming_cycles:
                    if cwi.total > 0:
                        render_cycle(cwi, "Upcoming", "cyan")

                # Unplanned active
                if report.unplanned_active:
                    lines.append(
                        f"\n[bold red]⚠ Unplanned Active ({len(report.unplanned_active)})[/bold red]"
                    )
                    for issue in report.unplanned_active[:5]:
                        lines.append(
                            f"  {issue.state_icon} [{issue.identifier}] {issue.title[:40]}"
                        )
                    if len(report.unplanned_active) > 5:
                        lines.append(
                            f"  ... and {len(report.unplanned_active) - 5} more"
                        )
                    lines.append("─" * 40)

                # Backlog
                if report.unplanned_backlog:
                    sus_count = sum(1 for b in report.unplanned_backlog if b.is_sus)
                    lines.append(
                        f"\n[yellow]📦 Backlog ({len(report.unplanned_backlog)}, {sus_count} suspicious)[/yellow]"
                    )
                    for item in report.unplanned_backlog[:3]:
                        prefix = "⚠️" if item.is_sus else "  "
                        days = (datetime.now(timezone.utc) - item.issue.updated_at).days
                        lines.append(
                            f"{prefix} [{item.issue.identifier}] {item.issue.title[:35]}... ({days}d)"
                        )
                    if len(report.unplanned_backlog) > 3:
                        lines.append(
                            f"  ... and {len(report.unplanned_backlog) - 3} more"
                        )

                content.update("\n".join(lines))
                debug("  show_report completed successfully")
            except Exception as e:
                debug(f"  ERROR in show_report: {e}\n{traceback.format_exc()}")

    class TeamListItem(ListItem):
        """A team in the sidebar list."""

        def __init__(self, report: TeamReport, **kwargs):
            super().__init__(**kwargs)
            self.report = report

        def compose(self) -> ComposeResult:
            planned = self.report.total_planned
            unplanned = len(self.report.unplanned_active)
            indicator = "🔴" if unplanned > 5 else "🟡" if unplanned > 0 else "🟢"
            yield Static(f"{indicator} {self.report.team.name} ({planned})")

    class LinearReportApp(App):
        """Linear Report TUI Application."""

        CSS = """
        Screen {
            layout: horizontal;
        }
        #sidebar {
            width: 30;
            border: solid green;
            height: 100%;
        }
        #sidebar-title {
            text-align: center;
            text-style: bold;
            padding: 1;
            background: $primary;
        }
        #main {
            width: 1fr;
            border: solid blue;
            height: 100%;
        }
        #team-detail-scroll {
            height: 100%;
            padding: 1;
        }
        .team-title {
            text-align: center;
            padding: 1;
        }
        .team-stats {
            text-align: center;
            color: $text-muted;
        }
        .section-label {
            padding-top: 1;
        }
        .cycle-header {
            padding: 0 1;
            background: $surface;
        }
        .issue-item {
            padding-left: 2;
        }
        .more-items {
            color: $text-muted;
            padding-left: 2;
        }
        .placeholder {
            text-align: center;
            padding: 5;
            color: $text-muted;
        }
        ListView {
            height: 1fr;
        }
        ListItem {
            padding: 0 1;
        }
        ListItem:hover {
            background: $surface;
        }
        ListView:focus > ListItem.--highlight {
            background: $primary;
        }
        """

        BINDINGS = [
            Binding("q", "quit", "Quit"),
            Binding("r", "refresh", "Refresh"),
            Binding("o", "open_url", "Open URL"),
            Binding("?", "help", "Help"),
        ]

        def __init__(self, reports: list[TeamReport], workspace_name: str):
            super().__init__()
            self.reports = reports
            self.workspace_name = workspace_name
            self.selected_report: Optional[TeamReport] = None

        def compose(self) -> ComposeResult:
            yield Header()
            with Horizontal():
                with Vertical(id="sidebar"):
                    yield Static(
                        f"[bold]{self.workspace_name}[/bold]", id="sidebar-title"
                    )
                    yield Static("[dim]Teams[/dim]")
                    with ListView(id="team-list"):
                        for report in self.reports:
                            yield TeamListItem(report)
                    yield Rule()
                    # Summary
                    total_unplanned = sum(len(r.unplanned_active) for r in self.reports)
                    total_backlog = sum(len(r.unplanned_backlog) for r in self.reports)
                    yield Static(f"[red]Unplanned: {total_unplanned}[/red]")
                    yield Static(f"[yellow]Backlog: {total_backlog}[/yellow]")
                with Container(id="main"):
                    yield TeamDetailPanel(id="detail-panel")
            yield Footer()

        def on_mount(self) -> None:
            """Show first team by default on startup."""
            debug("LinearReportApp.on_mount() called")
            if self.reports:
                debug(
                    f"  Deferring _show_team for first report: {self.reports[0].team.name}"
                )
                self.call_after_refresh(self._show_team, self.reports[0])

        def on_list_view_selected(self, event: ListView.Selected) -> None:
            debug(f"on_list_view_selected: {event.item}")
            if isinstance(event.item, TeamListItem):
                self._show_team(event.item.report)

        def on_list_view_highlighted(self, event: ListView.Highlighted) -> None:
            """Update detail panel as user navigates with arrow keys."""
            debug(f"on_list_view_highlighted: {event.item}")
            if event.item and isinstance(event.item, TeamListItem):
                self._show_team(event.item.report)

        def _show_team(self, report: TeamReport) -> None:
            """Display team details in the main panel."""
            debug(f"_show_team() called for: {report.team.name}")
            try:
                self.selected_report = report
                detail = self.query_one("#detail-panel", TeamDetailPanel)
                debug(f"  Got detail panel: {detail}")
                detail.show_report(report)
            except Exception as e:
                debug(f"  ERROR in _show_team: {e}\n{traceback.format_exc()}")

        def action_refresh(self) -> None:
            self.notify("Refreshing data...")
            # Would re-fetch data here

        def action_open_url(self) -> None:
            if self.selected_report and self.selected_report.current_cycle:
                issues = self.selected_report.current_cycle.issues
                if issues:
                    webbrowser.open(issues[0].url)
                    self.notify(f"Opened {issues[0].identifier}")

        def action_help(self) -> None:
            self.notify("Keys: q=quit, r=refresh, o=open URL, ↑↓=navigate", timeout=5)

    app = LinearReportApp(reports, workspace_name)
    app.run()


# ═══════════════════════════════════════════════════════════════════════════════
# File Output (--print-all mode)
# ═══════════════════════════════════════════════════════════════════════════════


def render_txt(reports: list[TeamReport], workspace_name: str, output_path: Path):
    """Render full report to plain text file."""
    lines = []

    def add(text: str = ""):
        lines.append(text)

    add("=" * 80)
    add("LINEAR CYCLES REPORT")
    add(f"Workspace: {workspace_name}")
    add(f"Generated: {datetime.now(timezone.utc).strftime('%Y-%m-%d %H:%M:%S UTC')}")
    add("=" * 80)
    add()

    for report in reports:
        add("-" * 80)
        add(f"TEAM: {report.team.name}")
        add(f"Planned: {report.total_planned} | Unplanned: {report.total_unplanned}")
        add("-" * 80)
        add()

        # Cycles
        for label, cwi in [
            ("PREVIOUS CYCLE", report.previous_cycle),
            ("CURRENT CYCLE", report.current_cycle),
        ]:
            if cwi and cwi.total > 0:
                c = cwi.cycle
                add(f"  {label}: Cycle {c.number} ({c.date_range}) - {c.status}")
                add(
                    f"    {cwi.completion_pct}% complete | Done: {cwi.done_count} | In Progress: {cwi.in_progress_count} | Todo: {cwi.todo_count}"
                )
                add()
                for issue in cwi.issues:
                    add(f"    {issue.state_icon} [{issue.identifier}] {issue.title}")
                    add(f"      URL: {issue.url}")
                    add(
                        f"      Assignee: {issue.assignee_name or 'Unassigned'} | State: {issue.state_name}"
                    )
                add()

        for cwi in report.upcoming_cycles:
            if cwi.total > 0:
                c = cwi.cycle
                add(f"  UPCOMING: Cycle {c.number} ({c.date_range})")
                add(f"    Todo: {cwi.todo_count}")
                for issue in cwi.issues:
                    add(f"    {issue.state_icon} [{issue.identifier}] {issue.title}")
                    add(f"      URL: {issue.url}")
                add()

        # Unplanned
        if report.unplanned_active:
            add(f"  UNPLANNED ACTIVE ({len(report.unplanned_active)} issues)")
            for issue in report.unplanned_active:
                add(f"    {issue.state_icon} [{issue.identifier}] {issue.title}")
                add(f"      URL: {issue.url}")
                add(f"      Assignee: {issue.assignee_name or 'Unassigned'}")
            add()

        if report.unplanned_backlog:
            sus_count = sum(1 for b in report.unplanned_backlog if b.is_sus)
            add(
                f"  BACKLOG ({len(report.unplanned_backlog)} issues, {sus_count} suspicious)"
            )
            for item in report.unplanned_backlog:
                sus = "[SUS] " if item.is_sus else ""
                days = (datetime.now(timezone.utc) - item.issue.updated_at).days
                add(f"    {sus}[{item.issue.identifier}] {item.issue.title}")
                add(f"      URL: {item.issue.url}")
                add(f"      Updated: {days} days ago | Activity: {item.activity_score}")
            add()

        if report.unplanned_done:
            add(f"  DONE/CANCELED UNPLANNED: {len(report.unplanned_done)} issues")
            add()

    output_path.write_text("\n".join(lines))
    print(f"Report written to: {output_path}")


def render_md(reports: list[TeamReport], workspace_name: str, output_path: Path):
    """Render full report to markdown file."""
    lines = []

    def add(text: str = ""):
        lines.append(text)

    add("# Linear Cycles Report")
    add()
    add(f"**Workspace:** {workspace_name}")
    add(
        f"**Generated:** {datetime.now(timezone.utc).strftime('%Y-%m-%d %H:%M:%S UTC')}"
    )
    add()

    for report in reports:
        add(f"## {report.team.name}")
        add()
        add(
            f"**Planned:** {report.total_planned} | **Unplanned:** {report.total_unplanned}"
        )
        add()

        for label, cwi in [
            ("Previous Cycle", report.previous_cycle),
            ("Current Cycle", report.current_cycle),
        ]:
            if cwi and cwi.total > 0:
                c = cwi.cycle
                status_emoji = {"ended": "✅", "active": "🔄", "upcoming": "📅"}.get(
                    c.status, ""
                )
                add(f"### {label}: Cycle {c.number} {status_emoji} ({c.date_range})")
                add()
                add(
                    f"**{cwi.completion_pct}% complete** | Done: {cwi.done_count} | In Progress: {cwi.in_progress_count} | Todo: {cwi.todo_count}"
                )
                add()
                add("| Status | ID | Title | Assignee |")
                add("|--------|-----|-------|----------|")
                for issue in cwi.issues:
                    title_link = f"[{issue.title[:35]}{'...' if len(issue.title) > 35 else ''}]({issue.url})"
                    add(
                        f"| {issue.state_icon} | {issue.identifier} | {title_link} | {issue.assignee_name or 'Unassigned'} |"
                    )
                add()

        for cwi in report.upcoming_cycles:
            if cwi.total > 0:
                c = cwi.cycle
                add(f"### Upcoming: Cycle {c.number} 📅 ({c.date_range})")
                add()
                add("| Status | ID | Title | Assignee |")
                add("|--------|-----|-------|----------|")
                for issue in cwi.issues:
                    title_link = f"[{issue.title[:35]}{'...' if len(issue.title) > 35 else ''}]({issue.url})"
                    add(
                        f"| {issue.state_icon} | {issue.identifier} | {title_link} | {issue.assignee_name or 'Unassigned'} |"
                    )
                add()

        if report.unplanned_active:
            add(f"### ⚠️ Unplanned Active ({len(report.unplanned_active)} issues)")
            add()
            add("| Status | ID | Title | Assignee |")
            add("|--------|-----|-------|----------|")
            for issue in report.unplanned_active:
                title_link = f"[{issue.title[:35]}{'...' if len(issue.title) > 35 else ''}]({issue.url})"
                add(
                    f"| {issue.state_icon} | {issue.identifier} | {title_link} | {issue.assignee_name or 'Unassigned'} |"
                )
            add()

        if report.unplanned_backlog:
            sus_count = sum(1 for b in report.unplanned_backlog if b.is_sus)
            add(
                f"### 📦 Backlog ({len(report.unplanned_backlog)} issues, {sus_count} suspicious)"
            )
            add()
            add("| Flag | ID | Title | Updated | Activity |")
            add("|------|-----|-------|---------|----------|")
            for item in report.unplanned_backlog:
                flag = "⚠️" if item.is_sus else ""
                days = (datetime.now(timezone.utc) - item.issue.updated_at).days
                title_link = f"[{item.issue.title[:30]}{'...' if len(item.issue.title) > 30 else ''}]({item.issue.url})"
                add(
                    f"| {flag} | {item.issue.identifier} | {title_link} | {days}d ago | {item.activity_score} |"
                )
            add()

        if report.unplanned_done:
            add(f"### ✅ Done/Canceled Unplanned: {len(report.unplanned_done)} issues")
            add()

    output_path.write_text("\n".join(lines))
    print(f"Report written to: {output_path}")


# ═══════════════════════════════════════════════════════════════════════════════
# Main
# ═══════════════════════════════════════════════════════════════════════════════


def main():
    parser = argparse.ArgumentParser(
        description="Linear Cycle Report - Interactive TUI"
    )
    parser.add_argument(
        "--print-all",
        action="store_true",
        help="Output complete report to file (no TUI)",
    )
    parser.add_argument(
        "--format",
        choices=["txt", "md"],
        default="txt",
        help="Output format for --print-all (default: txt)",
    )
    parser.add_argument(
        "--output",
        type=Path,
        help="Output file path (default: linear-report.{ext})",
    )
    parser.add_argument(
        "--tui",
        action="store_true",
        help="Force TUI mode even when not in a TTY",
    )
    args = parser.parse_args()

    # Auto-detect non-TTY (e.g., piped output), but allow --tui to override
    is_tty = sys.stdout.isatty()
    use_tui = args.tui or (is_tty and not args.print_all)

    api_key = load_api_key()
    client = LinearClient(api_key)

    # Fetch data
    if use_tui:
        print("Loading Linear data...")

    workspace_name = client.fetch_workspace_name()
    teams = client.fetch_teams()
    cycles = client.fetch_cycles()
    issues = client.fetch_all_issues()

    # Build reports
    reports = build_team_reports(teams, cycles, issues)

    # Early debug log (before TUI starts)
    debug_path = Path("/tmp/linear-tui-debug.log")
    debug_path.write_text(
        f"=== Pre-TUI Debug {datetime.now(timezone.utc).isoformat()} ===\n"
    )
    debug_path.open("a").write(f"use_tui={use_tui}, reports={len(reports)}\n")

    if use_tui:
        run_tui(reports, workspace_name)
    else:
        # File output
        ext = args.format
        output_path = args.output or Path(f"linear-report.{ext}")
        if ext == "md":
            render_md(reports, workspace_name, output_path)
        else:
            render_txt(reports, workspace_name, output_path)


if __name__ == "__main__":
    main()
