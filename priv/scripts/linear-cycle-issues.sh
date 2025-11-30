#!/usr/bin/env bash
# ╔═══════════════════════════════════════════════════════════════════════════╗
# ║ DEPRECATED: Use linear-report.py instead                                  ║
# ║                                                                           ║
# ║   ./priv/scripts/linear-report.py              # Interactive TUI         ║
# ║   ./priv/scripts/linear-report.py --print-all  # Full report to file     ║
# ║                                                                           ║
# ║ This script will be removed in a future version.                         ║
# ╚═══════════════════════════════════════════════════════════════════════════╝
set -euo pipefail

echo "⚠️  DEPRECATED: This script is deprecated. Use ./priv/scripts/linear-report.py instead."
echo ""

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# Load environment
if [ -f "$PROJECT_ROOT/.env" ]; then
  set -a
  source "$PROJECT_ROOT/.env"
  set +a
fi

if [ -z "${VIBEOS_LINEAR_API_KEY:-}" ]; then
  echo "Error: VIBEOS_LINEAR_API_KEY not set. Run 'vibeos setup' or set it in .env" >&2
  exit 1
fi

usage() {
  echo "Usage: $0 [OPTIONS]"
  echo ""
  echo "List Linear issues for a specific cycle and team(s)"
  echo ""
  echo "Options:"
  echo "  -c, --cycle NUM     Cycle number (default: 1)"
  echo "  -t, --team NAME     Team name (can be repeated, default: all tracked teams)"
  echo "  -f, --format FMT    Output format: table, json, csv (default: table)"
  echo "  -s, --state STATE   Filter by state: done, in-progress, todo, all (default: all)"
  echo "  -h, --help          Show this help"
  echo ""
  echo "Examples:"
  echo "  $0 -c 1 -t Ninjaneers"
  echo "  $0 -c 1 -t Frontend -t Ninjaneers -f json"
  echo "  $0 -c 1 --state done"
}

CYCLE_NUMBER=1
TEAMS=()
FORMAT="table"
STATE_FILTER="all"

while [[ $# -gt 0 ]]; do
  case $1 in
    -c|--cycle)
      CYCLE_NUMBER="$2"
      shift 2
      ;;
    -t|--team)
      TEAMS+=("$2")
      shift 2
      ;;
    -f|--format)
      FORMAT="$2"
      shift 2
      ;;
    -s|--state)
      STATE_FILTER="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      usage
      exit 1
      ;;
  esac
done

fetch_all_teams() {
  curl -s -X POST https://api.linear.app/graphql \
    -H "Content-Type: application/json" \
    -H "Authorization: $VIBEOS_LINEAR_API_KEY" \
    -d '{"query": "{ teams { nodes { key name } } }"}' | jq -r '.data.teams.nodes[].name'
}

# Default teams if none specified - fetch all available teams
if [ ${#TEAMS[@]} -eq 0 ]; then
  mapfile -t TEAMS < <(fetch_all_teams)
fi

fetch_all_issues() {
  local after=""
  local issues="[]"

  while true; do
    local query
    if [ -z "$after" ]; then
      query='{"query": "{ issues(first: 100) { nodes { id identifier title url team { key name } state { name type } assignee { name } labels { nodes { name } } priority estimate cycle { number } createdAt completedAt } pageInfo { hasNextPage endCursor } } }"}'
    else
      query="{\"query\": \"{ issues(first: 100, after: \\\"$after\\\") { nodes { id identifier title url team { key name } state { name type } assignee { name } labels { nodes { name } } priority estimate cycle { number } createdAt completedAt } pageInfo { hasNextPage endCursor } } }\"}"
    fi

    local result
    result=$(curl -s -X POST https://api.linear.app/graphql \
      -H "Content-Type: application/json" \
      -H "Authorization: $VIBEOS_LINEAR_API_KEY" \
      -d "$query")

    local has_next
    has_next=$(echo "$result" | jq -r '.data.issues.pageInfo.hasNextPage')
    after=$(echo "$result" | jq -r '.data.issues.pageInfo.endCursor')

    local new_issues
    new_issues=$(echo "$result" | jq '.data.issues.nodes')
    issues=$(echo "[$issues, $new_issues]" | jq -s 'add | add')

    if [ "$has_next" != "true" ]; then
      break
    fi
  done

  echo "$issues"
}

# Build team filter for jq
team_filter=""
for team in "${TEAMS[@]}"; do
  if [ -n "$team_filter" ]; then
    team_filter="$team_filter or "
  fi
  team_filter="$team_filter.team.name == \"$team\""
done

# Build state filter for jq
state_jq_filter=""
case "$STATE_FILTER" in
  done)
    state_jq_filter='and .state.type == "completed"'
    ;;
  in-progress)
    state_jq_filter='and .state.type == "started"'
    ;;
  todo)
    state_jq_filter='and (.state.type == "unstarted" or .state.type == "backlog")'
    ;;
  all)
    state_jq_filter=""
    ;;
  *)
    echo "Unknown state filter: $STATE_FILTER" >&2
    exit 1
    ;;
esac

ALL_ISSUES=$(fetch_all_issues)

filtered=$(echo "$ALL_ISSUES" | jq "[.[] | select(.cycle != null and .cycle.number == $CYCLE_NUMBER and ($team_filter) $state_jq_filter)]")

case "$FORMAT" in
  json)
    echo "$filtered" | jq '.'
    ;;
  csv)
    echo "identifier,title,team,state,assignee,priority,created_at,completed_at,url"
    echo "$filtered" | jq -r '.[] | [.identifier, .title, .team.name, .state.name, (.assignee.name // ""), (.priority // ""), .createdAt, (.completedAt // ""), .url] | @csv'
    ;;
  table)
    echo ""
    printf "%-10s %-50s %-20s %-15s %-20s\n" "ID" "TITLE" "TEAM" "STATE" "ASSIGNEE"
    printf "%-10s %-50s %-20s %-15s %-20s\n" "──────────" "──────────────────────────────────────────────────" "────────────────────" "───────────────" "────────────────────"
    echo "$filtered" | jq -r '.[] | [.identifier, (.title | if length > 48 then .[:45] + "..." else . end), .team.name, .state.name, (.assignee.name // "Unassigned")] | @tsv' | \
      while IFS=$'\t' read -r id title team state assignee; do
        printf "%-10s %-50s %-20s %-15s %-20s\n" "$id" "$title" "$team" "$state" "$assignee"
      done
    echo ""
    echo "Total: $(echo "$filtered" | jq 'length') issues"
    ;;
esac

