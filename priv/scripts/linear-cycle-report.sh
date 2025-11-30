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

CURRENT_DATE=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

echo "╔════════════════════════════════════════════════════════════════════════════╗"
echo "║                     LINEAR CYCLES REPORT                                   ║"
echo "║                     $(date '+%Y-%m-%d %H:%M')                                        ║"
echo "╚════════════════════════════════════════════════════════════════════════════╝"
echo ""

fetch_all_teams() {
  curl -s -X POST https://api.linear.app/graphql \
    -H "Content-Type: application/json" \
    -H "Authorization: $VIBEOS_LINEAR_API_KEY" \
    -d '{"query": "{ teams { nodes { key name } } }"}' | jq -r '.data.teams.nodes[].name'
}

fetch_all_cycles() {
  curl -s -X POST https://api.linear.app/graphql \
    -H "Content-Type: application/json" \
    -H "Authorization: $VIBEOS_LINEAR_API_KEY" \
    -d '{"query": "{ cycles(first: 100) { nodes { id number name startsAt endsAt team { name } } } }"}' | jq '.data.cycles.nodes'
}

fetch_all_issues() {
  local after=""
  local issues="[]"

  while true; do
    local query
    if [ -z "$after" ]; then
      query='{"query": "{ issues(first: 100) { nodes { id identifier title url team { key name } state { name type } assignee { name } labels { nodes { name } } priority estimate cycle { number startsAt endsAt } createdAt completedAt } pageInfo { hasNextPage endCursor } } }"}'
    else
      query="{\"query\": \"{ issues(first: 100, after: \\\"$after\\\") { nodes { id identifier title url team { key name } state { name type } assignee { name } labels { nodes { name } } priority estimate cycle { number startsAt endsAt } createdAt completedAt } pageInfo { hasNextPage endCursor } } }\"}"
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

echo "Fetching data from Linear..."
ALL_TEAMS=$(fetch_all_teams)
ALL_CYCLES=$(fetch_all_cycles)
ALL_ISSUES=$(fetch_all_issues)

mapfile -t TEAM_ARRAY <<< "$ALL_TEAMS"

# Get unique cycle numbers that have issues, sorted
CYCLES_WITH_ISSUES=$(echo "$ALL_ISSUES" | jq -r '[.[] | select(.cycle != null) | .cycle.number] | unique | sort | .[]')

# Build cycle metadata (aggregate across teams)
# For each cycle number, find the earliest start and latest end
declare -A CYCLE_STARTS
declare -A CYCLE_ENDS

while IFS= read -r cycle_json; do
  [ -z "$cycle_json" ] && continue
  num=$(echo "$cycle_json" | jq -r '.number')
  starts=$(echo "$cycle_json" | jq -r '.startsAt // empty')
  ends=$(echo "$cycle_json" | jq -r '.endsAt // empty')

  if [ -n "$starts" ]; then
    if [ -z "${CYCLE_STARTS[$num]:-}" ] || [[ "$starts" < "${CYCLE_STARTS[$num]}" ]]; then
      CYCLE_STARTS[$num]="$starts"
    fi
  fi
  if [ -n "$ends" ]; then
    if [ -z "${CYCLE_ENDS[$num]:-}" ] || [[ "$ends" > "${CYCLE_ENDS[$num]}" ]]; then
      CYCLE_ENDS[$num]="$ends"
    fi
  fi
done < <(echo "$ALL_CYCLES" | jq -c '.[]')

# Determine cycle status for each
# - "ended" if endsAt < now
# - "active" if startsAt <= now <= endsAt
# - "upcoming" if startsAt > now
get_cycle_status() {
  local num=$1
  local starts="${CYCLE_STARTS[$num]:-}"
  local ends="${CYCLE_ENDS[$num]:-}"

  if [ -n "$ends" ] && [[ "$ends" < "$CURRENT_DATE" ]]; then
    echo "ended"
  elif [ -n "$starts" ] && [[ "$starts" > "$CURRENT_DATE" ]]; then
    echo "upcoming"
  else
    echo "active"
  fi
}

echo ""

# ═══════════════════════════════════════════════════════════════════════════════
# UNPLANNED ISSUES (not in any cycle)
# ═══════════════════════════════════════════════════════════════════════════════

unplanned_issues=$(echo "$ALL_ISSUES" | jq '[.[] | select(.cycle == null)]')
unplanned_count=$(echo "$unplanned_issues" | jq 'length')
unplanned_active=$(echo "$unplanned_issues" | jq '[.[] | select(.state.type != "completed" and .state.type != "canceled")] | length')

echo "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓"
echo "┃ 📦 UNPLANNED ISSUES (not assigned to any cycle)                             ┃"
echo "┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫"
echo "┃ Total: $unplanned_count issues ($unplanned_active active, $((unplanned_count - unplanned_active)) done/canceled)"
echo "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛"
echo ""

# Show unplanned by team
echo "Unplanned by team:"
for team in "${TEAM_ARRAY[@]}"; do
  [ -z "$team" ] && continue
  team_unplanned=$(echo "$unplanned_issues" | jq "[.[] | select(.team.name == \"$team\")] | length")
  team_active=$(echo "$unplanned_issues" | jq "[.[] | select(.team.name == \"$team\" and .state.type != \"completed\" and .state.type != \"canceled\")] | length")
  if [ "$team_unplanned" -gt 0 ]; then
    printf "  %-30s %3d issues (%d active)\n" "$team" "$team_unplanned" "$team_active"
  fi
done
echo ""

# ═══════════════════════════════════════════════════════════════════════════════
# CYCLE REPORTS
# ═══════════════════════════════════════════════════════════════════════════════

# Find the first "active" or "ended" cycle to use as baseline
# Show: any ended cycles, all active cycles, all upcoming cycles with issues
first_active_or_ended=""
for cycle_num in $CYCLES_WITH_ISSUES; do
  status=$(get_cycle_status "$cycle_num")
  if [ "$status" = "active" ] || [ "$status" = "ended" ]; then
    first_active_or_ended="$cycle_num"
    break
  fi
done

# If no active/ended found, show all
if [ -z "$first_active_or_ended" ]; then
  first_active_or_ended=$(echo "$CYCLES_WITH_ISSUES" | head -1)
fi

for cycle_num in $CYCLES_WITH_ISSUES; do
  cycle_issues=$(echo "$ALL_ISSUES" | jq "[.[] | select(.cycle != null and .cycle.number == $cycle_num)]")
  cycle_count=$(echo "$cycle_issues" | jq 'length')

  [ "$cycle_count" -eq 0 ] && continue

  done_count=$(echo "$cycle_issues" | jq '[.[] | select(.state.type == "completed")] | length')
  in_progress=$(echo "$cycle_issues" | jq '[.[] | select(.state.type == "started")] | length')
  todo_count=$(echo "$cycle_issues" | jq '[.[] | select(.state.type == "unstarted" or .state.type == "backlog")] | length')
  canceled=$(echo "$cycle_issues" | jq '[.[] | select(.state.type == "canceled")] | length')

  # Determine cycle status label
  status=$(get_cycle_status "$cycle_num")
  case "$status" in
    ended)    status_label=" ✅ Ended" ;;
    active)   status_label=" 🔄 Active" ;;
    upcoming) status_label=" 📅 Upcoming" ;;
    *)        status_label="" ;;
  esac

  # Format dates
  starts="${CYCLE_STARTS[$cycle_num]:-}"
  ends="${CYCLE_ENDS[$cycle_num]:-}"
  date_range=""
  if [ -n "$starts" ] && [ -n "$ends" ]; then
    starts_fmt=$(echo "$starts" | cut -d'T' -f1)
    ends_fmt=$(echo "$ends" | cut -d'T' -f1)
    date_range=" ($starts_fmt → $ends_fmt)"
  fi

  completion_pct=0
  if [ "$cycle_count" -gt 0 ]; then
    completion_pct=$((done_count * 100 / cycle_count))
  fi

  echo "╔════════════════════════════════════════════════════════════════════════════╗"
  echo "║ CYCLE $cycle_num$status_label$date_range"
  echo "║ Total: $cycle_count | ✓ Done: $done_count | 🔄 In Progress: $in_progress | 📋 Todo: $todo_count | ✗ Canceled: $canceled"
  echo "║ Completion: ${completion_pct}%"
  echo "╚════════════════════════════════════════════════════════════════════════════╝"
  echo ""

  for team in "${TEAM_ARRAY[@]}"; do
    [ -z "$team" ] && continue

    team_cycle_issues=$(echo "$cycle_issues" | jq "[.[] | select(.team.name == \"$team\")]")
    count=$(echo "$team_cycle_issues" | jq 'length')

    [ "$count" -eq 0 ] && continue

    team_done=$(echo "$team_cycle_issues" | jq '[.[] | select(.state.type == "completed")] | length')
    team_in_progress=$(echo "$team_cycle_issues" | jq '[.[] | select(.state.type == "started")] | length')
    team_todo=$(echo "$team_cycle_issues" | jq '[.[] | select(.state.type == "unstarted" or .state.type == "backlog")] | length')
    team_canceled=$(echo "$team_cycle_issues" | jq '[.[] | select(.state.type == "canceled")] | length')

    echo "┌──────────────────────────────────────────────────────────────────────────────┐"
    echo "│ $team ($count issues)"
    echo "│ ✓ Done: $team_done | 🔄 In Progress: $team_in_progress | 📋 Todo: $team_todo | ✗ Canceled: $team_canceled"
    echo "├──────────────────────────────────────────────────────────────────────────────┤"

    echo "$team_cycle_issues" | jq -r '.[] |
      (if .state.type == "completed" then "✓"
       elif .state.type == "started" then "→"
       elif .state.type == "canceled" then "✗"
       else "○" end) as $icon |
      "│ \($icon) [\(.identifier)] \(.title | if length > 55 then .[:52] + "..." else . end)\n│   \(.url)"'

    echo "└──────────────────────────────────────────────────────────────────────────────┘"
    echo ""
  done
done

# ═══════════════════════════════════════════════════════════════════════════════
# GRAND SUMMARY
# ═══════════════════════════════════════════════════════════════════════════════

total_issues=$(echo "$ALL_ISSUES" | jq 'length')
total_planned=$(echo "$ALL_ISSUES" | jq '[.[] | select(.cycle != null)] | length')
total_done=$(echo "$ALL_ISSUES" | jq '[.[] | select(.state.type == "completed")] | length')
total_active=$(echo "$ALL_ISSUES" | jq '[.[] | select(.state.type != "completed" and .state.type != "canceled")] | length')

echo "════════════════════════════════════════════════════════════════════════════════"
echo "WORKSPACE SUMMARY"
echo "────────────────────────────────────────────────────────────────────────────────"
echo "Total Issues:     $total_issues"
echo "  📅 Planned:     $total_planned (in cycles)"
echo "  📦 Unplanned:   $unplanned_count (not in any cycle)"
echo "  ✓ Completed:    $total_done"
echo "  🔄 Active:      $total_active"
echo "════════════════════════════════════════════════════════════════════════════════"
