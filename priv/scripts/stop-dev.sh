#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
PID_DIR="$PROJECT_ROOT/.dev-pids"

echo "Stopping vibe-os development servers..."

stopped_count=0

# Stop backend
if [ -f "$PID_DIR/backend.pid" ]; then
  BACKEND_PID=$(cat "$PID_DIR/backend.pid")
  if kill -0 "$BACKEND_PID" 2>/dev/null; then
    echo "Stopping backend (PID: $BACKEND_PID)..."
    kill "$BACKEND_PID" || true
    # Wait up to 5 seconds for graceful shutdown
    for i in {1..10}; do
      if ! kill -0 "$BACKEND_PID" 2>/dev/null; then
        break
      fi
      sleep 0.5
    done
    # Force kill if still running
    if kill -0 "$BACKEND_PID" 2>/dev/null; then
      echo "  Force killing backend..."
      kill -9 "$BACKEND_PID" || true
    fi
    stopped_count=$((stopped_count + 1))
  fi
  rm -f "$PID_DIR/backend.pid"
fi

# Stop frontend
if [ -f "$PID_DIR/frontend.pid" ]; then
  FRONTEND_PID=$(cat "$PID_DIR/frontend.pid")
  if kill -0 "$FRONTEND_PID" 2>/dev/null; then
    echo "Stopping frontend (PID: $FRONTEND_PID)..."
    kill "$FRONTEND_PID" || true
    # Wait up to 5 seconds for graceful shutdown
    for i in {1..10}; do
      if ! kill -0 "$FRONTEND_PID" 2>/dev/null; then
        break
      fi
      sleep 0.5
    done
    # Force kill if still running
    if kill -0 "$FRONTEND_PID" 2>/dev/null; then
      echo "  Force killing frontend..."
      kill -9 "$FRONTEND_PID" || true
    fi
    stopped_count=$((stopped_count + 1))
  fi
  rm -f "$PID_DIR/frontend.pid"
fi

# Also clean up any orphaned processes
echo "Cleaning up any orphaned processes..."
pkill -f "vibeos.*serve" || true
pkill -f "cargo run.*serve" || true
# Kill pnpm and its child processes (vite, esbuild)
pkill -f "pnpm.*dev" || true
pkill -f "vite.*bin.*vite" || true
pkill -f "esbuild.*service" || true
# Also check for node processes running vite from the dashboard directory
pkill -f "node.*dashboard.*vite" || true

if [ $stopped_count -gt 0 ]; then
  echo "✓ Stopped $stopped_count process(es)"
else
  echo "No running processes found"
fi

# Clean up log files if desired
if [ -d "$PID_DIR" ] && [ -z "$(ls -A "$PID_DIR")" ]; then
  rmdir "$PID_DIR" 2>/dev/null || true
fi

