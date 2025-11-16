#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
PID_DIR="$PROJECT_ROOT/.dev-pids"

mkdir -p "$PID_DIR"

echo "Starting vibe-os development servers..."

# Start backend
echo "Starting backend API server..."
cd "$PROJECT_ROOT"
nix develop --command cargo run -- serve > "$PID_DIR/backend.log" 2>&1 &
BACKEND_PID=$!
echo $BACKEND_PID > "$PID_DIR/backend.pid"
echo "  Backend started (PID: $BACKEND_PID, logs: $PID_DIR/backend.log)"

# Wait a moment for backend to initialize
sleep 2

# Start frontend
echo "Starting frontend dev server..."
cd "$PROJECT_ROOT/dashboard"
nix develop --command pnpm dev > "$PID_DIR/frontend.log" 2>&1 &
FRONTEND_PID=$!
echo $FRONTEND_PID > "$PID_DIR/frontend.pid"
echo "  Frontend started (PID: $FRONTEND_PID, logs: $PID_DIR/frontend.log)"

echo ""
echo "✓ Development servers running"
echo "  Backend:  http://localhost:3000"
echo "  Frontend: http://localhost:5173"
echo ""
echo "To stop: run priv/scripts/stop-dev.sh"
echo "To view logs:"
echo "  Backend:  tail -f $PID_DIR/backend.log"
echo "  Frontend: tail -f $PID_DIR/frontend.log"

