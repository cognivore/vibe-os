## VibeOS

Async Rust toolkit that mirrors Slack conversations to local JSONL archives and creates Linear issues from the terminal. Ships as both a library and the `vibeos` CLI.

---

## Requirements

- macOS or Linux
- [`direnv`](https://direnv.net/) with the repo’s `.envrc` (`direnv allow`)
- Nix (flake-enabled) or any environment providing Rust 1.75+ with `cargo`

`direnv` automatically loads the flake dev shell that bundles the Rust toolchain, so no extra setup is needed once it’s allowed.

---

## Initial Setup

1. Clone the repository and enter it:
   ```bash
   git clone https://github.com/your-org/vibe-os.git
   cd vibe-os
   direnv allow
   ```

2. Import the Slack app manifest found at `priv/slack_app_manifest.yaml` (Slack → **Your Apps** → **Create New App** → **From manifest**). Review the scopes before installing the app into your workspace.

3. Run the interactive setup command to collect credentials:
   ```bash
   cargo run -- setup
   ```
   - Opens Slack app dashboard (`https://api.slack.com/apps`)
   - Opens Linear security page (`https://linear.app/settings/account/security`)
   - Opens OpenAI API key management (`https://platform.openai.com/settings/organization/api-keys`)
   - Prompts for the OpenAI GPT-5 key (`OPENAI_API_KEY`) and Blood Money / vLLM key (`BLOOD_MONEY_API_KEY`)
   - Persists all values plus `VIBEOS_SLACK_MIRROR_DIR` into `.env`, merging with any existing entries
   - Ensures `.env` is git-ignored

You can re-run `vibeos setup` (or `cargo run -- setup`) anytime to rotate credentials.

---

## Commands

### Mirror Slack

```bash
cargo run -- slack mirror
# or specify a destination
cargo run -- slack mirror --output-dir /path/to/archive
```
After installation, invoke `vibeos slack mirror ...` directly.

Creates JSONL files under `SLACK_MIRROR_DIR` (default `./slack_mirror`):
- `conversations/<channel_id>.jsonl`
- `threads/<channel_id>_<thread_ts>.jsonl`
- `profiles/<user_id>.jsonl` – chronological snapshots from `users.list` (names, bios, emails, avatars, custom fields)

Handles pagination, threaded replies, gzip/brotli, and Slack 429 rate limiting.

The mirror is **incremental and append-only**:
- Each run inspects existing JSONL files to find the latest Slack `ts` stored per conversation/thread, fetches only newer records via `oldest=...`, and appends them.
- When Slack has no new activity, rerunning the command is idempotent—the files stay byte-for-byte identical.
- Because only new timestamps are ingested, edits, deletions, and reaction changes to older messages are ignored; rerun after new posts/replies land to capture the latest context.
- The user directory is refreshed on every run. If a profile field changes (e.g. display name, bio, email), a new JSONL entry is appended so downstream identity tooling can pre-fill personas or audit historical bios.

### Create a Linear Issue

**Easy way (recommended):**
```bash
cargo run -- linear easy-send priv/examples/easy-send-example.yaml
# or from stdin
cat issue.yaml | cargo run -- linear easy-send
```

**Verbose way:**
```bash
cargo run -- linear create-issue \
  --team-id <TEAM_UUID> \
  --title "Bug title" \
  --description "Short repro" \
  --priority 3
```

Prints the issue identifier and URL on success.
After installation, invoke `vibeos linear create-issue ...` directly.

### Update a Linear Issue

```bash
cargo run -- linear easy-update NIN-62 priv/examples/easy-update-example.yaml
# or from stdin
cat updates.yaml | cargo run -- linear easy-update FE-115
```

Uses issue identifier (e.g., NIN-62) instead of UUID. The YAML file should contain `why` and `what` fields, and optionally `title` to update those sections.

### Mirror Linear

```bash
cargo run -- linear sync
# or specify a destination
cargo run -- linear sync --output-dir /path/to/linear_mirror
```

Creates/updates a Linear mirror under `VIBEOS_LINEAR_MIRROR_DIR` (default `./linear_mirror`):
- `issues.jsonl` – canonical latest snapshot per issue (upserted via `updatedAt` filters, so reruns only fetch and rewrite changed issues)
- `events.N.jsonl` – append-only history shards capped at ~1 MB each (`events.0.jsonl` is the active log, older shards shift to `events.1.jsonl`, `events.2.jsonl`, …)
- `meta.json` – tracks `last_full_sync_at` to make subsequent syncs incremental and stores the workspace name
- `users/<user_id>.jsonl` – Linear user directory snapshots (name, display name, email, avatar, active flag, timestamps)

The sync is idempotent:
- Re-running adds only new issue updates and new events; unchanged issues/events leave their files untouched
- Historical events are never refetched—new entries append to the active shard, and automatic log rotation preserves older shards for analysis (`linear events`, `stale`, etc. read all shards)
- Because `issues.jsonl` is canonical, edits, state changes, and deletions are reflected immediately on the next run even though older event shards remain immutable
- The user directory is always crawled in full. A new snapshot is appended whenever Linear reports a change so persona linking can pre-populate identity metadata.

### Serve the dashboard API

```bash
cargo run -- serve
# or use the flake app
nix run .#serve
```

This boots the Axum server that exposes `/api/*` plus the static React dashboard bundle (when built). It uses the mirror directories and arrow store paths from the config file/environment:

- `VIBEOS_SLACK_MIRROR_DIR` / `VIBEOS_LINEAR_MIRROR_DIR`
- `VIBEOS_ARROW_STORE_DIR` (default `./arrows`)
- `VIBEOS_DASHBOARD_BIND` (default `127.0.0.1:3000`)
- `VIBEOS_DASHBOARD_STATIC_DIR` (optional pre-built frontend directory)

The API server includes a Tantivy-backed full-text search index that indexes all Slack messages and Linear issues. The search index is stored in `VIBEOS_SEARCH_INDEX_DIR` (default `./search_index`) and is automatically created on first run. To manually trigger a full reindex, use the `/api/search/reindex` endpoint (POST). The index is also incrementally updated via a background task that runs every 5 minutes.

### Frontend dashboard (pnpm + Vite)

The React dashboard lives under `dashboard/` and talks to the Rust API.

```bash
cd dashboard
pnpm install
pnpm dev
```

By default it targets `http://localhost:3000/api`. Override via `VITE_API_BASE`.

---

## Development Workflow

1. Format and lint before committing:
   ```bash
   direnv exec . cargo fmt
   direnv exec . cargo clippy -- -D warnings
   ```

2. Optional tests/build:
   ```bash
   direnv exec . cargo build
   direnv exec . cargo test
   ```

3. Add your changes and craft a C4-style commit summary (per project guidelines), then push.

For a quick local loop, use the helper scripts:

```bash
./priv/scripts/start-dev.sh   # launches API + Vite dev server (logs under .dev-pids/)
./priv/scripts/stop-dev.sh    # stops both processes and cleans up PID/log files
```

---

## Environment Variables

| Variable               | Description                                                    | Default                                     |
|------------------------|----------------------------------------------------------------|---------------------------------------------|
| `VIBEOS_SLACK_TOKEN` (legacy `SLACK_TOKEN`) | Slack OAuth token with read scopes            | *required*                                  |
| `VIBEOS_LINEAR_API_KEY` (legacy `LINEAR_API_KEY`) | Linear personal API key                     | *required*                                  |
| `OPENAI_API_KEY`       | OpenAI GPT-5 Responses API key                                 | *required for HaskLLM OpenAI provider*      |
| `BLOOD_MONEY_API_KEY`  | Blood Money (vLLM/Qwen) API key                                | *required for HaskLLM Qwen provider*        |
| `BLOOD_MONEY_BASE_URL` | Override base URL for Blood Money deployment                   | `https://outland-dev-1.doubling-season.geosurge.ai` |
| `VIBEOS_SLACK_MIRROR_DIR` (legacy `SLACK_MIRROR_DIR`) | Mirror directory for Slack archives | `./slack_mirror`                            |
| `VIBEOS_LLM_API_BASE` (legacy `LLM_API_BASE`) | OpenAI-compatible base URL             | *required*                                  |
| `VIBEOS_LLM_API_KEY` (legacy `LLM_API_KEY`) | Optional bearer token for LLM endpoint | _(none)_                                    |
| `VIBEOS_LLM_MODEL` (legacy `LLM_MODEL`) | Model identifier to call                | *required*                                  |
| `VIBEOS_LLM_TEMPERATURE` (legacy `LLM_TEMPERATURE`) | Sampling temperature                 | `0.2`                                       |
| `PERSONA_ROOT_DIR`      | Directory for identity/persona store JSON                     | `./personas`                                |

These are loaded via `dotenvy`; the CLI will error with guidance if any are missing.

---

## Identities & Personas

- **Identities** represent real humans and are keyed by canonical email. Each identity stores preferred name, avatar, and notes.
- **Personas** are domain-specific aliases (Slack handles, Linear users, Calendar attendees) that belong to an identity. A persona is uniquely identified by `(domain, local_id)`.
- Event envelopes and arrows now include both persona and identity identifiers so the HTTP API and dashboard can render “Ada (Slack:@ada)” consistently and operators/LLMs can reason about real people rather than per-app user IDs.
- The dashboard exposes `/api/identities` endpoints (CRUD, persona linking, merges) and an Identities view where you can search by email, attach new domain aliases, and resolve duplicates.

This separation lets email act as the global bridge while still capturing domain-specific handles for UI context.

---

## HaskLLM Reference Module

`src/haskllm` ports the original Haskell reference implementation used by downstream tooling. It exposes providers for OpenAI GPT‑5, Blood Money vLLM/Qwen, a generic fallback combinator, and a high-level Pandoc-style chat helper with structured patch responses.

Key entry points:
- `haskllm::openai::OpenAI` – Responses API client with JSON-schema enforcement.
- `haskllm::vllm::Qwen` – Blood Money / Qwen shim with identical trait surface.
- `haskllm::fallback::FallbackProvider` – Compose providers for resilient flows.
- `haskllm::pandoc_chat` – Contract for attachment editing plus `apply_edits_to_bodies`.

All providers honor `RequestConfig` for retries/timeouts and pull credentials from the merged `.env` file (with environment-variable fallbacks for containerized deployments).

## Troubleshooting

- **Missing tokens**: Rerun `vibeos setup`.
- **Permission errors**: Ensure the Slack app has `conversations.history`, `im:history`, etc., and that the Linear key can create issues in the target team.
- **429 rate limits**: The Slack client waits the `Retry-After` duration automatically; rerun later if limits persist.

Feel free to open issues/PRs for improvements or additional subcommands.

