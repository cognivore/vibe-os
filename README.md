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

Handles pagination, threaded replies, gzip/brotli, and Slack 429 rate limiting.

The mirror is **incremental and append-only**:
- Each run inspects existing JSONL files to find the latest Slack `ts` stored per conversation/thread, fetches only newer records via `oldest=...`, and appends them.
- When Slack has no new activity, rerunning the command is idempotent—the files stay byte-for-byte identical.
- Because only new timestamps are ingested, edits, deletions, and reaction changes to older messages are ignored; rerun after new posts/replies land to capture the latest context.

### Create a Linear Issue

```bash
cargo run -- linear create-issue \
  --team-id <TEAM_UUID> \
  --title "Bug title" \
  --description "Short repro" \
  --priority 3
```

Prints the issue identifier and URL on success.
After installation, invoke `vibeos linear create-issue ...` directly.

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

The sync is idempotent:
- Re-running adds only new issue updates and new events; unchanged issues/events leave their files untouched
- Historical events are never refetched—new entries append to the active shard, and automatic log rotation preserves older shards for analysis (`linear events`, `stale`, etc. read all shards)
- Because `issues.jsonl` is canonical, edits, state changes, and deletions are reflected immediately on the next run even though older event shards remain immutable

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

These are loaded via `dotenvy`; the CLI will error with guidance if any are missing.

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

