## Slack Linear Tools

Async Rust toolkit that mirrors Slack conversations to local JSONL archives and creates Linear issues from the terminal. Ships as both a library and the `slack-linear` CLI.

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

2. Run the interactive setup command to collect credentials:
   ```bash
   cargo run -- slack-linear setup
   ```
   - Opens Slack app dashboard (`https://api.slack.com/apps`)
   - Opens Linear security page (`https://linear.app/settings/account/security`)
   - Opens OpenAI API key management (`https://platform.openai.com/settings/organization/api-keys`)
   - Prompts for the OpenAI GPT-5 key (`OPENAI_API_KEY`) and Blood Money / vLLM key (`BLOOD_MONEY_API_KEY`)
   - Persists all values plus `SLACK_MIRROR_DIR` into `.env`, merging with any existing entries
   - Ensures `.env` is git-ignored

You can re-run `slack-linear setup` anytime to rotate credentials.

---

## Commands

### Mirror Slack

```bash
cargo run -- slack-linear slack mirror
# or specify a destination
cargo run -- slack-linear slack mirror --output-dir /path/to/archive
```

Creates JSONL files under `SLACK_MIRROR_DIR` (default `./slack_mirror`):
- `conversations/<channel_id>.jsonl`
- `threads/<channel_id>_<thread_ts>.jsonl`

Handles pagination, threaded replies, gzip/brotli, and Slack 429 rate limiting.

### Create a Linear Issue

```bash
cargo run -- slack-linear linear create-issue \
  --team-id <TEAM_UUID> \
  --title "Bug title" \
  --description "Short repro" \
  --priority 3
```

Prints the issue identifier and URL on success.

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
| `SLACK_TOKEN`          | Slack OAuth token with read scopes                             | *required*                                  |
| `LINEAR_API_KEY`       | Linear personal API key                                        | *required*                                  |
| `OPENAI_API_KEY`       | OpenAI GPT-5 Responses API key                                 | *required for HaskLLM OpenAI provider*      |
| `BLOOD_MONEY_API_KEY`  | Blood Money (vLLM/Qwen) API key                                | *required for HaskLLM Qwen provider*        |
| `BLOOD_MONEY_BASE_URL` | Override base URL for Blood Money deployment                   | `https://outland-dev-1.doubling-season.geosurge.ai` |
| `SLACK_MIRROR_DIR`     | Mirror directory for Slack archives                            | `./slack_mirror`                            |

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

- **Missing tokens**: Rerun `slack-linear setup`.
- **Permission errors**: Ensure the Slack app has `conversations.history`, `im:history`, etc., and that the Linear key can create issues in the target team.
- **429 rate limits**: The Slack client waits the `Retry-After` duration automatically; rerun later if limits persist.

Feel free to open issues/PRs for improvements or additional subcommands.

