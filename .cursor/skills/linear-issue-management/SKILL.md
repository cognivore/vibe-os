---
name: linear-issue-management
description: >-
  Create, update, and comment on Linear issues following a structured issue
  template rooted in INCOSE systems engineering discipline. Use when the user
  asks to create issues, plan sprints, send issues to Linear, comment on issues,
  or manage the issue backlog. Covers candidate-first drafting, template
  compliance, backup-before-update, and research via Linear mirror, Slack mirror,
  and Git mirrors.
---

# Linear Issue Management

## Candidate-First Workflow

Never send an issue to Linear without drafting it first.

1. Write YAML to `priv/candidates/` using the naming convention `YYYY-MM-DD-C{cycle}-{nn}-{slug}.yaml`
2. Present the draft to the user
3. Wait for explicit approval before running `easy-send`
4. When sending, skip empty files (user may have deleted some candidates)

Before updating an existing Linear issue with `easy-update`, back up the current description:

1. Run `linear sync` to get the latest state
2. Read the issue from `linear_mirror/issues.jsonl`
3. Write the current description to `priv/candidates/backup/YYYYMMDD-{IDENTIFIER}.yaml`
4. Only then run `easy-update`

## Issue Template

Every issue must follow this template. See [template-reference.md](template-reference.md) for the full template with examples.

### Why field structure

```
**Business problem:**

One paragraph. What customer, revenue, risk, or strategic problem exists today?

**Affected stakeholders:** Groups of natural persons, not abstract concepts.

**Proposed solution:** One sentence. What needs to be done?
```

Stakeholders must be INCOSE-compliant: groups of natural persons who are affected by the problem. Never use abstract nouns like "system integration", "product delivery velocity", "incident forensics", or "operational scalability". Always name who: "sales team", "customers using the product", "on-call engineers during incidents", "the customer's marketing team".

### What field structure

The What section defines success, not tasks or implementation.

```
> This defines *success*, not tasks or implementation.

**What must be true when this is done?**
- [ ] Observable condition 1
- [ ] Observable condition 2

**What observable outcome, capability, or decision does this enable?**
- [ ] Outcome 1
- [ ] Outcome 2

**How will we know it worked?**
- [ ] Metric, behaviour, or test protocol 1
- [ ] Metric, behaviour, or test protocol 2

---
- [x] I acknowledge that anyone can read just the text of this issue and understand why we're doing the work, what success looks like, and how it serves the business.
```

### Formatting

- Each semantic paragraph flows as one line. No mid-sentence line breaks.
- Each bullet item gets its own line.
- Blank lines separate sections and paragraphs.

## Research Before Writing

Before creating issues, gather context from these sources:

### Linear mirror

Located at `linear_mirror/issues.jsonl`. One JSON object per line.

Search for duplicates and related issues. Key fields: `identifier`, `title`, `description`, `state_type`, `state_name`, `assignee_name`, `team_key`, `cycle_number`.

Active states: `backlog`, `unstarted`, `started`, `triage`.

### Slack mirror

Located at `slack_mirror/`. Contains `conversations/`, `threads/`, `profiles/`. Search for discussion context that informs issue writing.

### Git mirrors

If available, check recent commits in relevant repos for code-level context:

```bash
cd ~/Mirrors/Github/{org}/{repo} && git log --oneline --since="YYYY-MM-DD" --all
```

### Linking

Always link related existing Linear issues via the `related`, `builds_on`, `supersedes` fields. Only use real Linear identifiers (e.g. `TEAM-123`). Put non-Linear cross-references (like candidate file names) in `notes` as text.

## CLI Commands

All commands require the Nix dev shell. Run `eval "$(direnv export zsh 2>/dev/null)"` first.

### Sync mirror (always do this first)

```bash
cargo run -- linear sync
```

### Create issue from YAML

```bash
cargo run -- linear easy-send priv/candidates/FILE.yaml
```

### Update existing issue

```bash
cargo run -- linear easy-update IDENTIFIER priv/candidates/FILE.yaml
```

The YAML needs `title` (optional), `why`, and `what` fields.

### Add comment to issue

```bash
cargo run -- linear easy-comment IDENTIFIER -m "message"
cargo run -- linear easy-comment IDENTIFIER -f comment.md
echo "message" | cargo run -- linear easy-comment IDENTIFIER
```

### Search local mirror

```bash
cargo run -- linear browse --search "term"
cargo run -- linear browse --search "term" --json
```

## YAML Field Reference

Source: `crates/vibeos_cli/src/easy_send.rs`

| Field | Required | Format |
|-------|----------|--------|
| `who` | yes | `user@TEAM` (e.g. `alice@ENG`, `bob@OPS`) |
| `title` | no | String (auto-generated from `why` first line if missing) |
| `why` | yes | Block scalar with template structure |
| `what` | yes | Block scalar with success criteria |
| `priority` | no | 1=Urgent, 2=High, 3=Normal, 4=Low |
| `cycle` | no | Number or string (e.g. `17` or `"Sprint 17"`) |
| `dependencies` | no | List of Linear identifiers that block this issue |
| `supersedes` | no | List of Linear identifiers marked as duplicates |
| `builds_on` | no | List of Linear identifiers (related link) |
| `enables` | no | List of Linear identifiers (related link) |
| `related` | no | List of Linear identifiers (related link) |
| `notes` | no | Additional text appended as `# Notes` section |

## Safety Rules

1. **Never overwrite a live Linear issue without backing up first.** Someone will lose an hour of work. Always back up to `priv/candidates/backup/` before `easy-update`.

2. **Never easy-send empty or broken files.** Check file size before sending. The user may have deleted candidates they don't want.

3. **Sync the mirror before any operation** that reads issue state. The mirror is a point-in-time snapshot and goes stale.

4. **Relation fields only accept Linear identifiers** like `ENG-42`. Anything else (candidate file names, free text) causes a hard error. Put non-Linear cross-references in `notes`.

5. **Assignees must exist in Linear.** Check the mirror first. If the person has no Linear account (e.g. an external contractor), assign to a proxy and note the intended owner in `notes`.

6. **Never edit candidate files the user is reviewing** without being asked. Present drafts, wait for feedback, apply changes when instructed.

7. **When splitting an issue**, create new candidates and link back to the original via `related`. Don't delete the original from Linear — let the user cancel it.
