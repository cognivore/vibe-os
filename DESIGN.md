# VibeOS Threading Model

## Unified Timeline Philosophy

The unified timeline displays **threads only** - never individual messages or events.

### What is a Thread?

A thread is a conversation or issue that can accumulate events over time:

- **Slack Thread**: A message and its replies (identified by channel_id:thread_ts)
- **Linear Thread**: An issue and its comments/events (identified by issue_id)
- **Arrows**: Standalone cross-domain connections (not threaded)

### Thread Display Rules

1. **Thread Subject (Title)**:
   - Slack: Full text of the root message (first message), normalized for Slack markup
   - Linear: Issue title from Linear metadata
   - No truncation of thread subjects

2. **Thread Body** (shown in detail panel):
   - Slack: All messages chronologically (root + replies)
   - Linear: Issue description/body + comments

3. **Thread Ordering** (forum-style):
   - Threads sorted by timestamp of LATEST event in that thread
   - Old thread with new reply appears at top
   - Cross-domain: Slack and Linear threads intermixed by latest activity

4. **What Never Appears**:
   - Standalone Slack messages (not in threads)
   - Individual Linear events outside issue context
   - Any event that isn't part of a thread or an arrow

### Implementation Notes

- `buildSlackThreadEntries`: Groups Slack events by thread_ts
- `buildLinearThreadEntries`: Groups Linear events by issue_id
- Timeline filters: Remove individual events entirely, only show thread entries + arrows
- Backend enrichment: Fetches root message text for Slack threads on-demand

### Key Files

- `dashboard/src/pages/timeline/hooks/useTimelineData.ts`: Assembles timeline entries (threads + arrows only)
- `dashboard/src/pages/TimelinePage.tsx`: Handles search results (threads only)
- `dashboard/src/pages/timeline/threads/slackThreadAdapter.tsx`: Builds Slack thread entries
- `dashboard/src/pages/timeline/threads/linearThreadAdapter.tsx`: Builds Linear thread entries
- `crates/dashboard_api/src/routes/search.rs`: Backend thread title enrichment

