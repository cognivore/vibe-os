# Getting Rid of Data Leaks in Git Without BFG

You committed `secrets.json` and pushed to main. Now it's in the history. Forever. Right?

Wrong.

## The Problem

Files in git history are *really* in history. Even if you delete them and commit, they're still there. Anyone can `git log`, find the old commit, and read your secrets. `.gitignore` only stops *new* files from being tracked.

## The Solution (4 Steps)

### 1. Gitignore First

```bash
echo "personas/identities.json" >> .gitignore
git add .gitignore
git commit -m "Problem: identities.json should not be tracked"
```

You need a clean working tree before rewriting history.

### 2. Rewrite History

```bash
FILTER_BRANCH_SQUELCH_WARNING=1 \
git filter-branch \
  --index-filter 'git rm --cached --ignore-unmatch personas/identities.json' \
  --prune-empty \
  -- --all
```

This walks through *every commit* and removes the file. It's like the file never existed.

### 3. Force Push (Safely)

```bash
git push --force-with-lease origin main
```

`--force-with-lease` is the safe force push. It fails if someone else pushed while you were rewriting history.

If it fails with "stale info", fetch and specify the expected commit:

```bash
git fetch origin main
git push --force-with-lease=main:abc1234 origin main
```

### 4. Clean Up Locally

```bash
rm -rf .git/refs/original/
git reflog expire --expire=now --all
git gc --prune=now --aggressive
```

Filter-branch keeps backups. These commands actually free the disk space.

## Why This Works

Git commits reference their parents and the entire tree state. When you rewrite history, you create *new* commits with new hashes. The old commits become unreachable. Garbage collection removes them.

## The Catch

Everyone who cloned your repo has the old history. They need to:

```bash
git fetch origin
git reset --hard origin/main
```

Or just re-clone. Old commits are still in *their* local copies until they do this.

## When To Use BFG Instead

Use BFG Repo-Cleaner if:
- Your repo has thousands of commits (filter-branch is slow)
- Multiple files/patterns need removal
- You want simpler commands

But `git filter-branch` is built-in and works fine for small repos or one-off cleanups.

## Remember

The real lesson: **Don't commit secrets in the first place.** Use environment variables, secret managers, and proper `.gitignore` from day one.

But when you mess up, this is how you fix it.

