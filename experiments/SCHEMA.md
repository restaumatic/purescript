# Experiment folder schema

Each experiment lives at `experiments/<id>/`. `<id>` is a short
kebab-case name (e.g., `tc-queries`, `qualified-compare-intern`) and
matches the branch name and worktree name at `/workspace/p/<id>`.

## Required files

### `EXPERIMENT.md`

Live summary + status. YAML frontmatter is the machine-readable part;
the body is a short human narrative (hypothesis, scope, links).

```yaml
---
id: <id>
status: proposed | in-progress | blocked | shipped | abandoned
verdict: tbd | win | partial | no-win | abandoned
branch: <branch-name>
worktree: /workspace/p/<id>
baseline_sha: <short-sha>           # the baseline this is measured against
head_sha: <short-sha>               # tip of the experiment branch
hypothesis: >
  One or two sentences naming the hypothesis. What expensive thing
  are we trying to make cheaper, and what change will (supposedly)
  make that happen?
headline_delta: "+/-X% full, +/-Y% prelude-edit"  # null while in-progress
tags: [area, technique]             # e.g. [typechecker, interning]
started: YYYY-MM-DD
closed: YYYY-MM-DD | null
---
```

Body sections:

- **Hypothesis** (expanded from the frontmatter line).
- **Scope** — what's in, what's out.
- **Links** — worktree, related commits, pointers to `TASK.md`/`HANDOFF.md`.

### `TASK.md`

Detailed implementation plan. Same convention as
`/workspace/p/tc-queries/TASK.md`. Covers goal, background, proposed
approach, key files, things to measure, and known risks. Kept mostly
stable once the experiment starts — updates happen in `HANDOFF.md`.

### `HANDOFF.md`

Live work log. What's done, what's blocked, what the current numbers
are. Rewritten as understanding shifts. This is the file a successor
should be able to read and continue from. See
`/workspace/p/tc-queries/HANDOFF.md` for the model.

### `results.md`

Structured results table. One row per (scenario × run-set).

```markdown
| Date       | Scenario | Baseline SHA | Head SHA | Base (s) | Head (s) | Δ       | Notes                          |
| ---------- | -------- | ------------ | -------- | -------- | -------- | ------- | ------------------------------ |
| 2026-04-15 | full     | 3fcac773     | abc1234  | 73.4     | 70.2     | -4.4%   | median of 4, warm-up discarded |
| 2026-04-15 | nochange | 3fcac773     | abc1234  | 1.1      | 1.1      | 0%      |                                |
| 2026-04-15 | prelude  | 3fcac773     | abc1234  | 2.3      | 2.2      | -4.3%   |                                |
| 2026-04-15 | leaf     | 3fcac773     | abc1234  | 1.2      | 1.2      | 0%      |                                |
```

Append-only. Never rewrite historical rows — if a measurement is wrong,
add a new row and note it.

## Optional files

### `profiles/` (gitignored binaries, tracked sidecars)

Raw `.prof`/`.hp` files are too large to commit. When a profile is
saved, drop a sibling `<name>.meta.md` alongside it documenting:

- Commit SHA the profile was captured against
- Scenario (`full`, `prelude`, etc.)
- RTS flags used (usually `+RTS -p -hc -RTS`)
- Top cost centres (paste the output of `awk` over the `.prof`)
- Any one-line analysis

The `.meta.md` is committed. The `.prof`/`.hp` is not.

```
experiments/<id>/profiles/
  .gitignore              # *.prof, *.hp
  full-20260415.prof      # gitignored
  full-20260415.meta.md   # committed
```

## Naming conventions

- Experiment id: kebab-case, short, descriptive of the technique —
  not the hotspot. `qualified-compare-intern`, not `fix-names`.
- Branch name matches the experiment id.
- Worktree at `/workspace/p/<id>`.

## Lifecycle

1. **proposed** — `EXPERIMENT.md` exists with a plausible hypothesis,
   nothing built yet.
2. **in-progress** — worktree exists, code is changing, measurements
   may be incomplete.
3. **blocked** — known problem prevents progress; `HANDOFF.md` names
   the blocker.
4. **shipped** — merged to `restaumatic` (or the appropriate target
   branch). Verdict `win` or `partial`.
5. **abandoned** — not shipping. Verdict `no-win` or `abandoned`.

When an experiment leaves the active set (shipped or abandoned), the
author adds a one-paragraph entry to `experiments/LESSONS.md` capturing
the transferable learning — especially for dead ends, so the technique
isn't re-attempted by accident.
