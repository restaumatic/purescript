---
id: entailment-memo
status: in-progress
verdict: tbd
branch: entailment-memo
worktree: /workspace/p/entailment-memo
baseline_sha: ebb0a6bb
head_sha: 953c9149
hypothesis: >
  Memoizing solved entailment constraints within a module's typecheck
  avoids redundant O(n) row-alignment work when the same constraint
  (e.g. HasField "views" Translations) is solved repeatedly.
  pr-admin has 87% redundancy on wide-row HasField calls — expect
  ~15-20s saving on a 170s build.
headline_delta: "-15.5% full, 0% nochange/prelude/leaf"
tags: [entailment, memoization, hasfield, row-types]
started: 2026-04-16
closed: null
---

# entailment-memo

## Hypothesis

The entailment solver re-solves identical constraints many times within
a module. For cheap constraints (IsSymbol, Bind, etc.) this is
harmless. But for HasField on wide rows (e.g. the 667-field
Translations type), each solve costs 8-20ms due to O(n) row alignment
in `alignRowsWith`. pr-admin modules repeat the same top-level
Translations HasField lookup up to 87 times per module.

A within-module memoization of solved constraints — keyed on
(ClassName, ground types) — should eliminate the redundant work.
Unlike the tc-queries caching (which serialized to disk), this is
an in-memory Map lookup costing microseconds vs the 8-20ms it skips.

## Scope

**In:** Memo table in CheckState, populated when a constraint is
solved with fully-ground types, consulted before instance search.

**Out:** Cross-module caching, serialization, any changes to row
representation or HasField instance logic.

## Links

- Worktree: /workspace/p/entailment-memo
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
