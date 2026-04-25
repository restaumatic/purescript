---
id: row-cons-opt
status: in-progress
verdict: tbd
branch: row-cons-opt
worktree: /workspace/p/row-cons-opt
baseline_sha: e0125163
head_sha: 6acbfe02
hypothesis: >
  Row unification always sorts both sides (O(n log n)) before merge-joining,
  even when the RCons chains already share a common label prefix. A linear
  fast-path that walks both chains in parallel and falls back to sort+align
  only on mismatch would reduce Row.Cons entailment cost (currently #1 hotspot
  at 7.0s / 35.7% of entailment exclusive time) and all other row unification.
headline_delta: "-2.2% full, neutral others"
tags: [unification, rows, entailment]
started: 2026-04-17
closed: null
---

# row-cons-opt

## Hypothesis

`unifyRows` calls `alignRowsWith` which calls `rowToSortedList` on both
sides — O(n log n) sort + O(n) merge. For the vast majority of row
unification calls, the two rows already have the same label order (both
constructed from sorted sources, or one is a single-entry RCons from
Row.Cons). A linear fast-path that walks both RCons chains in parallel,
unifying field types as labels match, and falls back to the current
sort+align only when a label mismatch is detected, would:

1. Make identical-structure rows O(n) instead of O(n log n)
2. Reduce Row.Cons cost (single-entry left vs large right: if first
   label matches, immediate unify + tail recurse with no sort)
3. Benefit all row unification, not just entailment

On pr-admin with 667-field Translations records, each Row.Cons call
currently sorts 667 labels. With 36,532 Row.Cons calls, the sorting
overhead is substantial.

## Scope

**In:** Fast-path for `unifyRows` in Unify.hs.
**Out:** Changes to entailment solver, row representation, `alignRowsWith`.

**Risk:** Previous Unify.hs modifications caused 30-38% regressions
due to GHC inlining changes. Must check binary size as indicator.

## Links

- Worktree: /workspace/p/row-cons-opt
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
