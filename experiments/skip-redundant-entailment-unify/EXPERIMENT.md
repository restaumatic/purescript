---
id: skip-redundant-entailment-unify
status: shipped
verdict: win
branch: skip-redundant-entailment-unify
worktree: /workspace/p/skip-redundant-entailment-unify
baseline_sha: ebb0a6bb
head_sha: acecb0cf
hypothesis: >
  When the entailment solver enforces functional dependencies, it calls
  unifyTypes on each (inferredType, t2) pair. For HasField on wide rows
  (667-field Translations), inferredType is often structurally identical
  to t2 — but unifyTypes still dispatches to unifyRows, which sorts both
  sides and walks the merge-join. Guarding the call with an eqType check
  short-circuits the no-op cases without allocation.
headline_delta: "-15.5% full, ~0% nochange/prelude/leaf"
tags: [entailment, unification, rows]
started: 2026-04-16
closed: 2026-04-23
---

# skip-redundant-entailment-unify

## What shipped

A two-line change in `Entailment.hs:295` — `unless (eqType inferredType t2)`
guard in front of the existing `unifyTypes` call inside fundep enforcement.

```haskell
lift . lift $ zipWithM_ (\t1 t2 -> do
  let inferredType = replaceAllTypeVars (M.toList subst') t1
  unless (eqType inferredType t2) $
    unifyTypes inferredType t2) (tcdInstanceTypes tcd) tys''
```

Merged via PR #14 → `restaumatic` at commit `bb6850b0`. Released in
`v0.15.15-restaumatic9`.

## Hypothesis evolution

This folder was originally `entailment-memo` — the first hypothesis was a
within-module memo table keyed on (ClassName, ground types). Profiling on
pr-admin showed 87% redundancy on wide-row HasField calls and the memoization
approach was projected to save 15-20s.

While instrumenting fundep enforcement, we noticed the redundant work isn't
really about *repeated* solves — it's about *individual* solves that produce
no new information because both sides of the unification are already equal.
The simpler `eqType` guard captures this directly without any cache table,
keying logic, or memo invalidation concerns.

## Measured results

See `results.md`. Headline: -15.5% on full builds (74.1s → 62.6s), neutral
on the three incremental scenarios (nochange/prelude/leaf within noise).

## Why it works

`unifyTypes` on identical types is a no-op semantically, but `unifyRows`
still does:

1. `rowToSortedList` on both sides (O(n) walk + O(n log n) sort + allocates
   `[RowListItem]` lists)
2. Merge-join on the sorted lists
3. Recursive `unifyTypes` on each aligned field

`eqType` walks both type trees in lockstep with no allocation, returning
False on the first mismatch. For the wide-row HasField case, inferredType
and t2 are usually identical (the substitution maps each TypeVar back to
the constraint type it came from), so `eqType` is the cheap path.

## Links

- PR: https://github.com/restaumatic/purescript/pull/14
- Worktree: /workspace/p/skip-redundant-entailment-unify
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
