# Handoff: ptr-eq-unify

## TL;DR

**Partial win on `full` (-7.5%); regresses prelude (+4.8%) and leaf (+3.4%).**
The `reallyUnsafePtrEquality#` fast-path at the top of `unifyTypes` is
sound and produces a meaningful speedup on raw-throughput workloads,
but adds work on the cascade-rebuild scenarios. Same shape as
`skip-redundant-funapp-unify` (eqType upstream skip: prelude +6.4%) —
weaker regression, plus a real win on full instead of neutral.

| Scenario | Δ | Notes |
|---|---:|---|
| full | **-7.5%** | 53.1s → 49.1s |
| nochange | -3.6% | small win |
| prelude | **+4.8%** | regresses |
| leaf | +3.4% | regresses |

Binary 49,110,144 vs baseline 49,106,176 — +4 KB, no inlining shift.
Tests pass (1340/1340).

## The mechanism (best current guess)

Why full wins but prelude regresses on a change that, by hypothesis,
should be either neutral or strictly positive (ptr-eq skip is a pure
fast-path; fall-through preserves cache behaviour):

- **Full** is dominated by original work — per-call wrapper overhead
  (`substituteType`, `withErrorMessageHint`, cache lookup) accumulates
  across millions of `unifyTypes` calls. The ptr-eq fast-path skips
  all of that on trivially-equal pairs, where most of the 174k cache
  hits at `funAppHead` go.

- **Prelude / leaf cascade rebuilds** typecheck modules against a
  long-lived substitution + cache. They re-encounter the same pairs
  many times. Hypothesis: even though ptr-eq doesn't bypass cache
  insertion when ptrs differ, it ALSO doesn't insert when ptrs match.
  In the cascade, the first encounter ptr-matches and skips
  insertion. The second encounter (a different allocation of the
  same constants — e.g. via `substituteType` which can return
  ptr-different but structurally-equal results) misses the cache,
  runs unifyTypes', and inserts. With baseline, the first encounter
  inserts, all subsequent encounters hit cache.

  The net is: ptr-eq saves work on the dominant ptr-equal calls but
  shifts some work to cache-miss handling on cascade re-uses.

## What's left

- Branch `ptr-eq-unify` (commit b9fcf10c) — single-line change.
- Worktree at `/workspace/p/ptr-eq-unify`.
- Verdict: **partial win**. The +7.5% on full is real and valuable;
  the +4.8% prelude regression is an obstacle to shipping unmodified.

## Possible follow-ups

1. **Insert into cache even on ptr-eq match.** Keep the ptr-eq
   short-circuit (skip substituteType + hint stack + the `unifyTypes'`
   recursion), but still update the cache. This preserves cascade
   memoisation for fellow structurally-equal-but-different-allocation
   callers. Costs the cache write (StateT modify) but skips
   substituteType etc.

2. **Combine with `tfHasUnknowns` short-circuit on `substituteType`.**
   Most of the wrapper cost is `substituteType` walking closed types.
   If we add a `tfHasUnknowns` flag bit (next to `tfHasWildcards`) and
   short-circuit `substituteType` when both args have no unknowns, the
   benefit should generalise: cascade calls also skip the walk.

3. **Audit which prelude calls are not ptr-equal but should be.**
   Profiling could reveal if there's a specific construction site
   where `tyFunction` gets re-allocated (via `substituteType` returning
   a structurally-equal-but-fresh node) — fixing that one allocation
   site might let prelude benefit too.

(1) is the cleanest next step — it tests directly whether the prelude
regression is from missed cache writes.
