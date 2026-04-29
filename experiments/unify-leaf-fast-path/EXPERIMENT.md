---
id: unify-leaf-fast-path
status: closed
verdict: partial
branch: unify-leaf-fast-path
worktree: /workspace/p/unify-leaf-fast-path
baseline_sha: 5713e832
head_sha: 95aadd51
hypothesis: >
  unify-cache-anatomy showed 86% of cache hits are on 1-2-node pairs
  (constructor-self recurrences from recursive descent through
  TypeApp/KindApp/RCons). Replacing those hits with pre-substitute
  pattern clauses on `unifyTypes` should eliminate the dominant
  cache work without HashSet bookkeeping. Crucially this is finer-
  grained than the call-site skips that all regressed prelude by
  ~7% — those bypassed the wrapper for whole sites regardless of
  leaf-equality; this only triggers on trivially-equal leaves
  regardless of call site, so plausibly avoids the structural
  regression mechanism.
headline_delta: |
  Phase 1 (leaf fast-path alone): full +0.4%, nochange -1.2%,
  prelude -0.9%, leaf +2.2% — neutral.
  Phase 2 (+ cache dropped): full -0.3%, nochange -5.5%,
  prelude +3.4%, leaf +2.5% — leaf fast-path absorbs ~all of
  cache's full-build value (vs +24% from naked cache drop);
  residual cache value is prelude-cascade amortisation only.
tags: [unification, fast-path, leaf, caching]
started: 2026-04-29
closed: 2026-04-29
---

# unify-leaf-fast-path

## Hypothesis

The unification cache earns its keep by intercepting recursive
descents on trivially-equal leaf pairs (TypeConstructor c ~ TypeConstructor c,
TypeVar v ~ TypeVar v, etc.) — the survey at `unify-cache-anatomy`
quantified this at 86% of all cache hits.

Adding pre-substitute pattern clauses to `unifyTypes` for these
cases short-circuits before reaching the `substituteType` /
`withErrorMessageHint` / `HS.member` machinery. The HashSet cache
remains in place for non-leaf pairs.

Three prior call-site-skip experiments (skip-redundant-funapp-unify,
unify-pattern-survey Phase 2, funapp-pattern-match) all regressed
prelude by ~+7% with three different mechanisms — pointing to a
structural regression mechanism around skipping the wrapper for
whole call sites. This experiment doesn't bypass the wrapper for
sites; it bypasses *only* for trivially-equal leaves, regardless
of site. Different shape — should avoid the regression.

## Scope

**In.** Add five top-level pattern clauses to `unifyTypes` for
TypeConstructor / TypeVar / TypeLevelString / TypeLevelInt / Skolem
self-equality. Keep the cache and the existing `unifyTypes'`
clauses (mismatch errors still flow through them).

**Out.** Touching the cache HashSet itself (size-thresholded cache,
ring buffer) — that's a follow-up if this wins. Adding a TypeApp
fast-path (would require expensive recursive eqType).

## Falsification criterion

If `prelude` regresses by ≥ +3%, the structural regression mechanism
is also triggered by leaf fast-paths — the wrapper-skipping
hypothesis was wrong, and the regression is something else (probably
GC residency from the cache HashSet itself, or substituteType
allocation patterns). Close as `no-win` and pivot to surveying
HashSet allocation behaviour directly.

If all four scenarios are within ±1% of baseline, the leaf hits
weren't load-bearing on time even if they were 86% of cache work —
close as `no-win` characterizing that cache work is not on critical
path.

If `full` improves by ≥1.5% with neutral or positive `prelude`,
ship.

## Links

- Worktree: /workspace/p/unify-leaf-fast-path
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
