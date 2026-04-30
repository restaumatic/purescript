---
id: unify-lazy-subst-revive
status: abandoned
verdict: partial
branch: unify-lazy-subst-revive
worktree: /workspace/p/unify-lazy-subst-revive
baseline_sha: 6e04203c
head_sha: 497d51f5
hypothesis: >
  Avoid the O(N²) blowup from substituteType being called at every level
  of unification recursion. Walk-style refactor: drop substituteType
  from unifyTypes' entry path, resolve TUnknown chains lazily via a
  one-step substLookup at the leaf, and rely on Haskell laziness to
  defer error-hint substitution to the throw path.
headline_delta: "-11.1% full, -7.0% nochange, +2.9% prelude, -0.8% leaf"
tags: [typechecker, unification, substitution, revive]
started: 2026-04-30
closed: 2026-04-30
---

# unify-lazy-subst-revive

## Hypothesis

`unifyTypes t1 t2` currently substitutes both arguments *eagerly* at
the wrapper, then dispatches to `unifyTypes'`. The structural cases
in `unifyTypes'` recurse via the wrapper (`t3 \`unifyTypes\` t5`),
which substitutes again. For an N-node type pair that hits structural
recursion all the way down, this is O(N²) substituteType work — most
of which is rewriting `TUnknown` nodes that aren't there.

Maciej's branch `origin/unify-lazy-subst` (last commit 2025-05-13)
already implements the standard walk-style fix: defer substitution
until a `TUnknown` is actually encountered during the descent. The
branch predates the `experiments/` framework, so it has no
measurement trace and was never merged. This experiment exercises
those changes under the framework.

## Scope

**In:**
- Cherry-pick the three commits from `origin/unify-lazy-subst` onto
  current `restaumatic` (`6e04203c`):
  - `5323c41e Lazy substitution in unification`
  - `dd90b8bd Remove unification cache`
  - `0a9bc189 Update some changed types in error messages`
- Resolve conflicts caused by the 11 months of intervening work
  (synonym-opt, skip-redundant-entailment-unify, etc.).
- Run all four scenarios.

**Out:**
- The leaf fast-path from PR #18 (separate experiment / direction).
- Path compression in `substLookup` (deferred follow-up).
- Adding `tfHasUnknowns` flag (already characterised as marginal).

**Combination test (conditional, Step 6 in plan):** if lazy-subst
alone is partial, layer PR #18's leaf fast-path on top and remeasure
to see if the mechanisms are additive.

## Links

- Worktree: /workspace/p/unify-lazy-subst-revive
- Reference branch: `origin/unify-lazy-subst` (don't merge directly,
  too stale)
- PR being compared against: [restaumatic/purescript#18](https://github.com/restaumatic/purescript/pull/18)
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
- Parent plan (this session):
  `/home/user/.claude/plans/consider-this-experiment-https-github-co-stateful-graham.md`
