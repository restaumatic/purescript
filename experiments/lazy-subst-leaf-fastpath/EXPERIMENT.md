---
id: lazy-subst-leaf-fastpath
status: abandoned
verdict: no-win
branch: lazy-subst-leaf-fastpath
worktree: /workspace/p/lazy-subst-leaf-fastpath
baseline_sha: 6e04203c
head_sha: febbdb09
hypothesis: >
  Layer PR #18's 5-clause leaf fast-path on top of lazy-subst and
  measure additivity. Lazy-subst attacks substituteType blowup during
  recursion (-11.1% full standalone). PR #18 attacks the per-call
  wrapper overhead for trivially-equal leaf pairs (-18.6% full
  reported standalone, on a different baseline). The two mechanisms
  target different costs and should compose; if not, that tells us
  the costs overlap more than expected.
headline_delta: "-12.4% full (≈ lazy-subst alone -11.1%); leaf fast-path adds ~zero on top"
tags: [typechecker, unification, substitution, leaf-fast-path, combo]
started: 2026-04-30
closed: 2026-04-30
---

# lazy-subst-leaf-fastpath

## Hypothesis

`unify-lazy-subst-revive` (497d51f5) lands at -11.1% full standalone
against `restaumatic@6e04203c`. PR #18 (`origin/unify-leaf-no-hash`,
35ad956a) is reported at -18.6% full standalone against pre-type-hash
baseline 799e8208. The two changes attack different costs:

- **Lazy-subst** removes the O(N²) `substituteType` blowup during
  recursive descent — the recursive `unifyTypes` calls inside
  `unifyTypes'` no longer eagerly substitute, and the
  `ErrorUnifyingTypes` hint argument is forced only on the throw path.
- **PR #18 leaf fast-path** short-circuits 5 trivially-equal leaf
  pairs (TypeConstructor / TypeVar / TypeLevelString / TypeLevelInt /
  Skolem) before *any* wrapper work — no `gets`, no
  `withErrorMessageHint` bracket, no `substituteType` call.

These should compose: lazy-subst makes the recursive descent cheaper
across the board; PR #18 cuts the recursive descent off entirely for
the equal-leaf case, which `unify-callsite-survey` showed is 86% of
all cache hits.

**If they're additive** (Δ ≈ -11.1% + ≈ -18.6%, modulo the obvious
cap that we can't beat zero entailment cost), ship the combo. **If
combo ≈ -18.6%** (PR #18 alone), then the leaf fast-path already
absorbs the lazy-subst gain and the simpler patch wins. **If combo
≈ -11.1%** (lazy-subst alone), PR #18's mechanism doesn't compose
cleanly with the walk-style refactor — surprising, would need
investigation.

## Scope

**In:**
- Cherry-pick PR #18's leaf fast-path patch (the 5 clauses) onto the
  `unify-lazy-subst-revive` tip. The cache drop and `Data.Set` /
  `when` import cleanup are already in lazy-subst (via dd90b8bd), so
  this reduces to a 5-line addition before the catchall `unifyTypes
  t1 t2 = do`.
- Run all four scenarios.
- Compare directly to lazy-subst's headline (-11.1% full) and
  PR #18's reported headline (-18.6% full) — same `restaumatic@6e04203c`
  production baseline as lazy-subst was measured against.

**Out:**
- Path compression in `substLookup` (deferred follow-up regardless of
  outcome here).
- Removal of the unused `unificationCache` field from `CheckState`
  (cleanup commit, separate concern).
- The Core-dump survey (queue entry #5) — independent diagnostic.

## Links

- Worktree: /workspace/p/lazy-subst-leaf-fastpath
- Branch: `lazy-subst-leaf-fastpath` off `unify-lazy-subst-revive@497d51f5`
- Reference for the leaf fast-path: `origin/unify-leaf-no-hash`
  commit `e3425f4d` (5 clauses, drops cache+imports we already have)
- Predecessor: [unify-lazy-subst-revive/EXPERIMENT.md](../unify-lazy-subst-revive/EXPERIMENT.md)
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
