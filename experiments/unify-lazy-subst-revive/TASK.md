# Task: unify-lazy-subst-revive

## Goal

Eliminate the O(N²) substituteType-during-unification blowup by
deferring substitution until a `TUnknown` is encountered, and by
relying on the laziness of `withErrorMessageHint`'s hint argument so
the success path never substitutes at all.

## Background

Current shape (`Unify.hs:115-118`):

```haskell
unifyTypes t1 t2 = do
  sub <- gets checkSubstitution
  withErrorMessageHint (ErrorUnifyingTypes t1 t2) $
    unifyTypes'' (substituteType sub t1) (substituteType sub t2)
```

`unifyTypes'` then recurses via `t3 \`unifyTypes\` t5`, re-entering
the wrapper, which re-fetches the substitution and substitutes both
sides again. For an N-node type pair, that's O(N²) substituteType
work plus a per-call `gets` and hint bracket.

PR #18's leaf fast-path patches around the *symptoms* of this: equal
leaves at the bottom of recursion get caught before the wrapper runs.
This experiment attacks the cause — the substituteType pass itself.

## Approach

Cherry-pick from `origin/unify-lazy-subst` (last touched 2025-05-13,
3 commits off `c4923852`):

1. **`5323c41e Lazy substitution in unification`**
   - Drop `substituteType` from the entry of `unifyTypes`. Pass `t1`
     and `t2` raw to `unifyTypes'`.
   - Add `substLookup :: Int -> Substitution -> Maybe SourceType` —
     one-step unknown-resolution, treats `TUnknown ann u → TUnknown _ u`
     as unbound.
   - In `unifyTypes' (TUnknown _ u) t2'`: look up `u`. If unbound,
     `solveType u (substituteType sub t2')`. If bound to `t`, recurse
     `unifyTypes t t2'` (chain-following).
   - Mirror for `unifyTypes' t1' (TUnknown _ u)`.
   - Place `substituteType` only inside the lazy `ErrorUnifyingTypes
     (substituteType sub t1) (substituteType sub t2)` argument to
     `withErrorMessageHint` — forced only on throw.
   - Replace `unifyRows` row-pair-up with a TypeCheckM-flavoured
     `alignRowsWithM` + `rowToSortedListM` so row tails (which can be
     `TUnknown`) get walked using the live substitution.

2. **`dd90b8bd Remove unification cache`** — once substitution is
   off the hot path, the cache's reason for existing (avoiding
   resubstitution-and-comparison of the same type pair) is gone.

3. **`0a9bc189 Update some changed types in error messages`** —
   touch-up to keep error messages compiling against the new shape.

Conflict expectation: both the lazy-subst branch and current
`restaumatic` modify Unify.hs heavily. Likely strategy is to abandon
the cherry-picks at the first non-trivial conflict and re-apply the
design by hand, using `git show 5323c41e:src/.../Unify.hs` as the
reference.

## Key files

- `src/Language/PureScript/TypeChecker/Unify.hs` — the rewrite.
- `src/Language/PureScript/TypeChecker/Monad.hs:188-194` — confirm
  `withErrorMessageHint`'s hint argument is lazy on the success path
  (it should be — Haskell default).
- `src/Language/PureScript/Types.hs` — contains pattern synonyms
  added by synonym-opt; lazy-subst's reference doesn't have them, so
  any pattern matching needs to use the current pattern-synonym
  shapes.

## How to measure

Per `experiments/SCHEMA.md` and `CLAUDE.md`:

```sh
experiments/scripts/exp build-baseline 6e04203c   # if missing
experiments/scripts/exp run unify-lazy-subst-revive --scenarios all --runs 5
```

Discard run 1, median of remaining ≥ 4 with (min, max). Sanity check:
baseline `full` ≈ 72-73 s.

## Tests

```sh
stack test --fast   # 1340/1340 must pass
```

If tests fail: characterise. The lazy-subst design preserves
substituteType's *observable* semantics — only timing changes. A
failure is more likely a stale error-message snapshot or a row-
unification edge case from the rewritten `alignRowsWithM`. Fix it,
don't paper over with `--match` flags.

## Risks / things to watch

- **Stale base.** Lazy-subst was cut 11 months ago, before
  synonym-opt and skip-redundant-entailment-unify shipped. Code
  conflicts likely. The cache-removal commit may be partly
  obsoleted by type-hash work (no longer relevant since type-hash
  hasn't merged).
- **Chain-following recursion in `unifyTypes' (TUnknown _ u) t2'`.**
  If the substitution has long chains (`u₁ → u₂ → … → t`), each level
  re-enters `unifyTypes`. Path compression would help; out of scope
  for v1, but flag if profile shows the pattern.
- **`gets checkSubstitution` inside both TUnknown branches of
  `unifyTypes'`.** This is per-recursion. If it shows up in the
  profile, hoist to a single read at entry and pass `sub` down.
- **Row alignment correctness.** The new `alignRowsWithM`
  destructures rows lazily — it's a different shape from
  `alignRowsWith`. Verify row tests pass and the output ordering
  matches.
- **Soundness > performance.** If a perf number looks too good
  (>30% on full), assume semantic break per `LESSONS.md`. Look for
  it explicitly.
