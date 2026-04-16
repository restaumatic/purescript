# Task: entailment-memo

## Goal

Reduce redundant entailment solving within a module by memoizing
constraints that have been solved with fully-ground types. Primary
target: HasField on wide row types (Translations: 667 fields, 8-20ms
per solve, 87% redundancy across pr-admin modules).

## Background

The entailment solver (`Entailment.hs:solve/go`) resolves each
constraint independently. When a module accesses `t.views.foo` 80
times, `HasField "views" Translations` is solved 80 times — each
doing full instance search, `matches` with `alignRowsWith` (O(n)
row alignment on 667-field row), `withFreshTypes`, `unifyTypes`, and
`solveSubgoals`.

From eventlog profiling on pr-admin:
- HasField self-time: 16.6s (50.7% of entailment)
- Row.Cons self-time: 6.9s (21.0%)
- Total entailment: 96.3s inclusive, 32.7s exclusive
- 87% of top-level Translations HasField calls are redundant

LESSONS.md says "don't cache cheap work" — but this work is NOT
cheap (8-20ms per call). The tc-queries experiment failed because it
serialized to disk; this is in-memory only.

## Approach

1. Add `checkEntailmentMemo :: Map (Qualified (ProperName 'ClassName), [SourceType]) Expr`
   to `CheckState` in `TypeChecker/Monad.hs`.

2. In `Entailment.hs:go`, after substituting types:
   - Compute memo key = (className', kinds'' ++ tys'')
   - If key has no TUnknowns (fully ground), check memo
   - On cache hit: return cached Expr, skip instance search
   - On Solved: store result in memo before returning

3. Ground-check: only memo when all types in the key are ground
   (no TUnknown, no Skolem). If any are present, the constraint
   could resolve differently after unification.

4. Clear memo between modules (it's per-module state).

## Key files

- `src/Language/PureScript/TypeChecker/Monad.hs` — add memo field to CheckState
- `src/Language/PureScript/TypeChecker/Entailment.hs` — memo lookup/store in `go`

## How to measure

Run all four scenarios against pr-admin:
```bash
experiments/scripts/exp run entailment-memo --scenarios all --runs 5
```

## Tests

```bash
stack test --fast  # all tests must pass
```

## Risks / things to watch

- **Correctness**: memo key must be on fully-substituted, ground types
  only. TUnknown in the key means the constraint could resolve
  differently after unification fills in the unknown.
- **Memory**: the memo Map grows per-module. For pr-admin's largest
  modules (~3000 HasField calls), this is ~3000 Map entries — trivial.
- **Side effects**: `solve` both returns an Expr and performs
  unifications via `unifyTypes`. The memo caches the Expr but not the
  unification side effects. This is OK because for ground types, the
  unifications are deterministic (same types → same unification result).
  But we need to verify that replaying the unifications isn't needed.
  Actually — if all types are ground, the unifications are no-ops
  (ground type unified with itself). So the side effects don't matter.
