# Task: row-cons-opt

## Goal

Add a linear fast-path to `unifyRows` that avoids the O(n log n)
sort when both rows share a common label prefix. Primary target:
Row.Cons entailment (7.0s, 35.7% of entailment exclusive time).

## Background

`unifyRows` (Unify.hs:173) always calls `alignRowsWith` which calls
`rowToSortedList` (Types.hs:477) on both sides. This converts each
row's RCons chain to a list, sorts by label, then merge-joins.

In practice, PureScript rows are almost always in sorted order:
- Record type annotations list fields alphabetically
- `rowFromList` sorts entries
- The compiler normalizes rows during various passes

For Row.Cons entailment (36,532 calls on pr-admin), the left side is
always a single-entry row `RCons label ty tail`. The right side is
the goal row. Even for this trivial 1-entry left side, the right
side gets fully sorted every time.

## Approach

Add a `fastPath` that walks both RCons chains in parallel:

```haskell
unifyRows r1 r2 = fastPath r1 r2
  where
    fastPath (RCons _ l1 t1 rest1) (RCons _ l2 t2 rest2)
      | l1 == l2 = do
          withErrorMessageHint (ErrorInRowLabel l1) $ unifyTypes t1 t2
          fastPath rest1 rest2
    fastPath r1' r2' = slowPath r1' r2'

    slowPath = ... -- current sort+align implementation
```

When labels match in order: O(1) per label, total O(n).
When labels diverge: fall back to sort+align on the remaining
(shorter) rows. Worst case: same as current + small constant.

## Key files

| File | What |
|------|------|
| `src/Language/PureScript/TypeChecker/Unify.hs:173` | `unifyRows` — target |
| `src/Language/PureScript/Types.hs:477` | `rowToSortedList` — current sort |
| `src/Language/PureScript/Types.hs:491` | `alignRowsWith` — current align |
| `src/Language/PureScript/TypeChecker/Entailment.hs:705` | `solveRowCons` — main beneficiary |

## How to measure

See `experiments/CLAUDE.md` and `run-profile.sh`. Run all four
scenarios (`--scenarios all`) — an optimisation isn't a win unless
it holds on nochange and prelude too.

## Tests

```bash
stack test --fast  # all tests must pass
```

## Risks / things to watch

1. **GHC inlining regression**: Previous Unify.hs changes caused
   30-38% regressions (binary shrank 2MB, GHC made different inlining
   decisions). Monitor binary size. If regression detected, consider
   NOINLINE pragmas or moving the optimization to the call site.

2. **Correctness**: The fast path unifies in RCons-chain order vs
   sorted order. Unification is commutative so the result is the same.
   Verify with full test suite.
