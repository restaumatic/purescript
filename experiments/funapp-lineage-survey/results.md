# Results for funapp-lineage-survey

## Survey on full pr-admin compile (5713e832 type-hash baseline)

Run with `PURS_FUNAPP_LINEAGE=1` on a from-scratch full build of pr-admin
(1758 modules). Single from-scratch run; histogram dumped on shutdown.

**Total calls recorded: 214,268** across the 3 instrumented sites.

### By callsite

| Callsite        | Calls    | Distinct (h1, h2) pairs |
|-----------------|---------:|------------------------:|
| funAppHead      | 175,272  | **5** |
| checkAbsArrow   |  29,720  | 140 |
| checkArrayHead  |   9,276  | 63 |

### Top pair per callsite

| Callsite        | t1_hash               | t2_hash               | Count   | Share |
|-----------------|----------------------:|----------------------:|--------:|------:|
| funAppHead      | -4893469195459195356  | -4893469195459195356  | 175,268 | **99.998%** |
| checkAbsArrow   | -4893469195459195356  | -4893469195459195356  |  29,574 | 99.5% |
| checkArrayHead  |  3306836351214420920  |  3306836351214420920  |   9,207 | 99.3% |

The dominant hashes are `tyFunction` (-4893469195459195356) and `tyArray`
(3306836351214420920). t1_hash == t2_hash in every dominant row → both
arguments to `unifyTypes` are the same (or hash-equal-up-to-collision)
constant.

### Combined finding

**214,049 of 214,268 calls (99.9%) are trivially equal — same constant
on both sides.** The remaining 0.1% are calls where `t1` is a fresh
`TUnknown` (each gets a distinct hash because TUnknown salts include
the variable index) that needs to actually be solved.

### What this means

The pattern `TypeApp _ (TypeApp _ x argTy) retTy` at these 3 sites
matches **structurally** — it doesn't statically prove that `x` is
`tyFunction` / `tyArray`. So the existing code asserts the head with
`unifyTypes x tyFunction`. In 99.9% of compile-time calls, x already
**is** the constant — the type was constructed as a function/array
and flowed unchanged.

The cache catches these as hits. `skip-redundant-funapp-unify` showed
that bypassing the cache via `unless (eqType …)` regresses prelude.
But this survey shows the underlying problem more sharply: **we are
making 214k unifyTypes calls that the call-site type structure already
proves are no-ops.** The cache hides this from the throughput numbers
but it's pure waste at the call-graph level.

### Algorithmic fix path

Split each of the 3 sites into two clauses:

```haskell
-- Common case: head is exactly the expected constructor.
-- Match the constructor literally; no unifyTypes call.
checkFunctionApplication' fn (TypeApp _ (TypeApp _ (TypeConstructor _ C.Function) argTy) retTy) arg = ...

-- Rare case: head is something else (TUnknown / synonym / wildcard).
-- Fall through to the existing unifyTypes path.
checkFunctionApplication' fn (TypeApp _ (TypeApp _ tyHead argTy) retTy) arg = do
  unifyTypes tyHead tyFunction
  ...
```

This is structurally different from the `unless (eqType …)` skip:
- `eqType`-skip is a runtime function call after the outer pattern matched.
- A nested constructor pattern is a tag check fused into the same
  pattern-match dispatch GHC already generates for the outer
  `TypeApp _ (TypeApp _ x …) …`.

In Core, the eqType version expands to an `if` branch after the
case-tree; the nested-pattern version is part of the case-tree itself.
GHC may produce materially better code for the latter. (LESSONS
records that small Unify.hs/Types.hs changes can flip code-gen
significantly — this is the place to test it.)

### Next experiment

`funapp-pattern-match` — implement the two-clause pattern match at the
3 sites and benchmark all four scenarios. The hypothesis is that the
prelude regression seen in `skip-redundant-funapp-unify` and
`ptr-eq-unify` was code-gen-related, not algorithmic — and a clean
nested pattern match can avoid both the cost and the regression.
