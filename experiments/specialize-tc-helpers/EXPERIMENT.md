---
id: specialize-tc-helpers
status: proposed
verdict: tbd
branch: specialize-tc-helpers
worktree: /workspace/p/specialize-tc-helpers
baseline_sha: 6e04203c
head_sha: 7fe25412
hypothesis: >
  Add SPECIALIZE pragmas for TypeCheckM to four hot-path polymorphic
  helpers (withErrorMessageHint, guardWith, rethrow,
  rethrowWithPosition). Without them, every call goes through method
  dictionaries; the typechecker only ever uses TypeCheckM, so the
  polymorphism is paying for nothing. Lossless win expected if GHC
  isn't already specialising; null result if it is.
headline_delta: tbd
tags: [typechecker, specialization, ghc-pragmas, characterization]
started: 2026-04-30
closed: null
---

# specialize-tc-helpers

## Hypothesis

Four polymorphic helpers are called on every recursive descent of the
typechecker:

| Helper                      | Defined in       | Constraint                                       |
| --------------------------- | ---------------- | ------------------------------------------------ |
| `withErrorMessageHint`      | Monad.hs:183     | (MonadState CheckState, MonadError MultipleErrors) |
| `guardWith`                 | Monad.hs:352     | MonadError MultipleErrors                        |
| `rethrow`                   | Errors.hs:2010   | MonadError e                                     |
| `rethrowWithPosition`       | Errors.hs:2017   | MonadError MultipleErrors                        |

None has an `INLINE` or `SPECIALIZE` pragma. The typechecker only
calls them at `TypeCheckM`, so without specialisation every call
goes through method dictionaries (`withErrorMessageHint
@TypeCheckM $dMonadState $dMonadError ...`). Adding `SPECIALIZE`
pragmas tells GHC to emit a `TypeCheckM`-specialised version with
no dictionary args, which both cuts dispatch overhead and unlocks
further inlining at the call site.

This experiment adds SPECIALIZE pragmas in `Monad.hs` (TypeCheckM in
scope; cross-module SPECIALIZE for the two functions defined in
`Errors.hs`). Lossless — pure performance experiment.

**Decision criteria:**
- **Δfull ≤ -2%, neutral incremental:** GHC was *not* specialising;
  the dict cost is real. Ship the pragmas. Consider extending to
  Tier 2 helpers (lookupUnkName, insertUnkName, warnAndRethrow*).
- **-2% < Δfull ≤ -0.5%:** marginal; ship anyway since the change is
  4 lines and lossless.
- **|Δfull| < 0.5%:** GHC was already specialising at the call sites
  via inlining heuristics. The polymorphism is free. Close as
  no-win — but the Core dump comparison is the actual signal here,
  not the timing.

## Core dump as primary signal

Timing on a 4-pragma change can easily be in noise. The clearer
signal is the Core dump. Build with
`-ddump-simpl -ddump-to-file -dsuppress-uniques -dsuppress-coercions`
on `Unify.hs` / `Types.hs` / `Subsumption.hs` before and after, and
diff:

- **Before:** call sites in `unifyTypes` show
  `withErrorMessageHint @TypeCheckM $dMonadState $dMonadError ...`.
- **After (good outcome):** either
  `withErrorMessageHint_$sTypeCheckM ...` (specialised name, no
  dicts) or fully inlined into `unifyTypes`'s body.
- **After (no-op outcome):** call shape unchanged — GHC was already
  specialising via the SimplCore worker/wrapper pass; SPECIALIZE was
  redundant.

The Core dump diff also lets us cross-validate the result of
`noop-error-hint` (predecessor): if SPECIALIZE recovers most of the
noop's gain, the cost was dict indirection. If it doesn't, the cost
is in the bracket logic (modify+rethrow+modify) and the right fix is
a different data structure (e.g., reader-style hint stack).

## Scope

**In:**
- SPECIALIZE pragmas for the four Tier-1 helpers (commit 7fe25412).
- All four scenarios.
- Core dump comparison vs `restaumatic@6e04203c` for `Unify.hs`,
  `Types.hs`, `Subsumption.hs` — does the call shape change?

**Out:**
- Tier-2 helpers (`lookupUnkName`, `insertUnkName`, `warnAndRethrow*`,
  `newDictionaries`) — split into a follow-up if Tier 1 wins.
- Restructuring the bracket itself (different data structure) — a
  much bigger change, separate experiment if SPECIALIZE doesn't
  recover noop's delta.
- Specialising traversals (`everywhereOnTypesM` already has INLINE).

**Sequencing:**
- Run *after* `noop-error-hint` lands. The two results are
  complementary: noop = upper bound on what specialising can buy;
  SPECIALIZE = how much of that upper bound this approach actually
  recovers.

## Links

- Worktree: /workspace/p/specialize-tc-helpers
- Patch: commit `7fe25412` on `specialize-tc-helpers`
- Predecessor: [noop-error-hint/EXPERIMENT.md](../noop-error-hint/EXPERIMENT.md)
  (measures the *upper bound*; this experiment asks how much GHC's
  specialiser can recover lossy-free)
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
