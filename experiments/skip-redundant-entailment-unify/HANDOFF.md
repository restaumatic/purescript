# Handoff: entailment-memo

## TL;DR

Single two-line change in Entailment.hs gives -15.5% on full builds
(74s → 63s). All 1340 tests pass. Other scenarios (nochange, prelude,
leaf) unchanged. Ready for review and merge.

## What's done

- Identified root cause: entailment fundep enforcement at line 327-329
  of Entailment.hs calls `unifyTypes inferredType t2` where both sides
  are often structurally identical (the inferred type IS the constraint
  type after substitution). For row types like the 667-field
  Translations record, this triggers O(n) row alignment via
  `alignRowsWith` + `rowToSortedList` for no semantic benefit.

- Added `unless (eqType inferredType t2)` guard before the
  `unifyTypes` call. `eqType` is O(n) but much cheaper than the full
  `unifyRows` path (no sorting, no recursive unification of field
  types, no cache insertion).

- Important lesson: modifying Unify.hs (the hot path) caused -30%
  regression even with seemingly beneficial changes, because GHC -O2
  recompiled the module with different inlining decisions. The fix was
  to make the change in Entailment.hs (the call site) instead.

## What's blocked

Nothing.

## Next steps

- Run the formal `exp run` with all 4 scenarios and 5 runs each
- Merge to `restaumatic` branch if numbers hold
- The remaining HasField cost (~7s remaining after this fix) could be
  further reduced by a within-module entailment memo, but that's more
  complex and may not be worth it given the current improvement.
