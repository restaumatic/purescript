# Experiment queue

Candidates identified from per-declaration profiling of pr-admin.
`Data.Record.HasField` accounts for **81.7% of entailment time** (35s of 43s,
39,219 constraint resolutions). This is the dominant bottleneck.

## Queue

### 1. bypass-hasfield — Remove HasField desugaring for record access

**Impact:** ~35s elimination (potentially ~50% full-build speedup)
**Risk:** Low — only affects code with custom `HasField` instances on non-Record types
**Effort:** Small

The restaumatic fork's `Sugar/Accessor.hs` desugars every `record.field` into
`getField (Proxy :: Proxy "field") record`, forcing the typechecker through
`HasField` → `Row.Cons` → entailment solver. But the native typechecker
already handles `Accessor` directly via unification (TypeChecker/Types.hs:456),
which is much cheaper — no type class resolution needed.

**Change:** Disable `desugarAccessorModule` in the sugar pipeline. The native
`Accessor` path in `infer'`/`check'` handles it directly.

**What to verify:**
- Full build time (expect ~35-40s, down from ~72s)
- All four scenarios (full, nochange, prelude, leaf)
- Tests pass
- Check if any code relies on custom `HasField` instances

### 2. fast-path-hasfield — Short-circuit HasField in entailment solver

**Impact:** ~35s reduction if experiment 1 isn't viable
**Risk:** Low
**Effort:** Medium

If removing the desugaring entirely isn't possible (e.g., custom `HasField`
instances exist in the codebase), add a fast path in the entailment solver:
when solving `HasField label a (Record r)` where `r` is a known concrete row,
directly look up the field without going through the full `Row.Cons` → unify
cycle.

**Prerequisite:** Only needed if experiment 1 fails.

### 3. optimize-row-unify — Efficient row-type representation

**Impact:** Broad — benefits all row operations (RowCons, RowUnion, RowToList, etc.)
**Risk:** Medium — changes core type representation
**Effort:** Large

Currently row types are `RCons label type rest` chains — field lookup is O(n).
An internal `Map Label Type` representation would make it O(log n). This would
benefit the 39k HasField calls, the 2k Union calls, and all other row
operations.

**Prerequisite:** Profile first with experiment 1 applied to see if row
unification is still a bottleneck after removing the HasField overhead.

## Discovered from profiling

Source: eventlog run on pr-admin, 2026-04-16, commit on `experiments` branch.

### Entailment time by class (top 10)

| Class                              | Time   | Count  | % of entailment |
| ---------------------------------- | ------ | ------ | --------------- |
| Data.Record.HasField               | 35.0s  | 39,219 | 81.7%           |
| Restaumatic.Form.Internal.Initialize | 2.0s | 372    | 4.6%            |
| Restaumatic.Form.Query.Query       | 0.8s   | 3,597  | 1.8%            |
| Restaumatic.Form.Internal.Merge    | 0.5s   | 1,523  | 1.1%            |
| Restaumatic.Form.Internal.AddContext | 0.3s  | 2,439  | 0.8%            |
| Foreign.Generic.Class.GenericDecode | 0.3s   | 623    | 0.6%            |
| Foreign.Generic.Class.GenericEncode | 0.2s   | 629    | 0.6%            |
| Data.Show.Generic.GenericShow      | 0.2s   | 812    | 0.5%            |
| Data.Variant.Internal.VariantMatchCases | 0.2s | 60  | 0.4%            |
| Unscramble.Generic.GenericDecode   | 0.2s   | 601    | 0.4%            |

### Key finding

`Data.Record.HasField` is a restaumatic-prelude class
(`/workspace/restaumatic/libs/ps/restaumatic-prelude/src/Data/Record.purs`)
added for overloaded record accessors. Its single instance requires
`Row.Cons label a trash r`, which triggers expensive row-type unification
for every field access. The compiler's native `Accessor` handling
(TypeChecker/Types.hs:456) does the same job via direct unification,
bypassing the entailment solver entirely.
