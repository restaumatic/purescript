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

### 4. row-unify-as-join — Principled row alignment using join-optimisation techniques

**Impact:** Open. Row-cons-opt's ad-hoc fast-paths landed at -1.7% to -3.5% full
in their best-behaved variant; a more principled cost model could plausibly do
better, but GHC inlining-cliff in Unify.hs caps how aggressively `unifyRows`
can be restructured.
**Risk:** Medium-high. The Unify.hs inlining cliff (LESSONS.md "row-cons-opt"
HANDOFF; +185% prelude on the second helper-function attempt) means any
non-byte-identical-binary variant has to be measured carefully on all four
scenarios.
**Effort:** Medium-large.

Row alignment is a JOIN over `(Label, FieldType)` pairs. Both sides are
RCons chains, possibly with TUnknown tails. The current `alignRowsWith`
unconditionally sorts both sides and merges — this is sort-merge join, fine
when both sides are large and unsorted, suboptimal in two common cases:

1. **One side is tiny (1-3 fields):** nested-loop join is cheaper.
   Row.Cons constraints feed `unifyRows` exactly this shape: a single-entry
   row vs a wide record. `row-cons-opt`'s `removeLabel` did this for the
   single-entry case but couldn't scale to 2-3-entry cases without tripping
   the inlining cliff.
2. **Both sides already in source order with mostly-aligned labels:**
   linear merge without sort. `row-cons-opt`'s parallel-walk fast-path is
   this — the only variant that landed clean (-3.5 % full, byte-identical
   binary).

A proper cost model picks among **nested-loop / sort-merge / parallel-merge**
based on size estimates and (possibly) flags like "is this row sorted at
construction." Things to think about:

- **Build vs probe side selection:** for nested-loop, the smaller side is
  the probe driver. RCons chain length is O(1) to compute (or could be cached
  in TypeFlags as a small `Word8` with a saturating bound).
- **Already-sorted detection:** PureScript rows are typically in *source*
  order, not sorted. But within entailment-generated unifications, both
  sides come from the same construction site and may be in matching order.
  A "ordered prefix length" cache or a "sorted at construction" flag could
  short-circuit the sort.
- **Hash join:** probably overkill — labels are PSStrings, hashing is not free,
  and rows past ~20 fields are uncommon outside the 667-field Translations
  record. But worth measuring once.
- **Cost model thresholds:** would need to be empirically calibrated on
  pr-admin's actual row-size distribution. `unify-callsite-survey` didn't
  break this down by row size; would want a similar survey first.

**Prerequisite reading:** `row-cons-opt/HANDOFF.md` (especially the "Key
architecture insight" section: Unify.hs is dangerous for any non-trivial
restructure). The byte-identical-binary path is the only one that survived
GHC -O2 — any new variant needs a binary-size diff as a Stage-1 gate before
trusting timing numbers.

**Why this isn't (yet) #3:** `optimize-row-unify` (#3) proposes changing the
*representation* (RCons chain → Map). This entry is about better *algorithms*
on the existing representation. Cheaper to attempt; no representation
churn. Could compose with #3 if both pan out.

### 5. unify-coredump-survey — Compare GHC Core for `unifyTypes` across branches

**Impact:** Diagnostic, not a perf change directly. Goal: replace
hand-wavy "Unify.hs inlining cliff" / "byte-identical binary" framing
(used in `row-cons-opt/HANDOFF.md` and reused in `unify-lazy-subst-
revive`) with hard evidence.
**Risk:** None — read-only analysis.
**Effort:** Small-medium.

Build with `-ddump-simpl -ddump-to-file -dsuppress-uniques
-dsuppress-coercions -dsuppress-idinfo=False` on each of:

- `restaumatic` (current baseline, has `unificationCache`)
- `unify-lazy-subst-revive` (lazy-subst, no cache, -11.1% full)
- `origin/row-cons-opt` (parallel-walk fast-path, claimed
  byte-identical binary at -3.5% full)
- *optional:* a worktree with PR #18's leaf fast-path applied
  on top of `restaumatic` (5-clause patch)

Diff the `Unfolding` block (size + body) for `unifyTypes`,
`unifyTypes'`, `unifyRows`, and `solveType` between branches.

**Questions to answer:**
1. Is `row-cons-opt`'s binary actually byte-identical at the Core
   level, or did the commit message ("+8KB") catch a real difference
   that the HANDOFF then under-reported?
2. Does lazy-subst's unfolding size cross a GHC inliner threshold
   (`-funfolding-use-threshold`)? The +20 MB binary delta on clean
   rebuild suggests yes — but the perf numbers suggest the cliff is
   a non-issue, contradicting the "size delta is bad" framing.
3. For PR #18's leaf fast-path: do the 5 special-case clauses get
   inlined at the recursive call sites inside `unifyTypes'`, or do
   they live in the out-of-line body? This is the GHC-side answer to
   "why does PR #18 reach -18.6% on full when its mechanism is
   structurally simple."

**Why this is in the queue:** the `unify-lazy-subst-revive` HANDOFF
explicitly flags loose vocabulary borrowed from `row-cons-opt`; before
the next round of Unify.hs-touching experiments, we want the actual
Core-level picture to ground decisions.

**Prerequisite reading:** `row-cons-opt/HANDOFF.md` (the source of the
"byte-identical" / "inlining cliff" claims), `unify-lazy-subst-revive/
HANDOFF.md` (the methodological note), and `LESSONS.md:430-455` (the
already-known "binary size mismatch is a clue, not a verdict" lesson
from incremental-build contamination).

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
