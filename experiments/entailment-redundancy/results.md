# Results for entailment-redundancy

## Survey on full pr-admin compile (c84101d8 baseline, single thread)

Run with `PURS_ENTAILMENT_ANATOMY=1` on a from-scratch full build of
pr-admin (1758 modules). Hook at `Entailment.hs:259-269` in
`solve.go` records `(className, [briefType-of-arg])` per call.

**Total solves: 375,232**
**Distinct (className, fingerprint) pairs: 29,636**
**Average solves per distinct pair: 12.66× (global)**

### Solve volume concentration

The top 3 classes account for **32%** of all solves:

| Class | Solves | % of total |
|-------|-------:|-----------:|
| `Data.Record.HasField` | 42,537 | 11.34% |
| `Data.Symbol.IsSymbol` | 41,108 | 10.96% |
| `Prim.Row.Cons` | 37,226 | 9.92% |
| `Row.Extra.TestHasLabelRL` | 15,271 | 4.07% |
| `Prim.Row.Lacks` | 13,224 | 3.52% |
| `Prim.RowList.RowToList` | 10,748 | 2.86% |

These are **exactly the row-list machinery** that
`skip-redundant-entailment-unify` (-15.5% full, shipped) and
`synonym-opt` (-22.9% combined) attacked.

### Top recurring shape

`Prim.RowList.RowToList {..} ?` was solved **6,902 times** —
"reduce some record to its row-list representation" — with the
same constructor-shape on both args. That single shape is 1.8% of
all solves on its own.

Other notable recurrences:
- `HasField 'label' TranslationKey ?` × 2,172 — same field name
  and field type pattern across many records.
- `Semigroupoid Function` × 2,169 — function-composition
  dictionary lookup.
- `HeytingAlgebra Boolean` × 2,016 — Boolean dictionary lookup.

These look like dictionary-lookup recurrences where the args
collapse to small constant constructors.

### Caveat: this is GLOBAL, not per-decl

The 12.66× average occurrence-per-pair includes recurrences
**across modules**. A within-decl memo can only catch reuse
within a single decl's typecheck. Some fraction of the 12.66× is
intra-decl (same `HasField 'foo' {…} ?` solved 50 times within
one decl with a 50-field record), some fraction is inter-module
(same dictionary looked up in many modules independently).

To know how much is within-decl, the survey needs per-decl
attribution.

### Implications for the next experiment

**Strong signal that within-decl redundancy exists** — the
volume of `HasField` (42k), `IsSymbol` (41k), `Cons` (37k) on
1758 modules averages to 24/23/21 solves per module per class.
For the 5 most expensive decls (each spending 1.3-1.8s in solve)
the per-decl numbers are likely much higher.

The per-decl chrome trace already shows:
- `updateExternalMenuView`: 1,788 ms in solve
- `Restaurant.Settings.view`: 1,723 ms in solve
- `MenuV2.Menus.spec`: 1,584 ms in solve

Combined with this survey, the working hypothesis becomes:

> Within decls like `updateExternalMenuView`, the same row-list
> dictionary shapes (HasField/Cons/Lacks/RowToList) are solved
> repeatedly with identical args. A within-decl memo on
> `(className, normalised-args)` could short-circuit on hits with
> the previously-computed dictionary.

### Falsification of the per-decl-redundancy hypothesis

Need per-decl attribution to confirm. Two ways:

**A. Refine this survey to per-decl.** Add a current-decl IORef
in `EntailmentAnatomy`, set from a hook in `TypeChecker.hs` (where
the per-decl `traceMarker` is already wired). Records become
`(decl, className, args)`. Report top-N decls' per-decl
duplicate-rate.

**B. Just try the within-decl memo as a try-it experiment.** Add
a `Map (Qualified ClassName, [SourceType]) Expr` cleared at the
start of each value declaration's typecheck; on hit, return the
memoized expr instead of running solve. Measure all 4 scenarios.
If it wins → ship; if it regresses on prelude → understand why
(may have similar mechanism to the funapp regressions).

Recommendation: **do A first** — cheap (~1 hour), gives a clean
signal whether B is worth building. If per-decl reuse on the
top-5 decls is ≥3×, B becomes the obvious next experiment.

## Per-decl refinement (2026-05-07)

Extended the survey with a per-decl IORef set from
`TypeChecker.hs:withDeclTrace`. Same run, same totals.

### Top 30 decls by solve volume — with within-decl reuse rate

| Decl | Solves | Distinct | **Reuse** |
|---|---:|---:|---:|
| `MenuV2.Menus.spec` | 20,144 | 2,026 | **9.94×** |
| `MenuV2.Modifiers.spec` | 11,775 | 1,120 | **10.51×** |
| `Restaurant.Settings.view` | 6,778 | 608 | **11.15×** |
| `PR.GenerateTestHelpers.main` | 6,594 | 824 | 8.00× |
| `Restaurant.Settings.control` | 6,558 | 1,150 | 5.70× |
| `MenuV2.MenuSection.spec` | 6,161 | 959 | 6.42× |
| `MenuV2.Product.spec` | 5,142 | 1,178 | 4.37× |
| `MenuV2.AvailabilitySchedule.spec` | 4,803 | 483 | 9.94× |
| `MenuV2.PackagingUnit.detailsSpec` | 4,722 | 726 | 6.50× |
| `Restaurant.DeliveryZones.component` | 4,620 | 826 | 5.59× |

(Full top-30 in `anatomy-perdecl.txt`.)

### Within-decl reuse is decisive

**8 of the top-10 decls have ≥5× within-decl reuse**, three of
them >9×. The decl-level redundancy hypothesis is *confirmed* —
this is not just cross-module coincidence.

**`MenuV2.Menus.spec` alone**: 20,144 solves on 2,026 distinct
shapes — 18,118 of those solves (90%) would be cache hits if we
had a within-decl memo keyed exactly on the args fingerprint
used here.

### Top recurring shapes within hot decls

The patterns are diagnostic of the structure:

- `MenuV2.Menus.spec`: 680 × `AddContext ?`, 579 × `Row.Union ? ? ?`,
  574 × `RowToList {..} ?`. Form-machinery dictionary lookups
  hammered repeatedly as a wide record gets unfolded.
- `MenuV2.Modifiers.spec`: 432 × `RowToList {..} ?`, then
  field-by-field `TestHasLabelRL 'ageRestricted' Cons ?` × 180,
  `'availableWhen' Cons ?` × 180, `'photo' Cons ?` × 169, etc.
  Per-field repeated lookups on the same record.
- `Restaurant.Settings.view`: 376 × `HasErrors ?`, 311 × `Wrap
  FormField' Cons ?`, 251 × `HasFieldId ?`. Same form-widget
  dictionaries asked for hundreds of times.
- `PR.GenerateTestHelpers.main`: 697 × `RecordToHelper Cons`,
  546 × `ToHelper FormField'`. Generated test helpers walking
  every field of every record-typed test.
- `Restaurant.Settings.control`: 194 × `HasField 'label'
  TranslationKey ?`, 186 × `RowToList ? ?`. Repeated label-field
  lookups.

### Caveat on fingerprint precision

The fingerprint uses `briefType` (top-level constructor only).
Two solves with different actual args could collapse to the same
fingerprint, **inflating** apparent reuse. The real within-decl
reuse rate (under structural type equality, which is what an
honest memo would need) is **upper-bounded** by these numbers.

Even halving the rate, `MenuV2.Menus.spec` would still have ~5×
real reuse — 16,000 of its 20,000 solves duplicating earlier
work.

### Recommendation: within-decl entailment memo

This is now well-supported. **Proposed experiment:
`entailment-decl-memo`.**

Design sketch:
- Add `declSolveCache :: Map (Qualified ClassName, [SourceType])
  Expr` to `CheckState`, or equivalently to the per-decl scope.
- Clear at the start of each value declaration's typecheck
  (`withDeclTrace` boundary or `withFreshSubstitution`).
- At the top of `entails.solve.go`, after `substituteType subst`
  (so the args are post-substitution), look up the cache. On
  hit, return the cached `Expr`. On miss, run solve, cache the
  result before returning.
- Soundness considerations:
  - Args must be substituted before keying — otherwise unknowns
    keyed differently across calls would be cache misses
    spuriously.
  - The cached `Expr` must not depend on per-call state that
    could differ. It does not (it's a dictionary expression).
  - Side effects of solve: solve writes to `inferred` context
    via `WriterT`. On a cache hit we'd skip that. **This is the
    main risk** — need to verify whether the WriterT writes are
    idempotent (i.e., same constraint solved twice produces the
    same writes; replaying on hit is unnecessary because the
    first call already wrote).
- Falsifiable on prelude: same risk pattern as 4 prior unify
  experiments — caches that look great on full but regress
  prelude. Likely safer because per-decl scope means the cache
  doesn't accumulate across modules' worth of state.

## Verdict

Survey done; findings strongly support a within-decl entailment
memo. Recommend `entailment-decl-memo` as the next experiment.
