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

## Verdict

Survey done. Findings strongly support a within-decl entailment
memo as a candidate. Next: refine to per-decl breakdown, then
decide on the memo experiment.
