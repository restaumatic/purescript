---
id: entailment-redundancy
status: done (research)
verdict: tbd
branch: entailment-redundancy
worktree: /workspace/p/entailment-redundancy
baseline_sha: c84101d8
head_sha: c84101d8
hypothesis: >
  Per-decl chrome-trace on post-PR-#18 baseline shows the top-5
  decls are all Specular/React component specs where solve time
  dominates infer 5-10x. updateExternalMenuView alone spends 1.79s
  in solve. The cost-centre `compare (ProperName)` 3.9% headline
  is amortised across this constraint-solving path — the
  Environment Map keyed by Qualified ProperName.

  `skip-redundant-entailment-unify` previously found 87% of solves
  ended in a no-op final unification. With that fix shipped, we
  expect a different shape of redundancy to remain — most likely
  repeated identical solves on the same (className, args) within a
  single decl. Survey: count entails calls grouped by
  (className, brief arg shape) per decl, report top duplicates.
headline_delta: 375k solves / 29.6k distinct (className, args) shapes / 12.66× avg occurrences (global). HasField+IsSymbol+Cons = 32% of solve volume. Top recurring shape `RowToList {..} ?` = 6,902 calls. Per-decl breakdown still needed.
tags: [entailment, solve, characterization, redundancy, anatomy]
started: 2026-05-07
closed: null
---

# entailment-redundancy

## Why

Per-declaration eventlog on the current `restaumatic` tip
(c84101d8, post-PR-#18) shows the top-5 expensive decls drive
~10% of total typecheck time, all with solve-time-dominates-infer:

| Decl | Time | Inf | Solve |
|---|---:|---:|---:|
| `updateExternalMenuView` | 2,214 ms | 422 | **1,788** |
| `Restaurant.Settings.view` | 2,074 ms | 347 | **1,723** |
| `Menus.spec` | 1,715 ms | 129 | **1,584** |
| `Product.spec` | 1,602 ms | 248 | **1,351** |
| `Restaurant.Settings.control` | 1,432 ms | 121 | **1,309** |

The cost-centre `compare (ProperName)` 3.9% headline is the
Environment Map (Qualified ProperName -> TypeClassData) doing
many lookups during constraint solving — it's a side-effect of
solve volume, not an attack target on its own.

`skip-redundant-entailment-unify` (-15.5% full, shipped) attacked
the *trailing unification* at the end of each solve. That fix is
in. The question now: what residual redundancy is in solve itself?

## Hypothesis

The top-5 decls each emit many constraint solves, with the same
(className, args-shape) pair recurring multiple times within one
decl's typecheck. If a single `HasField "foo" {…} _` is solved
N times for the same N>>1 row shape (because the row appears in
multiple expression positions), we have redundancy that a
within-decl memo table could capture.

## Scope

**In.** A read-only survey: instrument the top of `entails.solve.go`
in `Entailment.hs` with a separate-module hook recording
`(decl-name, className, brief-arg-fingerprint)` per call and
counting duplicates. Dump on shutdown.

**Out.** Implementing any cache/memo. Touching the solver itself
beyond the one-line hook. Per-LESSONS Unify.hs sensitivity, treat
Entailment.hs as similarly inlining-sensitive — minimal hot-path
edit, all infrastructure in a separate module.

## Falsification

If the histogram shows that >50% of solves on `updateExternalMenuView`
are unique `(className, args-shape)` pairs (i.e., no obvious
within-decl redundancy), then the cost is irreducible solve work
and the next experiment should be elsewhere (e.g., the Environment
Map structure itself, or constraint-solving short-circuits). If
duplicates dominate (e.g., 5x average call count per pair), this
becomes a clear within-decl-memo experiment.

## Links

- Worktree: /workspace/p/entailment-redundancy
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
