# Results for funapp-pattern-match

Baseline: 5713e832 (post-type-hash tip; HashSet unification cache).

| Phase | full | nochange | prelude | leaf | Notes |
|-------|-----:|---------:|--------:|-----:|-------|
_(First run on 2026-04-28 was contaminated by `stack test --fast`
re-installing an unoptimised purs binary in-place — discarded;
`--fast` shrank the binary 49.1 → 46.7 MB, mirroring the LESSONS
"binary shrinks → code-gen disturbance" signal.)_

### Clean results (2026-04-28, optimised binary 49,114,400 bytes)

| Scenario | Baseline (5713e832) | Head | Δ | Notes |
|----------|--------------------:|-----:|---:|-------|
| full     | 48.6 s              | 48.5 s |  -0.2% | median of 4, 48,319–49,180 ms |
| nochange | 0.583 s             | 0.569 s | -2.4% | median of 4, 561–583 ms |
| prelude  | 3.91 s              | 4.18 s | **+7.0%** | median of 4, 4,141–4,189 ms |
| leaf     | 1.59 s              | 1.61 s | +1.4% | median of 4, 1,595–1,697 ms |

### Verdict: no-win

The hypothesis was that a nested constructor pattern fuses into
GHC's case-tree dispatch (vs `unless (eqType …)` adding an `if`-branch
on top of it), avoiding the prelude regression seen in
`skip-redundant-funapp-unify`. **Falsified.**

The pattern-match variant produces the same +7% prelude regression
shape as the two prior experiments that also skipped these calls:

| Experiment | Mechanism | prelude Δ |
|------------|-----------|----------:|
| `unify-pattern-survey` Phase 2 | typeHash + eqType, no cache | +7.1% |
| `skip-redundant-funapp-unify` (HashSet baseline) | `unless (eqType x const) $ unifyTypes x const` | +6.4% |
| `funapp-pattern-match` (this) | nested `TypeConstructor _ C.Function` pattern, skip call | **+7.0%** |

Three different implementations, three near-identical regressions.
The mechanism is structural to **skipping the unifyTypes call itself**
in the funApp/abs/array sites, not to how the skip is expressed in
Haskell source. Most plausible: the cache amortises something
beneficial across the 1342-module prelude cascade that the call
absence breaks (e.g., interaction between `unificationCache`
HashSet residency and downstream recursive `unifyTypes'` calls
through these sites), and the substitution path through
`unifyTypes` (substituteType + hint stack + cache lookup) is part
of what keeps the cascade fast on incremental rebuilds even when
the outer call is a no-op.

This experiment is the cleanest falsifier of the code-gen
hypothesis to date. Combined takeaway across all four experiments:
**the unification cache plus its substitution wrapper at these 3
hot sites is doing something the cascade depends on**, beyond just
caching `(t1, t2)` pairs. Don't re-attempt skipping these 3
specific sites without a measurement that explains the +7% prelude
mechanism.
