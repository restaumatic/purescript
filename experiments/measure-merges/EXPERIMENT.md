---
id: measure-merges
status: shipped
verdict: win
branch: measure-merges
worktree: /workspace/p/measure-merges
baseline_sha: e0125163
head_sha: f7cf7747
hypothesis: >
  Measurement-only experiment: capture the combined effect of synonym-opt
  (PR #11) and skip-redundant-entailment-unify (PR #14) against the
  pre-merges restaumatic tip e0125163 — to put a number on what shipped
  in v0.15.15-restaumatic9.
headline_delta: "-22.9% full, -2.5% nochange, -2.1% prelude, -9.4% leaf"
tags: [measurement, baseline]
started: 2026-04-23
closed: 2026-04-23
---

# measure-merges

Measurement-only experiment. No code changes. Compares the merged
restaumatic tip (synonym-opt + skip-redundant-entailment-unify) against
the pre-merges baseline.

## Results

See `results.md`. Combined effect on pr-admin (1758 modules):

| Scenario | Pre-merges (e0125163) | Post-merges (f7cf7747) | Δ      |
| -------- | --------------------- | ---------------------- | ------ |
| full     | 73.2s                 | 56.4s                  | -22.9% |
| nochange | 0.60s                 | 0.58s                  | -2.5%  |
| prelude  | 3.8s                  | 3.7s                   | -2.1%  |
| leaf     | 1.7s                  | 1.5s                   | -9.4%  |

Note the wide min/max range on prelude (3649-5081ms) and leaf
(1478-9238ms) — those headline numbers are noisy. The full scenario is
tight (56308-56783ms) and the headline delta there is trustworthy.

## What this means for the next experiment

- The hotspot table in `experiments/README.md` is now stale. The 16.9%
  attributable to `replaceAllTypeSynonyms'.go` is gone (synonym-opt) and
  the entailment fundep no-op work is gone (skip-redundant-entailment-unify).
  Re-profile before picking the next target.
- The -2.1% prelude and -9.4% leaf deltas suggest the optimizations help
  not just the cold full-build path but also the typecheck-on-edit path,
  though noise makes the magnitude hard to pin down.

## Links

- Plan: [TASK.md](TASK.md) (boilerplate, no plan needed)
- Live state: [HANDOFF.md](HANDOFF.md) (boilerplate)
- Results: [results.md](results.md)
- See also: [synonym-opt](../synonym-opt/EXPERIMENT.md), [skip-redundant-entailment-unify](../skip-redundant-entailment-unify/EXPERIMENT.md)
