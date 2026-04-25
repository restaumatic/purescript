# Compiler performance experiments

This directory tracks performance optimization experiments on the
PureScript compiler. Each experiment is a branch + worktree + a folder
here capturing its hypothesis, plan, measurements, and outcome.

See `SCHEMA.md` for what each experiment folder contains and how the
lifecycle works. See `LESSONS.md` for cross-experiment learnings —
read it before starting a new experiment so you don't re-attempt a
known dead end.

## How to run an experiment

```
# start a new experiment
experiments/scripts/exp new <id> [--from <baseline-sha>]

# profile the baseline (before)
experiments/scripts/exp profile <id> --phase before

# ...hack in /workspace/p/<id>...

# profile the result (after)
experiments/scripts/exp profile <id> --phase after

# measure all scenarios
experiments/scripts/exp run <id> --scenarios all --runs 5

# close it out
experiments/scripts/exp close <id> --verdict win|partial|no-win|abandoned
```

See `scripts/README.md` for details and `CLAUDE.md` (repo root) for the
agent-facing overview.

## Active experiments

| Id                                             | Status       | Verdict | Baseline   | Headline Δ                           | Tags                           |
| ---------------------------------------------- | ------------ | ------- | ---------- | ------------------------------------ | ------------------------------ |
| [tc-queries](tc-queries/EXPERIMENT.md)         | blocked      | no-win  | 2e89bd4f   | +0.1% full, +9% prelude-edit         | incrementality, rock, caching  |
| [rust-interning](rust-interning/EXPERIMENT.md) | in-progress  | tbd     | (varies)   | conflicting — see EXPERIMENT.md      | interning, psstring, label     |
| [row-cons-opt](row-cons-opt/EXPERIMENT.md)     | in-progress  | tbd     | e0125163   | -2.2% full, neutral others           | unification, rows, entailment  |
| [type-hash](type-hash/EXPERIMENT.md)           | in-progress  | tbd     | 799e8208   | tbd                                   | typechecker, hashing, type-flags |

## Closed experiments

| Id                                             | Status  | Verdict | Baseline | Headline Δ                          | Tags            |
| ---------------------------------------------- | ------- | ------- | -------- | ----------------------------------- | --------------- |
| [synonym-opt](synonym-opt/EXPERIMENT.md)       | shipped | win     | 3fcac773 | combined w/ skip-redundant: -22.9% full | typechecker, synonyms, flags |
| [skip-redundant-entailment-unify](skip-redundant-entailment-unify/EXPERIMENT.md) | shipped | win | ebb0a6bb | -15.5% full, ~0% others | entailment, unification, rows |
| [measure-merges](measure-merges/EXPERIMENT.md) | shipped | win     | e0125163 | -22.9% full (combined synonym-opt + skip-redundant) | measurement, baseline |
| [noise-check](noise-check/EXPERIMENT.md)       | shipped | win     | 3fcac773 | +0.1% full (within noise — harness OK) | meta, framework |

## Hotspots being tracked

Updated after each profile run. Current snapshot: post-merges baseline
`799e8208` (synonym-opt + skip-redundant-entailment-unify shipped),
profiled run on pr-admin from 2026-04-25 (see
`experiments/type-hash/profiles/baseline.meta.md`).

| Cost Centre                      | Module                    | % time | Status                                       |
| -------------------------------- | ------------------------- | ------ | -------------------------------------------- |
| `compareType`                    | Types.hs:990–1027         | 7.7%   | being attacked in `type-hash`                |
| `compare` (PSString)             | PSString.hs:52            | 4.6%   | unattacked                                   |
| `compare` (Qualified a)          | Names.hs:233              | 4.1%   | unattacked (was 20.8%, fell out as side effect of synonym-opt) |
| `everywhereOnValuesTopDownM.g'`  | AST.Traversals            | 2.9%   | unattacked                                   |
| `compare` (ProperName)           | Names.hs:192              | 1.3%   | unattacked                                   |
| `replaceAllTypeSynonyms'.go`     | TypeChecker.Synonyms      | 0.2%   | shipped via `synonym-opt` (was 16.9%)        |

The pattern-synonym matcher cluster (`$mTypeApp.\`, `$mKindApp.\`, …)
shows up at ~36% combined in the cost-centre summary, but that's a
profile-build artifact — GHC suppresses inlining of bidirectional
pattern synonyms when SCC annotations are present. In optimised
builds those costs fold back into their callers (mostly `compareType`).
