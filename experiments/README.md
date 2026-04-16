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

# ...hack in /workspace/p/<id>...

# measure
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
| [synonym-opt](synonym-opt/EXPERIMENT.md)       | in-progress  | tbd     | 3fcac773   | (unmeasured under framework)         | typechecker, synonyms, flags   |
| [rust-interning](rust-interning/EXPERIMENT.md) | in-progress  | tbd     | (varies)   | conflicting — see EXPERIMENT.md      | interning, psstring, label     |
| [entailment-memo](entailment-memo/EXPERIMENT.md) | in-progress | tbd   | ebb0a6bb   | -15.5% full, 0% others              | entailment, unification, rows  |

## Closed experiments

| Id                                             | Status  | Verdict | Baseline | Headline Δ                          | Tags            |
| ---------------------------------------------- | ------- | ------- | -------- | ----------------------------------- | --------------- |
| [noise-check](noise-check/EXPERIMENT.md)       | shipped | win     | 3fcac773 | +0.1% full (within noise — harness OK) | meta, framework |

## Hotspots being tracked

Updated after each profile run. Source: `p/tc-queries/PROFILING.md:105–112`.

| Cost Centre                      | Module                    | % time | Status                                   |
| -------------------------------- | ------------------------- | ------ | ---------------------------------------- |
| `compare` (Qualified a)          | Names.hs:234              | 20.8%  | unattacked                               |
| `replaceAllTypeSynonyms'.go`     | TypeChecker.Synonyms      | 16.9%  | see `synonym-opt`                        |
| `compare` (PSString)             | PSString.hs:52            | 8.6%   | unattacked                               |
| `compareType`                    | Types.hs                  | 4.2%   | unattacked                               |
| `everywhereOnTypes.go`           | Types.hs                  | 3.0%   | unattacked                               |
| `introduceSkolemScope`           | TypeChecker.Skolems       | 2.4%   | unattacked                               |
| `replaceTypeWildcards`           | TypeChecker.Unify         | 2.2%   | unattacked                               |
