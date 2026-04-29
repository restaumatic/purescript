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
| [type-hash](type-hash/EXPERIMENT.md)           | ready-to-ship | win    | 799e8208   | -15.4% full, neutral incremental      | typechecker, hashing, type-flags |
| [unify-callsite-survey](unify-callsite-survey/EXPERIMENT.md) | done (research) | tbd | 799e8208 | survey: 75% of cache hits at 3 sites in Types.hs (97-99% hit rate) | unification, callsites, characterization |
| [ptr-eq-unify](ptr-eq-unify/EXPERIMENT.md) | in-progress | partial | 5713e832 | full **-7.5%**, nochange -3.6%, prelude +4.8%, leaf +3.4%, PR #17 | unification, fast-path, pointer-equality |
| [funapp-lineage-survey](funapp-lineage-survey/EXPERIMENT.md) | done (research) | tbd | 5713e832 | 99.9% of calls at 3 hot sites are trivially equal — only 5/140/63 distinct (h1,h2) pairs | unification, callsites, characterization, lineage |
| [unify-cache-anatomy](unify-cache-anatomy/EXPERIMENT.md) | done (research) | tbd | 5713e832 | 86% of cache hits are 1-2-node pairs (constructor-self recurrences); 97% ≤10 nodes — points to a leaf-tag fast-path | unification, caching, characterization, anatomy |
| [unify-leaf-no-hash](unify-leaf-no-hash/EXPERIMENT.md) | ready-to-ship | win | 5713e832 | full -0.4%, nochange -1.4%, prelude +1.4%, leaf +2.6% vs current shipped — leaf fast-path replaces type-hash + cache with simpler code, equivalent perf | unification, fast-path, simplification, type-hash |

## Closed experiments

| Id                                             | Status  | Verdict | Baseline | Headline Δ                          | Tags            |
| ---------------------------------------------- | ------- | ------- | -------- | ----------------------------------- | --------------- |
| [unify-leaf-fast-path](unify-leaf-fast-path/EXPERIMENT.md) | closed | partial | 5713e832 | Phase 1 (leaf fast-path alone): neutral. Phase 2 (+ cache dropped): full -0.3%, nochange -5.5%, prelude **+3.4%**, leaf +2.5% — leaf fast-path absorbs ~all of cache's full-build value; residual cache value is prelude-cascade amortisation | unification, fast-path, leaf, caching |
| [funapp-pattern-match](funapp-pattern-match/EXPERIMENT.md) | closed | no-win | 5713e832 | full -0.2%, nochange -2.4%, prelude **+7.0%**, leaf +1.4% — same shape as skip-redundant-funapp-unify; falsifies code-gen hypothesis | unification, constructor-pattern, code-gen, types |
| [skip-redundant-funapp-unify](skip-redundant-funapp-unify/EXPERIMENT.md) | closed | no-win | 799e8208 + 43f6b613 | phase 2 (HashSet baseline, the right one): prelude **+6.4%**, others neutral — same shape as unify-pattern-survey phase 2 | unification, redundancy, types, skip |
| [unify-cache](unify-cache/EXPERIMENT.md)       | abandoned | no-win | 43f6b613 | drop = +24%, UnifyKey wrap = +8% (no-win) | unification, caching, measurement |
| [unify-pattern-survey](unify-pattern-survey/EXPERIMENT.md) | abandoned | no-win | 43f6b613 | survey: 99.9% hits hash-equal; opt phases neutral or unsound | unification, caching, characterization |
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
