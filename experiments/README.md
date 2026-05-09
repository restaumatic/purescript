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
| [unify-callsite-survey](unify-callsite-survey/EXPERIMENT.md) | done (research) | tbd | 799e8208 | survey: 75% of cache hits at 3 sites in Types.hs (97-99% hit rate) | unification, callsites, characterization |
| [ptr-eq-unify](ptr-eq-unify/EXPERIMENT.md) | in-progress | partial | 5713e832 | full **-7.5%**, nochange -3.6%, prelude +4.8%, leaf +3.4%, PR #17 | unification, fast-path, pointer-equality |
| [funapp-lineage-survey](funapp-lineage-survey/EXPERIMENT.md) | done (research) | tbd | 5713e832 | 99.9% of calls at 3 hot sites are trivially equal — only 5/140/63 distinct (h1,h2) pairs | unification, callsites, characterization, lineage |
| [unify-cache-anatomy](unify-cache-anatomy/EXPERIMENT.md) | done (research) | tbd | 5713e832 | 86% of cache hits are 1-2-node pairs (constructor-self recurrences); 97% ≤10 nodes — points to a leaf-tag fast-path | unification, caching, characterization, anatomy |
| [entailment-redundancy](entailment-redundancy/EXPERIMENT.md) | done (research) | tbd | c84101d8 | 375k solves / 29.6k shapes / 12.66× avg global. Per-decl: top decls 9.94×/10.51×/11.15× within-decl reuse — but follow-up `entailment-decl-memo` ruled the cache unsound (withFreshTypes non-idempotent) | entailment, solve, characterization, redundancy, anatomy |
| [name-compare-survey](name-compare-survey/EXPERIMENT.md) | done (research) | tbd | c84101d8 | 1.45M Env-Map lookups across 4 sites: typeClassDictionaries 55% + typeClasses 26% dominate (337 keys each), `HasField` alone 15.2%. Top-25 cover 58–66%. Distribution motivates HashMap migration over closed-set fast-path — but `env-hashmap` (below) showed the migration itself is a no-win. | name-compare, environment, characterization |
| [name-compare-lineage](name-compare-lineage/EXPERIMENT.md) | done (research) | tbd | b831b298 | Cycle-attribution survey on the residual 6.6% name-compare cluster (after env-hashmap no-win). 4.10% of `compare Qualified` (≈80% of the SCC) lives in `replaceAllTypeSynonyms'.go → M.lookup ctor syns` — the **SynonymMap**, which env-hashmap missed. PSString 1.4% is row-label compares (structural). Recommends `synonym-fast-path` — HashSet-of-ProperName miss-prefix filter, est. -1% to -3% on full builds. | name-compare, lineage, synonym-walker, characterization |

## Closed experiments

| Id                                             | Status  | Verdict | Baseline | Headline Δ                          | Tags            |
| ---------------------------------------------- | ------- | ------- | -------- | ----------------------------------- | --------------- |
| [entailment-decl-memo](entailment-decl-memo/EXPERIMENT.md) | closed | abandoned | c84101d8 | unsound — solve.go's withFreshTypes + fundep unifications are non-idempotent state effects; cache hits cause "instance head contains unknown type variables" downstream. 9.12% structural-key hit rate (vs survey's 9–11× briefType reuse). | entailment, memo, decl-scope, abandoned |
| [env-hashmap](env-hashmap/EXPERIMENT.md) | abandoned | no-win | c84101d8 | full +1.4%, nochange -1.4%, prelude +1.3%, leaf +3.8% — Map→HashMap migration on `typeClasses` (337 keys), `types` (3642 keys), `typeClassDictionaries` inner. Hypothesis was -5% to -10%; per-lookup hash+eq cost roughly balances log₂(337) short-ASCII compares. | environment, hashmap, hashable, no-win |
| [traversal-inline](traversal-inline/EXPERIMENT.md) | shipped | win | c84101d8 | full **-8.9% to -9.6%** across two runs, others within noise — 2-line diff (`{-# INLINABLE #-}` on `everywhereOnValuesTopDownM`/`everywhereOnValuesM` so GHC can specialise the polymorphic helper at the heavy `WriterT/StateT/TypeCheckM` call site in `Entailment.replaceTypeClassDictionaries`). +213 KB binary. | ast, traversal, inlinable, specialise, entailment, win |
| [error-helpers-inline](error-helpers-inline/EXPERIMENT.md) | abandoned | no-win | b831b298 | full -0.4%, nochange +1.4%, prelude +1.1%, leaf +2.7% — INLINABLE on `rethrow`/`warnAndRethrow`/`withErrorMessageHint`/etc. binary grew only +32 B (no actual specialisation). The traversal-inline pattern doesn't transfer to stateless one-shot wrappers — those have no recursive bind chain to collapse. | errors, monad-helpers, inlinable, no-win |
| [logger-inline](logger-inline/EXPERIMENT.md) | abandoned | no-win | b831b298 | full -1.1%/+0.9% (signs flipped between two runs), nochange -3.4%/-1.5% (small absolute), others sign-flip noise — INLINE on Logger's Monad/Applicative/Functor/MonadIO/MonadWriter methods. Same-size binary, different MD5. Profile cost-centre %time of 1.3% on `>>=.\` was an SCC artefact, not real overhead. | monad, logger, inline, no-win |
| [unify-leaf-no-hash](unify-leaf-no-hash/EXPERIMENT.md) | shipped | win | 799e8208 | full **-18.6%**, nochange +3.9%, prelude -0.9%, leaf -0.1% — leaf fast-path replaces type-hash + cache, merged via PR #18 | unification, fast-path, simplification, type-hash |
| [type-hash](type-hash/EXPERIMENT.md)           | abandoned | abandoned | 799e8208 | -15.4% full standalone — superseded by unify-leaf-no-hash (-18.6% same baseline, simpler diff). Never merged. | typechecker, hashing, type-flags, superseded |
| [unify-leaf-fast-path](unify-leaf-fast-path/EXPERIMENT.md) | closed | partial | 5713e832 | Phase 1 (leaf fast-path alone): neutral. Phase 2 (+ cache dropped): full -0.3%, nochange -5.5%, prelude **+3.4%**, leaf +2.5% — leaf fast-path absorbs ~all of cache's full-build value; residual cache value is prelude-cascade amortisation | unification, fast-path, leaf, caching |
| [funapp-pattern-match](funapp-pattern-match/EXPERIMENT.md) | closed | no-win | 5713e832 | full -0.2%, nochange -2.4%, prelude **+7.0%**, leaf +1.4% — same shape as skip-redundant-funapp-unify; falsifies code-gen hypothesis | unification, constructor-pattern, code-gen, types |
| [skip-redundant-funapp-unify](skip-redundant-funapp-unify/EXPERIMENT.md) | closed | no-win | 799e8208 + 43f6b613 | phase 2 (HashSet baseline, the right one): prelude **+6.4%**, others neutral — same shape as unify-pattern-survey phase 2 | unification, redundancy, types, skip |
| [unify-lazy-subst-revive](unify-lazy-subst-revive/EXPERIMENT.md) | abandoned | partial | 6e04203c | -11.1% full, -7.0% nochange, +2.9% prelude, -0.8% leaf — weaker than PR #18's reported -18.6% but on a different baseline; combo subsumes PR #18 | typechecker, unification, substitution, revive |
| [lazy-subst-leaf-fastpath](lazy-subst-leaf-fastpath/EXPERIMENT.md) | abandoned | no-win | 6e04203c | -12.4% full vs baseline ≈ lazy-subst alone (-11.1%); leaf fast-path subsumed by lazy-subst | typechecker, unification, leaf-fast-path, combo |
| [noop-error-hint](noop-error-hint/EXPERIMENT.md) | abandoned | no-win | 6e04203c | +0.1% to +3.0% (slower) on incremental scenarios — bracket cost on success path is ≤0%, full row corrupted | typechecker, error-hints, characterization |
| [unify-cache](unify-cache/EXPERIMENT.md)       | abandoned | no-win | 43f6b613 | drop = +24%, UnifyKey wrap = +8% (no-win) | unification, caching, measurement |
| [unify-pattern-survey](unify-pattern-survey/EXPERIMENT.md) | abandoned | no-win | 43f6b613 | survey: 99.9% hits hash-equal; opt phases neutral or unsound | unification, caching, characterization |
| [synonym-opt](synonym-opt/EXPERIMENT.md)       | shipped | win     | 3fcac773 | combined w/ skip-redundant: -22.9% full | typechecker, synonyms, flags |
| [skip-redundant-entailment-unify](skip-redundant-entailment-unify/EXPERIMENT.md) | shipped | win | ebb0a6bb | -15.5% full, ~0% others | entailment, unification, rows |
| [measure-merges](measure-merges/EXPERIMENT.md) | shipped | win     | e0125163 | -22.9% full (combined synonym-opt + skip-redundant) | measurement, baseline |
| [noise-check](noise-check/EXPERIMENT.md)       | shipped | win     | 3fcac773 | +0.1% full (within noise — harness OK) | meta, framework |

## Hotspots being tracked

Updated after each profile run. Current snapshot: `traversal-inline`
tip (b831b298) — recommend merging this branch first; it's the
shipped post-PR-#18 win (-9% on full) currently sitting on a feature
branch. Profiled `-N1` run on pr-admin from 2026-05-09 (see
`experiments/traversal-inline/profiles/`). Δ columns compare against
the pre-`unify-leaf-no-hash` baseline `799e8208` where the trail is
clear, otherwise just `traversal-inline` tip.

| Cost Centre                      | Module                    | % time | Status                                       |
| -------------------------------- | ------------------------- | ------ | -------------------------------------------- |
| `compare` (Qualified a)          | Names.hs:233              | 3.6%   | unattacked — env-hashmap was no-win, so the cost is *not* from Environment Map lookups (those didn't move under HashMap migration); origin TBD |
| `everywhereOnValuesTopDownM.g'`  | AST/Traversals.hs         | 2.6%   | partly attacked (was 3.5% pre-traversal-inline; -9% full overall) |
| `replaceAllTypeSynonyms'.walk`   | Synonyms.hs:76-82         | 2.0%   | residual after `synonym-opt`. `name-compare-lineage` (2026-05-09) showed that *also* most of the 3.6% `compare Qualified` (4.10% inherited) flows through this same call site via `M.lookup ctor syns`. Combined "synonym walker" cluster ≈ 6%. **Top unattacked target.** See `name-compare-lineage/results.md` for `synonym-fast-path` recommendation. |
| `compare` (ProperName)           | Names.hs:192              | 1.6%   | unattacked (was 3.9% pre-traversal-inline; partly absorbed by ENV map-lookups disappearing as types/typeClasses lookup paths sped up) |
| `replaceIdents.replace`          | CoreImp/Optimizer/Common.hs:27-28 | 1.4% | unattacked (codegen, not typecheck) |
| `==` (PSString)                  | PSString.hs:52            | 1.4%   | unattacked, ord→eq downgrade   |
| `>>=.\` `>>=.\.\` `fmap.\`       | Control.Monad.Logger      | 3.3% combined | profile-build SCC artefact — `logger-inline` confirmed it's not a real overhead in optimised builds |
| `replaceAllTypeSynonyms'.walkChildren` | Synonyms.hs:107-134 | 1.2% | residual after `synonym-opt` |
| `entails.solve.go.solveSubgoals` | Entailment.hs:445-447     | 1.1%   | unattacked (was 1.7% pre-traversal-inline) |
| `withErrorMessageHint`           | Monad.hs:188-194          | 1.1%   | tried — `error-helpers-inline` was no-win |
| `everywhereOnTypes.go`           | Types.hs                  | 1.7%   | already INLINE; called from substituteType etc. |
| `applyAll`                       | CoreImp/Optimizer/Common.hs | 1.1% | unattacked (codegen) |
| `applyExternsFileToEnvironment.applyDecl` | Externs.hs       | 0.9%   | unattacked (cold-start I/O cost) |

**Aggregate clusters** (better targets than individual centres):

- **Name-compare cluster** ~6.6% — `compare (Qualified)` 3.6% + `compare (ProperName)` 1.6% + `==` (PSString) 1.4%. NB: `env-hashmap` showed Map → HashMap migration on the surveyed Environment maps did *not* move this cluster — so the cost lives mostly elsewhere (likely `compareType`/`eqType` callers, AST sorting/dedup, JSON serialisation of types). **A new "where does compare actually fire from?" survey is required before another structural attack.**
- **Synonym walk cluster** ~3.2% — residual `replaceAllTypeSynonyms'.walk` 2.0% + `walkChildren` 1.2% after `synonym-opt` shipped. Biggest *single-source* unattacked target.
- **Codegen optimizer cluster** ~3.1% — `replaceIdents.replace` 1.4% + `applyAll` 1.1% + `inlineCommonOperators.runFn'.go` 0.3% + others. Different domain than typechecker work.

The pattern-synonym matcher cluster (`$mTypeApp.\`, `$mKindApp.\`, …)
shows up at ~28% combined in the cost-centre summary (down from ~36%
on 799e8208), but that's a profile-build artifact — GHC suppresses
inlining of bidirectional pattern synonyms when SCC annotations are
present. In optimised builds those costs fold back into their callers.
The Logger SCC (~3.3%) is the same kind of artifact (confirmed by
`logger-inline` no-win).
