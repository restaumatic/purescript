---
id: env-hashmap
status: abandoned
verdict: no-win
branch: env-hashmap
worktree: /workspace/p/env-hashmap
baseline_sha: c84101d8
head_sha: a45e938b
hypothesis: >
  Per `name-compare-survey`, ~1.45M Environment Map lookups per full
  pr-admin build are dominated by three Maps keyed on
  `Qualified (ProperName _)`: `typeClassDictionaries` (55%, 337 keys),
  `typeClasses` (26%, 337 keys), and `types` (16%, 3642 keys). Every
  lookup currently does ~log₂N text-comparison probes through
  `compare (Qualified)` → `compare (ProperName)` → `compare (PSString)`
  — that's the entire ~7.6% name-compare cluster in the cost-centre
  table. Replacing those Maps with `HashMap` and adding INLINEd
  `Hashable` instances on `Qualified`, `ProperName`, and `PSString`
  should collapse a log-n string-compare chain into one hash + one
  equality check. Headline target: -5% to -10% on full builds.
headline_delta: full +1.4%, nochange -1.4%, prelude +1.3%, leaf +3.8% — neutral / slight regression. The 7.6% name-compare cluster is not all from these Map lookups; per-lookup costs are roughly balanced for short ASCII keys at N~337. PR #18's win came from eliminating compareType (whole-Type recursion); a similar move over short-string compares produces no measurable speedup.
tags: [environment, hashmap, hashable, name-compare, no-win]
started: 2026-05-08
closed: 2026-05-08
---

# env-hashmap

## Hypothesis

The post-PR-#18 hotspot table shows a ~7.6% name-compare cluster
(`compare (ProperName)` 3.9% + `compare (Qualified)` 2.3% + `==`
(PSString) 1.4%). The `name-compare-survey` characterised it: 1.17M
of those compares happen across two Maps keyed on
`Qualified (ProperName 'ClassName)` with only ~337 distinct keys
each, so each lookup is doing ~8.4 text-compare probes to hit a
key from a tiny set. A `HashMap` replacement turns that into one
INLINE hash + one equality check.

The `types` Map (237k lookups, 3642 keys) gets a smaller per-lookup
ratio but the same structural fix applies, and its Hashable
instances are already paid for by the class-Map migration.

## Scope

**In:**

- New module `Language.PureScript.Environment.HashableInstances` (or
  inline in existing modules) with `Hashable` instances for
  `PSString`, `ProperName`, `ModuleName`, `QualifiedBy`, `Qualified`.
  Every method **must** be `{-# INLINE #-}` (see Trap below).
- Migrate three fields on `Environment`:
  - `typeClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData`
    → `HashMap`
  - `typeClassDictionaries :: M.Map QualifiedBy (M.Map (Qualified (ProperName 'ClassName)) ...)`
    → outer `HashMap`, inner can stay `Map` (only ~30 candidate
    instances per class — log factor is irrelevant). Outer migration
    is the win because it's the inner-map indirection through
    `findDicts` that drives 55% of all name-compares.
  - `types :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)`
    → `HashMap`
- Update every site in the compiler that constructs/queries those
  fields. Most pass through smart functions in `Environment.hs`
  (`lookupTypeClass`, `lookupConstructor`, etc.) but a few inline
  `M.lookup`/`M.insert` calls exist.

**Out (this experiment):**

- `dataConstructors` — only 35k lookups (2.4%), not worth the diff.
- Inner per-class dictionary Maps in `typeClassDictionaries`. The
  inner Maps are tiny (~30 candidates) and their `compareType`-style
  cost is dominated by the *outer* Map miss before it.
- Closed-set fast-path on top-N classes. Survey ruled this out —
  top-25 only covers 60–66%, so 35–40% still pays the Map cost.
- Restructuring `Environment` itself or changing the smart accessors'
  signatures.

## Trap

Per LESSONS.md "Hashable instance methods must be INLINE": when
`type-hash` first added a `Hashable (Type a)` instance without
`{-# INLINE #-}` on `hash`/`hashWithSalt`, full builds went **+102%**
slower (worse than baseline). Every method on every new Hashable
instance in this experiment must be `{-# INLINE #-}` or we burn
the run.

Mitigation: add a CI/local sanity check after each instance batch:
single full build to confirm no >5% regression before continuing.

## Plan

See [TASK.md](TASK.md) for the implementation plan.

## Outcome (closed 2026-05-08)

Migration shipped cleanly: 19 files, +157/-92, all 1340 tests pass.
But the perf result was **no-win** — full +1.4%, nochange -1.4%,
prelude +1.3%, leaf +3.8%. See [results.md](results.md) for full
breakdown and the four candidate explanations for why the
hypothesised log-N → O(1) win didn't materialise. The headline
takeaway: **the 7.6% cost-centre cluster is not all attributable to
these Map lookups**, and per-lookup costs (HashMap hash+eq vs Map's
~8 short-string compares) are roughly balanced when keys are short
ASCII.

The branch is preserved at `env-hashmap` for archaeology. Not merged.

## Links

- Worktree: /workspace/p/env-hashmap
- Predecessor survey: [../name-compare-survey/results.md](../name-compare-survey/results.md)
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
- Trap precedent: [../type-hash/EXPERIMENT.md](../type-hash/EXPERIMENT.md), LESSONS.md
