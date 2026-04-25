---
id: synonym-opt
status: shipped
verdict: win
branch: synonym-opt
worktree: /workspace/p/synonym-opt
baseline_sha: 3fcac773
head_sha: 5c30138a
hypothesis: >
  Short-circuit hot type-tree traversals — replaceAllTypeSynonyms,
  replaceTypeWildcards, introduceSkolemScope, substituteType — by caching
  structural properties of each subtree on the type node itself. Most
  subtrees are already synonym-free / wildcard-free / scoped / unknown-free,
  and walking them is pure overhead.
headline_delta: "see measure-merges (combined with skip-redundant-entailment-unify): -22.9% full"
tags: [typechecker, synonyms, type-flags]
started: 2026-04-12
closed: 2026-04-23
---

# synonym-opt — per-node TypeFlags for traversal short-circuits

## What shipped

Per-node `TypeFlags` field on every `Type` constructor, auto-computed on
construction via pattern synonyms. Hot traversals short-circuit on subtrees
that are flagged as already-clean.

Flags shipped:
- `tfSynonymsFree` — subtree contains no `TypeConstructor`s referencing
  type synonyms; `replaceAllTypeSynonyms'.go` skips it.
- `tfWildcardsFree` — subtree contains no `TypeWildcard`s; `replaceTypeWildcards`
  skips it.
- `tfScoped` — subtree has no unscoped `ForAll`s; `introduceSkolemScope` skips it.
- `tfHasUnknowns` — subtree contains a `TUnknown`; `substituteType` skips
  unknown-free subtrees entirely.

Merged via PR #11 → `restaumatic` at commit `f7cf7747`. Released in
`v0.15.15-restaumatic9`.

## Measured results

Isolated synonym-opt measurements were not run under the experiment
framework. The combined effect of synonym-opt + skip-redundant-entailment-unify
was measured at -22.9% full build time on pr-admin
(see `experiments/measure-merges/results.md`):

| Scenario | Pre-merges (e0125163) | Post-merges (f7cf7747) | Δ      |
| -------- | --------------------- | ---------------------- | ------ |
| full     | 73.2s                 | 56.4s                  | -22.9% |
| nochange | 0.60s                 | 0.58s                  | -2.5%  |
| prelude  | 3.8s                  | 3.7s                   | -2.1%  |
| leaf     | 1.7s                  | 1.5s                   | -9.4%  |

Subtracting the standalone skip-redundant-entailment-unify measurement
(-15.5% full) gives an approximate synonym-opt contribution of around -7-8%
full, but the two changes likely have compound effects so this should be
treated as a rough estimate.

## Why it works

Each `Type` node now carries an `Int` bitfield of structural properties.
Pattern synonyms hide the field at construction sites — when you build
`TypeApp ann f x`, the pattern synonym auto-computes `tfFor (TypeApp ann f x)`
by combining the children's flags. Hot traversals that would otherwise
walk every node can check the flag and skip clean subtrees in O(1).

The biggest single win is `tfHasUnknowns` for `substituteType`: every
`unifyTypes` call substitutes both sides before unifying, and most types
in flight contain no unknowns at all (already-instantiated record types,
function types from imported modules, etc.). Walking them was pure overhead.

## Correctness

Debug assertions (commit `16abb5a9`) verify TypeFlags invariants — they
trip if a flag claims clean but a child node violates it. `combineFlags`
must clear `tfSynonymsFree` when combining flags across a synonym boundary
(commit `92bd49e5`).

## Links

- PR: https://github.com/restaumatic/purescript/pull/11
- Worktree: /workspace/p/synonym-opt
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Hotspot at start: `replaceAllTypeSynonyms'.go` (16.9% of full-build time
  on `restaumatic` at baseline 3fcac773)
