---
id: synonym-opt
status: in-progress
verdict: tbd
branch: synonym-opt
worktree: /workspace/p/synonym-opt
baseline_sha: 3fcac773
head_sha: 92bd49e5
hypothesis: >
  Short-circuit the three hot type-tree traversals — replaceAllTypeSynonyms,
  replaceTypeWildcards, introduceSkolemScope — by caching structural
  properties of each subtree on the type node itself. Most subtrees are
  already synonym-free / wildcard-free / scoped, and walking them is
  pure overhead.
headline_delta: tbd
tags: [typechecker, synonyms, type-flags]
started: 2026-04-12
closed: null
---

# synonym-opt — per-node TypeFlags for traversal short-circuits

## Hypothesis

See frontmatter. The 16.9% `replaceAllTypeSynonyms'.go` cost centre,
along with `replaceTypeWildcards` and `introduceSkolemScope`, walks the
full type tree at ~40 call sites per typecheck. If each `Type` node
carries a small `TypeFlags` field tracking "this subtree is synonym-free
/ wildcard-free / has no unscoped ForAlls", pattern synonyms hide the
field and the hot traversals can short-circuit on an already-clean
subtree.

## Scope

In scope: add `TypeFlags` to every `Type` constructor; auto-compute on
construction via pattern synonyms; short-circuit the three named
traversals.

Out of scope: changing callers or eagerly expanding synonyms at entry
points (that was an alternative approach listed in `TASK.md`).

## Status

**In progress.** Four commits on top of `restaumatic` tip:

- `68330ed5` — Optimize replaceAllTypeSynonyms with per-node TypeFlags
- `d2d682b4` — Remove unused clearFlag, markAllTypeFlags, setTypeFlags
- `16abb5a9` — Add debug assertions to verify TypeFlags invariants
- `92bd49e5` — Document why combineFlags must clear tfSynonymsFree

No measured numbers recorded in-worktree yet — needs a proper run
through the experiment framework.

## Measured results

_Not yet measured under the new framework._ Results will land in
`results.md`.

## Links

- Plan: `/workspace/p/synonym-opt/TASK.md`
- Measurement procedure: `/workspace/p/synonym-opt/PROFILING.md`
- Hotspot: `replaceAllTypeSynonyms'.go` (16.9% of full-build time on
  the `restaumatic` baseline)

## Open problems

- **No measurements yet under the new framework.** Must run all four
  scenarios against baseline `3fcac773` before this experiment can move
  to `shipped` or `abandoned`.
- **Correctness of `tfSynonymsFree` invariant** — see commit
  `92bd49e5` for a subtle case where `combineFlags` must clear the
  flag. Assertions added in `16abb5a9` catch violations in debug
  builds.
