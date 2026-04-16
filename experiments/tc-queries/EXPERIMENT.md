---
id: tc-queries
status: blocked
verdict: no-win
branch: tc-queries
worktree: /workspace/p/tc-queries
baseline_sha: 2e89bd4f
head_sha: ba8d7d25
hypothesis: >
  Converting per-binding-group typecheck operations into Rock queries
  enables within-module incrementality — edit a function body without
  changing its type, and downstream binding groups in the same module
  can be skipped from a persisted cross-build cache.
headline_delta: "+0.1% full, +9% prelude-edit (no-win)"
tags: [incrementality, typechecker, rock, caching]
started: 2025-12-22
closed: null
---

# tc-queries — binding-group Rock queries for within-module incrementality

## Hypothesis

See frontmatter. The module-level cache already exists via Rock; the bet
is that breaking the per-module typecheck into binding-group queries
lets us skip downstream groups when a body changes but the type doesn't.

## Scope

In scope: extract `kindsOfAll` and `typesOf` (both use
`withFreshSubstitution` — natural boundaries) into Rock queries; add a
persistent cross-build group cache at
`output/<Module>/groups.cbor`; serialise elaborated `Declaration` for
value groups.

Out of scope: changing `infer`/`check` or the unification algorithm
itself. Those are internal-to-a-group and not query boundaries.

## Status

**Blocked / no-win as measured.** Infrastructure is fully wired (cache
types, fingerprinting, delta compute/apply, persistence, Serialise on
full `Declaration` AST). Runtime caching is enabled only for
`DataBindingGroupDeclaration`; broader caching regressed.

- Caching cheap decl kinds: fingerprint + delta overhead exceeds the
  typecheck work it skips. Adds +4–6% to full builds, +13% to
  incremental edits, even with 100% cache hit rate.
- Caching value groups: serialised elaborated `Declaration` blows up
  to ~2 GB on pr-admin; full build goes 73s → 217s.

Current branch matches baseline within noise (+0.1% full, +9% on
prelude-edit).

## Measured results

See `/workspace/p/tc-queries/HANDOFF.md` for the full numbers. Headline:

| Scenario                      | Baseline (2e89bd4f) | Head (ba8d7d25) | Δ     |
| ----------------------------- | ------------------- | --------------- | ----- |
| Full build                    | 73.4s               | 73.5s           | +0.1% |
| No change                     | 1.1s                | 1.1s            | 0%    |
| Prelude edit (1342 retypecheck) | 2.3s              | 2.5s            | +9%   |
| Data-heavy module edit        | 3.2s                | 3.3s            | +3%   |

## Links

- Plan: `/workspace/p/tc-queries/TASK.md`
- Live state: `/workspace/p/tc-queries/HANDOFF.md`
- Measurement procedure: `/workspace/p/tc-queries/PROFILING.md`
- Key commits: `ba8d7d25`, `f6075ab3`, `42b8665d`, `2e89bd4f`,
  `a312cec7`, `9aa391e9`, `65ce13f7`, `a485b425`, `cd4dd1b9`

## Open problems (what it would take to unblock)

The unshipped prize is value-group within-module incrementality. Would
require one of:

1. A smaller cache shape — store env-delta only, refactor codegen to
   work from un-elaborated decls plus separately-cached type info.
2. Aggressive compression (zstd) on the elaborated representation.
3. A normalised, source-span-free variant reconstructed on cache hit.

Each is a significant refactor; none has been attempted.
