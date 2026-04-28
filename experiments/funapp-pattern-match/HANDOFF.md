# Handoff: funapp-pattern-match

## TL;DR

Scaffolded off type-hash baseline 5713e832. Hypothesis derived from
`funapp-lineage-survey` (99.9% of calls at 3 hot sites are
trivially equal). Plan: nested constructor pattern matches the
expected head literally and skips the unifyTypes call; the rare
fallback clause keeps the existing unify path. Goal is to win where
`skip-redundant-funapp-unify`'s `unless (eqType …)` lost on prelude
(+6.4% on HashSet baseline) — by fusing into GHC's case-tree
instead of adding an `if`-branch on top of it.

## What's done

- Worktree created at `/workspace/p/funapp-pattern-match`.
- Survey result captured in `LESSONS.md` (entry: "99.9% of unifyTypes
  calls at the 3 hot sites are trivially equal").
- Plan written in `TASK.md`.

## What's blocked

- **First benchmark run (2026-04-28) was contaminated.** `stack test
  --fast` rebuilt the library *unoptimised* and replaced the
  installed `purs` binary in-place: 22:38 had 49,114,400 bytes
  (optimised), 22:39 had 46,741,152 bytes (unoptimised). The
  benchmark with `--skip-build` then ran the unoptimised binary,
  producing catastrophic +138% / +166% numbers. Discard those rows
  in results.md.

## Next steps

- ✅ Implement the two-clause split.
- ✅ Build optimised once; binary size matched baseline (49.1 MB).
- ✅ Tests pass (1340/1340). But `stack test --fast` trashed the
  optimised binary.
- 🔄 Re-run `stack build` (no `--fast`) to restore the optimised
  binary, then re-run `exp run` *without* `--skip-build`.
- ✅ Annotate / replace the contaminated rows in results.md once we
  have clean numbers.

## Lesson candidate

`stack test --fast` rebuilds the library with `--fast` and reinstalls
in-place. Order test runs *after* benchmarking, or rebuild
optimised before the harness. The signal here was the same one
LESSONS already documents (binary shrinks ~2 MB, perf collapses) —
but triggered by `--fast` rather than incremental staleness.
