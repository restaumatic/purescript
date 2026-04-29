# Handoff: unify-cache-anatomy

## TL;DR

Read-only survey to decompose the unification cache's behaviour on
pr-admin. Three call-elimination experiments + cost-centre data
agree the cache's benefit isn't memoizing the trivial outer calls
at the 3 hot funApp/abs/array sites — those calls are individually
near-zero-cost. The cache is per-module and intercepts every
recursive `unifyTypes` descent. Hypothesis: the cache's load-bearing
job is dedup of recursive descents from structural unification
(TypeApp's two-child case in particular). Survey collects depth /
pair-size / per-module distribution to confirm and guide the next
algorithmic alternative.

## What's done

- Worktree at `/workspace/p/unify-cache-anatomy` off 5713e832.
- EXPERIMENT.md / TASK.md filled in.

## What's blocked

- Nothing yet.

## Next steps

- Implement `UnifyAnatomy.hs` (separate module — keep IORefs and
  unsafePerformIO out of Unify.hs).
- Hook `Unify.hs:121-123` minimally: one `recordLookup` call before
  `HS.member`, one `recordInsert` before `HS.insert`. Verify binary
  size 49.1 MB.
- Hook depth tracking from outside if possible (counted-cache
  wrapper), to avoid Unify.hs hot-path edit. If that turns out to
  need restructuring, fall back to a small Unify.hs hook and verify
  binary size.
- Run `PURS_UNIFY_ANATOMY=1 spago build` on pr-admin once.
- Capture stderr to `anatomy.txt`, summarise in `results.md`.
- Pick the next algorithmic-alternative experiment based on what
  the data shows.