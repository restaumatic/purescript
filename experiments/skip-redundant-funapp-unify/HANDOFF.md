# Handoff: skip-redundant-funapp-unify

## TL;DR

**Closed: no-win.** Three `unless (eqType x const) $ unifyTypes x const`
guards at the survey-identified concentrated sites (funAppHead,
checkAbsArrow, checkArrayHead) eliminated ~75% of unification-cache
hits but produced no measurable speed change on any scenario.

| Scenario | Δ |
|---|---:|
| full | -0.0% |
| nochange | -1.9% |
| prelude | +2.3% |
| leaf | -1.6% |

All within ±2.3% noise. Tests pass (1340/1340). Binary +4 KB —
no inlining shift after `stack clean`.

## Why no win

The dominant pairs at these sites are 1-node `TypeConstructor`
constants. The cache's S.member is O(log n × O(1)-per-compare); the
eqType I substitute is also O(1) per compare. We trade one trivial
check for another. Cache lookups on tiny pairs were already
essentially free, so removing them produces no measurable saving.

This closes the "reduce cache cost upstream" path opened by
`unify-pattern-survey` (cache is hash-equal memoizer) and explored
by `unify-callsite-survey` (75% of hits at 3 sites). The cache is
essentially optimal for tiny-pair memoization; removing its inputs
doesn't help because they weren't expensive in the first place.

## Process lesson

The first benchmark run showed +74% on full, +164% on prelude with
the head binary 2 MB *smaller* than baseline (46.6 MB vs 48.6 MB) —
classic stale-incremental-build symptom. `stack clean` + rebuild
fixed it: binary returned to 48.6 MB and timings to neutral.

Same shape as the Unify.hs inlining sensitivity LESSON, but
manifesting via stale `.stack-work/dist` artifacts rather than a
deliberate Unify.hs edit. **Always `stack clean` before
benchmarking when results look implausible.**

## What's left in the worktree

- Branch `skip-redundant-funapp-unify` at commit `15540bba`
  contains the three eqType guards.
- The change is sound (tests pass), neutral on perf, and tiny
  (3 lines). No reason to ship it; no reason not to either.
- If a future change makes the cache more expensive (e.g.,
  switching to a slower data structure), these guards become
  net positive again. Keep the branch around as a parking lot.

## Implications for next experiment

- Skip cache-redundancy paths — already exhausted.
- Largest remaining unattacked hotspot per README: `compare`
  (Qualified a) at ~20%. That's where to look next.
