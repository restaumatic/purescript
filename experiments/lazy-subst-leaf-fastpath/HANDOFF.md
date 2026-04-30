# Handoff: lazy-subst-leaf-fastpath

## TL;DR

Combo of `unify-lazy-subst-revive`'s walk-style refactor + PR #18's
5-clause leaf fast-path. First measurement run was contaminated by
parallel worktree creation; needs a clean re-run before drawing
conclusions.

## What's done

- Branched `lazy-subst-leaf-fastpath` off `unify-lazy-subst-revive@497d51f5`.
- Added the 5 leaf fast-path clauses before the catchall `unifyTypes
  t1 t2 = do` (commit `febbdb09`). The cache+import cleanup that
  PR #18's commit also included was already in lazy-subst (via the
  cherry-picked `dd90b8bd`), so the combo patch is the 5 clauses
  alone.
- `stack build` succeeded; head binary 69.1 MB (matches lazy-subst
  clean-rebuild size — the 5 extra clauses don't inflate it).
- One full measurement run, **invalidated** by parallel-worktree
  contention.

## Run 1 — INVALID (parallel-worktree contention)

| Scenario | Base (s) | Head (s) | Δ       | Notes |
| -------- | -------: | -------: | ------: | ----- |
| full     |    157.6 |    108.5 |  -31.1% | baseline 2× slower than expected (lazy-subst saw 85.0 s on same baseline binary) |
| nochange |      0.5 |      0.4 |   -9.0% | |
| prelude  |      3.0 |      3.1 |   +3.2% | |
| leaf     |      1.5 |      1.5 |   +0.4% | |

The baseline binary at `experiments/baselines/6e04203c/purs` is
unchanged from the lazy-subst measurement (48.7 MB, mtime 04-30
06:24). The full-build runs ranged 157–211s with variance >25%,
versus the typical <2% noise floor. Cause: I created the
`noop-error-hint` and `specialize-tc-helpers` worktrees while this
measurement was running. Each `git worktree add` checks out 1961
files — heavy parallel I/O + CPU contention. The head-vs-baseline
*direction* is probably real (run ranges don't overlap), but the
magnitude is unreliable.

**Same trap as `unify-lazy-subst-revive` Run 1:** "binary size or
machine state mismatch makes the numbers wrong." Diagnostic: variance
much larger than expected. Fix: clean re-run on an idle machine.

## Run 2 — also unreliable (shared-machine contention)

| Scenario | Base (s) | Head (s) | Δ       | Notes |
| -------- | -------: | -------: | ------: | ----- |
| full     |    130.3 |     79.2 |  -39.2% | baseline still anomalous (vs lazy-subst's 85.0 s on same precompiled binary) |
| nochange |      0.5 |      0.5 |   -1.7% | |
| prelude  |      2.9 |      3.0 |   +1.8% | |
| leaf     |      1.6 |      1.6 |   -1.7% | |

I sequenced this run after combo finished (no parallel worktree
creation), but `uptime` showed load average 2.75/7.53/9.66. `ps aux`
revealed **six other Claude sessions** running concurrently (working
on unrelated repos: e2e-typecheck-ci, vo-default-ingredient,
vo-web-call-elevenlabs, etc.), plus a storybook dev server, a
taskrunner, and a restaumatic server. The other Claude sessions
were each consuming 7-30 % CPU; one started at 09:22 directly in
my measurement window.

The head numbers (78–88 ms range, median 79.2 s) are tight and close
to lazy-subst's head (75.6 s). The baseline numbers are anomalous
(should be ~85 s; we got 130 s median).

Two readings, can't distinguish without re-measurement on an idle
machine:

- **If baseline 130 s is real:** combo Δ = -39% full, leaf fast-path
  is strongly additive on top of lazy-subst.
- **If baseline 130 s is contaminated and combo head is ~79 s:**
  combo Δ ≈ -7% vs lazy-subst's -11.1%, meaning leaf fast-path *fights
  with* lazy-subst and the combo is slightly *worse* than lazy-subst
  alone. (Possible reason: 5 extra clauses cross an inlining threshold
  in `unifyTypes`'s unfolding.)

Either reading would be a real result. We need a clean re-run to
choose.

## Run 3 — clean, with interleaved harness

| Scenario | Base (s) | Head (s) | Δ       | Notes |
| -------- | -------: | -------: | ------: | ----- |
| full     |     89.7 |     78.6 |  -12.4% | head 78–94 s, base 88–116 s |
| nochange |      0.5 |      0.5 |   -4.5% | head 455–491 ms, base 475–2546 ms |
| prelude  |      3.0 |      3.1 |   +0.7% | head 3050–3194 ms, base 2998–5444 ms |
| leaf     |      1.6 |      1.6 |   +2.2% | head 1589–3817 ms, base 1610–4032 ms |

This run used `run-profile.sh`'s new interleaved orchestrator: per
scenario, 5 rounds of `(baseline run, head run)` back-to-back, with
per-variant `output-baseline/` and `output-head/` dirs. Time-varying
machine load is now shared near-equally between variants. Round 1 is
discarded as warm-up.

## Verdict: **no-win** (vs lazy-subst alone)

| Variant                     | Δfull   | Source |
| --------------------------- | ------: | ------ |
| `restaumatic@6e04203c`      |    0.0% | baseline |
| `unify-lazy-subst-revive`   |  -11.1% | this campaign |
| `lazy-subst-leaf-fastpath`  |  -12.4% | this experiment |

The combo's full-scenario delta (-12.4 %) is within noise of
lazy-subst alone (-11.1 %). The leaf fast-path's contribution on top
of lazy-subst is ~-1 %, which is below the run-to-run variance even
with the interleaved harness.

**Mechanism interpretation:** PR #18's leaf fast-path saves three
costs per equal-leaf pair: (1) `gets checkSubstitution`, (2)
`withErrorMessageHint` bracket, (3) `substituteType` traversal.
Lazy-subst already removed (3) (substituteType only runs on the
error-throw path now), and (1) is cheap. (2) is the remaining
overhead, but its full cost is bounded by the `noop-error-hint`
experiment (next).

**What this says about PR #18's reported -18.6 %:** that number was
measured against a much older baseline (`799e8208`, pre-synonym-opt
and pre-skip-redundant-entailment-unify). On today's baseline
(`6e04203c`, both shipped), PR #18's contribution is ~-1 % at best.
The earlier wins came from a different overhead profile that
synonym-opt + skip-redundant-entailment-unify have since eliminated.

## Followup experiments (already scaffolded)

1. `noop-error-hint` — measures upper bound on
   `withErrorMessageHint`'s aggregate cost. The combo's barely-there
   improvement over lazy-subst suggests this upper bound is small,
   but the experiment is committed and worth running for the record.
2. `specialize-tc-helpers` — adds SPECIALIZE pragmas. Pair with
   noop-error-hint to determine "is the bracket cost real and is it
   recoverable lossless via specialisation?"

Both worktrees are committed (`f3982880`, `7fe25412`) and ready to
build + run sequentially.

## Recommendation

Ship `unify-lazy-subst-revive` (`497d51f5`, -11.1 % standalone). Do
not ship the leaf fast-path on top — it's ~zero contribution. PR #18
on `restaumatic@6e04203c` would also be ~zero; the lazy-subst
refactor is the right pick.
