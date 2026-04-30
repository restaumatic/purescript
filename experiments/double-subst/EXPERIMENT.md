---
id: double-subst
status: proposed
verdict: tbd
branch: double-subst
worktree: /workspace/p/double-subst
baseline_sha: 6e04203c
head_sha: 92cd49d5
hypothesis: >
  Apply substituteType twice in the unifyTypes wrapper. Idempotent
  semantics, but doubles the per-call substitution work. Measures
  the cost of one full substituteType pass per unifyTypes
  invocation — the inverse direction of unify-lazy-subst-revive
  (which removed it entirely for -11.1% full). Expected: ~+11%
  full, mirror-image of lazy-subst's gain. If the symmetry holds,
  it cross-validates the lazy-subst result.
headline_delta: tbd
tags: [typechecker, unification, substitution, characterization, do-not-ship]
started: 2026-04-30
closed: null
---

# double-subst

## Hypothesis

`unify-lazy-subst-revive` (497d51f5) measured -11.1 % full by *removing*
the eager `substituteType` from the `unifyTypes` wrapper (substitution
now happens lazily on the throw path or when chasing TUnknown chains).
That established a lower bound on the cost: at least 11 % of full-build
time is spent in eager `substituteType` calls during recursive
unification.

This experiment goes the opposite way: keep the eager call AND add
a second one. `substituteType sub (substituteType sub t1)` is a no-op
semantically (substitution is idempotent on its result — a second pass
finds no TUnknowns to rewrite), but it doubles the structural traversal
work per `unifyTypes` invocation.

If lazy-subst's gain is symmetric with this slowdown, we expect:
- **Δfull ≈ +11 %** — confirms the eager substitution is the load-bearing
  cost lazy-subst removed.
- **Δfull < +5 %** — the eager call must be cheap on most types (probably
  because most types are small leaves with nothing to substitute), and
  lazy-subst's win came from somewhere else (e.g., avoiding the *thunk
  construction* itself, not the work).
- **Δfull > +20 %** — there's some second-order amplification: maybe
  doubled substituteType pollutes a downstream cache or breaks GHC
  inlining. Worth investigating before drawing conclusions.

## Scope

**In:**
- Single 3-line patch to `unifyTypes` in
  `src/Language/PureScript/TypeChecker/Unify.hs:115-119`. Wrap
  `substituteType sub t1` and `substituteType sub t2` in another
  `substituteType sub`.
- All four scenarios.
- Compare directly to `unify-lazy-subst-revive`'s -11.1 %; the two
  experiments measure the same axis (eager substitution cost) from
  opposite directions.

**Out:**
- Doubling the substituteType inside the recursive descent of
  `unifyTypes'` — that would multiply the cost geometrically per
  recursion depth, an interesting variant but not what we're after
  here.
- Tripling or quadrupling. If +11 % shows up as expected, that's
  enough; if not, the next experiment is profiling, not piling on
  more substitutions.
- Any test-snapshot updates: error messages may include
  doubly-substituted types, but they should be identical to once-
  substituted types (idempotence). If `stack test --fast` fails,
  investigate before assuming it's a test-only issue.

## Predicted result and what each outcome means

| Δfull   | Interpretation |
| ------- | -------------- |
| ~+11 %  | Symmetry with lazy-subst confirms the cost was eager substituteType. Cross-validates. |
| +5 %–+10 % | Cost is real but smaller per call than lazy-subst's mechanism would suggest — lazy-subst's win includes some other factor (thunk-construction avoidance, error-hint laziness, or cache effects). |
| <+5 %   | Per-call substitution is cheap; lazy-subst's gain came from *not constructing intermediate types* (laziness of the error-hint argument), not from skipping the work. Reframes lazy-subst's "value." |
| >+20 %  | Doubled substitution stresses GC / inliner / cache; non-symmetric inverse, would need profiling. |

## Sequencing

Build is done (`92cd49d5`); ready to run on a quiet machine. Queue
after `specialize-tc-helpers` (currently the next pending experiment)
unless prioritised earlier.

## Links

- Worktree: /workspace/p/double-subst
- Patch commit: `92cd49d5`
- Inverse experiment: [unify-lazy-subst-revive](../unify-lazy-subst-revive/EXPERIMENT.md)
  (-11.1 % full standalone — this is the number to beat in magnitude
  for symmetry confirmation)
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
