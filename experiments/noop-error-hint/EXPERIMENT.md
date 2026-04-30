---
id: noop-error-hint
status: abandoned
verdict: no-win
branch: noop-error-hint
worktree: /workspace/p/noop-error-hint
baseline_sha: 6e04203c
head_sha: 25249c0b
hypothesis: >
  Quantify the per-call cost of `withErrorMessageHint` by replacing
  its body with `action` (no-op, dropping all hint context from
  error messages). The function is polymorphic over
  (MonadState CheckState m, MonadError MultipleErrors m) with no
  SPECIALIZE pragma — calls go through dictionary indirection unless
  GHC happens to inline across the module boundary. Result is an
  upper bound on what specialising/optimising the bracket could buy.
headline_delta: "+0.1% to +3.0% (slower) on incremental scenarios; bracket cost is ≤0% on success path. full scenario corrupted, not re-run"
closed: 2026-04-30
tags: [typechecker, error-hints, characterization, do-not-ship]
started: 2026-04-30
---

# noop-error-hint

## Hypothesis

`withErrorMessageHint` (`Monad.hs:183`) wraps every `unifyTypes`,
`subsumes`, `infer`, `check`, `checkFunctionApplication` etc. call.
On the success path it costs:

1. `get` (whole CheckState),
2. `modify` to cons a hint onto `checkHints`,
3. `rethrow (addHint hint) action` — wraps a `catchError` around
   the action,
4. `modify` again to restore `checkHints`.

Plus dictionary indirection through the polymorphic constraints,
because the function has no `SPECIALIZE` and is not marked `INLINE`.

The question: **how much does this cost in aggregate**, across the
whole typechecker? This experiment replaces the body with `action`
(no-op) and measures the delta vs `restaumatic@6e04203c`. The result
is an upper bound on what we could buy by specialising the bracket
or replacing it with a cheaper data structure (e.g., reader-style
hint stack instead of state).

## Scope

**In:**
- Patch `withErrorMessageHint` body to `action` in
  `src/Language/PureScript/TypeChecker/Monad.hs:188-194`.
- All four scenarios.
- Do not ship: the patch destroys hint context in error messages.
  The `rethrowWithPositionTC` and `withErrorMessageHint'` callers
  also degrade, since they're built on top of `withErrorMessageHint`.

**Out:**
- Actually shipping the no-op (this is purely a measurement).
- A real fix (specialise / INLINE / SPECIALIZE pragma / different
  data structure) — that's a separate experiment, gated on this one
  showing meaningful cost.
- Test-snapshot updates: error messages will differ, so `stack test
  --fast` is expected to fail on snapshot diffs. Build success is
  the only correctness gate here.

**Decision criteria:**
- **Δfull ≤ -3% with neutral incremental:** real cost worth attacking.
  Open a follow-up experiment for a SPECIALIZE pragma or an
  alternative implementation.
- **-3% < Δfull ≤ -1%:** marginal. Probably not worth the engineering
  cost of restructuring; record and move on.
- **Δfull > -1%:** GHC is already inlining or specialising it well
  enough that the dictionary cost is negligible. The polymorphism is
  a non-issue.

## Links

- Worktree: /workspace/p/noop-error-hint
- Patched function: `src/Language/PureScript/TypeChecker/Monad.hs:183-194`
- Predecessor context: [unify-lazy-subst-revive/HANDOFF.md](../unify-lazy-subst-revive/HANDOFF.md)
  (lazy-subst made the hint argument lazy on the success path; this
  experiment asks how much the *bracket itself* still costs)
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
