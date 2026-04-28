---
id: funapp-pattern-match
status: closed
verdict: no-win
branch: funapp-pattern-match
worktree: /workspace/p/funapp-pattern-match
baseline_sha: 5713e832
head_sha: 5713e832
hypothesis: >
  At three hot sites in TypeChecker/Types.hs (checkFunctionApplication',
  Abs check, ArrayLiteral check), 99.9% of unifyTypes calls are
  trivially equal (funapp-lineage-survey). Replacing the existing
  outer pattern + unifyTypes call with a nested constructor pattern
  (matching the head literally as a TypeConstructor) avoids both the
  call and the unifyTypes wrapper overhead. Hypothesis: this avoids
  the prelude regression seen with `unless (eqType …)` skip
  (skip-redundant-funapp-unify, +6.4% prelude on the HashSet baseline)
  because the nested pattern fuses into the same case-tree GHC already
  generates, vs. `eqType` adding a runtime if-branch on top of it.
headline_delta: full -0.2% / nochange -2.4% / prelude +7.0% / leaf +1.4% — falsifies code-gen hypothesis (same shape as skip-redundant-funapp-unify and unify-pattern-survey Phase 2)
tags: [unification, constructor-pattern, code-gen, types]
started: 2026-04-28
closed: 2026-04-28
---

# funapp-pattern-match

## Hypothesis

99.9% of `unifyTypes` calls at three hot sites in
`TypeChecker/Types.hs` (`checkFunctionApplication'`,
`check'` Abs, `check'` ArrayLiteral) are trivially equal — head is
already `tyFunction` or `tyArray` (per `funapp-lineage-survey`,
214,049 / 214,268 = 99.9% on a full pr-admin compile).

Splitting each site into two clauses — common case matches the head
constructor literally and skips `unifyTypes`, rare case keeps the
existing path — should:

1. **Eliminate ~214k calls per full build** (cache hits today).
2. **Avoid the prelude regression** that the `unless (eqType …)`
   variant suffered (`skip-redundant-funapp-unify`: +6.4% prelude
   on HashSet baseline). The mechanism: a nested constructor pattern
   fuses into GHC's existing case-tree dispatch on `TypeApp`; an
   `eqType` check is an `if`-branch on top of the case-tree. Same
   skip behaviour, different Core, different code-gen.

## Scope

**In.** Two-clause pattern match at the 3 sites in `Types.hs`. No
changes elsewhere — not Unify.hs (per LESSONS sensitivity), not
TypeFlags, not the cache. Baseline is 5713e832 (post-type-hash tip).

**Out.** Other call sites of `unifyTypes`. The 0.1% rare case
(t1 = TUnknown) — falls through the rare-case clause unchanged.
Constraint solving paths (Entailment.hs).

## Falsification criterion

If `prelude` regresses ≥3% with the nested pattern, the code-gen
hypothesis is refuted — the regression has another mechanism
(probably cache-residency interaction across the cascade). Document
the new mechanism rather than tweaking inline pragmas blindly.

## Links

- Worktree: /workspace/p/funapp-pattern-match
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
