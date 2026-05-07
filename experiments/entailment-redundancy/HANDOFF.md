# Handoff: entailment-redundancy

## TL;DR

Survey-only experiment to characterise constraint-solving
redundancy on the post-PR-#18 baseline. Per-decl chrome-trace shows
the top-5 decls are all Specular component specs with solve
dominating infer 5-10×. `skip-redundant-entailment-unify` already
shipped a fix for trailing-unification redundancy — this survey
looks for residual redundancy *inside* solve, specifically repeated
identical `(className, arg-shape)` solves within a single decl
that a within-decl memo could capture.

## What's done

- Worktree at `/workspace/p/entailment-redundancy` off c84101d8.
- EXPERIMENT.md / TASK.md filled in.

## What's blocked

- Nothing yet.

## Next steps

- Implement `EntailmentAnatomy.hs` (separate module, env-gated
  PURS_ENTAILMENT_ANATOMY=1).
- One-line hook in `Entailment.hs:259` `solve.go` clause.
- Verify binary size matches baseline (~48-49 MB) after build.
- Run `PURS_ENTAILMENT_ANATOMY=1 spago build` on pr-admin once.
- Capture stderr to `anatomy.txt`, summarise in `results.md`.
- Decide next experiment based on duplicate-rate finding.