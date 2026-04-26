# Handoff: unify-callsite-survey

## TL;DR

**Survey done. Clear concentrated win available.**

75% of `unifyTypes` cache hits originate at three "extract a type
constructor from a TypeApp shape, then unify with a constant"
assertions in `Types.hs`. All three have 97–99% cache-hit rates,
meaning they're almost always trivial:

| Site | Hits | Hit% | Share |
|---|---:|---:|---:|
| `Types:funAppHead` (`unifyTypes tyFunction' tyFunction`) | 174,161 | 99.4% | 62.0% |
| `Types:checkAbsArrow` (`unifyTypes t tyFunction`) | 29,067 | 97.8% | 10.3% |
| `Types:checkArrayHead` (`unifyTypes a tyArray`) | 9,009 | 97.0% | 3.2% |
| **subtotal** | **212,237** | | **75.5%** |

Replacing each with `unless (eqType x const) $ unifyTypes x const`
should eliminate ~75% of cache traffic at the cost of three
pattern-match-against-constant comparisons per call. The cache
becomes much less load-bearing; potentially droppable entirely.

## What's done

- Branch + worktree at `/workspace/p/unify-callsite-survey`,
  baseline 799e8208 (the current shipped restaumatic state — has
  the unification cache as `S.Set (SourceType, SourceType)`).
- New module `Language.PureScript.TypeChecker.UnifyCallSiteSurvey`:
  - Map String (Int, Int) of (cache_hits, total) per call-site tag.
  - Reads live `unificationCache` from `CheckState` for the exact
    cache-hit predicate (not a hash-equality proxy).
  - Env-gated on `PURS_UNIFY_CALLSITE_SURVEY=1`.
- Tagged 25 external `unifyTypes` call sites across:
  - `Subsumption.hs` (2)
  - `Entailment.hs` (2)
  - `Types.hs` (21)
- Wired `Survey.dumpSurvey` into `app/Command/Compile.hs`.
- Built clean. Ran on pr-admin (1758 modules) once. Histogram
  captured to `results.md`.

## What's blocked

Nothing on the survey itself.

## Open follow-up (separate experiment)

**`skip-redundant-funapp-unify`** (or similar): add eqType
short-circuits at the three concentrated sites and measure all
four scenarios.

Expected pattern:
```haskell
-- Types.hs:1015 (Types:funAppHead)
checkFunctionApplication' fn (TypeApp _ (TypeApp _ tyFunction' argTy) retTy) arg = do
  unless (eqType tyFunction' tyFunction) $ unifyTypes tyFunction' tyFunction
  ...

-- Types.hs:841 (Types:checkAbsArrow)
check' (Abs binder ret) ty@(TypeApp _ (TypeApp _ t argTy) retTy)
  | VarBinder ss arg <- binder = do
      unless (eqType t tyFunction) $ unifyTypes t tyFunction
      ...

-- Types.hs:835 (Types:checkArrayHead)
check' (Literal ss (ArrayLiteral vals)) t@(TypeApp _ a ty) = do
  unless (eqType a tyArray) $ unifyTypes a tyArray
  ...
```

Soundness: `eqType` is structural equality; if true, calling
`unifyTypes` on the same pair would be a guaranteed no-op (same
structure → no new substitutions, no new errors). Skipping is
safe. (This is the same justification the existing
skip-redundant-entailment-unify uses at Entailment.hs:337.)

After landing those three, re-run the callsite survey. If the
cache hit rate drops below ~10%, drop the cache entirely (the
unify-pattern-survey already established that the cache earns
its keep only because of these hash-equal hits — without them
it should be net negative).

## How to reproduce the survey

```bash
cd /workspace/p/unify-callsite-survey
stack build
cd /workspace/restaumatic/apps/pr-admin
rm -rf output
set -f
SOURCES=$(spago sources 2>/dev/null | grep -v "^\[")
PURS=/workspace/p/unify-callsite-survey/.stack-work/install/x86_64-linux/.../bin/purs
PURS_UNIFY_CALLSITE_SURVEY=1 "$PURS" compile $SOURCES \
  1>/dev/null 2>/tmp/survey.stderr
grep -A 200 "unify-callsite-survey" /tmp/survey.stderr
```

The full `purs` path is in `stack path --local-install-root`/bin
inside the worktree.
