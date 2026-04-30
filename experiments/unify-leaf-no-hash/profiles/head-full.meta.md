# Post-`unify-leaf-no-hash` profile — `35ad956a` (HEAD)

Cost-centre profile of the unify-leaf-no-hash branch tip, the new
likely shipping baseline. Captures where time goes once the major
unification-cache hotspot has been removed.

- Date: 2026-04-30 11:00 UTC
- Branch: `unify-leaf-no-hash` @ `35ad956a` (worktree `/workspace/p/unify-leaf-no-hash`)
- Source diff vs `restaumatic`: leaf fast-path + `S.Set` cache drop in `Unify.hs` only
- Build: `stack build --profile --system-ghc` in worktree
- pr-admin: 1758 modules, full clean build (`rm -rf output`)
- Run: `purs +RTS -p -xc -N1 -RTS compile $(spago sources)`
- `total time = 289.79 secs (289785 ticks @ 1000 us, 1 processor)`
- `total alloc = 335,024,310,880 bytes` (excludes profiling overheads)
- Raw `.prof` not committed (~193 MB) — at `experiments/unify-leaf-no-hash/profiles/head-full.prof` (gitignored)

**Methodology note.** The 799e8208 baseline profile in
`experiments/type-hash/profiles/baseline.meta.md` was multi-threaded
(`-N`, 32 procs); this one is `-N1` for clean nesting. The relative
%share is the comparable signal between them; absolute total time
isn't.

## Top cost centres (individual %time)

| Rank | %time | %alloc | Cost centre | Module:line | Was on 799e8208 |
|------|------:|-------:|-------------|-------------|------------------|
| 1 | 7.2% | 5.4% | `$mTypeApp.\` | Types.hs:243 | 6.1% (artifact) |
| 2 | 4.3% | 0.5% | `$mTypeConstructor.\` | Types.hs:235 | 5.4% (artifact) |
| 3 | 4.0% | 4.2% | `$mKindApp.\` | Types.hs:247 | 6.2% (artifact) |
| 4 | **3.9%** | 0.0% | `compare` (ProperName) | Names.hs:192 | **1.3% → 3.9% (3×)** |
| 5 | 3.6% | 3.0% | `$mRCons.\` | Types.hs:267 | (artifact) |
| 6 | 3.5% | 6.9% | `everywhereOnValuesTopDownM.g'` | AST/Traversals.hs | 2.9% |
| 7 | 3.3% | 0.4% | `$mForAll.\` | Types.hs:251 | (artifact) |
| 8 | 3.1% | 0.8% | `$mConstrainedType.\` | Types.hs:255 | (artifact) |
| 9 | 2.5% | 0.4% | `$mSkolem.\` | Types.hs:259 | (artifact) |
| 10 | **2.3%** | 0.0% | `compare` (Qualified) | Names.hs:233 | **4.1% → 2.3%** |
| 11 | 1.7% | 2.0% | `entails.solve.go.solveSubgoals` | Entailment.hs:445-447 | not in top |
| 12 | 1.6% | 1.7% | `withErrorMessageHint` | Monad.hs:188-194 | not in top |
| 13 | 1.6% | 1.7% | `replaceAllTypeSynonyms'.walkChildren` | Synonyms.hs:107-134 | not in top |
| 14 | 1.6% | 0.0% | `replaceIdents.replace` | CoreImp.Optimizer.Common.hs:27-28 | not in top |
| 15 | **1.4%** | 0.0% | `==` (PSString) | PSString.hs:52 | **`compare` 4.6% → `==` 1.4%** |
| 16 | 1.2% | 0.3% | `replaceAllTypeSynonyms'.walk` | Synonyms.hs:76-82 | not in top |
| 17 | 1.0% | 1.7% | `sndM` | Traversals.hs:7 | not in top |
| 18 | 1.0% | 1.8% | `guardedExprM` | AST/Traversals.hs:33-34 | not in top |
| 19 | 1.0% | 1.9% | `applyExternsFileToEnvironment.applyDecl` | Externs.hs:179-199 | not in top |

**Gone from the top:** `compareType` (was #1 at 7.7%) — no longer in top 30.
The `Set (Type, Type)` `unificationCache` was its primary caller
(`Unify.hs:121-123`), and that's now removed. The `unifyTypes'`
recursive equality cases hit `compareType` directly only in rare
ConstrainedType paths; structural type-walks instead use the matcher
expansions visible in the cluster above.

## What changed vs the 799e8208 baseline

### Confirmed wins
- **`compareType` defanged** (was 7.7%, now <1%). Cache-removal eliminated its bulk caller.
- **`compare` (Qualified)** halved (4.1% → 2.3%) — same cause, compareType used `Qualified` ordering for `Set (Type, Type)` keys.
- **`compare` (PSString)** collapsed: was 4.6% as `compare`; now 1.4% as `==`. With Set ordering gone, only Eq survives.
- Total alloc dropped 441 GB → 335 GB (-24%) — consistent with skipping `everywhereOnTypes` + Set bookkeeping on cache-hit paths.

### New / grew
- **`compare` (ProperName) 3.9%** — grew 3× (was 1.3%). With Map ordering on `Qualified (ProperName _)` keys still in play and the type-Set cache gone, ProperName ordering is the new headline `compare` cost. Driven by Environment lookups (`names`, `types`, `typeClasses`, `typeClassDictionaries`) which are all `Map (Qualified (ProperName *)) v`.
- **`withErrorMessageHint` 1.6%** — push/pop hint stack. The leaf fast-path skips it; recursive cases still pay. Could be cheaper or skippable on more constructors.
- **`entails.solve.go.solveSubgoals` 1.7%** — typeclass entailment. Surfaced as the cache cost moved out of the way.
- **`replaceIdents.replace` 1.6%** — JS-optimizer pass; not typecheck. Not in scope for compiler perf, but worth noting it now exceeds many typecheck centres.

### Pattern-matcher cluster (~28% combined)
Profile-build artifact from bidirectional pattern synonyms not inlining
under SCC. In the optimised build these fold back into their callers.
With `compareType` gone, the bulk of these now feed `eqType`,
`everywhereOnTypes`, `replaceAllTypeSynonyms'.walk`, `freeTypeVariables`,
and the per-constructor `unifyTypes'` cases. Combined matcher cost was
~36% on the 799e8208 baseline; ~28% here — a smaller pie reflecting the
shrunk compareType usage.

## Headline aggregations for next experiment selection

| Cluster | %time | Mechanism | Note |
|---------|------:|-----------|------|
| Name-compare cluster | **~7.6%** | `Map` keyed on `Qualified (ProperName _)` / `Label PSString` | Biggest unattacked aggregate. Targets: HashMap-Environment, name interning. |
| Pattern-matcher cluster (artifact) | ~28% | Pattern-synonym matchers in optimised builds fold to callers | Real cost is in callers — `eqType`, `everywhereOnTypes`, type-walks |
| AST decl-traversal | ~5% | `everywhereOnValuesTopDownM.g'` + `sndM` + `guardedExprM` + `everywhereOnValuesM.g'` | Touched on every typecheck pass over a module |
| Synonym walk | ~2.8% | `replaceAllTypeSynonyms'.walkChildren/walk` | `synonym-opt` already shipped; this is residual walk on no-substitute path |
| Entailment | 1.7% | `entails.solve.go.solveSubgoals` | Typeclass constraint solving |
| Hint bracket | 1.6% | `withErrorMessageHint` push/pop on `checkHints` | Already skipped by leaf fast-path; recursive cases still pay |

## How to reproduce

```sh
# Build profiled binary in worktree (one-off, ~10 min)
cd /workspace/p/unify-leaf-no-hash
stack build --profile --system-ghc

PROFILED="$(stack path --local-install-root --profile)/bin/purs"

# Run pr-admin compile, profiled, single-threaded (clean cost-centre nesting)
cd /workspace/restaumatic/apps/pr-admin
rm -rf output
bash -c '
  set -f
  SOURCES=$(spago sources 2>/dev/null | grep -v "^\[")
  "$0" compile $SOURCES +RTS -p -xc -N1 -RTS
' "$PROFILED"

# Result: ./purs.prof (~193 MB)
```
