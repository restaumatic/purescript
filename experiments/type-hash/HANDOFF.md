# Handoff: type-hash

_Live work log. Update as you go._

## TL;DR

Scaffolded 2026-04-25 against post-merges baseline `799e8208`. Plan
in `TASK.md` is three sequenced steps: (1) extend TypeFlags with a
hash, (2) hash short-circuit eqType, (3) convert unificationCache to
HashSet. Each step measurable on its own. Currently before step 1.

## Baseline data

- Branch off: `restaumatic` @ `799e8208`
- Baseline binary: `experiments/baselines/799e8208/purs`
- Source profile: `experiments/type-hash/profiles/baseline.meta.md`
  (cost-centre, 558.6s wall on -N profiled build, top hotspot
  `compareType` 7.7%)
- Reference numbers (post-merges, optimised):
  full ~56s, nochange ~0.6s, prelude ~3.7s, leaf ~1.5s
  (from `experiments/measure-merges/results.md`)

## What's done

- Worktree + scaffold + plan written
- Baseline profile captured

## What's blocked

- Nothing — ready to start step 1

## Next steps

1. **Step 1**: implement TypeFlags-with-hash. Build, run tests,
   measure construction-only overhead (`exp run type-hash --scenarios
   all --runs 5`). Decision point: if any scenario regresses >+2%,
   abandon.
2. Step 2: eqType hash short-circuit + measure
3. Step 3: instrument unificationCache hit rate, then either drop or
   HashSet-convert based on data
