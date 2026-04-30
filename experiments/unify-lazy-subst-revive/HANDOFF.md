# Handoff: unify-lazy-subst-revive

## TL;DR

Reviving the dormant `origin/unify-lazy-subst` branch (last commit
2025-05-13, predates the experiments framework) under the framework
so we can compare against PR #18's leaf fast-path. The branch
implements walk-style lazy substitution in `unifyTypes`, eliminating
the O(N²) substituteType blowup. Currently: scaffolded, about to
cherry-pick.

## What's done

- Scaffolded experiment (this directory, branch
  `unify-lazy-subst-revive` off `restaumatic@6e04203c`, worktree at
  `/workspace/p/unify-lazy-subst-revive`).
- Filled in EXPERIMENT.md hypothesis + scope and TASK.md plan.
- Cherry-picked the three lazy-subst commits onto the worktree:
  `5323c41e` → `82e267cc` (auto-merged Unify.hs and Types.hs cleanly,
  the Types.hs touch was just a small import-list adjustment that
  composed with synonym-opt's pattern-synonym additions),
  `dd90b8bd` → `7d18734d` (had a 2-hunk conflict in Unify.hs imports;
  resolved by keeping `Control.Exception (assert)` from synonym-opt
  and `hasFlag, tfHasWildcards, typeFlags` in the `Types` import,
  while taking dd90b8bd's removal of `when` and `Data.Set qualified
  as S`. Also stripped a stray `Monoid (Maybe a)` dev-note comment
  block that came in via the cherry-pick),
  `0a9bc189` → `497d51f5` (test snapshot updates; auto-merged).
- Verified no `unificationCache` reads/writes left in
  `Unify.hs` (the `S.Set` field is still defined in `CheckState`
  but is unused; PR #18 noted this is a safe followup cleanup).

## What's blocked

- `stack test --fast` blocked on `registry.bower.io` access (test
  harness pulls purescript-arrays etc. via bower). pyloros approval
  request `apr_0000000010` is still pending. The `purs` binary built
  fine; the test failure was 100% from bower setup, not from the
  compiler. Pr-admin compile via `exp run` is the de-facto soundness
  check and ran successfully.

## Measurements

### Run 1 — INVALID (incremental-build contamination)

| Scenario | Base (s) | Head (s) | Δ        |
| -------- | -------: | -------: | -------: |
| full     |     87.5 |    168.9 |  +92.9%  |
| nochange |      0.5 |      0.6 |  +40.5%  |
| prelude  |      2.9 |      6.7 | +127.6%  |
| leaf     |      1.6 |      2.9 |  +82.6%  |

Head binary at this point: 64.9 MB (vs baseline 48.7 MB). I
**incorrectly diagnosed this as the lazy-subst code being slow**.
The actual cause is unclear: the head worktree had gone through a
half-failed `stack test` (bower-blocked) before this build, so the
working theory was incremental-build contamination — same diagnostic
shape as `LESSONS.md:430-455`, even though that lesson is about
*smaller* binaries running slow and here the slow binary was
*larger*. But the cause could equally have been transient machine
load, a stale `.stack-work` from a different angle, or something
else; we didn't isolate it. **What's reproducible** is the
diagnostic: "binary size mismatched against expectations after a
non-clean rebuild" → **clean before measuring** is sufficient to
recover correct numbers, regardless of the underlying cause.

### Run 2 — clean rebuild

| Scenario | Base (s) | Head (s) | Δ       |
| -------- | -------: | -------: | ------: |
| full     |     85.0 |     75.6 |  -11.1% |
| nochange |      0.5 |      0.5 |   -7.0% |
| prelude  |      3.0 |      3.1 |   +2.9% |
| leaf     |      1.6 |      1.6 |   -0.8% |

Head binary after `stack clean`: 69.1 MB (still +42 % vs baseline
48.7 MB). So the size delta IS intrinsic to lazy-subst — but it
does **not** correlate with a runtime regression. Bigger binary,
faster code. That falsifies any narrative that frames the size
delta as an "inlining cliff" with negative perf consequences.

prelude `+2.9 %` and leaf `-0.8 %` are within run-to-run noise
(prelude max 5286 ms vs min 3061 ms in the same 4-run sample is
huge spread on a 3 s scenario; baseline-vs-baseline noise itself
likely accounts for most of this column).

## Verdict: small standalone win, but weaker than PR #18

Lazy-subst alone, **measured properly**, is **-11.1 % full** with
near-neutral other scenarios. This makes it a real but modest
optimisation. Compared to PR #18's reported -18.6 % full, the leaf
fast-path is the bigger lever. Two complementary mechanisms:

- **Lazy-subst** removes the substituteType-during-recursion blowup
  (algorithmic O(N²) → O(N) on substituteType cost).
- **PR #18 leaf fast-path** removes per-wrapper-call overhead
  (gets, hint bracket, substLookup) for equal-leaf pairs that the
  recursive descent would otherwise re-process from scratch.

On the current restaumatic baseline (which has the cache),
PR #18's mechanism (deduplicate equal-leaf hits) wins more than
lazy-subst's (don't re-substitute). That makes sense given
`type-hash`'s anatomy: 86 % of cache hits ARE the equal-leaf
pairs, and the leaf fast-path catches them before *any* wrapper
work, while lazy-subst still pays per-recursion `gets` +
`substLookup` + a (lazy) hint constructor.

## Methodological note (important)

I twice in this experiment uncritically applied the "Unify.hs
inlining cliff" framing from the row-cons-opt HANDOFF.md. That
framing has thin evidence — see updated note in
`experiments/experiment_queue.md` entry #4 — and the user pushed
back on both occurrences. The reproducible signal in this
experiment is **`stack clean` matters; binary size is a clue but
not a verdict; full perf measurement is the verdict.** Don't
borrow loaded vocabulary without checking what it actually buys.

## Followup

1. Core-dump analysis (task #12) — answer "how is `unifyTypes`
   actually being inlined across branches?" with hard evidence
   instead of vocabulary.
2. Combination experiment `lazy-subst-leaf-fastpath` — measure
   whether layering PR #18's fast-path on top of lazy-subst beats
   PR #18 alone.

## Known dev-note stripped

The lazy-subst-era Maciej commit `dd90b8bd` added a comment block
sketching `Monoid (Maybe a)`-style notes inside `Unify.hs`. Stripped
during conflict resolution — it was a scratch dev note, not load-
bearing. Mention this in the cleanup commit when squashing.
