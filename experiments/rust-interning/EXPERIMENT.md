---
id: rust-interning
status: in-progress
verdict: tbd
branch: rust-interning
worktree: /workspace/p/rust-interning
baseline_sha: restaumatic-at-2026-04-13
head_sha: e6f85f32
hypothesis: >
  Interning PSString and Label (Rust-compiler-style symbol interning)
  converts row-label and type-level-string comparisons from O(n) byte
  compares to O(1) integer compares. Row unification and type-class
  resolution should get dramatically faster on label-heavy workloads.
headline_delta: "see HANDOFF — conflicting measurements"
tags: [interning, psstring, label, row-unification]
started: 2025-12-23
closed: null
---

# rust-interning — PSString + Label interning

## Hypothesis

See frontmatter. The key insight (per Rust compiler's `Symbol`) is that
labels are finite and reused extensively; switching equality/ordering
from byte-compare to int-compare removes an entire class of cost from
row-unification inner loops.

## Scope

In scope: intern PSString and Label globally (via `atomicModifyIORef'`);
change `Eq`/`Ord` to compare ids; preserve API compatibility via
`mkLabel` / `runLabel`.

Out of scope (Phase 3 in the original plan): interning whole `Type`
nodes.

## Status

**In progress, results conflicting.** Historical documentation
(`PHASE2-RESULTS.md`) claims 80.7% improvement (57s → 11s). That
measurement turned out to be partly an artefact of an incorrect `Ord`
instance — once `Ord` was made semantically correct
(`phase1+2-corrected-ord`), the variant was 29% *slower* than baseline.
Further tuning recovered to `-3.4%`. Most recent run (2026-04-13) on
the `rust-interning-fixed` variant measured **+148% slower** (64s → 159s),
suggesting further regression after subsequent commits.

Raw history in `/workspace/p/rust-interning/profile-results.log`.

## Measured results (from worktree log, to be re-run under framework)

| Date       | Variant                            | Baseline | Head  | Δ             | Notes                          |
| ---------- | ---------------------------------- | -------- | ----- | ------------- | ------------------------------ |
| 2025-12-23 | `phase1-psstring`                  | 58s      | 55s   | -5.1%         | 3-run avg                      |
| 2025-12-23 | `phase1+2-psstring+label`          | 57s      | 11s   | -80.7%        | **ORD INSTANCE WAS INCORRECT** |
| 2025-12-23 | `phase1+2-corrected-ord`           | 58s      | 75s   | +29.3%        | correct Ord, profile build     |
| 2025-12-23 | `corrected-ord-no-profile`         | 57s      | 66s   | +15.7%        |                                |
| 2025-12-23 | `optimized-label-cached-psstring`  | 58s      | 56s   | -3.4%         | best corrected-Ord variant     |
| 2026-04-13 | `rust-interning-fixed`             | 64s      | 159s  | +148%         | latest tip — catastrophic regression |

## Links

- Plan v2: `/workspace/p/rust-interning/INTERNING-PLAN-V2.md`
- Historical write-up (MISLEADING ON HEADLINE): `/workspace/p/rust-interning/PHASE2-RESULTS.md`
- Analysis: `/workspace/p/rust-interning/phase2-results-analysis.md`,
  `/workspace/p/rust-interning/profile-analysis.md`
- Earlier failed attempt (reverted): `/workspace/p/rust-interning/perf.md`
  — PSString with `ShortByteString` representation, -3.5%

## Open problems

- **The 80.7% headline is not real.** It came from an incorrect `Ord`
  instance that compared interning ids instead of the underlying
  string bytes, which is wrong whenever label iteration order affects
  behaviour (row normalisation, error formatting, deterministic
  output). Correct `Ord` needs to hit the string via a second lookup,
  which costs more than the int-compare saves.
- **Current tip regresses heavily.** Root cause of the 2026-04-13
  +148% regression unknown; `e6f85f32 Add plan v2` may just have
  re-organised without landing a performing variant.
- **What's the right experiment structure?** Phase 1 (PSString) alone
  produced a solid -5.1% and is probably worth landing. Phase 2 as
  implemented is unviable; a correct-Ord variant that actually wins
  would need a different interning scheme (ordered ids? deterministic
  hash?).
