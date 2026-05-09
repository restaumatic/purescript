---
id: name-compare-lineage
status: done (research)
verdict: tbd
branch: name-compare-lineage
worktree: /workspace/p/name-compare-lineage
baseline_sha: b831b298
head_sha: b831b298
hypothesis: >
  Post-traversal-inline cost-centre table still credits ~6.6% to a
  "name-compare cluster": `compare (Qualified)` 3.6% + `compare (ProperName)`
  1.6% + `==(PSString)` 1.4%. The prior `name-compare-survey` measured
  Environment Map lookup *volume* (1.45M lookups across 4 sites) and
  motivated `env-hashmap`, which then turned out to be a no-win
  (full +1.4%/-1.1%, all within noise). The natural inference is that
  the 6.6% cluster is NOT dominated by those Environment Map lookups
  — most of the cycles live somewhere else (likely `compareType`/
  `eqType` callers, AST sorting/dedup, error/JSON serialisation, or
  some structural compare path the survey didn't catalogue).
  This experiment is a research survey: identify where the 6.6%
  actually lives by call site, weighted by cycles (not just volume),
  and recommend the next concrete optimisation.
headline_delta: 4.10% of `compare Qualified` (≈80% of the SCC's inherited time) lives in `replaceAllTypeSynonyms'.go → M.lookup ctor syns` — a Map keyed on `Qualified (ProperName 'TypeName)` that env-hashmap didn't migrate (it's the SynonymMap, separate from typeClasses/typeClassDictionaries/types). The companion 1.4% `==` PSString is row-label compares in `alignRowsWith` during entailment unification — structural row-representation cost, harder to attack. Recommended follow-up: `synonym-fast-path` — add a `HashSet (ProperName 'TypeName)` miss-prefix filter before the M.lookup. Hypothesis -1% to -3% on full builds, ~30-line diff.
tags: [name-compare, characterization, lineage, research, synonym-walker]
started: 2026-05-09
closed: 2026-05-09
---

# name-compare-lineage

## Why

`name-compare-survey` (closed) and `env-hashmap` (closed no-win)
together leave a paradox:

- The survey identified 1.45M Environment Map lookups across 4 sites,
  with `typeClassDictionaries` (55%) + `typeClasses` (26%) accounting
  for 81% of that lookup volume. It motivated a HashMap migration.
- `env-hashmap` then migrated all three (`typeClasses`,
  `typeClassDictionaries`, `types`) to HashMap with carefully INLINEd
  Hashable instances. Wall-clock didn't move (full +1.4% / -1.1% across
  two runs, all within noise).
- Yet `compare (Qualified)` is still 3.6% on the post-traversal-inline
  cost-centre table.

The simplest explanation: the 3.6% is NOT dominated by those
Environment Map lookups. (Or the Map lookups are dominated by some
*different* cost than the per-key compare — but env-hashmap removed
the per-key compare step entirely and didn't move wall-clock either,
which rules that out for those specific Maps.)

The right move before another structural attack is to figure out
where `compare (Qualified)` / `compare (ProperName)` / `==(PSString)`
actually fire from. Survey first, then act.

## Approach

Three-phase survey, in increasing cost order:

### Phase A: Static call-site enumeration

`grep` the codebase for every container and direct compare/eq site
that uses these key types. Categorise by domain (Environment lookup,
unification cache, AST sort/dedup, error rendering, JSON, externs,
imports). Output: a candidate list of ~20-50 hot call sites.

Cheap and immediate. Produces a list to weight later. Cannot answer
"which is dominant" by itself.

### Phase B: `-fprof-late` profile

GHC 9.4+ supports `-fprof-late`, which inserts SCC annotations
**after** the optimiser has finished. The standard `-prof` insertion
is at source level — it happens before specialisation, so a generic
`compare` for `Qualified a` shows up as one cost-centre summed across
every caller. Late-prof SCCs bin by concrete call site post-
specialisation, so the 3.6% `compare (Qualified)` should explode into
per-call-site shares.

This is the killer move. If it works, Phase C is unnecessary.

Caveats:
- Late-prof can change inlining behaviour somewhat — wall-clock %
  is not directly comparable to the standard `-prof` baseline.
- The output cost-centre tree gets much bigger; have to filter to
  the compare/eq SCCs.
- We've never run this on the codebase before; might hit ergonomic
  hiccups (build-time, eventlog interaction).

### Phase C: Targeted counters (only if needed)

If Phase B is ambiguous (e.g., a `compare`-heavy library function
shows up in many places without enough specificity), wrap the
candidate compare sites with env-gated counters in a separate module
(per the LESSONS warning about Unify.hs sensitivity). Run on
pr-admin, dump per-site histograms, weight by approximate work.

### Phase D: Findings & next-experiment recommendation

Distil into `results.md`: where the 6.6% cluster lives by call site.
Recommend the most actionable follow-up — one of:
- A specific call-site fast-path / fix (think `eqType` guard pattern).
- A targeted structural change (e.g. cache a hash on `Qualified` /
  `ProperName`, similar to how `type-hash` worked but on Names rather
  than Types).
- A non-obvious refactor (e.g. AST representation tweak that removes
  the compare entirely from a hot path).
- Or: this cluster is structurally irreducible — move on.

## Scope

**In:**

- Static enumeration of compare/eq sites for Qualified, ProperName,
  ModuleName, PSString.
- `-fprof-late` build + pr-admin compile + cost-centre analysis.
- Optional targeted counters in a separate (non-Unify.hs!) module,
  env-gated.
- Recommendation for one concrete follow-up experiment.

**Out:**

- Implementing the recommended fix (that's a separate experiment).
- Re-litigating env-hashmap (it's closed; trust the no-win verdict).
- Touching Unify.hs (LESSONS warns: instrumentation there causes
  +135% wall-clock from inlining contamination).

## Trap

- **Profile-build cost-centre artefacts.** SCCs suppress some
  inlining; non-profile builds may have very different shape. The
  6.6% cluster might itself be a profile-build artefact (compare
  Logger lesson). Sanity-check: does optimised binary size change
  when we remove a compare? If not, the cluster is partly a measuring
  artefact.
- **Don't trust hit-count without weighting by cost.** The prior
  survey did exactly this and it cost a 19-file structural migration
  that didn't pay. Phase B (cycles via `-fprof-late`) is what we
  ultimately need.

## Links

- Worktree: /workspace/p/name-compare-lineage
- Plan: [TASK.md](TASK.md)
- Live state: [HANDOFF.md](HANDOFF.md)
- Results: [results.md](results.md)
- Predecessor (volume survey): [../name-compare-survey/EXPERIMENT.md](../name-compare-survey/EXPERIMENT.md)
- Falsified follow-up: [../env-hashmap/EXPERIMENT.md](../env-hashmap/EXPERIMENT.md)
