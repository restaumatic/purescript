# TASK — name-compare-lineage

Three-phase characterisation survey. Goal: identify where the residual
6.6% name-compare cluster (`compare Qualified` 3.6% + `compare ProperName`
1.6% + `==(PSString)` 1.4%) actually fires from, by call site, weighted
by cycles.

## Phase A — static call-site enumeration

Status: in progress (running in fork)

Output: `static-enumeration.md` — a categorised list of every call site
in src/ that triggers compare/eq on Qualified, ProperName, ModuleName,
or PSString. Buckets:

1. Environment Map lookups (already attacked by env-hashmap, no-win)
2. Other Map/Set/HashMap/HashSet operations on these key types
3. Sort/dedup/nub
4. Direct compare/== calls
5. AST equality recursion (compareType, eqType, expr Eq, etc.)

Deliverable: candidate list of ~50-80 sites with file:line, snippet,
and per-pr-admin-build hot/med/low frequency hint.

## Phase B — `-fprof-late` profile

Status: in progress (build running)

GHC's `-fprof-late` inserts SCCs after the optimiser; cost-centres bin
by *post-specialisation* call site rather than by source definition.
The 3.6% `compare (Qualified)` SCC at Names.hs:233 (currently summed
across every caller because `-fprof-auto` puts the SCC at the deriving)
should explode into per-call-site shares.

Steps:

1. ✅ Add `ghc-prof-options: -fprof-late` to library stanza in
   `purescript.cabal`.
2. ✅ `stack build --profile --system-ghc` (in progress).
3. ⏳ Run pr-admin compile with profiling:
   `purs +RTS -p -RTS compile $(spago sources)`
4. ⏳ Inspect `purs.prof`:
   - cost-centre summary at top (top 30 by %time)
   - call-tree section: filter to compare/eq SCCs and aggregate parents
5. ⏳ Stash profile under `experiments/name-compare-lineage/profiles/`
   with a `.meta.md` recording the build flags and the top-30 summary.

## Phase C — targeted counters (only if needed)

Status: pending

Skip if Phase B leaves no ambiguity. Otherwise: add env-gated counters
in a separate module (NOT in Unify.hs — see LESSONS.md "Instrumentation
in Unify.hs contaminates measurement"), wrapping the candidate hot
compare sites identified in Phase A+B. Run pr-admin, dump per-site
volume + approximate per-call cost. Confirm or refute the Phase B
attribution.

## Phase D — findings + next-experiment recommendation

Status: pending

Output: `results.md`. Must include:

- Top 5-10 call sites of `compare (Qualified)`, ranked by cycles share.
- Same for `compare (ProperName)` and `==(PSString)` if distinguishable.
- Distinguish "real cost in optimised binary" from "profile-build SCC
  artefact" (per `logger-inline` lesson — test with binary-size delta
  if a candidate fix is proposed).
- Recommendation for one concrete follow-up experiment, with hypothesis
  and rough effort estimate.

Possible recommendation shapes:

- A fast-path / `eqType`-style guard at a hot specific call site.
- A targeted local Map → HashMap (e.g. `SynonymMap`) — but only if
  Phase B shows a single Map dominates a meaningful slice. NB:
  env-hashmap already showed the *Environment* Maps don't pay; same
  caution applies here.
- A structural change: hash-on-Qualified (analogue to type-hash) so
  every Map/Set keyed on Qualified gets an O(1) hash+1eq cost.
- Or "structurally irreducible — abandon and pick a different hotspot".

## Early signals from the existing prof (b831b298 head)

Without -fprof-late, the regular `-fprof-auto` profile at
`experiments/traversal-inline/profiles/head-full-20260509-080817.prof`
already shows useful call-tree structure. Top observation:

**`replaceAllTypeSynonyms'.go` does `M.lookup ctor syns` per
TypeConstructor**, where `syns :: SynonymMap = Map (Qualified
(ProperName 'TypeName)) ...`. This map is constructed locally per
caller and was NOT touched by `env-hashmap` (which migrated only
Environment fields). One inherited-time slice through the call tree
shows ~1.9% `compare Qualified` under
`replaceAllTypeSynonyms'.go → $mTypeConstructor.\`. Strong candidate
for the SynonymMap to be the next-experiment target — pending
Phase B confirmation that this isn't an artefact and the rest of
the 3.6% lives elsewhere.

Other notable parents from the existing prof:

- `applyExternsFileToEnvironment.applyDecl` (Externs.hs:179) — 0.7%
  inherited via `compare Qualified`. One-shot per externs decl on
  module load (probably builds Maps).
- `typeCheckAll.go.\` (TypeChecker.hs:405) — 0.2% inherited.

This still leaves ~0.8-1.0% of `compare Qualified` to attribute,
plus all of the ProperName 1.6% (probably mostly nested inside the
Qualified compare recursion) and the PSString 1.4%. Phase B should
clarify.
