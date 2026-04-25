# Task: type-hash

## Goal

Reduce `compareType` from 7.7% (currently #1 hotspot post-merges) to
near-zero in the unification-cache path, by:

1. Caching a structural hash on each `Type` node, computed at
   construction.
2. Adding a `Hashable Type` instance using that cached hash (O(1)).
3. Replacing `unificationCache :: Set (Type, Type)` with
   `HashSet (Type, Type)` in `Unify.hs`.

Secondary: a hash short-circuit on `eqType` (which is also called
from `skip-redundant-entailment-unify` and other places) for further
gains where structural equality testing is hot.

## Background

`Type a` already carries a `TypeFlags` (`Word8` bitfield) on every
constructor, computed at construction via the bidirectional pattern
synonyms in `Types.hs:214–279`. The synonym-opt experiment proved
this discipline pays off: ~479M `combineFlags` calls per build cost
0% time individually, while enabling massive traversal short-circuits
elsewhere.

The cost of `compareType` (7.7% on pr-admin full build,
`profiles/baseline.meta.md`) is dominated by `Set (Type, Type)`
membership in `Unify.hs:121–123`:

```haskell
unifyTypes'' t1' t2' = do
  cache <- gets unificationCache
  when (S.notMember (t1', t2') cache) $ do
    modify $ \st -> st { unificationCache = S.insert (t1', t2') cache }
    unifyTypes' t1' t2'
```

Each `notMember` and `insert` walks the tree to log(cache size) depth,
and at each step calls `compareType` which walks the type structurally
to log(type depth). Wide-row HasField types (667-field Translations
record) can have very deep `compareType` calls.

## Approach

Three steps, each measurable independently. We measure after each so
we can attribute the win.

### Step 1 — Extend `TypeFlags` with a hash field

Replace `newtype TypeFlags = TypeFlags Word8` with a small product:

```haskell
data TypeFlags = TypeFlags { tfBits :: !Word8, tfHash :: !Int }
  deriving (Show, Eq, Ord, Generic)
```

(or `Word8 → Word16` if we ever want more flag bits — for now Word8
is fine since the existing flags are 3 bits.)

**Hashing rules:**

- *Leaf nodes* (TUnknown, TypeVar, TypeLevelString/Int, TypeWildcard,
  TypeConstructor, TypeOp, REmpty) — hash the leaf's payload(s)
  combined with a constructor-tag salt:
  ```haskell
  hashLeaf :: Hashable x => Int -> x -> Int
  hashLeaf salt x = salt `hashWithSalt` x
  ```
  The salt distinguishes constructors so `TypeVar "x"` and
  `TypeLevelString "x"` get different hashes.

- *Inner nodes* (TypeApp, KindApp, ForAll, ConstrainedType, Skolem,
  RCons, KindedType, BinaryNoParensType, ParensInType) — combine the
  cached hashes of children and any value-level fields:
  ```haskell
  combineHash :: Int -> Int -> Int
  combineHash a b = a * 0x9E3779B1 + b   -- Knuth/golden ratio mix
  ```
  Use `tfHash` to fetch children's hashes; do *not* re-traverse.

**Pattern synonym builders update**

Before:
```haskell
pattern TypeApp a t1 t2 <- TypeApp_ _ a t1 t2
  where TypeApp a t1 t2 = TypeApp_ (typeFlags t1 `combineFlags` typeFlags t2) a t1 t2
```

After:
```haskell
pattern TypeApp a t1 t2 <- TypeApp_ _ a t1 t2
  where TypeApp a t1 t2 = TypeApp_ (combineNodeFlags TypeAppTag (typeFlags t1) (typeFlags t2)) a t1 t2
```

Where `combineNodeFlags tag f1 f2` returns a `TypeFlags` with bit
flags combined per existing `combineFlags` rules and hash combined
via `combineHash tag (combineHash (tfHash f1) (tfHash f2))`.

For leaf builders:
```haskell
where TypeVar a t = TypeVar_ (TypeFlags 0 (hashLeaf typeVarSalt t)) a t
```

The `_` salts are small `Int` consts, one per constructor — used
both as the hash salt and as a way to avoid collisions between
same-shape leaves (`TypeVar "x"` vs `TypeLevelString "x"`).

**Update `combineFlags` consumers**

`combineFlags` currently masks down to `structuralMask` to clear the
processing flag `tfSynonymsFree`. With hash added, the mask only
applies to bits, never the hash. Easy update.

**`forAllNodeFlags`, `constraintNodeFlags`, `skolemNodeFlags`** —
these need parallel hash computations. For `ConstrainedType` the
hash needs to incorporate the `Constraint`'s hash too — so
`Constraint` may need its own `Hashable` instance (cheap, reuses
the type hashes from constraintArgs).

**Measure step 1 alone:** the build should still work bit-identically
(no behaviour change, just bigger TypeFlags). Run `exp run type-hash
--scenarios all --runs 5`. Expectation: small construction overhead,
maybe -1% to +2% on full build, neutral elsewhere. If it's >+2% on
any scenario, abandon — the construction cost isn't paying for itself
yet (no savings until step 2/3).

### Step 2 — Hash short-circuit `eqType`

```haskell
eqType :: Type a -> Type b -> Bool
eqType t1 t2 = tfHash (typeFlags t1) == tfHash (typeFlags t2)
            && eqTypeStructural t1 t2

eqTypeStructural :: Type a -> Type b -> Bool
-- the existing implementation, renamed
```

This is correctness-safe: hash mismatch ⇒ structurally different ⇒
return False without traversal. Hash match falls through to the
existing structural check (handles collisions).

This affects:
- The `eqType` guard in `Entailment.hs:295` (skip-redundant-entailment-unify)
- Any other call site of `eqType` (grep `eqType` in src/)

**Measure step 2:** expect a small win on fundep-heavy code paths,
larger on full builds. If neutral, the leaf-construction overhead is
overwhelming the comparison savings.

### Step 3 — Convert `unificationCache` to `HashSet`

```haskell
-- in CheckState
unificationCache :: HS.HashSet (SourceType, SourceType)

-- in Unify.hs
unifyTypes'' t1' t2' = do
  cache <- gets unificationCache
  when (HS.notMember (t1', t2') cache) $ do
    modify $ \st -> st { unificationCache = HS.insert (t1', t2') cache }
    unifyTypes' t1' t2'
```

Requires `Hashable (SourceType, SourceType)` — provided by
`Hashable Type` (step 1) plus the standard tuple instance.

**Before committing, instrument the cache** (one-off, throw-away):
log `(hits, misses)` after each module rebuild. If hit rate is < 5%,
the cache is dead weight and we should drop it entirely instead of
converting it. If high, HashSet is the right move.

**Measure step 3:** this is where the headline number is expected.
Estimate: -3 to -7% on full builds, neutral on incremental. If
neutral or worse, look at allocation changes — HashSet `insert` may
be allocating more than `Set.insert` for our distribution.

## Key files

| File | Change |
| --- | --- |
| `src/Language/PureScript/Types.hs` | extend TypeFlags; update pattern synonym builders; add Hashable Type instance; hash-short-circuit eqType |
| `src/Language/PureScript/TypeChecker/Unify.hs` | `Set` → `HashSet` for unificationCache (step 3); also cache instrumentation in step 3 |
| `src/Language/PureScript/TypeChecker/Monad.hs` | CheckState.unificationCache type change |
| `purescript.cabal` | possibly add a hashable-related dep if not already (it's already used in the codebase, so probably nothing needed) |

## How to measure

```sh
# Step-by-step (after each commit)
experiments/scripts/exp run type-hash --scenarios all --runs 5

# With profile, after the full implementation
experiments/scripts/exp run type-hash --scenarios full --runs 5 --profile
experiments/scripts/exp profile type-hash --phase after  # eventlog
```

Then compare against baseline 799e8208 (already built at
`experiments/baselines/799e8208/purs`).

The four standard scenarios — full / nochange / prelude / leaf —
must all be neutral or better. A win on `full` that loses on
`nochange` is not a ship.

## Tests

```sh
stack test --fast
```

Important snapshot tests to watch:
- `tests/purs/passing/` — output deterministic, watch for diffs
- `tests/purs/failing/` — error message ordering may shift if any
  Ord-Type-keyed Map is iterated for output (we shouldn't be doing
  this since we're NOT changing Ord, but verify)

## Risks / things to watch

- **Construction overhead at scale**: ~480M Type allocations per
  build. Hash combine must stay in the "0% individual time" club like
  combineFlags. Watch the post-step-1 profile for new entries from
  the hash machinery.
- **Hash collisions**: false-collision rate should be tiny with a
  good combiner. Watch for slowdown on collision-heavy inputs (which
  would manifest as eqType structural-fallback time growing).
- **Memory**: peak RSS may grow ~10–15% with the bigger TypeFlags.
  Compare baseline vs head RSS during the full scenario.
- **Don't touch `Ord Type`** — see EXPERIMENT.md scope. The win comes
  from avoiding `Ord` (HashSet) not from making it faster (would
  break iteration order).
- **The cache might be obsolete**: if step-3 instrumentation shows
  near-zero hit rate post-merges, the right move is to drop the
  cache, not convert it. Decide based on the data.

## Sequencing summary

1. Step 1: extend TypeFlags with hash, no behaviour change. Measure.
   If construction overhead is bad, abandon.
2. Step 2: eqType hash short-circuit. Measure.
3. Step 3: instrument unificationCache hit rate. Decide HashSet
   conversion or removal. Implement, measure.
4. Close the experiment, write a LESSONS.md entry.
