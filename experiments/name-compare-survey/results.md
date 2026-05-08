# Results for name-compare-survey

## Survey on full pr-admin compile (c84101d8 baseline)

Run with `PURS_NAME_COMPARE_SURVEY=1` on a from-scratch build of
pr-admin (1758 modules). Hooks recorded `(mapName, showQualified key)`
per lookup at four sites:

| Site                                 | Map                       | What it does                  |
| ------------------------------------ | ------------------------- | ----------------------------- |
| `Entailment.solve.go:288`            | `typeClasses`             | look up class data per solve  |
| `Entailment.findDicts:103`           | `typeClassDictionaries`   | walk instance candidates      |
| `Kinds.go.TypeConstructor:169`+`:179`| `types`                   | resolve a type constructor    |
| `Types.infer'.Constructor:504`       | `dataConstructors`        | type pattern's data ctor      |

**Total lookups recorded: 1,448,269**

## Per-map breakdown

### typeClassDictionaries (findDicts) — 794,706 lookups (54.9% of total)

337 distinct class names. **Top-25 cover 465,019 (58.5%)**:

| count   | class                                              |
|--------:|----------------------------------------------------|
| 121,017 | `Data.Record.HasField`                             |
|  45,718 | `Row.Extra.TestHasLabelRL`                         |
|  21,860 | `Restaumatic.Form.Auto.Wrap`                       |
|  20,791 | `Data.Show.Show`                                   |
|  20,407 | `Restaumatic.Form.Query.Query`                     |
|  18,360 | `Control.Bind.Bind`                                |
|  17,697 | `Data.Eq.Eq`                                       |
|  16,338 | `Foreign.Generic.Class.Encode`                     |
|  14,556 | `Data.Enum.Generic.GenericBoundedEnum`             |
|  14,299 | `Restaumatic.Form.Internal.AddContext`             |
|  13,283 | `Unscramble.Decode`                                |
|  12,061 | `Data.Generic.Rep.Generic`                         |
|  12,011 | `Foreign.Generic.Class.Decode`                     |
|  11,579 | `Data.Newtype.Newtype`                             |
|  11,136 | `Foreign.Generic.Class.EncodeWithOptions`          |
| ... (full top-25 in `survey-output.txt`)                     |

`Data.Record.HasField` alone is 15.2% of all class-Map lookups
through one key.

### typeClasses (Entailment.solve) — 375,232 lookups (25.9% of total)

337 distinct class names. **Top-25 cover 247,589 (66.0%)**:

| count   | class                                              |
|--------:|----------------------------------------------------|
|  42,537 | `Data.Record.HasField`                             |
|  41,108 | `Data.Symbol.IsSymbol`                             |
|  37,226 | `Prim.Row.Cons`                                    |
|  15,271 | `Row.Extra.TestHasLabelRL`                         |
|  13,224 | `Prim.Row.Lacks`                                   |
|  10,748 | `Prim.RowList.RowToList`                           |
|   7,349 | `Data.Show.Show`                                   |
|   6,263 | `Data.Eq.Eq`                                       |
|   6,244 | `Control.Bind.Bind`                                |
|   5,565 | `Restaumatic.Form.Query.Query`                     |
|   5,564 | `Restaumatic.Form.Internal.AddContext`             |
| ... |

The top-3 here (HasField, IsSymbol, Cons) match the entailment-redundancy
finding precisely — same class machinery driving the hot path through
both Maps.

### types (Kinds.TypeConstructor + ConstrainedType) — 237,038 lookups (16.4% of total)

3,642 distinct type names. **Top-25 cover 137,823 (58.1%)**:

| count   | type                                               |
|--------:|----------------------------------------------------|
|  30,619 | `Type.Proxy.Proxy`                                 |
|  21,191 | `Prim.Function`                                    |
|  14,915 | `Prim.Symbol`                                      |
|  11,644 | `Prim.Record`                                      |
|   8,993 | `Prim.String`                                      |
|   6,806 | `Data.Maybe.Maybe`                                 |
|   4,073 | `Prim.Array`                                       |
|   3,931 | `Data.Generic.Rep.Constructor`                     |
|   3,675 | `Data.Unit.Unit`                                   |
|   3,496 | `Prim.Int`                                         |
|   2,951 | `Prim.Boolean`                                     |
| ... |

Notice how much more diverse this is — 3,642 vs 337 keys. The Prim.\*
constructors dominate at the very top, but the long tail is much
fatter than the class Maps.

### dataConstructors (Types.infer') — 34,956 lookups (2.4% of total)

2,850 distinct constructors. Top-25 cover 75.5%, but absolute volume
is small — this site is not worth attacking on its own.

## What the survey rules in / out

**In**:

- HashMap migration on `typeClasses` and `typeClassDictionaries`
  (the two class-keyed Maps). 1.17M lookups, ~337 keys each,
  shared key shape `Qualified (ProperName 'ClassName)`. Tons of
  log-factor compares to remove.
- HashMap migration on `types` (3642 keys, broader distribution
  also benefits more from O(1) vs O(log n)).

**Mostly out**:

- Closed-set fast-path on top class names. Even the most aggressive
  25-arm pattern won't catch more than 60–66% of lookups, and the
  remaining 34–40% still pay the Map cost. A 5-arm fast-path
  (HasField+IsSymbol+Cons+TestHasLabelRL+RowToList) covers 38%
  of typeClasses lookups but adds maintenance overhead for a
  ~3% of-build-time prize.
- `dataConstructors` migration as a standalone experiment — too
  little volume.

**Caveat — the "Hashable instance trap":** the type-hash
LESSONS.md entry shows that `Hashable (Type a)` without `INLINE`
on `hash`/`hashWithSalt` measured **+102% on full builds**. The
same risk applies to any new Hashable instance we add for
`Qualified`, `ProperName`, or `PSString`. The migration must mark
these methods INLINE or it's a regression.

## Verdict

Survey done; recommends `env-hashmap` as the next experiment.
Specifically: migrate `typeClasses`, `typeClassDictionaries`, and
`types` to HashMap. The first two get the bulk of the win; the
third is a "while we're at it" addition since the same Hashable
instances apply.

## Numbers

| Site                              | lookups | distinct keys | top-25 coverage |
|-----------------------------------|--------:|--------------:|----------------:|
| typeClassDictionaries (findDicts) | 794,706 | 337           | 58.5%           |
| typeClasses (solve)               | 375,232 | 337           | 66.0%           |
| types (Kinds)                     | 237,038 | 3,642         | 58.1%           |
| dataConstructors (infer')         |  34,956 | 2,850         | 75.5%           |
| **total**                         | **1,448,269** |         |                 |

`Data.Record.HasField` alone accounts for 163,554 lookups (11.3%
of all hooked lookups) — a single dictionary class that the
solver hits over and over as records' fields are queried.
