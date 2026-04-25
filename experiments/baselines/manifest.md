# Baseline binary manifest

Each row records a pre-built baseline `purs` binary stored at
`experiments/baselines/<short-sha>/purs` (not committed — the binary
is ~170 MB, rebuild from the SHA with `exp build-baseline <sha>`).

Experiments reference baselines by `short-sha` in their
`EXPERIMENT.md` frontmatter (`baseline_sha:` field).

| Short SHA | Branch at build | Built (UTC)          | GHC   | Stack resolver | Built by | Notes                  |
| --------- | --------------- | -------------------- | ----- | -------------- | -------- | ---------------------- |
| 799e8208 | restaumatic | 2026-04-25 09:09 UTC | The Glorious Glasgow Haskell Compilation System, version 9.6.6 | lts-22.43 | user | new |
| e0125163 | e0125163 | 2026-04-17 09:35 UTC | The Glorious Glasgow Haskell Compilation System, version 9.6.6 | lts-22.43 | user | new |
| ebb0a6bb | ebb0a6bb | 2026-04-16 20:27 UTC | The Glorious Glasgow Haskell Compilation System, version 9.6.6 | lts-22.43 | user | new |
| 3fcac773  | restaumatic     | 2026-04-15 19:11 UTC | 9.6.6 | lts-22.43      | user     | current restaumatic tip |

_Entries sorted most-recent-first._

