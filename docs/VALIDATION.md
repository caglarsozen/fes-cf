# Validation scope

## Current execution records

The audit uploads received on 30 September 2026 were reconciled with the unchanged V3.2.0 source and supplied result exports. Seven distinct audit files are included under `results/current_run_audit/`; five duplicate progress copies are retained in the separate manuscript revision package and are not counted as additional evidence.

| Evidence | Observed result |
|---|---|
| Publication audit | 24/24 emitted flags true; check names match the source |
| Separate log/output review | 67 reconciliation checks passed |
| Leakage records | 6,902 response-end dates strictly precede the later forecast origin |
| Fold-local preprocessing | 1,860 rows: 930 FPCA and 930 graph; fold numbering, counts, dates and paired fit-index hashes agree |
| FPCA audit | 186 records reconcile with all 1,860 fields of the A30 export, allowing its documented formatting |
| Ridge audit | 1,188 anchor/cellwise CV records, 590 guard selections and 580 final residual refits |
| Penalty carry-over | All 580 final residual penalties agree with the preceding matching guard selection |
| Numerical paths | 34–80 eligible common penalties; no path marked numerically truncated |
| Dykstra | 23,839 grouped calls report complete convergence; maximum eight iterations; all deviations below 1e-10 |

The 23,839 projection records are grouped calls that include guard and sensitivity operations, not a count of distinct test forecasts. Recorded index hashes were compared but could not be regenerated without the underlying indices. These checks support the implementation's recorded execution; they do not independently reconstruct predictions from the raw data.

Five displayed A30 penalties show 792.01640 for the full value 792.0164050192541. The source's six-decimal rounding followed by column formatting explains the last digit; an R-only formatting check reproduced all 186 displayed lambda values. No fitted value was modified.

## Synthetic validation

`tests/FES_CF_V3_2_0_SELF_TEST.R` extracts and tests the canonical functions on generated data. It covers fold-local transformations, chronological leakage rejection, residual penalty carry-over, projection tolerance and a small integrated split with checkpoint resumption. The archived record has 29 passing checks. It is a separate, previously executed source test, not a new empirical run or a result from GitHub Actions.

## Repository checks

`python3 scripts/verify_artifacts.py` checks the SHA-256/size manifest using only the Python standard library. The provided GitHub workflow runs this check. It neither fits models nor installs R dependencies. A passing integrity workflow says that the recorded files match; it is not evidence of forecasting superiority or complete independent replication.

During repository preparation on 30 September 2026, all R files parsed, the dependency list matched the canonical source, and the wrapper's help, missing-input, configuration-mismatch and output-path cases were exercised. A minimal synthetic file fixture passed the basic `--check-only` path without changing inputs or creating output; this did not validate real price values. The dependency helper reported the existing package versions without fitting models. Citation/workflow YAML and relative documentation links were checked. All 86 repository files were staged in a disposable Git repository, and all 67 scientific-file hashes remained unchanged with automatic line-ending conversion enabled. These local checks preceded upload to the public repository. They do not report a hosted workflow result; see the repository Actions page for that separate status.
