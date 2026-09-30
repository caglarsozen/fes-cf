# Reproduction and data provenance

## Frozen inputs

The V3.2.0 pipeline reads a prior frozen snapshot. It does not obtain replacement prices through a fresh Yahoo Finance request. The input directory needs:

1. `rds/config.rds`, an R list whose `tickers`, `start_date`, `end_date` and `vix_symbol` exactly match the canonical configuration.
2. `cache/adjusted_prices_primary_universe.rds`, an R list with a `prices` xts object containing the required 38 ticker columns, and no failed ticker entries.
3. `cache/vix_adjusted.rds`, a corresponding list with an xts `prices` object whose column is `^VIX`, and no failed symbol entries. The pipeline subsequently renames the selected series to `VIX` internally.

The source checks unique sorted dates, the declared sample range and finite observations for every required series. It hashes the cached input files, copies them into the new run output and writes the current input hashes. Do not substitute an arbitrary data frame, a bare xts object or a newly downloaded series and label it the original snapshot.

The configured sample is 2010-01-01 to 2025-12-31. The 38 fixed tickers are:

```text
SPY QQQ IWM DIA GLD SLV TLT IEF SHY LQD HYG
XLF XLK XLE XLV XLP XLU XLY XLI XLB VNQ EEM EFA
AAPL MSFT NVDA META TSLA AMZN JPM BAC XOM CVX
JNJ WMT PG V MA
```

This fixed study universe is not a historical-membership reconstruction or proof of preregistration. The paper retains the corresponding survivorship/composition limitation.

## Environment and checkpoints

The source records R/package/runtime information and combines it with code, data and configuration identities. An existing output directory is accepted for resumption only if the run identity agrees. A new software environment can therefore invalidate checkpoint reuse; this is intentional protection against mixing results.

The package list names direct dependencies only. The archived synthetic test ran with R 4.3.3 and glmnet 4.1-8. Those versions must not be presented as the current financial-data run environment. Its `meta/session-info.txt`, configuration and frozen-input hash record have not yet been supplied to the preparation bundle. No exact `renv.lock` has been invented.

## Included evidence

- `results/rounded_exports/`: 46 displayed-precision exports, suitable for inspecting reported numbers; insufficient to repeat bootstrap or MCS calculations exactly.
- `results/current_run_audit/`: the original seven distinct current audit logs, with upload suffixes removed from two filenames. Bytes are preserved.
- `results/runtime_log.csv`: the 673-row runtime log, byte-identical to the subsequently supplied progress-runtime file. Nested timers should not be added to infer wall-clock duration.
- `docs/validation/synthetic_validation_29.txt`: prior synthetic source validation, explicitly not the market-data run.
- `PROVENANCE_SHA256.csv`: hashes of the bundled source, tests, results, figures and validation records. This is an integrity record, not an authentication signature.

The primary-test projection summary and the full projection-call log have different scopes. The former's maximum raw deviation is 1.6653345369377348e-16; the latter also covers guard/refit/sensitivity calls and reaches 2.220446049250313e-16. Both are below the configured 1e-10 tolerance.

## Completing an exact-reproduction archive

Retain the completed run's configuration, `meta/session-info.txt`, `meta/frozen_input_sha256.txt`, permitted frozen caches, full forecast/loss arrays and detailed delayed-feedback ACI path. They should be tied to the versioned software and actual repository/release identifier. A cache-access arrangement should respect the source data's applicable terms.

Collecting these existing files does not require refitting models. The optional `scripts/COLLECT_EXISTING_V320_OUTPUTS.R` archives existing audit/meta/csv/runtime/rds folders and excludes cache/checkpoint folders. It does not by itself gather the frozen caches, validate all audit content or prove exact replication.

The code repository is [caglarsozen/fes-cf](https://github.com/caglarsozen/fes-cf). No release DOI, public frozen-data link or successful independent raw-data rerun is asserted.
