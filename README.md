# FES-CF: financial tail-risk surface forecasting

R research code for **Forecasting Forward Financial Tail-Risk Surfaces: Guarded Functional Models and Conformal Bands**, by **Çağlar Sözen** and **Mervenur Sözen**.

**Code version: 3.2.0.** This repository contains the analysis source, supplied results and execution records. Repository: [caglarsozen/fes-cf](https://github.com/caglarsozen/fes-cf). No separate tagged release or DOI is claimed. [Türkçe açıklama](README_TR.md).

## Scope

Functional Extreme-Surface Conformal Forecasting (FES-CF) forecasts cross-sectional quantiles of forward cumulative absolute returns. The primary response is a 30-cell surface: five horizons `{1, 2, 3, 5, 10}` and six quantiles `{0.800, 0.850, 0.900, 0.925, 0.950, 0.975}`. The application uses a fixed set of 38 ETFs/equities over 2010–2025, with lagged VIX predictors.

The pipeline includes purged chronological evaluation, fold-local FPCA and graph transformations, functional and simpler benchmarks, guard-selected residual corrections, Dykstra shape projection, and conformal-band comparisons. See [method changes](docs/METHOD_CHANGES.md) and [reproduction requirements](docs/REPRODUCIBILITY.md).

## What can be reproduced from this bundle?

| Task | Availability |
|---|---|
| Inspect the exact analysis source, figures, rounded result tables and current execution audits | Included |
| Re-run the supplied synthetic mechanism tests | Included; install the R dependencies first |
| Check bundled scientific-file hashes | Included; Python standard library only |
| Re-run the full financial experiment | Requires the original frozen input files described below |
| Independently reproduce every reported number in the original software environment | Not yet established; current run configuration/session files and full forecast/loss arrays remain outside this bundle |

The analysis source is unchanged from the supplied V3.2.0 run:

```text
cdf61c669c40afb85bf875247144136b87753a5d0a90ab2f15861856de035117
```

The source header's instruction to regenerate tables refers to the earlier method revision from V3.1.5 to V3.2.0. It does not require a new model run merely to publish the already supplied V3.2.0 results.

## Installation

Open a terminal in this repository. The stored synthetic validation used **R 4.3.3** and **glmnet 4.1-8**; its full environment is recorded in [the validation log](docs/validation/synthetic_validation_29.txt). These are not asserted to be the financial run's package versions.

```bash
Rscript scripts/install_dependencies.R
```

The 15 direct dependencies are listed in [config/required_packages.txt](config/required_packages.txt). The installer adds missing packages from CRAN; it does not create an exact historical environment or upgrade packages that are already installed. No empirical `renv.lock` is supplied because the corresponding session information has not been provided.

## Quick checks without the financial data

```bash
python3 scripts/verify_artifacts.py
Rscript tests/FES_CF_V3_2_0_SELF_TEST.R analysis/FES_CF_V3_2_0_FULL.R
```

On Windows, use `py -3` instead of `python3` if appropriate. The synthetic test exercises the mechanisms on generated data. Its archived record contains **29 checks** and is separate from the real-data audit. The included GitHub Actions workflow performs **file-integrity checks only**; it does not download market data or run the financial experiment. Its hosted execution status is available on the repository [Actions page](https://github.com/caglarsozen/fes-cf/actions); the archived synthetic validation is independent of that workflow.

## Run the financial pipeline

The canonical entry point is [analysis/FES_CF_V3_2_0_FULL.R](analysis/FES_CF_V3_2_0_FULL.R). It uses the **original frozen prior-run snapshot**, with these three files under a single input directory:

| Relative input path | Required content |
|---|---|
| `rds/config.rds` | Prior configuration matching the fixed tickers, dates and VIX symbol |
| `cache/adjusted_prices_primary_universe.rds` | Complete cached primary-universe price object |
| `cache/vix_adjusted.rds` | Cached VIX price object |

These input files are **not included**. The loader does not refresh Yahoo Finance observations. A new provider download may differ from the original adjusted-price snapshot. File formats and the remaining archival requirements are described in [REPRODUCIBILITY.md](docs/REPRODUCIBILITY.md).

First check paths, configuration and package availability. Replace both example paths with your own:

```bash
Rscript scripts/run_pipeline.R "/path/to/frozen_previous_output" "/path/to/new_v320_output" --check-only
```

Then start the full experiment explicitly:

```bash
Rscript scripts/run_pipeline.R "/path/to/frozen_previous_output" "/path/to/new_v320_output"
```

The wrapper calls the unchanged canonical source. Its preflight checks do not validate all cache values or reproduce input hashes; the source performs its additional cache, runtime and checkpoint checks. The output directory must differ from the input directory. Existing completed checkpoints can resume only when the recorded code/data/settings/runtime identities agree. An unfinished split restarts from its beginning.

In R/RStudio, the equivalent call is:

```r
options(
  fescf.previous_output_dir = "C:/research/frozen_previous_output",
  fescf.output_dir = "C:/research/new_v320_output"
)
source("analysis/FES_CF_V3_2_0_FULL.R", encoding = "UTF-8")
```

The main script runs inside a private function and restores options and the random seed. It creates `csv`, `tex`, `figs`, `rds`, `audit`, `meta`, `runtime`, `cache` and `checkpoints` output directories. A completed run does not need retraining merely to collect existing files; [COLLECT_EXISTING_V320_OUTPUTS.R](scripts/COLLECT_EXISTING_V320_OUTPUTS.R) collects existing outputs.

## Included results and interpretation

- [Rounded result exports](results/rounded_exports/): 46 CSVs transcribed from supplied LaTeX tables. They retain **displayed precision**, not the original full-precision result arrays.
- [Current execution audits](results/current_run_audit/): seven original audit files, with canonical filenames. The runtime log is [results/runtime_log.csv](results/runtime_log.csv).
- [Figures](figures/): eight figures; F7 also has a vector PDF. F7 was redrawn from rounded A20 percentages for readability, with both axes on the weighted-MSE reduction scale. F1/F6 show raw W-RMSE, whereas the tables multiply it by 100.

The reported main-holdout W-RMSE ×100 is **3.723** for the equal-weight combination and **3.750** for FES-CF. The simpler global residual-correction ablation achieves **3.674**. FES-CF's pooled rolling comparison with the rolling mean has an interval including zero. Main global surface coverage of **97.30%** coexists with **43.33%** coverage in the initial 2020 rolling stress episode. These outcomes support a competitive empirical architecture with regime dependence; they do not establish uniform predictive superiority, demonstrated economic utility or exact conformal validity under arbitrary dependence.

The supplied publication audit reports **24/24 true flags**. A separate review performed **67 log/output reconciliation checks**, including 6,902 date boundaries, 1,860 fold-local preprocessing records and 580 unchanged guard-to-refit penalties. See [VALIDATION.md](docs/VALIDATION.md) for the scope and limitations. These checks are not an independent refit of the financial experiment.

## Citation and license

[CITATION.cff](CITATION.cff) contains the software authors, version and repository URL. Please cite this software and the companion manuscript when using the work, and record the actual commit used. No DOI or formal release date is supplied because neither has been issued.

The software and accompanying documentation are offered under the **MIT License**, reproduced in [LICENSE](LICENSE). Third-party market observations and dependencies retain their own applicable terms. The repository does not redistribute the frozen provider data.

## Contributing

For corrections or extensions, see [CONTRIBUTING.md](CONTRIBUTING.md). Changes to the scientific source or results require a new version and corresponding validation; they should not silently replace the archived V3.2.0 evidence.
