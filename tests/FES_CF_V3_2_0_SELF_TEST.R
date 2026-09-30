# Synthetic validation only. Does not read market data or execute the full study.
# Rscript FES_CF_V3_2_0_SELF_TEST.R FES_CF_V3_2_0_FULL.R
args <- commandArgs(trailingOnly = TRUE)
code_file <- if (length(args)) args[1] else "FES_CF_V3_2_0_FULL.R"
if (!file.exists(code_file)) stop("Cannot find pipeline R file: ", code_file)
required <- c("quantmod", "xts", "zoo", "dplyr", "tidyr", "purrr", "tibble",
              "ggplot2", "glmnet", "knitr", "readr", "stringr", "scales", "digest", "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages first: ", paste(missing, collapse = ", "))
suppressPackageStartupMessages(invisible(lapply(required, library, character.only = TRUE)))
parsed <- parse(code_file, encoding = "UTF-8")
pipeline <- eval(parsed[[1]][[1]])
stopifnot(is.function(pipeline))
env <- new.env(parent = globalenv())
nodes <- as.list(body(pipeline))[-1L]
for (node in nodes) {
  if (is.call(node) && identical(node[[1]], as.name("<-")) && is.symbol(node[[2]])) {
    name <- as.character(node[[2]])
    if ((is.call(node[[3]]) && identical(node[[3]][[1]], as.name("function"))) ||
        name %in% c("cfg", "asset_dictionary")) eval(node, env)
  }
}
env$audit_env <- new.env(parent = emptyenv())
slots <- c("runtime", "tuning", "fpca", "projection", "leakage", "warnings", "preprocessing")
for (nm in slots) env$audit_env[[nm]] <- tibble::tibble()
cfg <- env$cfg
cfg$ridge_lambda_grid <- 10^seq(-5, 3, length.out = 80)
cfg$blocked_cv_k <- 3L
cfg$glmnet_min_common_count <- 20L
cfg$h_grid <- c(1, 3)
cfg$tau_grid <- c(0.9, 0.975)
cfg$fpca_k_max <- 3L
cfg$top3_simplex_step <- 0.25
cfg$secondary_blend_grid <- c(0, 0.25, 0.5)
cfg$residual_lambda_grid <- c(0, 0.25, 0.5)
cfg$bootstrap_reps <- 19L
cfg$mcs_reps <- 19L
cfg$output_dir <- tempfile("fescf-synthetic-")
for (d in c("", "checkpoints", "audit", "csv", "tex", "figs", "rds", "meta", "runtime")) {
  dir.create(file.path(cfg$output_dir, d), recursive = TRUE, showWarnings = FALSE)
}
env$cfg <- cfg
env$implementation_hash <- digest::digest(paste(deparse(body(pipeline), width.cutoff = 500L),
  collapse = "\n"), algo = "sha256", serialize = FALSE)
env$runtime_signature <- list(R = R.version.string, glmnet = as.character(packageVersion("glmnet")))
checks <- character()
check <- function(label, condition) {
  if (!isTRUE(condition)) stop("FAIL: ", label)
  checks <<- c(checks, label)
  cat("PASS:", label, "\n")
}
check("Complete R file parses", TRUE)
saved_user_options <- options(fescf.previous_output_dir = tempfile("missing-frozen-inputs-"),
                              fescf.output_dir = tempfile("unused-output-"))
set.seed(1414)
saved_seed <- .Random.seed
saved_global_cfg <- cfg
missing_input_error <- try(pipeline(), silent = TRUE)
check("Missing frozen inputs stop before fitting or data download", inherits(missing_input_error, "try-error") &&
        grepl("Frozen inputs not found", as.character(missing_input_error)))
check("Early stop preserves existing workspace objects and RNG", identical(cfg, saved_global_cfg) &&
        identical(.Random.seed, saved_seed))
options(saved_user_options)
set.seed(91327)
n <- 720L
dates <- as.Date("2015-01-01") + seq_len(n)
tickers <- cfg$tickers[1:6]
returns <- matrix(rnorm(n * length(tickers), sd = 0.01), n, length(tickers))
returns <- returns * (0.8 + 0.4 * sin(seq_len(n) / 19)^2)
prices <- xts::xts(100 * exp(apply(returns, 2, cumsum)), order.by = dates)
colnames(prices) <- tickers
vix <- xts::xts(20 + 3 * sin(seq_len(n) / 17) + runif(n), order.by = dates)
colnames(vix) <- "VIX"
design <- env$build_design(prices, vix, cfg, tickers, design_name = "synthetic")
check("Synthetic design has finite surfaces and features",
      all(is.finite(design$Y)) && all(is.finite(design$X)) && design$J == 4L)
tr <- 1:210
va <- 225:270
mutated <- design
mutated$Y[va, ] <- mutated$Y[va, ] + 999
for (kind in c("fpca", "graph")) {
  a <- env$learn_surface_transform(design, tr, va, cfg, kind)
  b <- env$learn_surface_transform(mutated, tr, va, cfg, kind)
  check(paste(kind, "transform ignores validation outcomes"), identical(a, b))
  c <- design
  c$P[-tr, ] <- c$P[-tr, ] * 17 + 11
  changed <- env$learn_surface_transform(c, tr, va, cfg, kind)
  check(paste(kind, "training representation ignores future predictor rows"),
        identical(a$Xtr, changed$Xtr) && identical(a$response, changed$response) &&
        identical(a$basis, changed$basis))
  pred <- env$fit_nested_surface_ridge(design, 1:320, 340:359, cfg, "unit", kind)
  check(paste(kind, "nested CV returns finite surface predictions"),
        identical(dim(pred), c(20L, 4L)) && all(is.finite(pred)))
}
check("All logged transform folds satisfy response-date purging",
      all(env$audit_env$preprocessing$zero_overlap))
bad_cv <- try(env$make_purged_cv_folds(210, 3, 0, design$dates[tr],
                                     design$label_end_dates[tr], "unit", "bad-purge"), silent = TRUE)
check("Leaking chronological split is rejected", inherits(bad_cv, "try-error"))
# Expected negative test is removed from the later integration audit.
env$audit_env$leakage <- dplyr::filter(env$audit_env$leakage, zero_overlap)
guard <- 225:270
res_fit <- design$Y[tr, , drop = FALSE] - design$P[tr, , drop = FALSE]
res_guard <- design$Y[guard, , drop = FALSE] - design$P[guard, , drop = FALSE]
tuned <- env$tune_residual_guard(design, tr, res_fit, guard, res_guard, cfg, "unit", "residual")
saved_cv <- env$fit_multi_ridge
env$fit_multi_ridge <- function(...) stop("Residual refit must not call CV")
fixed <- env$fit_residual_ridge(design, tr, res_fit, 300:305, cfg, "unit", "fixed", tuned$selected_lambda)
check("Residual final refit carries guard lambda without invoking CV", all(is.finite(fixed)))
env$fit_multi_ridge <- saved_cv
small <- 1:65
small_tuned <- env$tune_residual_guard(design, small,
  design$Y[small, , drop = FALSE] - design$P[small, , drop = FALSE], guard,
  res_guard, cfg, "unit", "small-state")
check("Eligible residual state with 65 fit observations works", all(is.finite(small_tuned$prediction)))
zero <- matrix(0, length(tr), 4)
constant <- env$ridge_path_predict(design$X[tr, , drop = FALSE], zero,
  design$X[guard, , drop = FALSE], cfg$ridge_lambda_grid, cfg, "unit", "constant")
check("Zero residual response produces exact zero predictions", all(constant$prediction == 0))
one <- env$predict_fixed_ridge(design$X[tr, , drop = FALSE], res_fit[, 1, drop = FALSE],
  design$X[300, , drop = FALSE], 1, cfg, "unit", "one-row-one-output")
check("One-row / one-response ridge prediction retains matrix dimensions", identical(dim(one), c(1L, 1L)))
bad_path <- try(env$check_candidate_path(tibble::tibble(lambda = 1:24,
    mean_loss = 1:24, se_loss = 0), 1:24, TRUE, cfg, "bad-edge", FALSE), silent = TRUE)
check("Truncated weak-penalty optimum is rejected", inherits(bad_path, "try-error"))
nonmonotone <- matrix(c(-1, 3, 1, -2, 5, 1, 4, 2), 2, 4, byrow = TRUE)
projected <- env$project_surface_matrix(nonmonotone, design$cell_grid, cfg, "unit", "shape", "test")
shape <- env$surface_violation_summary(projected, design$cell_grid, cfg$projection_tol)
check("Dykstra yields nonnegative bimonotone surfaces within tolerance", shape$max_raw_violation <= cfg$projection_tol)
tiny <- matrix(c(1, 1 - 1e-12, 2, 3), 1, 4)
check("A3 treats sub-tolerance floating-point violations as zero",
      env$surface_violation_summary(tiny, design$cell_grid, cfg$projection_tol)$tau_violation_count == 0)
check("A3 retains the unrounded maximum raw violation",
      env$surface_violation_summary(tiny, design$cell_grid, cfg$projection_tol)$max_raw_violation > 0)

cat("Starting one synthetic split with anchors, external benchmarks, top-3, ablations, conformal and inference...\n")
sp <- list(scheme = "synthetic_holdout", train_idx = 1:410, cal_idx = 425:524,
           test_idx = 539:620, embargo = cfg$embargo)
result <- env$run_split(design, sp, cfg, TRUE, TRUE, TRUE, TRUE)
check("Synthetic integrated split includes all nine primary models", nrow(result$performance) == 9L)
check("Synthetic integrated split includes conformal output", !is.null(result$conformal))
check("Synthetic integrated split includes top-three sensitivity", !is.null(result$fescf$top3))
check("Synthetic integrated split includes stationarity diagnostics", nrow(result$stationarity) >= 2L)
check("Integrated split has finite model losses", all(is.finite(result$performance$Weighted_RMSE)))
check("Integrated split has no above-tolerance post-projection violations",
      all(result$projection_effect$post_max_raw_violation <= cfg$projection_tol))
check("Integrated split leakage audits pass", all(env$audit_env$leakage$zero_overlap))
# Force a failure if resume tries to refit. The actual cached result must be exact.
uncached <- env$run_split_uncached
env$run_split_uncached <- function(...) stop("Checkpoint hit should not fit")
cached <- env$run_split(design, sp, cfg, TRUE, TRUE, TRUE, TRUE)
check("Completed split resumes without fitting and returns identical result", identical(result, cached))
cfg_changed <- cfg
cfg_changed$fpca_pve <- 0.99
stale <- try(env$run_split(design, sp, cfg_changed, TRUE, TRUE, TRUE, TRUE), silent = TRUE)
check("Changed settings cannot reuse stale checkpoint", inherits(stale, "try-error"))
env$run_split_uncached <- uncached
cat("\n", length(checks), " synthetic checks passed.\n", sep = "")
cat("Actual market-data full run: NOT EXECUTED by this test.\n")
cat("Pipeline SHA256:", digest::digest(file = code_file, algo = "sha256", serialize = FALSE), "\n")
print(sessionInfo())
