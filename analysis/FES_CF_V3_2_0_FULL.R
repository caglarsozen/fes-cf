# ============================================================
# FES-CF FULL PIPELINE V3.2.0 -- FOLD-LOCAL ANCHORS / GUARD RESIDUAL RIDGE
# Method revision: all empirical tables and figures must be regenerated.
# Original V3.1.5 is retained separately; no legacy fitted model is reused.
# Run: source(file.choose(), encoding = "UTF-8")
# Select this R file. Default input: previous output folder under getwd().
# Functional Extreme-Surface Conformal Forecasting
# Reviewer-driven revision pipeline
# ============================================================
# Core revision principles:
# 1) full horizon purging / embargo (primary e = h_max = 10);
# 2) purged chronological ridge CV;
# 3) explicit leakage audits at fit->guard, train->cal, cal->test;
# 4) systematic ablation and stronger benchmarks;
# 5) fine top-2 blending + top-3 simplex sensitivity;
# 6) exact-to-tolerance Dykstra projection onto bimonotone/nonnegative cone;
# 7) primary mb = 1.00, residual-based scaling, multiple conformal comparators;
# 8) NW lag 10 + block-bootstrap loss-difference intervals + HLN-style Model Confidence Set;
# 9) target/universe/hyperparameter robustness;
# 10) delayed-feedback adaptive conformal, runtime, manifest, checksums,
#     functional stationarity diagnostics, and strict publication audit.
# 11) strict 38/38 primary-universe integrity check;
# 12) genuine one-sided upper conformal sensitivity;
# 13) top-three blend kept outside primary ranking/MCS;
# 14) manuscript tables exported as LaTeX and figure labels sanitized;
# 15) cache integrity prevents partial-universe reuse and permits VIX-only download.
# 16) residual-correction rho upper-bound sensitivity extended to 1.00;
# 17) daily loss-difference table/figure, pooled rolling ablation, and direct rolling-mean comparison;
# 18) manuscript tuning-protocol table and scaled appendix accuracy tables.
# 19) VIX is mandatory: any VIX download or integrity failure aborts FULL run.
# 20) glmnet path is numerical-audit-safe: only common converged lambdas enter CV, and truncated lower-tail edge optima abort.
#
# IMPORTANT:
# - The primary target remains the cross-sectional quantile surface of
#   cumulative absolute returns, but alternative target definitions are
#   evaluated as robustness checks.
# - "High-risk" replaces "OOD" throughout.
# - No test outcome is used for model/feature/gate/tuning decisions.
# ============================================================

# Execute in a private function environment: open workspace objects are retained.
(function() {
old_options <- options()
had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
on.exit({
  options(old_options)
  if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
  else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    rm(".Random.seed", envir = .GlobalEnv)
}, add = TRUE)
options(stringsAsFactors = FALSE, scipen = 999, warn = 1)
set.seed(20260920)

# ============================================================
# Packages
# ============================================================

required_pkgs <- c(
  "quantmod", "xts", "zoo", "dplyr", "tidyr", "purrr", "tibble",
  "ggplot2", "glmnet", "knitr", "readr", "stringr", "scales",
  "digest", "jsonlite"
)

missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}
invisible(lapply(required_pkgs, require, character.only = TRUE))

# ============================================================
# Configuration
# ============================================================

cfg <- list(
  version = "V3.2.0-fold-local-anchors-guard-residual-ridge",
  run_profile = "FULL",
  strict_publication_audit = TRUE,
  full_run_required = TRUE,
  seed = 20260920,

  # Sample and fixed ex-ante universe
  start_date = "2010-01-01",
  end_date = "2025-12-31",
  tickers = c(
    "SPY", "QQQ", "IWM", "DIA", "GLD", "SLV", "TLT", "IEF", "SHY", "LQD", "HYG",
    "XLF", "XLK", "XLE", "XLV", "XLP", "XLU", "XLY", "XLI", "XLB", "VNQ", "EEM", "EFA",
    "AAPL", "MSFT", "NVDA", "META", "TSLA", "AMZN", "JPM", "BAC", "XOM", "CVX",
    "JNJ", "WMT", "PG", "V", "MA"
  ),
  vix_symbol = "^VIX",
  use_vix_feature = TRUE,
  require_vix_feature = TRUE,

  # Primary surface
  score_type = "absolute_return",
  h_grid = c(1, 2, 3, 5, 10),
  tau_grid = c(0.800, 0.850, 0.900, 0.925, 0.950, 0.975),
  quantile_type = 8,
  min_asset_coverage = 0.85,

  # Chronological evaluation
  main_train_frac = 0.60,
  main_cal_frac = 0.20,
  embargo = 10,                       # primary = h_max
  embargo_sensitivity = c(10, 15, 20),
  rolling_train = 1000,
  rolling_cal = 250,
  rolling_test = 120,
  rolling_step = 120,
  rolling_max_splits = 20,

  # Ridge / CV -- custom chronological purged CV
  ridge_lambda_grid = 10^seq(-5, 3, length.out = 80),
  blocked_cv_k = 5,
  cv_purge = 10,
  ridge_one_se_rule = TRUE,
  glmnet_maxit = 1000000L,
  glmnet_min_common_count = 20L,
  glmnet_edge_buffer = 2L,

  # Features / functional anchors
  rolling_mean_window = 22,
  har_windows = c(1, 5, 22),
  fpca_pve = 0.95,
  fpca_k_max = 8,
  graph_top_k = 3,
  graph_min_abs_cor = 0.05,

  # Train-only guard
  guard_frac = 0.30,
  guard_min_n = 80,
  min_guard_improvement = 0.00,

  # Blending
  secondary_blend_grid = seq(0, 0.50, by = 0.025),
  top3_simplex_step = 0.05,

  # Residual correction
  residual_lambda_grid = seq(0, 0.50, by = 0.05),
  residual_min_state_fit_n = 60,
  residual_min_state_guard_n = 35,

  # State definition
  state_probs = c(1 / 3, 2 / 3),
  state_terminal_weight = 0.50,

  # Projection
  projection_tol = 1e-10,
  projection_max_iter = 5000,

  # Conformal
  conformal_alpha = 0.10,
  conformal_min_state_cal_n = 40,
  primary_conformal_method = "global_split",
  primary_band_multiplier = 1.00,
  band_multiplier_grid = c(1.00, 1.05, 1.10, 1.15, 1.20, 1.30),
  conformal_alpha_grid = c(0.05, 0.10, 0.15),
  scale_epsilon = 1e-8,
  aci_gamma = 0.01,

  # Evaluation weights
  tail_weight_lambda = 4.0,
  terminal_weight_lambda = 1.0,

  # Inference
  dm_nw_lags = c(3, 5, 10, 15),
  primary_dm_lag = 10,
  bootstrap_reps = 999,
  bootstrap_block_length = 10,
  mcs_alpha = 0.10,
  mcs_reps = 499,

  # Revision modules
  run_bottom_up_har = TRUE,
  run_top3_sensitivity = TRUE,
  run_stationarity_diagnostic = TRUE,
  run_hyperparameter_sensitivity = TRUE,
  run_target_robustness = TRUE,
  run_universe_robustness = TRUE,
  run_grid_robustness = TRUE,

  # Output
  previous_output_dir = getOption("fescf.previous_output_dir",
    "out_fescf_v3_1_5_glmnet_path_safe_vix_strict_reviewer_compliance_final_full"),
  # Default: exactly the frozen input cache from the previous run; no Yahoo refresh.
  output_dir = getOption("fescf.output_dir", "out_fescf_v3_2_0_foldlocal_guardridge")
)

set.seed(cfg$seed)
implementation_hash <- digest::digest(paste(deparse(body(sys.function()), width.cutoff = 500L),
  collapse = "\n"), algo = "sha256", serialize = FALSE)
runtime_signature <- list(R = R.version.string, platform = R.version$platform,
  packages = setNames(vapply(required_pkgs, function(p) as.character(utils::packageVersion(p)),
    character(1)), required_pkgs), RNGkind = RNGkind(), BLAS = extSoftVersion()["BLAS"])
if (normalizePath(cfg$previous_output_dir, winslash = "/", mustWork = FALSE) ==
    normalizePath(cfg$output_dir, winslash = "/", mustWork = FALSE)) {
  stop("New output directory must differ from previous_output_dir.")
}
old_config_path <- file.path(cfg$previous_output_dir, "rds", "config.rds")
required_cache_names <- c("adjusted_prices_primary_universe.rds", "vix_adjusted.rds")
old_cache_paths <- file.path(cfg$previous_output_dir, "cache", required_cache_names)
if (!file.exists(old_config_path) || any(!file.exists(old_cache_paths))) {
  stop(paste0("Frozen inputs not found. Set the OLD output folder before rerunning:\n",
    'options(fescf.previous_output_dir = "C:/your/path/out_fescf_v3_1_5_glmnet_path_safe_vix_strict_reviewer_compliance_final_full")',
    "\nRequired: rds/config.rds and both cache/*.rds input files.\n",
    "Current working directory: ", getwd()))
}
old_config <- readRDS(old_config_path)
for (nm in c("tickers", "start_date", "end_date", "vix_symbol")) {
  if (!identical(cfg[[nm]], old_config[[nm]])) stop("Frozen input config mismatch: ", nm)
}
input_hashes <- setNames(vapply(old_cache_paths, function(p)
  digest::digest(file = p, algo = "sha256", serialize = FALSE), character(1)), required_cache_names)
run_identity <- digest::digest(list(implementation_hash, runtime_signature, cfg, input_hashes),
                                algo = "sha256")
identity_path <- file.path(cfg$output_dir, "meta", "run_identity.rds")
if (dir.exists(cfg$output_dir) && length(list.files(cfg$output_dir, all.files = FALSE)) > 0L) {
  if (!file.exists(identity_path) || !identical(readRDS(identity_path)$identity, run_identity)) {
    stop("Existing output belongs to different code/data/settings. Use a NEW fescf.output_dir.")
  }
}
message("FES-CF V3.2.0: frozen-cache run; output = ", cfg$output_dir)

# FULL is the only supported execution profile for the reviewer-safe build.
# The pipeline stops immediately if any major revision module is disabled.
if (isTRUE(cfg$full_run_required)) {
  required_full_flags <- c(
    run_bottom_up_har = cfg$run_bottom_up_har,
    run_top3_sensitivity = cfg$run_top3_sensitivity,
    run_stationarity_diagnostic = cfg$run_stationarity_diagnostic,
    run_hyperparameter_sensitivity = cfg$run_hyperparameter_sensitivity,
    run_target_robustness = cfg$run_target_robustness,
    run_universe_robustness = cfg$run_universe_robustness,
    run_grid_robustness = cfg$run_grid_robustness,
    use_vix_feature = cfg$use_vix_feature,
    require_vix_feature = cfg$require_vix_feature
  )
  if (!identical(cfg$run_profile, "FULL") || any(!required_full_flags)) {
    stop(
      "V3.1 reviewer-safe build requires FULL execution. Disabled module(s): ",
      paste(names(required_full_flags)[!required_full_flags], collapse = ", ")
    )
  }
}

# ============================================================
# Fixed universe metadata
# ============================================================

asset_dictionary <- tibble::tribble(
  ~ticker, ~asset_group, ~asset_class,
  "SPY",  "ETF", "US broad equity",
  "QQQ",  "ETF", "US growth/technology equity",
  "IWM",  "ETF", "US small-cap equity",
  "DIA",  "ETF", "US large-cap equity",
  "GLD",  "ETF", "Gold",
  "SLV",  "ETF", "Silver",
  "TLT",  "ETF", "Long-duration US Treasury",
  "IEF",  "ETF", "Intermediate US Treasury",
  "SHY",  "ETF", "Short-duration US Treasury",
  "LQD",  "ETF", "Investment-grade corporate bond",
  "HYG",  "ETF", "High-yield corporate bond",
  "XLF",  "ETF", "Financial sector equity",
  "XLK",  "ETF", "Technology sector equity",
  "XLE",  "ETF", "Energy sector equity",
  "XLV",  "ETF", "Health-care sector equity",
  "XLP",  "ETF", "Consumer-staples sector equity",
  "XLU",  "ETF", "Utilities sector equity",
  "XLY",  "ETF", "Consumer-discretionary sector equity",
  "XLI",  "ETF", "Industrials sector equity",
  "XLB",  "ETF", "Materials sector equity",
  "VNQ",  "ETF", "Real-estate equity",
  "EEM",  "ETF", "Emerging-market equity",
  "EFA",  "ETF", "Developed ex-US equity",
  "AAPL", "Equity", "Large-cap equity",
  "MSFT", "Equity", "Large-cap equity",
  "NVDA", "Equity", "Large-cap equity",
  "META", "Equity", "Large-cap equity",
  "TSLA", "Equity", "Large-cap equity",
  "AMZN", "Equity", "Large-cap equity",
  "JPM",  "Equity", "Large-cap equity",
  "BAC",  "Equity", "Large-cap equity",
  "XOM",  "Equity", "Large-cap equity",
  "CVX",  "Equity", "Large-cap equity",
  "JNJ",  "Equity", "Large-cap equity",
  "WMT",  "Equity", "Large-cap equity",
  "PG",   "Equity", "Large-cap equity",
  "V",    "Equity", "Large-cap equity",
  "MA",   "Equity", "Large-cap equity"
)

stopifnot(setequal(asset_dictionary$ticker, cfg$tickers))

# ============================================================
# Output folders
# ============================================================

for (d in c(
  "", "csv", "tex", "figs", "meta", "rds", "cache",
  "robustness", "audit", "runtime", "checkpoints"
)) {
  dir.create(file.path(cfg$output_dir, d), showWarnings = FALSE, recursive = TRUE)
}

saveRDS(list(identity = run_identity, implementation_sha256 = implementation_hash,
             runtime = runtime_signature, input_sha256 = input_hashes), identity_path)
for (i in seq_along(old_cache_paths)) {
  dest <- file.path(cfg$output_dir, "cache", required_cache_names[i])
  if (file.exists(dest)) {
    if (!identical(digest::digest(file = dest, algo = "sha256", serialize = FALSE), input_hashes[[i]])) {
      stop("Copied cache changed unexpectedly: ", dest)
    }
  } else if (!file.copy(old_cache_paths[i], dest, overwrite = FALSE)) {
    stop("Cannot copy frozen cache: ", old_cache_paths[i])
  }
}
writeLines(c(paste("code body SHA256:", implementation_hash),
             paste(names(input_hashes), input_hashes)),
           file.path(cfg$output_dir, "meta", "frozen_input_sha256.txt"))
saveRDS(cfg, file.path(cfg$output_dir, "rds", "config.rds"))

# ============================================================
# Logging environments
# ============================================================

audit_env <- new.env(parent = emptyenv())
audit_env$runtime <- tibble::tibble()
audit_env$tuning <- tibble::tibble()
audit_env$fpca <- tibble::tibble()
audit_env$projection <- tibble::tibble()
audit_env$leakage <- tibble::tibble()
audit_env$warnings <- tibble::tibble()
audit_env$preprocessing <- tibble::tibble()

append_log <- function(slot, row) {
  old <- audit_env[[slot]]
  audit_env[[slot]] <- dplyr::bind_rows(old, row)
  invisible(NULL)
}

warn_log <- function(module, message_text) {
  append_log(
    "warnings",
    tibble::tibble(
      timestamp = as.character(Sys.time()),
      module = module,
      message = as.character(message_text)
    )
  )
  warning(message_text, call. = FALSE)
}

timed <- function(label, scheme, expr_fun) {
  t0 <- proc.time()[["elapsed"]]
  out <- expr_fun()
  elapsed <- proc.time()[["elapsed"]] - t0
  append_log(
    "runtime",
    tibble::tibble(
      scheme = scheme,
      component = label,
      elapsed_seconds = as.numeric(elapsed)
    )
  )
  out
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# ============================================================
# General helpers
# ============================================================

make_clean_label <- function(x) {
  x <- as.character(x)
  x <- gsub("_", " ", x, fixed = TRUE)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

model_display_name <- function(x) {
  dplyr::case_when(
    x == "FES-CF full" ~ "FES-CF",
    x == "FES-CF top3 sensitivity" ~ "FES-CF top-3 sensitivity",
    x == "FPCA-X ridge" ~ "FPCA-X ridge",
    x == "Functional HAR-X ridge" ~ "Functional HAR-X ridge",
    x == "Graph Functional HAR-X ridge" ~ "Graph Functional HAR-X ridge",
    x == "Cell-by-cell HAR-X ridge" ~ "Cell-by-cell HAR-X ridge",
    x == "Bottom-up asset HAR" ~ "Bottom-up asset HAR",
    x == "Equal-weight anchor combination" ~ "Equal-weight anchor combination",
    x == "Rolling mean completed surface" ~ "Rolling mean completed surface",
    x == "Naive completed surface" ~ "Naive completed surface",
    TRUE ~ make_clean_label(x)
  )
}

cell_name <- function(h, tau) {
  paste0("h", h, "_tau", gsub("\\.", "p", sprintf("%.3f", tau)))
}

make_cell_grid <- function(cfg_local) {
  tidyr::expand_grid(h = cfg_local$h_grid, tau = cfg_local$tau_grid) |>
    dplyr::mutate(cell = purrr::map2_chr(h, tau, cell_name))
}

make_cell_weights <- function(cell_grid, cfg_local) {
  tau_scaled <- if (diff(range(cell_grid$tau)) > 0) {
    (cell_grid$tau - min(cell_grid$tau)) / diff(range(cell_grid$tau))
  } else {
    rep(0, nrow(cell_grid))
  }
  h_scaled <- cell_grid$h / max(cell_grid$h)
  w <- (1 + cfg_local$tail_weight_lambda * tau_scaled) *
    (1 + cfg_local$terminal_weight_lambda * h_scaled)
  w <- w / mean(w)
  names(w) <- cell_grid$cell
  w
}

safe_quantile <- function(x, p, type = 8) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(stats::quantile(
    x, probs = p, type = type, na.rm = TRUE, names = FALSE
  ))
}

weighted_mse_rows <- function(y, pred, weights) {
  y <- as.matrix(y)
  pred <- as.matrix(pred)
  stopifnot(all(dim(y) == dim(pred)))
  w <- as.numeric(weights[colnames(y)])
  if (any(!is.finite(w))) w <- rep(1, ncol(y))
  rowSums(t(t((y - pred)^2) * w), na.rm = TRUE) / sum(w)
}

rmse_all <- function(y, pred) {
  sqrt(mean((as.matrix(y) - as.matrix(pred))^2, na.rm = TRUE))
}

mae_all <- function(y, pred) {
  mean(abs(as.matrix(y) - as.matrix(pred)), na.rm = TRUE)
}

weighted_rmse_all <- function(y, pred, weights) {
  sqrt(mean(weighted_mse_rows(y, pred, weights), na.rm = TRUE))
}

terminal_rmse_all <- function(y, pred, cell_grid) {
  cc <- cell_grid$cell[cell_grid$h == max(cell_grid$h)]
  sqrt(mean(
    (as.matrix(y[, cc, drop = FALSE]) -
       as.matrix(pred[, cc, drop = FALSE]))^2,
    na.rm = TRUE
  ))
}

high_tau_rmse_all <- function(y, pred, cell_grid) {
  cc <- cell_grid$cell[cell_grid$tau >= 0.95]
  sqrt(mean(
    (as.matrix(y[, cc, drop = FALSE]) -
       as.matrix(pred[, cc, drop = FALSE]))^2,
    na.rm = TRUE
  ))
}

write_table_outputs <- function(df, name, caption, digits = 6, subdir = "") {
  out_csv <- file.path(cfg$output_dir, subdir, "csv")
  out_tex <- file.path(cfg$output_dir, subdir, "tex")
  dir.create(out_csv, recursive = TRUE, showWarnings = FALSE)
  dir.create(out_tex, recursive = TRUE, showWarnings = FALSE)

  readr::write_csv(df, file.path(out_csv, paste0(name, ".csv")))
  display_df <- df
  names(display_df) <- make_clean_label(names(display_df))
  latex_label <- gsub("_", "-", name, fixed = TRUE)
  tex <- knitr::kable(
    display_df,
    format = "latex",
    booktabs = TRUE,
    longtable = nrow(display_df) > 40L,
    digits = digits,
    caption = caption,
    label = latex_label,
    escape = TRUE
  )
  tex_file <- file.path(out_tex, paste0(name, ".tex"))
  writeLines(tex, tex_file)
  if (!file.exists(tex_file)) {
    stop("LaTeX table export failed: ", tex_file)
  }
}

theme_paper <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 2),
      plot.subtitle = ggplot2::element_text(size = base_size),
      axis.title = ggplot2::element_text(face = "bold"),
      legend.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
}

save_figure <- function(
  p, filename, width = 8.5, height = 5.2, dpi = 600,
  subdir = ""
) {
  out_fig <- file.path(cfg$output_dir, subdir, "figs")
  dir.create(out_fig, recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    file.path(out_fig, paste0(filename, ".png")),
    p, width = width, height = height, dpi = dpi
  )
  ggplot2::ggsave(
    file.path(out_fig, paste0(filename, ".tiff")),
    p, width = width, height = height, dpi = dpi,
    compression = "lzw"
  )
}

# ============================================================
# Exact-to-tolerance Dykstra projection
# Intersection of:
#  - rowwise nondecreasing tau cone,
#  - columnwise nondecreasing horizon cone,
#  - nonnegative orthant.
# ============================================================

pava_increasing <- function(x) {
  idx <- which(is.finite(x))
  out <- x
  if (length(idx) <= 1) return(out)
  out[idx] <- stats::isoreg(seq_along(idx), x[idx])$yf
  out
}

project_rows <- function(M) {
  out <- M
  for (i in seq_len(nrow(out))) out[i, ] <- pava_increasing(out[i, ])
  out
}

project_cols <- function(M) {
  out <- M
  for (j in seq_len(ncol(out))) out[, j] <- pava_increasing(out[, j])
  out
}

dykstra_surface_vector <- function(v, cell_grid, cfg_local) {
  nh <- length(unique(cell_grid$h))
  nt <- length(unique(cell_grid$tau))
  x <- matrix(as.numeric(v), nrow = nh, ncol = nt, byrow = TRUE)

  p1 <- matrix(0, nh, nt)
  p2 <- matrix(0, nh, nt)
  p3 <- matrix(0, nh, nt)

  converged <- FALSE
  it_used <- cfg_local$projection_max_iter

  for (it in seq_len(cfg_local$projection_max_iter)) {
    x_old <- x

    y1 <- x + p1
    z1 <- project_rows(y1)
    p1 <- y1 - z1
    x <- z1

    y2 <- x + p2
    z2 <- project_cols(y2)
    p2 <- y2 - z2
    x <- z2

    y3 <- x + p3
    z3 <- pmax(y3, 0)
    p3 <- y3 - z3
    x <- z3

    delta <- sqrt(sum((x - x_old)^2))
    base <- max(1, sqrt(sum(x_old^2)))
    feasibility_error <- max(c(0, -x,
      -unlist(lapply(seq_len(nh), function(i) diff(x[i, ]))),
      -unlist(lapply(seq_len(nt), function(j) diff(x[, j])))))
    if (is.finite(delta) && delta <= cfg_local$projection_tol * base &&
        feasibility_error <= cfg_local$projection_tol) {
      converged <- TRUE
      it_used <- it
      break
    }
  }

  # cell_grid is horizon-major (all tau values within each h);
  # flatten row-wise to preserve the original surface-cell ordering.
  z <- as.numeric(t(x))
  attr(z, "converged") <- converged
  attr(z, "iterations") <- it_used
  z
}

project_surface_matrix <- function(M, cell_grid, cfg_local, scheme = NA_character_,
                                   model = NA_character_, stage = NA_character_) {
  M <- as.matrix(M)
  out <- matrix(NA_real_, nrow(M), ncol(M))
  colnames(out) <- colnames(M)
  rownames(out) <- rownames(M)

  conv <- logical(nrow(M))
  its <- integer(nrow(M))

  for (i in seq_len(nrow(M))) {
    z <- dykstra_surface_vector(M[i, ], cell_grid, cfg_local)
    conv[i] <- isTRUE(attr(z, "converged"))
    its[i] <- as.integer(attr(z, "iterations") %||% NA_integer_)
    out[i, ] <- z
  }

  shape_check <- surface_violation_summary(out, cell_grid, cfg_local$projection_tol)
  append_log(
    "projection",
    tibble::tibble(
      scheme = scheme,
      model = model,
      stage = stage,
      n_rows = nrow(M),
      convergence_rate = mean(conv),
      max_raw_violation = shape_check$max_raw_violation,
      tolerance = cfg_local$projection_tol,
      max_iterations = max(its, na.rm = TRUE),
      mean_iterations = mean(its, na.rm = TRUE)
    )
  )

  if (any(!conv) || shape_check$max_raw_violation > cfg_local$projection_tol) {
    stop("Projection failed convergence/feasibility checks: ", scheme, " / ", model, " / ", stage)
  }
  if (any(!conv)) {
    warn_log(
      "projection",
      paste0(
        "Dykstra projection did not meet tolerance for ",
        sum(!conv), " row(s): ", scheme, " / ", model, " / ", stage
      )
    )
  }
  out
}

surface_violation_summary <- function(M, cell_grid, tol = 0) {
  M <- as.matrix(M)
  nh <- length(unique(cell_grid$h))
  nt <- length(unique(cell_grid$tau))

  if (any(!is.finite(M))) stop("Non-finite surface in shape diagnostic.")
  max_raw_violation <- 0
  neg_count <- 0L
  tau_count <- 0L
  h_count <- 0L
  neg_mag <- 0
  tau_mag <- 0
  h_mag <- 0

  for (i in seq_len(nrow(M))) {
    mat <- matrix(as.numeric(M[i, ]), nrow = nh, ncol = nt, byrow = TRUE)

    neg <- pmax(-mat, 0)
    max_raw_violation <- max(max_raw_violation, neg)
    neg_count <- neg_count + sum(neg > tol)
    neg_mag <- neg_mag + sum(neg[neg > tol])

    for (r in seq_len(nh)) {
      d <- diff(mat[r, ])
      max_raw_violation <- max(c(max_raw_violation, -d))
      tau_count <- tau_count + sum(d < -tol)
      tau_mag <- tau_mag + sum(abs(d[d < -tol]))
    }
    for (cc in seq_len(nt)) {
      d <- diff(mat[, cc])
      max_raw_violation <- max(c(max_raw_violation, -d))
      h_count <- h_count + sum(d < -tol)
      h_mag <- h_mag + sum(abs(d[d < -tol]))
    }
  }

  tibble::tibble(
    negative_count = neg_count,
    negative_magnitude = neg_mag,
    tau_violation_count = tau_count,
    tau_violation_magnitude = tau_mag,
    horizon_violation_count = h_count,
    horizon_violation_magnitude = h_mag,
    max_raw_violation = max_raw_violation
  )
}

# ============================================================
# Data download, caching, manifest, checksums
# ============================================================

# Input loader intentionally uses the previous frozen snapshot only.
download_adjusted_prices <- function(tickers, start_date, end_date, cache_name) {
  path <- file.path(cfg$output_dir, "cache", paste0(cache_name, ".rds"))
  if (!file.exists(path)) stop("Missing frozen cache: ", path)
  obj <- readRDS(path)
  if (is.null(obj$prices) || !inherits(obj$prices, "xts") ||
      !setequal(tickers, colnames(obj$prices)) ||
      length(obj$failed %||% character()) > 0L) {
    stop("Frozen cache is incomplete or has an unexpected format: ", path)
  }
  dates <- as.Date(zoo::index(obj$prices))
  if (!length(dates) || anyNA(dates) || anyDuplicated(dates) || is.unsorted(dates)) {
    stop("Frozen cache dates are invalid: ", path)
  }
  if (any(colSums(is.finite(obj$prices)) == 0L)) stop("All-missing cached symbol: ", path)
  if (min(dates) < as.Date(start_date) || max(dates) > as.Date(end_date)) {
    stop("Frozen cache lies outside the declared sample: ", path)
  }
  obj$cache_used <- TRUE
  obj
}

asset_dl <- download_adjusted_prices(
  cfg$tickers, cfg$start_date, cfg$end_date, "adjusted_prices_primary_universe"
)

# FINAL-FULL invariant: the prespecified primary universe must be complete.
# A missing Yahoo ticker changes the cross-sectional target itself, so the
# primary analysis is not allowed to continue with a silently reduced panel.
if (length(asset_dl$failed) > 0L) {
  stop(
    "Primary fixed universe incomplete. Failed ticker(s): ",
    paste(asset_dl$failed, collapse = ", ")
  )
}
if (!setequal(colnames(asset_dl$prices), cfg$tickers)) {
  missing_primary <- setdiff(cfg$tickers, colnames(asset_dl$prices))
  extra_primary <- setdiff(colnames(asset_dl$prices), cfg$tickers)
  stop(
    "Primary fixed-universe integrity check failed. Missing: ",
    ifelse(length(missing_primary) > 0L, paste(missing_primary, collapse = ", "), "none"),
    "; unexpected: ",
    ifelse(length(extra_primary) > 0L, paste(extra_primary, collapse = ", "), "none")
  )
}
prices_xts_full <- asset_dl$prices[, cfg$tickers, drop = FALSE]

vix_xts <- NULL
if (!isTRUE(cfg$use_vix_feature) || !isTRUE(cfg$require_vix_feature)) {
  stop(
    "FINAL-FULL V3.1.5 requires VIX features. ",
    "Both use_vix_feature and require_vix_feature must be TRUE."
  )
}

vix_dl <- try(
  download_adjusted_prices(
    cfg$vix_symbol, cfg$start_date, cfg$end_date, "vix_adjusted"
  ),
  silent = TRUE
)

if (inherits(vix_dl, "try-error")) {
  stop(
    "Required VIX download failed. FULL run aborted. Original error: ",
    as.character(vix_dl)
  )
}

if (length(vix_dl$failed %||% character(0)) > 0L) {
  stop(
    "Required VIX series failed Yahoo retrieval. FULL run aborted: ",
    paste(vix_dl$failed, collapse = ", ")
  )
}

if (is.null(vix_dl$prices) || !(cfg$vix_symbol %in% colnames(vix_dl$prices))) {
  stop(
    "Required VIX series is missing from downloaded prices. FULL run aborted."
  )
}

vix_xts <- vix_dl$prices[, cfg$vix_symbol, drop = FALSE]
colnames(vix_xts) <- "VIX"

if (NCOL(vix_xts) != 1L || !any(is.finite(as.numeric(vix_xts)))) {
  stop(
    "Required VIX series contains no usable finite observations. FULL run aborted."
  )
}

# Asset manifest: fixed universe, no replacement of failed/missing instruments.
asset_manifest <- asset_dictionary |>
  dplyr::left_join(asset_dl$manifest_download, by = "ticker") |>
  dplyr::mutate(
    requested_start = cfg$start_date,
    requested_end = cfg$end_date,
    prespecified_universe = TRUE,
    replacement_if_missing = FALSE,
    eligibility_rule = paste0(
      "Prespecified ticker; included when Yahoo download succeeds. ",
      "Cell retained only if >= ", sprintf("%.0f%%", 100 * cfg$min_asset_coverage),
      " of downloaded assets have finite cumulative scores."
    ),
    included_in_downloaded_panel = ticker %in% colnames(prices_xts_full)
  )

readr::write_csv(
  asset_manifest,
  file.path(cfg$output_dir, "csv", "asset_manifest.csv")
)

# ============================================================
# Surface construction
# ============================================================

score_matrix_from_returns <- function(ret_mat, score_type) {
  if (score_type == "absolute_return") {
    return(abs(ret_mat))
  }
  if (score_type == "downside_variation") {
    return(pmax(-ret_mat, 0))
  }
  if (score_type == "squared_return") {
    return(ret_mat^2)
  }
  stop("Unknown score_type: ", score_type)
}

construct_surfaces <- function(
  score_mat, return_dates, asset_names, cfg_local
) {
  n <- nrow(score_mat)
  p <- ncol(score_mat)
  cell_grid <- make_cell_grid(cfg_local)
  cell_names <- cell_grid$cell
  J <- nrow(cell_grid)
  Hmax <- max(cfg_local$h_grid)

  valid_t <- seq(Hmax, n - Hmax)
  Y <- matrix(NA_real_, nrow = length(valid_t), ncol = J)
  P <- matrix(NA_real_, nrow = length(valid_t), ncol = J)
  Nfuture <- matrix(NA_real_, nrow = length(valid_t), ncol = J)
  Npast <- matrix(NA_real_, nrow = length(valid_t), ncol = J)

  colnames(Y) <- colnames(P) <- colnames(Nfuture) <- colnames(Npast) <- cell_names

  threshold_n <- ceiling(cfg_local$min_asset_coverage * p)

  for (ii in seq_along(valid_t)) {
    t <- valid_t[ii]

    for (cc in seq_len(nrow(cell_grid))) {
      h <- cell_grid$h[cc]
      tau <- cell_grid$tau[cc]

      fut <- colSums(
        score_mat[(t + 1):(t + h), , drop = FALSE],
        na.rm = FALSE
      )
      pst <- colSums(
        score_mat[(t - h + 1):t, , drop = FALSE],
        na.rm = FALSE
      )

      ok_f <- is.finite(fut)
      ok_p <- is.finite(pst)

      Nfuture[ii, cc] <- sum(ok_f)
      Npast[ii, cc] <- sum(ok_p)

      if (sum(ok_f) >= threshold_n) {
        Y[ii, cc] <- safe_quantile(
          fut[ok_f], tau, type = cfg_local$quantile_type
        )
      }
      if (sum(ok_p) >= threshold_n) {
        P[ii, cc] <- safe_quantile(
          pst[ok_p], tau, type = cfg_local$quantile_type
        )
      }
    }
  }

  keep <- complete.cases(Y) & complete.cases(P)
  kept_t <- valid_t[keep]

  # Asset-level direct multi-horizon targets used by bottom-up HAR benchmark.
  future_asset_targets <- lapply(cfg_local$h_grid, function(h) {
    M <- matrix(
      NA_real_, nrow = length(kept_t), ncol = p,
      dimnames = list(NULL, asset_names)
    )
    for (ii in seq_along(kept_t)) {
      t <- kept_t[ii]
      M[ii, ] <- colSums(
        score_mat[(t + 1):(t + h), , drop = FALSE],
        na.rm = FALSE
      )
    }
    M
  })
  names(future_asset_targets) <- paste0("h", cfg_local$h_grid)

  list(
    dates = return_dates[kept_t],
    label_end_dates = return_dates[kept_t + Hmax],
    return_index = kept_t,
    Y = Y[keep, , drop = FALSE],
    P = P[keep, , drop = FALSE],
    Nfuture = Nfuture[keep, , drop = FALSE],
    Npast = Npast[keep, , drop = FALSE],
    future_asset_targets = future_asset_targets,
    cell_grid = cell_grid,
    cell_names = cell_names,
    J = J
  )
}

roll_mean_matrix <- function(M, width) {
  M <- as.matrix(M)
  out <- matrix(NA_real_, nrow(M), ncol(M))
  colnames(out) <- colnames(M)
  for (j in seq_len(ncol(M))) {
    out[, j] <- zoo::rollapplyr(
      M[, j], width = width, FUN = mean,
      fill = NA_real_, partial = FALSE, na.rm = TRUE
    )
  }
  out
}

align_lagged_vix <- function(dates, vix_xts) {
  if (is.null(vix_xts)) return(NULL)

  shell <- xts::xts(rep(NA_real_, length(dates)), order.by = dates)
  colnames(shell) <- "shell"
  m <- merge(shell, vix_xts, join = "left")
  v <- as.numeric(m[, "VIX"])
  v <- zoo::na.locf(v, na.rm = FALSE)
  v <- c(NA_real_, v[-length(v)])

  cbind(
    vix_lag1 = v,
    vix_ma5 = zoo::rollapplyr(v, 5, mean, fill = NA_real_, na.rm = TRUE),
    vix_ma22 = zoo::rollapplyr(v, 22, mean, fill = NA_real_, na.rm = TRUE)
  )
}

make_features <- function(P, dates, cell_grid, cfg_local, vix_xts = NULL) {
  P <- as.matrix(P)
  X_list <- list()

  X_list[["past_surface"]] <- P
  colnames(X_list[["past_surface"]]) <- paste0("past_", colnames(P))

  R22 <- roll_mean_matrix(P, cfg_local$rolling_mean_window)
  colnames(R22) <- paste0("roll22_", colnames(P))
  X_list[["rolling_mean_surface"]] <- R22

  for (w in cfg_local$har_windows) {
    Rw <- roll_mean_matrix(P, w)
    colnames(Rw) <- paste0("har", w, "_", colnames(P))
    X_list[[paste0("har_", w)]] <- Rw
  }

  tail_cells <- cell_grid$cell[cell_grid$tau >= 0.95]
  terminal_cells <- cell_grid$cell[cell_grid$h == max(cell_grid$h)]

  S <- cbind(
    surface_mean = rowMeans(P),
    tail_mean = rowMeans(P[, tail_cells, drop = FALSE]),
    terminal_mean = rowMeans(P[, terminal_cells, drop = FALSE]),
    surface_spread = apply(P, 1, max) - apply(P, 1, min)
  )
  X_list[["summaries"]] <- S

  for (w in cfg_local$har_windows) {
    Sw <- roll_mean_matrix(S, w)
    colnames(Sw) <- paste0("summary_har", w, "_", colnames(S))
    X_list[[paste0("summary_har_", w)]] <- Sw
  }

  V <- NULL
  if (isTRUE(cfg_local$use_vix_feature) && !is.null(vix_xts)) {
    V <- align_lagged_vix(dates, vix_xts)
    X_list[["vix"]] <- V
  }

  X <- do.call(cbind, X_list)
  X[!is.finite(X)] <- NA_real_

  list(X = X, vix_features = V)
}

make_asset_har_predictors <- function(
  score_mat, return_index, return_dates, surface_dates, vix_xts
) {
  n_ret <- nrow(score_mat)
  p <- ncol(score_mat)

  ma5 <- roll_mean_matrix(score_mat, 5)
  ma22 <- roll_mean_matrix(score_mat, 22)

  cur <- score_mat[return_index, , drop = FALSE]
  r5 <- ma5[return_index, , drop = FALSE]
  r22 <- ma22[return_index, , drop = FALSE]

  colnames(cur) <- colnames(score_mat)
  colnames(r5) <- colnames(score_mat)
  colnames(r22) <- colnames(score_mat)

  V <- align_lagged_vix(surface_dates, vix_xts)

  list(cur = cur, ma5 = r5, ma22 = r22, vix = V)
}

build_design <- function(
  prices_xts, vix_xts, cfg_local,
  universe_tickers = colnames(prices_xts),
  score_type = cfg_local$score_type,
  design_name = "primary"
) {
  universe_tickers <- intersect(universe_tickers, colnames(prices_xts))
  if (length(universe_tickers) < 5) {
    stop("Design ", design_name, " has fewer than five available instruments.")
  }

  px <- prices_xts[, universe_tickers, drop = FALSE]
  returns_xts <- diff(log(px))
  ret_mat <- as.matrix(returns_xts)
  return_dates <- as.Date(index(returns_xts))
  score_mat <- score_matrix_from_returns(ret_mat, score_type)

  surf <- construct_surfaces(
    score_mat, return_dates, colnames(px), cfg_local
  )

  feat <- make_features(
    surf$P, surf$dates, surf$cell_grid, cfg_local, vix_xts
  )

  ok <- complete.cases(feat$X) &
    complete.cases(surf$Y) &
    complete.cases(surf$P)

  future_targets <- lapply(
    surf$future_asset_targets,
    function(M) M[ok, , drop = FALSE]
  )

  design <- list(
    name = design_name,
    score_type = score_type,
    universe = universe_tickers,
    asset_group = asset_dictionary$asset_group[
      match(universe_tickers, asset_dictionary$ticker)
    ],
    dates = surf$dates[ok],
    label_end_dates = surf$label_end_dates[ok],
    return_index = surf$return_index[ok],
    Y = surf$Y[ok, , drop = FALSE],
    P = surf$P[ok, , drop = FALSE],
    X = feat$X[ok, , drop = FALSE],
    vix_features = if (is.null(feat$vix_features)) NULL else feat$vix_features[ok, , drop = FALSE],
    Nfuture = surf$Nfuture[ok, , drop = FALSE],
    Npast = surf$Npast[ok, , drop = FALSE],
    cell_grid = surf$cell_grid,
    cell_names = surf$cell_names,
    J = surf$J,
    weights = make_cell_weights(surf$cell_grid, cfg_local),
    future_asset_targets = future_targets
  )

  # Asset-level HAR predictors are aligned after the feature-complete filter.
  ah <- make_asset_har_predictors(
    score_mat = score_mat,
    return_index = surf$return_index,
    return_dates = return_dates,
    surface_dates = surf$dates,
    vix_xts = if (isTRUE(cfg_local$use_vix_feature)) vix_xts else NULL
  )
  design$asset_har <- list(
    cur = ah$cur[ok, , drop = FALSE],
    ma5 = ah$ma5[ok, , drop = FALSE],
    ma22 = ah$ma22[ok, , drop = FALSE],
    vix = if (is.null(ah$vix)) NULL else ah$vix[ok, , drop = FALSE]
  )

  design
}

primary_design <- timed(
  "build_primary_design",
  "global",
  function() build_design(
    prices_xts = prices_xts_full,
    vix_xts = vix_xts,
    cfg_local = cfg,
    universe_tickers = cfg$tickers,
    score_type = cfg$score_type,
    design_name = "primary"
  )
)

message("Primary design rows: ", nrow(primary_design$Y))
message("Primary design instruments: ", length(primary_design$universe))
message("Primary design cells: ", primary_design$J)

# Availability summary
cell_availability <- purrr::map_dfr(
  seq_along(primary_design$cell_names),
  function(j) {
    n_j <- primary_design$Nfuture[, j]
    tibble::tibble(
      cell = primary_design$cell_names[j],
      h = primary_design$cell_grid$h[j],
      tau = primary_design$cell_grid$tau[j],
      min_N = min(n_j, na.rm = TRUE),
      q25_N = safe_quantile(n_j, 0.25, type = 7),
      median_N = median(n_j, na.rm = TRUE),
      mean_N = mean(n_j, na.rm = TRUE),
      q75_N = safe_quantile(n_j, 0.75, type = 7),
      max_N = max(n_j, na.rm = TRUE),
      missing_cells = sum(!is.finite(n_j))
    )
  }
)


design_summary <- tibble::tibble(
  design = primary_design$name,
  score_type = primary_design$score_type,
  n_instruments = length(primary_design$universe),
  n_surface_rows = nrow(primary_design$Y),
  n_surface_cells = primary_design$J,
  n_features = ncol(primary_design$X),
  n_graph_augmented_features = ncol(primary_design$X) + primary_design$J,
  h_grid = paste(cfg$h_grid, collapse = ","),
  tau_grid = paste(cfg$tau_grid, collapse = ","),
  quantile_type = cfg$quantile_type,
  min_asset_coverage = cfg$min_asset_coverage,
  primary_embargo = cfg$embargo,
  cv_purge = cfg$cv_purge
)

tail_cell_resolution <- cell_availability |>
  dplyr::filter(tau >= 0.925) |>
  dplyr::transmute(
    h = h,
    tau = tau,
    median_cross_section_n = median_N,
    mean_cross_section_n = mean_N,
    approximate_upper_tail_count = median_N * (1 - tau),
    quantile_type = cfg$quantile_type
  )

compute_tail_asset_occupancy <- function(design, tau_cut = 0.925) {
  rows <- list()
  rr <- 1L

  for (h in unique(design$cell_grid$h)) {
    A <- design$future_asset_targets[[paste0("h", h)]]
    if (is.null(A)) next

    for (tau in unique(design$cell_grid$tau[design$cell_grid$tau >= tau_cut])) {
      cell <- design$cell_grid$cell[
        design$cell_grid$h == h &
          abs(design$cell_grid$tau - tau) < 1e-12
      ]
      if (length(cell) != 1) next

      threshold <- design$Y[, cell]
      hit_counts <- setNames(integer(ncol(A)), colnames(A))
      eligible_counts <- setNames(integer(ncol(A)), colnames(A))

      for (i in seq_len(nrow(A))) {
        v <- A[i, ]
        ok <- is.finite(v) & is.finite(threshold[i])
        eligible_counts[ok] <- eligible_counts[ok] + 1L
        hit <- ok & (v >= threshold[i])
        hit_counts[hit] <- hit_counts[hit] + 1L
      }

      rows[[rr]] <- tibble::tibble(
        h = h,
        tau = tau,
        ticker = names(hit_counts),
        tail_cell_hits = as.integer(hit_counts),
        eligible_origins = as.integer(eligible_counts),
        tail_occupancy_rate = ifelse(
          eligible_counts > 0,
          hit_counts / eligible_counts,
          NA_real_
        )
      )
      rr <- rr + 1L
    }
  }

  dplyr::bind_rows(rows) |>
    dplyr::left_join(
      asset_dictionary,
      by = "ticker"
    ) |>
    dplyr::arrange(
      dplyr::desc(tau),
      h,
      dplyr::desc(tail_occupancy_rate)
    )
}

tail_asset_occupancy <- compute_tail_asset_occupancy(primary_design)

# ============================================================
# Chronological splits, purging, and leakage audits
# ============================================================

make_main_split <- function(design, cfg_local, embargo = cfg_local$embargo) {
  n <- nrow(design$Y)
  n_train <- floor(cfg_local$main_train_frac * n)
  n_cal <- floor(cfg_local$main_cal_frac * n)

  cal_start <- n_train + embargo + 1
  cal_end <- cal_start + n_cal - 1
  test_start <- cal_end + embargo + 1

  if (test_start > n) stop("Insufficient observations for main split.")

  list(
    scheme = "main_holdout",
    train_idx = seq_len(n_train),
    cal_idx = cal_start:cal_end,
    test_idx = test_start:n,
    embargo = embargo
  )
}

make_rolling_splits <- function(design, cfg_local, embargo = cfg_local$embargo) {
  n <- nrow(design$Y)
  out <- list()
  start <- 1L
  k <- 1L

  repeat {
    tr_start <- start
    tr_end <- tr_start + cfg_local$rolling_train - 1
    cal_start <- tr_end + embargo + 1
    cal_end <- cal_start + cfg_local$rolling_cal - 1
    test_start <- cal_end + embargo + 1
    test_end <- test_start + cfg_local$rolling_test - 1

    if (test_end > n || k > cfg_local$rolling_max_splits) break

    out[[k]] <- list(
      scheme = sprintf("rolling_%02d", k),
      train_idx = tr_start:tr_end,
      cal_idx = cal_start:cal_end,
      test_idx = test_start:test_end,
      embargo = embargo
    )

    start <- start + cfg_local$rolling_step
    k <- k + 1L
  }

  out
}

guard_partition <- function(train_idx, cfg_local, purge = cfg_local$cv_purge) {
  n <- length(train_idx)
  g <- max(cfg_local$guard_min_n, ceiling(cfg_local$guard_frac * n))
  g <- min(g, n - purge - 80)

  if (g < cfg_local$guard_min_n) {
    stop("Training block too short for requested guard and purge.")
  }

  guard_start_pos <- n - g + 1
  fit_end_pos <- guard_start_pos - purge - 1

  list(
    fit_idx = train_idx[seq_len(fit_end_pos)],
    purge_idx = if (purge > 0) {
      train_idx[(fit_end_pos + 1):(guard_start_pos - 1)]
    } else integer(0),
    guard_idx = train_idx[guard_start_pos:n]
  )
}

audit_boundary <- function(
  design, earlier_idx, later_idx, scheme, boundary_name
) {
  latest_response <- max(design$label_end_dates[earlier_idx])
  first_later_origin <- min(design$dates[later_idx])
  ok <- latest_response < first_later_origin

  row <- tibble::tibble(
    design = design$name,
    scheme = scheme,
    boundary = boundary_name,
    earlier_n = length(earlier_idx),
    later_n = length(later_idx),
    latest_response_date_used = as.character(latest_response),
    first_later_forecast_origin = as.character(first_later_origin),
    zero_overlap = ok
  )
  append_log("leakage", row)

  if (!ok) {
    stop(
      "Leakage audit failed at ", scheme, " / ", boundary_name,
      ": latest response date used = ", latest_response,
      ", first later origin = ", first_later_origin
    )
  }

  invisible(row)
}

audit_split_leakage <- function(design, sp, cfg_local) {
  audit_boundary(
    design, sp$train_idx, sp$cal_idx,
    sp$scheme, "train_to_calibration"
  )
  audit_boundary(
    design, sp$cal_idx, sp$test_idx,
    sp$scheme, "calibration_to_test"
  )

  gp <- guard_partition(sp$train_idx, cfg_local)
  audit_boundary(
    design, gp$fit_idx, gp$guard_idx,
    sp$scheme, "inner_fit_to_guard"
  )

  invisible(TRUE)
}

make_purged_cv_folds <- function(
  n, k, purge,
  origin_dates = NULL,
  label_end_dates = NULL,
  scheme = "unknown",
  tag = "ridge"
) {
  if (n < 100) stop("Too few observations for purged chronological CV.")

  if (!is.null(origin_dates) || !is.null(label_end_dates)) {
    if (is.null(origin_dates) || is.null(label_end_dates)) {
      stop("Both origin_dates and label_end_dates must be supplied for CV leakage audit.")
    }
    if (length(origin_dates) != n || length(label_end_dates) != n) {
      stop("CV date vectors must have length n.")
    }
    origin_dates <- as.Date(origin_dates)
    label_end_dates <- as.Date(label_end_dates)
  }

  k <- max(2L, min(as.integer(k), 8L))
  initial <- max(80L, floor(n / (k + 1)))

  # Validation blocks partition the post-initial segment chronologically.
  edges <- floor(seq(initial + 1, n + 1, length.out = k + 1))
  folds <- vector("list", k)

  for (i in seq_len(k)) {
    val_start <- edges[i]
    val_end <- edges[i + 1] - 1
    train_end <- val_start - purge - 1

    if (train_end < 50 || val_start > val_end) {
      folds[[i]] <- NULL
      next
    }

    tr <- seq_len(train_end)
    va <- val_start:val_end
    pg <- if (purge > 0) (train_end + 1):(val_start - 1) else integer(0)

    if (!is.null(origin_dates)) {
      latest_response <- max(label_end_dates[tr])
      first_validation_origin <- min(origin_dates[va])
      ok <- latest_response < first_validation_origin

      append_log(
        "leakage",
        tibble::tibble(
          design = "ridge_cv",
          scheme = scheme,
          boundary = paste0("cv_", tag, "_fold_", i),
          earlier_n = length(tr),
          later_n = length(va),
          latest_response_date_used = as.character(latest_response),
          first_later_forecast_origin = as.character(first_validation_origin),
          zero_overlap = ok
        )
      )

      if (!ok) {
        stop(
          "CV leakage audit failed at ", scheme, " / ", tag,
          " / fold ", i,
          ": latest response date used = ", latest_response,
          ", first validation origin = ", first_validation_origin
        )
      }
    }

    folds[[i]] <- list(
      train = tr,
      validation = va,
      purge = pg
    )
  }

  folds <- Filter(Negate(is.null), folds)
  if (length(folds) < 2) stop("Fewer than two valid purged CV folds.")
  folds
}

# ============================================================
# Purged chronological ridge tuning
# ============================================================

standardize_train_test <- function(X_train, X_new) {
  X_train <- as.matrix(X_train)
  X_new <- as.matrix(X_new)

  mu <- colMeans(X_train, na.rm = TRUE)
  sdv <- apply(X_train, 2, stats::sd, na.rm = TRUE)
  sdv[!is.finite(sdv) | sdv <= 1e-12] <- 1

  Xs <- sweep(sweep(X_train, 2, mu, "-"), 2, sdv, "/")
  Xn <- sweep(sweep(X_new, 2, mu, "-"), 2, sdv, "/")

  Xs[!is.finite(Xs)] <- 0
  Xn[!is.finite(Xn)] <- 0

  list(train = Xs, new = Xn)
}

select_lambda_one_se <- function(cv_table, one_se = TRUE) {
  cv_table <- cv_table |>
    dplyr::filter(is.finite(mean_loss), is.finite(se_loss)) |>
    dplyr::arrange(lambda)

  if (nrow(cv_table) == 0) return(NA_real_)

  i_min <- which.min(cv_table$mean_loss)
  if (!one_se) return(cv_table$lambda[i_min])

  threshold <- cv_table$mean_loss[i_min] + cv_table$se_loss[i_min]
  eligible <- cv_table |>
    dplyr::filter(mean_loss <= threshold)

  # One-SE rule: select strongest regularization among eligible lambdas.
  max(eligible$lambda)
}

fit_multi_ridge <- function(
  X_train, Y_train, X_new, cfg_local,
  scheme = "unknown", tag = "multi_ridge",
  origin_dates = NULL, label_end_dates = NULL
) {
  X_train <- as.matrix(X_train)
  X_new <- as.matrix(X_new)
  Y_train <- as.matrix(Y_train)

  lambdas <- sort(unique(cfg_local$ridge_lambda_grid), decreasing = TRUE)
  folds <- make_purged_cv_folds(
    nrow(X_train), cfg_local$blocked_cv_k, cfg_local$cv_purge,
    origin_dates = origin_dates,
    label_end_dates = label_end_dates,
    scheme = scheme,
    tag = tag
  )

  fold_losses <- vector("list", length(folds))
  truncated_folds <- logical(length(folds))

  for (ff in seq_along(folds)) {
    f <- folds[[ff]]
    prep <- standardize_train_test(
      X_train[f$train, , drop = FALSE],
      X_train[f$validation, , drop = FALSE]
    )

    y_tr <- Y_train[f$train, , drop = FALSE]
    y_val <- Y_train[f$validation, , drop = FALSE]

    y_mu <- colMeans(y_tr, na.rm = TRUE)
    y_center <- sweep(y_tr, 2, y_mu, "-")
    y_center[!is.finite(y_center)] <- 0

    glmnet_messages <- character(0)
    fit <- withCallingHandlers(
      glmnet::glmnet(
        x = prep$train,
        y = y_center,
        family = "mgaussian",
        alpha = 0,
        lambda = lambdas,
        standardize = FALSE,
        intercept = TRUE,
        maxit = cfg_local$glmnet_maxit
      ),
      warning = function(w) {
        msg <- conditionMessage(w)
        if (grepl("Convergence for .*lambda value not reached", msg)) {
          glmnet_messages <<- c(glmnet_messages, msg)
          invokeRestart("muffleWarning")
        }
      }
    )

    fit_lambdas <- as.numeric(fit$lambda)
    if (length(fit_lambdas) == 0L) {
      stop("glmnet returned no converged lambda values for ", scheme, " / ", tag,
           " / fold ", ff)
    }
    truncated_folds[ff] <- length(fit_lambdas) < length(lambdas)

    if (length(glmnet_messages) > 0L || truncated_folds[ff]) {
      append_log(
        "warnings",
        tibble::tibble(
          timestamp = as.character(Sys.time()),
          module = "glmnet_convergence",
          message = paste0(
            scheme, " / ", tag, " / fold ", ff,
            ": converged ", length(fit_lambdas), "/", length(lambdas),
            " requested lambdas; smallest converged lambda = ",
            format(min(fit_lambdas), scientific = TRUE),
            if (length(glmnet_messages) > 0L) {
              paste0("; glmnet: ", paste(unique(glmnet_messages), collapse = " | "))
            } else ""
          )
        )
      )
    }

    # IMPORTANT: evaluate only lambda values actually returned by glmnet.
    # This avoids silently interpolating/extrapolating predictions at a
    # non-converged tail of the requested path.
    pr <- predict(fit, newx = prep$new, s = fit_lambdas)
    if (length(dim(pr)) == 2) {
      pr <- array(
        pr,
        dim = c(nrow(prep$new), ncol(Y_train), length(fit_lambdas))
      )
    }

    loss_lam <- numeric(length(fit_lambdas))
    for (ll in seq_along(fit_lambdas)) {
      pp <- pr[, , ll, drop = FALSE]
      dim(pp) <- c(nrow(prep$new), ncol(Y_train))
      pp <- sweep(pp, 2, y_mu, "+")
      loss_lam[ll] <- mean((y_val - pp)^2, na.rm = TRUE)
    }

    fold_losses[[ff]] <- tibble::tibble(
      fold = ff,
      lambda = fit_lambdas,
      loss = loss_lam
    )
  }

  cv_detail <- dplyr::bind_rows(fold_losses)
  cv_table <- cv_detail |>
    dplyr::group_by(lambda) |>
    dplyr::summarise(
      n_folds = dplyr::n_distinct(fold),
      mean_loss = mean(loss, na.rm = TRUE),
      se_loss = stats::sd(loss, na.rm = TRUE) /
        sqrt(sum(is.finite(loss))),
      .groups = "drop"
    ) |>
    dplyr::filter(n_folds == length(folds))

  # A fixed fraction of the original path is not itself a scientific
  # requirement. What matters is that enough common converged candidates
  # remain and that neither the CV minimum nor the 1-SE choice sits on the
  # truncated weak-penalty edge, where unseen smaller lambdas could matter.
  min_common <- min(
    length(lambdas),
    max(10L, as.integer(cfg_local$glmnet_min_common_count))
  )
  if (nrow(cv_table) < min_common) {
    stop(
      "Too few lambda values converged in every CV fold for ", scheme, " / ", tag,
      ": ", nrow(cv_table), "/", length(lambdas),
      ". At least ", min_common, " common candidates are required."
    )
  }

  common_asc <- sort(cv_table$lambda)
  i_min_loss <- which.min(cv_table$mean_loss)
  lambda_min_loss <- cv_table$lambda[i_min_loss]
  min_loss_edge_rank <- match(lambda_min_loss, common_asc)

  if (any(truncated_folds) &&
      is.finite(min_loss_edge_rank) &&
      min_loss_edge_rank <= cfg_local$glmnet_edge_buffer) {
    stop(
      "CV loss minimum lies on/near the truncated weak-penalty edge for ",
      scheme, " / ", tag,
      " (edge rank ", min_loss_edge_rank, "). FULL run aborted because ",
      "smaller non-converged lambdas could still improve validation loss."
    )
  }

  lambda_star <- select_lambda_one_se(
    cv_table, one_se = cfg_local$ridge_one_se_rule
  )
  if (!is.finite(lambda_star)) {
    stop("No finite common-fold lambda selected for ", scheme, " / ", tag)
  }

  selected_edge_rank <- match(lambda_star, common_asc)
  if (any(truncated_folds) &&
      is.finite(selected_edge_rank) &&
      selected_edge_rank <= cfg_local$glmnet_edge_buffer) {
    stop(
      "Selected 1-SE lambda lies on/near a truncated glmnet boundary for ",
      scheme, " / ", tag,
      " (edge rank ", selected_edge_rank, "). FULL run aborted."
    )
  }

  prep_final <- standardize_train_test(X_train, X_new)
  y_mu <- colMeans(Y_train, na.rm = TRUE)
  y_center <- sweep(Y_train, 2, y_mu, "-")
  y_center[!is.finite(y_center)] <- 0

  final_warning <- NULL
  fit_final <- withCallingHandlers(
    glmnet::glmnet(
      x = prep_final$train,
      y = y_center,
      family = "mgaussian",
      alpha = 0,
      lambda = lambda_star,
      standardize = FALSE,
      intercept = TRUE,
      maxit = cfg_local$glmnet_maxit
    ),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("Convergence for .*lambda value not reached", msg)) {
        final_warning <<- msg
        invokeRestart("muffleWarning")
      }
    }
  )
  if (!is.null(final_warning) || length(fit_final$lambda) != 1L) {
    stop(
      "Final glmnet fit failed to converge at selected lambda for ",
      scheme, " / ", tag,
      if (!is.null(final_warning)) paste0(": ", final_warning) else ""
    )
  }

  pred <- predict(fit_final, newx = prep_final$new, s = lambda_star)
  if (length(dim(pred)) == 3) pred <- pred[, , 1]
  pred <- sweep(pred, 2, y_mu, "+")
  colnames(pred) <- colnames(Y_train)

  append_log(
    "tuning",
    tibble::tibble(
      scheme = scheme,
      component = tag,
      family = "mgaussian_ridge",
      selected_lambda = lambda_star,
      n_train = nrow(X_train),
      n_features = ncol(X_train),
      n_outputs = ncol(Y_train),
      cv_folds = length(folds),
      cv_purge = cfg_local$cv_purge,
      common_lambda_count = nrow(cv_table),
      requested_lambda_count = length(lambdas),
      truncated_cv_folds = sum(truncated_folds),
      cv_min_loss_lambda = lambda_min_loss,
      cv_min_loss_edge_rank = min_loss_edge_rank,
      selected_lambda_edge_rank = selected_edge_rank
    )
  )

  pred
}

fit_single_ridge <- function(
  X_train, y_train, X_new, cfg_local,
  scheme = "unknown", tag = "single_ridge",
  origin_dates = NULL, label_end_dates = NULL
) {
  X_train <- as.matrix(X_train)
  X_new <- as.matrix(X_new)
  y_train <- as.numeric(y_train)

  lambdas <- sort(unique(cfg_local$ridge_lambda_grid), decreasing = TRUE)
  folds <- make_purged_cv_folds(
    nrow(X_train), cfg_local$blocked_cv_k, cfg_local$cv_purge,
    origin_dates = origin_dates,
    label_end_dates = label_end_dates,
    scheme = scheme,
    tag = tag
  )

  fold_losses <- vector("list", length(folds))
  truncated_folds <- logical(length(folds))

  for (ff in seq_along(folds)) {
    f <- folds[[ff]]
    prep <- standardize_train_test(
      X_train[f$train, , drop = FALSE],
      X_train[f$validation, , drop = FALSE]
    )

    y_tr <- y_train[f$train]
    y_val <- y_train[f$validation]

    y_mu <- mean(y_tr, na.rm = TRUE)
    ys <- y_tr - y_mu
    ys[!is.finite(ys)] <- 0

    glmnet_messages <- character(0)
    fit <- withCallingHandlers(
      glmnet::glmnet(
        x = prep$train,
        y = ys,
        family = "gaussian",
        alpha = 0,
        lambda = lambdas,
        standardize = FALSE,
        intercept = TRUE,
        maxit = cfg_local$glmnet_maxit
      ),
      warning = function(w) {
        msg <- conditionMessage(w)
        if (grepl("Convergence for .*lambda value not reached", msg)) {
          glmnet_messages <<- c(glmnet_messages, msg)
          invokeRestart("muffleWarning")
        }
      }
    )

    fit_lambdas <- as.numeric(fit$lambda)
    if (length(fit_lambdas) == 0L) {
      stop("glmnet returned no converged lambda values for ", scheme, " / ", tag,
           " / fold ", ff)
    }
    truncated_folds[ff] <- length(fit_lambdas) < length(lambdas)

    if (length(glmnet_messages) > 0L || truncated_folds[ff]) {
      append_log(
        "warnings",
        tibble::tibble(
          timestamp = as.character(Sys.time()),
          module = "glmnet_convergence",
          message = paste0(
            scheme, " / ", tag, " / fold ", ff,
            ": converged ", length(fit_lambdas), "/", length(lambdas),
            " requested lambdas; smallest converged lambda = ",
            format(min(fit_lambdas), scientific = TRUE),
            if (length(glmnet_messages) > 0L) {
              paste0("; glmnet: ", paste(unique(glmnet_messages), collapse = " | "))
            } else ""
          )
        )
      )
    }

    pr <- predict(fit, newx = prep$new, s = fit_lambdas)
    if (is.vector(pr)) pr <- matrix(pr, ncol = length(fit_lambdas))
    pr <- sweep(pr, 2, rep(y_mu, ncol(pr)), "+")

    loss_lam <- colMeans(
      (matrix(y_val, nrow = length(y_val), ncol = ncol(pr)) - pr)^2,
      na.rm = TRUE
    )

    fold_losses[[ff]] <- tibble::tibble(
      fold = ff,
      lambda = fit_lambdas,
      loss = loss_lam
    )
  }

  cv_detail <- dplyr::bind_rows(fold_losses)
  cv_table <- cv_detail |>
    dplyr::group_by(lambda) |>
    dplyr::summarise(
      n_folds = dplyr::n_distinct(fold),
      mean_loss = mean(loss, na.rm = TRUE),
      se_loss = stats::sd(loss, na.rm = TRUE) /
        sqrt(sum(is.finite(loss))),
      .groups = "drop"
    ) |>
    dplyr::filter(n_folds == length(folds))

  # A fixed fraction of the original path is not itself a scientific
  # requirement. What matters is that enough common converged candidates
  # remain and that neither the CV minimum nor the 1-SE choice sits on the
  # truncated weak-penalty edge, where unseen smaller lambdas could matter.
  min_common <- min(
    length(lambdas),
    max(10L, as.integer(cfg_local$glmnet_min_common_count))
  )
  if (nrow(cv_table) < min_common) {
    stop(
      "Too few lambda values converged in every CV fold for ", scheme, " / ", tag,
      ": ", nrow(cv_table), "/", length(lambdas),
      ". At least ", min_common, " common candidates are required."
    )
  }

  common_asc <- sort(cv_table$lambda)
  i_min_loss <- which.min(cv_table$mean_loss)
  lambda_min_loss <- cv_table$lambda[i_min_loss]
  min_loss_edge_rank <- match(lambda_min_loss, common_asc)

  if (any(truncated_folds) &&
      is.finite(min_loss_edge_rank) &&
      min_loss_edge_rank <= cfg_local$glmnet_edge_buffer) {
    stop(
      "CV loss minimum lies on/near the truncated weak-penalty edge for ",
      scheme, " / ", tag,
      " (edge rank ", min_loss_edge_rank, "). FULL run aborted because ",
      "smaller non-converged lambdas could still improve validation loss."
    )
  }

  lambda_star <- select_lambda_one_se(
    cv_table, one_se = cfg_local$ridge_one_se_rule
  )
  if (!is.finite(lambda_star)) {
    stop("No finite common-fold lambda selected for ", scheme, " / ", tag)
  }

  selected_edge_rank <- match(lambda_star, common_asc)
  if (any(truncated_folds) &&
      is.finite(selected_edge_rank) &&
      selected_edge_rank <= cfg_local$glmnet_edge_buffer) {
    stop(
      "Selected 1-SE lambda lies on/near a truncated glmnet boundary for ",
      scheme, " / ", tag,
      " (edge rank ", selected_edge_rank, "). FULL run aborted."
    )
  }

  prep_final <- standardize_train_test(X_train, X_new)
  y_mu <- mean(y_train, na.rm = TRUE)
  ys <- y_train - y_mu
  ys[!is.finite(ys)] <- 0

  final_warning <- NULL
  fit_final <- withCallingHandlers(
    glmnet::glmnet(
      x = prep_final$train,
      y = ys,
      family = "gaussian",
      alpha = 0,
      lambda = lambda_star,
      standardize = FALSE,
      intercept = TRUE,
      maxit = cfg_local$glmnet_maxit
    ),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("Convergence for .*lambda value not reached", msg)) {
        final_warning <<- msg
        invokeRestart("muffleWarning")
      }
    }
  )
  if (!is.null(final_warning) || length(fit_final$lambda) != 1L) {
    stop(
      "Final glmnet fit failed to converge at selected lambda for ",
      scheme, " / ", tag,
      if (!is.null(final_warning)) paste0(": ", final_warning) else ""
    )
  }

  pred <- as.numeric(
    predict(fit_final, newx = prep_final$new, s = lambda_star)
  ) + y_mu

  append_log(
    "tuning",
    tibble::tibble(
      scheme = scheme,
      component = tag,
      family = "gaussian_ridge",
      selected_lambda = lambda_star,
      n_train = nrow(X_train),
      n_features = ncol(X_train),
      n_outputs = 1L,
      cv_folds = length(folds),
      cv_purge = cfg_local$cv_purge,
      common_lambda_count = nrow(cv_table),
      requested_lambda_count = length(lambdas),
      truncated_cv_folds = sum(truncated_folds),
      cv_min_loss_lambda = lambda_min_loss,
      cv_min_loss_edge_rank = min_loss_edge_rank,
      selected_lambda_edge_rank = selected_edge_rank
    )
  )

  pred
}

# ============================================================
# Forecast comparison inference
# ============================================================

nw_se_mean <- function(x, lag) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n <= 2) return(NA_real_)

  z <- x - mean(x)
  v <- sum(z * z) / n
  L <- min(as.integer(lag), n - 1L)

  if (L > 0) {
    for (ell in seq_len(L)) {
      gl <- sum(
        z[(ell + 1):n] * z[1:(n - ell)]
      ) / n
      v <- v + 2 * (1 - ell / (L + 1)) * gl
    }
  }

  sqrt(v / n)
}

dm_test <- function(loss_bm, loss_prop, nw_lag) {
  d <- as.numeric(loss_bm) - as.numeric(loss_prop)
  d <- d[is.finite(d)]

  md <- mean(d)
  se <- nw_se_mean(d, nw_lag)

  if (!is.finite(se) || se <= 0) {
    return(tibble::tibble(
      nw_lag = nw_lag,
      mean_diff = md,
      DM = NA_real_,
      p_two_sided = NA_real_,
      p_one_sided_favors_prop = NA_real_
    ))
  }

  stat <- md / se

  tibble::tibble(
    nw_lag = nw_lag,
    mean_diff = md,
    DM = stat,
    p_two_sided = 2 * (1 - stats::pnorm(abs(stat))),
    p_one_sided_favors_prop = 1 - stats::pnorm(stat)
  )
}

moving_block_bootstrap_means <- function(
  x, B = 999, block_length = 10, seed = 1
) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 20) return(rep(NA_real_, B))

  set.seed(seed)
  L <- min(block_length, n)
  starts <- seq_len(n - L + 1)
  n_blocks <- ceiling(n / L)

  out <- numeric(B)
  for (b in seq_len(B)) {
    chosen <- sample(starts, n_blocks, replace = TRUE)
    xb <- unlist(lapply(chosen, function(s) x[s:(s + L - 1)]), use.names = FALSE)
    xb <- xb[seq_len(n)]
    out[b] <- mean(xb)
  }
  out
}

block_bootstrap_mean_ci <- function(
  x, B, block_length, conf = 0.95, seed = 1
) {
  x <- x[is.finite(x)]
  if (length(x) < 20) {
    return(tibble::tibble(
      mean = mean(x), lower = NA_real_, upper = NA_real_,
      p_one_sided_gt0 = NA_real_
    ))
  }

  bm <- moving_block_bootstrap_means(
    x, B = B, block_length = block_length, seed = seed
  )
  a <- (1 - conf) / 2

  tibble::tibble(
    mean = mean(x),
    lower = safe_quantile(bm, a, type = 8),
    upper = safe_quantile(bm, 1 - a, type = 8),
    p_one_sided_gt0 = mean(bm <= 0, na.rm = TRUE)
  )
}

# Hansen--Lunde--Nason-style Model Confidence Set (MCS) using the
# pairwise range statistic T_R and moving-block bootstrap. The procedure
# tests equal predictive ability among the currently active models and,
# when rejected, eliminates the model with the largest standardized
# relative loss. The same block resamples are used for all pairwise
# differentials within each elimination step.
mcs_block_bootstrap <- function(
  loss_df, alpha = 0.10, B = 499, block_length = 10, seed = 1
) {
  L <- as.matrix(loss_df)
  keep_rows <- complete.cases(L)
  L <- L[keep_rows, , drop = FALSE]

  if (nrow(L) < 30 || ncol(L) < 2) {
    return(list(
      survivors = colnames(L),
      path = tibble::tibble(),
      statistic = "TR"
    ))
  }

  active <- seq_len(ncol(L))
  path <- list()
  step <- 1L

  while (length(active) > 1) {
    LA <- L[, active, drop = FALSE]
    n <- nrow(LA)
    m <- ncol(LA)
    model_names <- colnames(LA)

    mean_loss <- colMeans(LA)
    dbar <- outer(mean_loss, mean_loss, "-")

    # Moving-block bootstrap of model-specific mean losses.
    set.seed(seed + step)
    Lb <- min(as.integer(block_length), n)
    starts <- seq_len(n - Lb + 1L)
    n_blocks <- ceiling(n / Lb)
    boot_means <- matrix(NA_real_, nrow = B, ncol = m)

    for (b in seq_len(B)) {
      chosen <- sample(starts, n_blocks, replace = TRUE)
      idx <- unlist(
        lapply(chosen, function(s) s:(s + Lb - 1L)),
        use.names = FALSE
      )
      idx <- idx[seq_len(n)]
      boot_means[b, ] <- colMeans(LA[idx, , drop = FALSE])
    }

    # Pairwise standard errors of average loss differentials.
    se_mat <- matrix(NA_real_, m, m)
    t_obs <- matrix(0, m, m)
    pair_list <- list()
    rr <- 1L

    for (i in seq_len(m)) {
      for (j in seq_len(m)) {
        if (i == j) next
        db <- boot_means[, i] - boot_means[, j]
        se_ij <- stats::sd(db, na.rm = TRUE)
        se_mat[i, j] <- se_ij
        t_obs[i, j] <- ifelse(
          is.finite(se_ij) && se_ij > 0,
          dbar[i, j] / se_ij,
          0
        )
        pair_list[[rr]] <- c(i, j)
        rr <- rr + 1L
      }
    }

    TR_obs <- max(abs(t_obs[is.finite(t_obs)]), na.rm = TRUE)

    # Null-centered bootstrap range statistic.
    TR_boot <- numeric(B)
    for (b in seq_len(B)) {
      tb <- matrix(0, m, m)
      for (pair in pair_list) {
        i <- pair[1]
        j <- pair[2]
        se_ij <- se_mat[i, j]
        if (!is.finite(se_ij) || se_ij <= 0) next
        d_boot <- (boot_means[b, i] - boot_means[b, j]) - dbar[i, j]
        tb[i, j] <- d_boot / se_ij
      }
      TR_boot[b] <- max(abs(tb[is.finite(tb)]), na.rm = TRUE)
    }

    p_value <- mean(TR_boot >= TR_obs, na.rm = TRUE)

    # e_R elimination rule: remove the model whose worst standardized
    # loss differential against another active model is largest.
    worst_score <- apply(t_obs, 1, max, na.rm = TRUE)
    worst_local <- which.max(worst_score)
    worst_global <- active[worst_local]

    path[[step]] <- tibble::tibble(
      step = step,
      n_models = length(active),
      statistic = "TR",
      test_statistic = TR_obs,
      bootstrap_p = p_value,
      eliminated_model = ifelse(
        is.finite(p_value) && p_value < alpha,
        colnames(L)[worst_global],
        NA_character_
      ),
      worst_relative_t = worst_score[worst_local],
      worst_mean_loss = mean_loss[worst_local],
      active_models = paste(model_names, collapse = " | ")
    )

    if (!is.finite(p_value) || p_value >= alpha) break

    active <- active[-worst_local]
    step <- step + 1L
  }

  list(
    survivors = colnames(L)[active],
    path = dplyr::bind_rows(path),
    statistic = "TR"
  )
}

# ============================================================
# Base forecasting models
# ============================================================

make_graph_features <- function(P_train, P_all_local, cfg_local) {
  C <- suppressWarnings(stats::cor(P_train, use = "pairwise.complete.obs"))
  C[!is.finite(C)] <- 0
  diag(C) <- 0

  G <- matrix(0, ncol(P_train), ncol(P_train))
  rownames(G) <- colnames(G) <- colnames(P_train)

  for (j in seq_len(ncol(C))) {
    ord <- order(abs(C[j, ]), decreasing = TRUE)
    ord <- ord[
      abs(C[j, ord]) >= cfg_local$graph_min_abs_cor
    ]
    if (length(ord) > 0) {
      ord <- ord[seq_len(min(length(ord), cfg_local$graph_top_k))]
      w <- abs(C[j, ord])
      if (sum(w) > 0) G[j, ord] <- w / sum(w)
    }
  }

  Gf <- P_all_local %*% t(G)
  colnames(Gf) <- paste0("graph_", colnames(P_all_local))
  Gf
}

fit_predict_functional_harx_raw <- function(
  design, train_idx, new_idx, cfg_local, scheme
) {
  pred <- fit_multi_ridge(
    design$X[train_idx, , drop = FALSE],
    design$Y[train_idx, , drop = FALSE],
    design$X[new_idx, , drop = FALSE],
    cfg_local = cfg_local,
    scheme = scheme,
    tag = "Functional HAR-X ridge",
    origin_dates = design$dates[train_idx],
    label_end_dates = design$label_end_dates[train_idx]
  )
  rownames(pred) <- new_idx
  pred
}

fit_predict_fpcax_raw <- function(design, train_idx, new_idx, cfg_local, scheme) {
  fit_nested_surface_ridge(design, train_idx, new_idx, cfg_local, scheme, "fpca")
}

fit_predict_graph_harx_raw <- function(design, train_idx, new_idx, cfg_local, scheme) {
  fit_nested_surface_ridge(design, train_idx, new_idx, cfg_local, scheme, "graph")
}

fit_predict_cellwise_harx_raw <- function(
  design, train_idx, new_idx, cfg_local, scheme
) {
  P <- design$P
  out <- matrix(
    NA_real_,
    nrow = length(new_idx),
    ncol = design$J,
    dimnames = list(as.character(new_idx), design$cell_names)
  )

  R5 <- roll_mean_matrix(P, 5)
  R22 <- roll_mean_matrix(P, 22)

  V <- design$vix_features

  for (j in seq_len(design$J)) {
    Xj <- cbind(
      past = P[, j],
      har5 = R5[, j],
      har22 = R22[, j]
    )
    if (!is.null(V)) Xj <- cbind(Xj, V)

    ok_train <- train_idx[
      complete.cases(Xj[train_idx, , drop = FALSE]) &
        is.finite(design$Y[train_idx, j])
    ]

    if (length(ok_train) < 100) next

    Xnew <- Xj[new_idx, , drop = FALSE]
    bad_new <- !complete.cases(Xnew)

    if (all(bad_new)) next

    pred_good <- fit_single_ridge(
      Xj[ok_train, , drop = FALSE],
      design$Y[ok_train, j],
      Xnew[!bad_new, , drop = FALSE],
      cfg_local = cfg_local,
      scheme = scheme,
      tag = paste0("Cell HAR-X ", design$cell_names[j]),
      origin_dates = design$dates[ok_train],
      label_end_dates = design$label_end_dates[ok_train]
    )
    out[!bad_new, j] <- pred_good
  }

  # Persistence fallback if an isolated cell forecast cannot be formed.
  fallback <- design$P[new_idx, , drop = FALSE]
  bad <- !is.finite(out)
  out[bad] <- fallback[bad]
  out
}

ols_predict_safe <- function(X_train, y_train, X_new) {
  X_train <- as.matrix(X_train)
  X_new <- as.matrix(X_new)
  y_train <- as.numeric(y_train)

  ok <- complete.cases(X_train) & is.finite(y_train)
  if (sum(ok) < ncol(X_train) + 20) {
    return(rep(NA_real_, nrow(X_new)))
  }

  Xtr <- cbind(Intercept = 1, X_train[ok, , drop = FALSE])
  Xnw <- cbind(Intercept = 1, X_new)

  fit <- try(stats::lm.fit(Xtr, y_train[ok]), silent = TRUE)
  if (inherits(fit, "try-error")) {
    return(rep(NA_real_, nrow(X_new)))
  }

  beta <- fit$coefficients
  beta[!is.finite(beta)] <- 0
  as.numeric(Xnw %*% beta)
}

fit_predict_bottom_up_har_raw <- function(
  design, train_idx, new_idx, cfg_local, scheme
) {
  n_assets <- length(design$universe)
  threshold_n <- ceiling(cfg_local$min_asset_coverage * n_assets)

  asset_pred_by_h <- vector("list", length(cfg_local$h_grid))
  names(asset_pred_by_h) <- paste0("h", cfg_local$h_grid)

  for (hh in seq_along(cfg_local$h_grid)) {
    h <- cfg_local$h_grid[hh]
    target_mat <- design$future_asset_targets[[paste0("h", h)]]

    pred_assets <- matrix(
      NA_real_,
      nrow = length(new_idx),
      ncol = n_assets,
      dimnames = list(as.character(new_idx), design$universe)
    )

    for (a in seq_len(n_assets)) {
      Xa <- cbind(
        daily = design$asset_har$cur[, a],
        weekly = design$asset_har$ma5[, a],
        monthly = design$asset_har$ma22[, a]
      )
      if (!is.null(design$asset_har$vix)) {
        Xa <- cbind(Xa, design$asset_har$vix)
      }

      pr <- ols_predict_safe(
        Xa[train_idx, , drop = FALSE],
        target_mat[train_idx, a],
        Xa[new_idx, , drop = FALSE]
      )

      pred_assets[, a] <- pmax(pr, 0)
    }

    asset_pred_by_h[[hh]] <- pred_assets
  }

  out <- matrix(
    NA_real_,
    nrow = length(new_idx),
    ncol = design$J,
    dimnames = list(as.character(new_idx), design$cell_names)
  )

  for (cc in seq_len(nrow(design$cell_grid))) {
    h <- design$cell_grid$h[cc]
    tau <- design$cell_grid$tau[cc]
    hh <- match(h, cfg_local$h_grid)
    A <- asset_pred_by_h[[hh]]

    out[, cc] <- apply(A, 1, function(v) {
      v <- v[is.finite(v)]
      if (length(v) < threshold_n) return(NA_real_)
      safe_quantile(v, tau, type = cfg_local$quantile_type)
    })
  }

  # If the bottom-up model is unavailable for a row, use past surface only
  # as a transparent numerical fallback and record it.
  fallback_rows <- which(!complete.cases(out))
  if (length(fallback_rows) > 0) {
    warn_log(
      "bottom_up_har",
      paste0(
        scheme, ": persistence fallback used for ",
        length(fallback_rows), " bottom-up row(s)."
      )
    )
    out[fallback_rows, ] <- design$P[new_idx[fallback_rows], , drop = FALSE]
  }

  out
}

fit_candidate_anchors_raw <- function(
  design, train_idx, new_idx, cfg_local, scheme
) {
  naive <- design$P[new_idx, , drop = FALSE]
  rownames(naive) <- new_idx

  roll_all <- roll_mean_matrix(design$P, cfg_local$rolling_mean_window)
  roll <- roll_all[new_idx, , drop = FALSE]
  rownames(roll) <- new_idx
  bad <- !complete.cases(roll)
  if (any(bad)) roll[bad, ] <- naive[bad, , drop = FALSE]

  out <- list(
    "Naive completed surface" = naive,
    "Rolling mean completed surface" = roll,
    "FPCA-X ridge" = timed(
      "FPCA-X ridge", scheme,
      function() fit_predict_fpcax_raw(
        design, train_idx, new_idx, cfg_local, scheme
      )
    ),
    "Functional HAR-X ridge" = timed(
      "Functional HAR-X ridge", scheme,
      function() fit_predict_functional_harx_raw(
        design, train_idx, new_idx, cfg_local, scheme
      )
    ),
    "Graph Functional HAR-X ridge" = timed(
      "Graph Functional HAR-X ridge", scheme,
      function() fit_predict_graph_harx_raw(
        design, train_idx, new_idx, cfg_local, scheme
      )
    )
  )

  out
}

project_model_list <- function(
  raw_list, design, cfg_local, scheme, stage = "benchmark"
) {
  purrr::imap(
    raw_list,
    function(M, nm) {
      project_surface_matrix(
        M,
        design$cell_grid,
        cfg_local,
        scheme = scheme,
        model = nm,
        stage = stage
      )
    }
  )
}

fit_external_benchmarks_raw <- function(
  design, train_idx, new_idx, cfg_local, scheme
) {
  out <- list()

  out[["Cell-by-cell HAR-X ridge"]] <- timed(
    "Cell-by-cell HAR-X ridge", scheme,
    function() fit_predict_cellwise_harx_raw(
      design, train_idx, new_idx, cfg_local, scheme
    )
  )

  if (isTRUE(cfg_local$run_bottom_up_har)) {
    out[["Bottom-up asset HAR"]] <- timed(
      "Bottom-up asset HAR", scheme,
      function() fit_predict_bottom_up_har_raw(
        design, train_idx, new_idx, cfg_local, scheme
      )
    )
  }

  out
}

# ============================================================
# State definition
# ============================================================

state_score <- function(design, idx, cfg_local) {
  tail_cells <- design$cell_grid$cell[design$cell_grid$tau >= 0.95]
  terminal_cells <- design$cell_grid$cell[
    design$cell_grid$h == max(design$cell_grid$h)
  ]

  rowMeans(design$P[idx, tail_cells, drop = FALSE]) +
    cfg_local$state_terminal_weight *
    rowMeans(design$P[idx, terminal_cells, drop = FALSE])
}

compute_state_thresholds <- function(design, reference_idx, cfg_local) {
  sc <- state_score(design, reference_idx, cfg_local)
  qs <- stats::quantile(
    sc,
    probs = cfg_local$state_probs,
    type = cfg_local$quantile_type,
    na.rm = TRUE,
    names = FALSE
  )
  list(
    probs = cfg_local$state_probs,
    thresholds = as.numeric(qs)
  )
}

state_labels <- function(n_states) {
  if (n_states == 3) {
    return(c("S1_Normal", "S2_Elevated", "S3_High-risk"))
  }
  if (n_states == 2) {
    return(c("S1_Lower-risk", "S2_High-risk"))
  }
  if (n_states == 4) {
    return(c("S1_Low", "S2_Moderate", "S3_Elevated", "S4_High-risk"))
  }
  c(
    paste0("S", seq_len(n_states - 1L)),
    paste0("S", n_states, "_High-risk")
  )
}

assign_states <- function(design, idx, thresholds, cfg_local) {
  sc <- state_score(design, idx, cfg_local)
  cuts <- c(-Inf, thresholds$thresholds, Inf)
  labs <- state_labels(length(cuts) - 1L)
  as.character(
    cut(
      sc,
      breaks = cuts,
      labels = labs,
      include.lowest = TRUE,
      right = TRUE
    )
  )
}

# ============================================================
# Guard loss, blending, and residual correction
# ============================================================

guard_loss_for_pred <- function(design, idx, pred, weights) {
  weighted_rmse_all(
    design$Y[idx, , drop = FALSE],
    pred[as.character(idx), , drop = FALSE],
    weights
  )^2
}

select_top2_blend <- function(
  design, guard_idx, pred1_guard, pred2_guard,
  cfg_local, weights, scheme, anchor1, anchor2
) {
  grid <- purrr::map_dfr(
    cfg_local$secondary_blend_grid,
    function(lam) {
      raw <- (1 - lam) * pred1_guard + lam * pred2_guard
      pp <- project_surface_matrix(
        raw, design$cell_grid, cfg_local,
        scheme = scheme,
        model = "top2_blend_guard",
        stage = "guard_blend"
      )
      tibble::tibble(
        scheme = scheme,
        chosen_anchor = anchor1,
        second_anchor = anchor2,
        secondary_blend_lambda = lam,
        guard_loss = weighted_rmse_all(
          design$Y[guard_idx, , drop = FALSE],
          pp,
          weights
        )^2
      )
    }
  )

  best <- grid |>
    dplyr::arrange(guard_loss, secondary_blend_lambda) |>
    dplyr::slice(1)

  list(
    lambda = best$secondary_blend_lambda,
    table = grid,
    best = best
  )
}

make_top3_simplex_grid <- function(step = 0.05) {
  vals <- seq(0, 1, by = step)
  grid <- expand.grid(w1 = vals, w2 = vals, KEEP.OUT.ATTRS = FALSE)
  grid$w3 <- 1 - grid$w1 - grid$w2
  grid <- grid[grid$w3 >= -1e-12 & grid$w3 <= 1 + 1e-12, , drop = FALSE]
  grid$w3 <- pmax(grid$w3, 0)
  tibble::as_tibble(grid)
}

select_top3_blend <- function(
  design, guard_idx, preds_guard, cfg_local, weights,
  scheme, anchors
) {
  grid <- make_top3_simplex_grid(cfg_local$top3_simplex_step)

  score_rows <- vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    w <- as.numeric(grid[i, c("w1", "w2", "w3")])
    raw <- w[1] * preds_guard[[1]] +
      w[2] * preds_guard[[2]] +
      w[3] * preds_guard[[3]]

    pp <- project_surface_matrix(
      raw, design$cell_grid, cfg_local,
      scheme = scheme,
      model = "top3_blend_guard",
      stage = "guard_top3"
    )

    score_rows[[i]] <- tibble::tibble(
      w1 = w[1], w2 = w[2], w3 = w[3],
      guard_loss = weighted_rmse_all(
        design$Y[guard_idx, , drop = FALSE],
        pp,
        weights
      )^2
    )
  }

  tab <- dplyr::bind_rows(score_rows) |>
    dplyr::mutate(
      scheme = scheme,
      anchor1 = anchors[1],
      anchor2 = anchors[2],
      anchor3 = anchors[3],
      .before = 1
    )

  best <- tab |>
    dplyr::arrange(guard_loss) |>
    dplyr::slice(1)

  list(
    weights = c(best$w1, best$w2, best$w3),
    table = tab,
    best = best
  )
}

# V3.2.0: the transform is learned only on the supplied training rows.
# In CV those rows are the purged fold-training rows, never validation rows.
learn_surface_transform <- function(design, train_idx, new_idx, cfg_local, kind) {
  Xtr <- design$X[train_idx, , drop = FALSE]
  Xnew <- design$X[new_idx, , drop = FALSE]
  Ytr <- design$Y[train_idx, , drop = FALSE]
  if (any(!is.finite(Ytr))) stop("Non-finite training response in surface transform.")
  if (identical(kind, "fpca")) {
    mu <- colMeans(Ytr)
    Yc <- sweep(Ytr, 2, mu, "-")
    sv <- svd(Yc, nu = 0)
    total <- sum(sv$d^2)
    ve <- if (total > 0) sv$d^2 / total else rep(0, length(sv$d))
    hit <- which(cumsum(ve) >= cfg_local$fpca_pve)
    K <- if (length(hit)) hit[1] else length(ve)
    K <- max(1L, min(K, cfg_local$fpca_k_max, ncol(Ytr), nrow(Ytr) - 1L))
    L <- sv$v[, seq_len(K), drop = FALSE]
    return(list(Xtr = Xtr, Xnew = Xnew, response = Yc %*% L,
                basis = L, mean = mu, K = K, variance_share = ve,
                learned_on = train_idx, kind = kind))
  }
  if (!identical(kind, "graph")) stop("Unknown surface transform: ", kind)
  ids <- c(train_idx, new_idx)
  graph <- make_graph_features(design$P[train_idx, , drop = FALSE],
                               design$P[ids, , drop = FALSE], cfg_local)
  nt <- length(train_idx)
  list(Xtr = cbind(Xtr, graph[seq_len(nt), , drop = FALSE]),
       Xnew = cbind(Xnew, graph[nt + seq_along(new_idx), , drop = FALSE]),
       response = Ytr, basis = NULL, mean = NULL, K = NA_integer_,
       learned_on = train_idx, kind = kind)
}

# Explicitly evaluate only the lambdas actually returned by glmnet.
# Constant response columns are represented by their training means.
ridge_path_predict <- function(X_train, Y_train, X_new, lambdas, cfg_local,
                               scheme, tag) {
  X_train <- as.matrix(X_train); X_new <- as.matrix(X_new)
  Y_train <- as.matrix(Y_train)
  if (nrow(X_train) != nrow(Y_train) || nrow(Y_train) < 3L ||
      any(!is.finite(Y_train))) stop("Invalid ridge training matrix: ", tag)
  lambdas <- sort(unique(as.numeric(lambdas)), decreasing = TRUE)
  if (!length(lambdas) || any(!is.finite(lambdas) | lambdas <= 0)) {
    stop("Ridge lambda candidates must be finite and positive.")
  }
  prep <- standardize_train_test(X_train, X_new)
  mu <- colMeans(Y_train)
  centered <- sweep(Y_train, 2, mu, "-")
  variable <- which(apply(centered, 2, function(z) sum(z^2)) > 1e-24)
  messages <- character()
  fit <- NULL
  if (length(variable)) {
    family <- if (length(variable) == 1L) "gaussian" else "mgaussian"
    yy <- centered[, variable, drop = FALSE]
    if (identical(family, "gaussian")) yy <- as.numeric(yy)
    fit <- withCallingHandlers(
      glmnet::glmnet(x = prep$train, y = yy, family = family, alpha = 0,
                     lambda = lambdas, standardize = FALSE, intercept = TRUE,
                     maxit = cfg_local$glmnet_maxit),
      warning = function(w) {
        if (grepl("Convergence for .*lambda value not reached", conditionMessage(w))) {
          messages <<- c(messages, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      })
    actual <- as.numeric(fit$lambda)
    if (!length(actual) || any(!is.finite(actual)) ||
        (!is.null(fit$jerr) && fit$jerr > 0)) {
      stop("No usable converged ridge path: ", scheme, " / ", tag)
    }
    pr <- predict(fit, newx = prep$new, s = actual)
    dim(pr) <- c(nrow(X_new), length(variable), length(actual))
  } else {
    actual <- lambdas
  }
  out <- array(0, c(nrow(X_new), ncol(Y_train), length(actual)))
  for (i in seq_along(actual)) {
    pp <- matrix(rep(mu, each = nrow(X_new)), nrow(X_new), length(mu))
    if (length(variable)) {
      z <- matrix(pr[, , i], nrow(X_new), length(variable))
      pp[, variable] <- sweep(z, 2, mu[variable], "+")
    }
    out[, , i] <- pp
  }
  if (any(!is.finite(out))) stop("Non-finite ridge predictions: ", tag)
  truncated <- length(actual) < length(lambdas)
  if (truncated || length(messages)) {
    append_log("warnings", tibble::tibble(timestamp = as.character(Sys.time()),
      module = "glmnet_convergence", message = paste0(scheme, " / ", tag, ": ",
      length(actual), "/", length(lambdas), " converged candidates; ",
      paste(unique(messages), collapse = " | "))))
  }
  list(prediction = out, lambda = actual, truncated = truncated)
}

reconstruct_surface_path <- function(path, transform) {
  if (!identical(transform$kind, "fpca")) return(path$prediction)
  pp <- path$prediction
  ans <- array(0, c(dim(pp)[1], length(transform$mean), dim(pp)[3]))
  for (i in seq_len(dim(pp)[3])) {
    scores <- matrix(pp[, , i], dim(pp)[1], transform$K)
    ans[, , i] <- sweep(scores %*% t(transform$basis), 2, transform$mean, "+")
  }
  ans
}

check_candidate_path <- function(tab, requested, truncated, cfg_local, tag,
                                 one_se = TRUE) {
  tab <- tab[is.finite(tab$mean_loss) & is.finite(tab$se_loss), , drop = FALSE]
  minimum <- min(length(requested), max(10L, cfg_local$glmnet_min_common_count))
  if (nrow(tab) < minimum) stop("Too few common converged lambdas: ", tag)
  # A tie at the minimum is resolved toward stronger regularization.
  tab <- tab[order(tab$mean_loss, -tab$lambda), , drop = FALSE]
  minimum_lambda <- tab$lambda[1]
  selected <- if (one_se) max(tab$lambda[tab$mean_loss <=
                           tab$mean_loss[1] + tab$se_loss[1]]) else minimum_lambda
  asc <- sort(tab$lambda)
  min_rank <- match(minimum_lambda, asc)
  selected_rank <- match(selected, asc)
  if (any(truncated) && min(min_rank, selected_rank) <= cfg_local$glmnet_edge_buffer) {
    stop("Selected/minimum loss lies near truncated weak-penalty edge: ", tag)
  }
  list(lambda = selected, minimum_lambda = minimum_lambda,
       min_rank = min_rank, selected_rank = selected_rank, common_count = nrow(tab))
}

predict_fixed_ridge <- function(X_train, Y_train, X_new, selected_lambda,
                                cfg_local, scheme, tag) {
  if (length(selected_lambda) != 1L || !is.finite(selected_lambda) || selected_lambda <= 0) {
    stop("Missing or invalid carried ridge penalty: ", tag)
  }
  # Warm starts at stronger penalties; never retune on full-training residuals.
  path_grid <- sort(unique(c(cfg_local$ridge_lambda_grid[
    cfg_local$ridge_lambda_grid >= selected_lambda], selected_lambda)), decreasing = TRUE)
  path <- ridge_path_predict(X_train, Y_train, X_new, path_grid, cfg_local, scheme, tag)
  where <- match(selected_lambda, path$lambda)
  if (is.na(where)) stop("Final ridge path did not reach selected lambda: ", tag)
  pp <- matrix(path$prediction[, , where], nrow(X_new), ncol(as.matrix(Y_train)))
  colnames(pp) <- colnames(Y_train)
  pp
}

fit_nested_surface_ridge <- function(design, train_idx, new_idx, cfg_local, scheme, kind) {
  tag <- paste0(if (kind == "fpca") "FPCA-X" else "Graph Functional HAR-X",
                " fold-local ridge")
  folds <- make_purged_cv_folds(length(train_idx), cfg_local$blocked_cv_k,
    cfg_local$cv_purge, design$dates[train_idx], design$label_end_dates[train_idx],
    scheme, tag)
  lambdas <- sort(unique(cfg_local$ridge_lambda_grid), decreasing = TRUE)
  losses <- vector("list", length(folds)); truncated <- logical(length(folds))
  for (ff in seq_along(folds)) {
    tr <- train_idx[folds[[ff]]$train]; va <- train_idx[folds[[ff]]$validation]
    transform <- learn_surface_transform(design, tr, va, cfg_local, kind)
    stopifnot(identical(transform$learned_on, tr), length(intersect(tr, va)) == 0L)
    path <- ridge_path_predict(transform$Xtr, transform$response, transform$Xnew,
                               lambdas, cfg_local, scheme, paste0(tag, " fold ", ff))
    pp <- reconstruct_surface_path(path, transform)
    lv <- vapply(seq_along(path$lambda), function(i) {
      pred <- matrix(pp[, , i], length(va), ncol(design$Y))
      mean((design$Y[va, , drop = FALSE] - pred)^2)
    }, numeric(1))
    losses[[ff]] <- tibble::tibble(fold = ff, lambda = path$lambda, loss = lv)
    truncated[ff] <- path$truncated
    append_log("preprocessing", tibble::tibble(scheme = scheme,
      component = kind, stage = "purged_cv", fold = ff, n_fit = length(tr),
      n_validation = length(va), selected_K = transform$K,
      latest_fit_origin = as.character(max(design$dates[tr])),
      latest_fit_label = as.character(max(design$label_end_dates[tr])),
      first_validation_origin = as.character(min(design$dates[va])),
      zero_overlap = max(design$label_end_dates[tr]) < min(design$dates[va]),
      fit_indices_sha256 = digest::digest(tr, algo = "sha256"), design = design$name))
  }
  detail <- dplyr::bind_rows(losses)
  tab <- detail |>
    dplyr::group_by(lambda) |>
    dplyr::summarise(n_folds = dplyr::n_distinct(fold), mean_loss = mean(loss),
      se_loss = stats::sd(loss) / sqrt(length(folds)), .groups = "drop") |>
    dplyr::filter(n_folds == length(folds))
  choice <- check_candidate_path(tab, lambdas, truncated, cfg_local, tag,
                                  one_se = cfg_local$ridge_one_se_rule)
  transform <- learn_surface_transform(design, train_idx, new_idx, cfg_local, kind)
  pred <- predict_fixed_ridge(transform$Xtr, transform$response, transform$Xnew,
                              choice$lambda, cfg_local, scheme, tag)
  if (kind == "fpca") {
    pred <- sweep(pred %*% t(transform$basis), 2, transform$mean, "+")
    append_log("fpca", tibble::tibble(design = design$name, scheme = scheme,
      n_train = length(train_idx), selected_K = transform$K,
      pve_threshold = cfg_local$fpca_pve, k_max = cfg_local$fpca_k_max,
      first_pc_variance_share = transform$variance_share[1],
      selected_pve = sum(transform$variance_share[seq_len(transform$K)]),
      selected_lambda = choice$lambda, penalty_protocol = "Shared-score lambda; surface-loss CV"))
  }
  append_log("tuning", tibble::tibble(scheme = scheme, component = tag,
    family = "mgaussian_ridge", selected_lambda = choice$lambda,
    selection_protocol = "Purged CV with fold-local preprocessing; surface loss; 1-SE when enabled",
    n_train = length(train_idx), n_features = ncol(transform$Xtr),
    n_outputs = ncol(design$Y), cv_folds = length(folds), cv_purge = cfg_local$cv_purge,
    common_lambda_count = choice$common_count, requested_lambda_count = length(lambdas),
    truncated_cv_folds = sum(truncated), cv_min_loss_lambda = choice$minimum_lambda,
    cv_min_loss_edge_rank = choice$min_rank, selected_lambda_edge_rank = choice$selected_rank))
  colnames(pred) <- colnames(design$Y); rownames(pred) <- new_idx
  pred
}

# Residual penalty is selected on the existing training-only guard block.
# It is subsequently carried unchanged to the full-training refit.
# This is deliberately not CV on residual targets built with enclosing-block anchors.
tune_residual_guard <- function(design, fit_idx, residual_fit, guard_idx,
                                residual_guard, cfg_local, scheme, tag) {
  audit_boundary(design, fit_idx, guard_idx, scheme, paste0("residual_guard_", tag))
  lambdas <- sort(unique(cfg_local$ridge_lambda_grid), decreasing = TRUE)
  path <- ridge_path_predict(design$X[fit_idx, , drop = FALSE], residual_fit,
    design$X[guard_idx, , drop = FALSE], lambdas, cfg_local, scheme, tag)
  losses <- vapply(seq_along(path$lambda), function(i) {
    pp <- matrix(path$prediction[, , i], length(guard_idx), design$J)
    mean((residual_guard - pp)^2)
  }, numeric(1))
  tab <- tibble::tibble(lambda = path$lambda, mean_loss = losses, se_loss = 0)
  choice <- check_candidate_path(tab, lambdas, path$truncated, cfg_local, tag, one_se = FALSE)
  at <- match(choice$lambda, path$lambda)
  pp <- matrix(path$prediction[, , at], length(guard_idx), design$J,
               dimnames = list(as.character(guard_idx), design$cell_names))
  append_log("tuning", tibble::tibble(scheme = scheme, component = tag,
    family = "residual_ridge_guard", selection_protocol = "Training guard residual MSE; then projected rho gate",
    selected_lambda = choice$lambda, n_train = length(fit_idx), n_guard = length(guard_idx),
    n_features = ncol(design$X), n_outputs = design$J, cv_folds = 0L,
    cv_purge = cfg_local$cv_purge, common_lambda_count = choice$common_count,
    requested_lambda_count = length(lambdas), truncated_cv_folds = as.integer(path$truncated),
    cv_min_loss_lambda = choice$minimum_lambda, cv_min_loss_edge_rank = choice$min_rank,
    selected_lambda_edge_rank = choice$selected_rank))
  list(prediction = pp, selected_lambda = choice$lambda)
}

fit_residual_ridge <- function(design, fit_idx, residual_fit, new_idx,
                               cfg_local, scheme, tag, selected_lambda) {
  pred <- predict_fixed_ridge(design$X[fit_idx, , drop = FALSE], residual_fit,
    design$X[new_idx, , drop = FALSE], selected_lambda, cfg_local, scheme, tag)
  append_log("tuning", tibble::tibble(scheme = scheme, component = tag,
    family = "residual_ridge_fixed_guard", selection_protocol = "Carried guard lambda; no residual CV",
    selected_lambda = selected_lambda, n_train = length(fit_idx),
    n_features = ncol(design$X), n_outputs = design$J, cv_folds = 0L))
  rownames(pred) <- new_idx
  pred
}


tune_global_correction <- function(
  design, fit_idx, guard_idx,
  base_fit, base_guard,
  cfg_local, weights, scheme,
  label = "global"
) {
  residual_fit <- design$Y[fit_idx, , drop = FALSE] - base_fit

  residual_tuning <- tune_residual_guard(
    design, fit_idx, residual_fit, guard_idx,
    design$Y[guard_idx, , drop = FALSE] - base_guard,
    cfg_local, scheme, paste0("Residual correction ", label)
  )
  R_guard <- residual_tuning$prediction

  anchor_loss <- weighted_rmse_all(
    design$Y[guard_idx, , drop = FALSE],
    base_guard,
    weights
  )^2

  tab <- purrr::map_dfr(
    cfg_local$residual_lambda_grid,
    function(rho) {
      raw <- base_guard + rho * R_guard
      pp <- project_surface_matrix(
        raw, design$cell_grid, cfg_local,
        scheme = scheme,
        model = paste0("correction_", label),
        stage = "guard_correction"
      )
      tibble::tibble(
        rho = rho,
        guard_loss = weighted_rmse_all(
          design$Y[guard_idx, , drop = FALSE],
          pp,
          weights
        )^2
      )
    }
  )

  best <- tab |>
    dplyr::arrange(guard_loss, rho) |>
    dplyr::slice(1)

  rel <- if (
    is.finite(anchor_loss) && anchor_loss > 0
  ) {
    (anchor_loss - best$guard_loss) / anchor_loss
  } else {
    0
  }

  active <- is.finite(rel) &&
    rel > cfg_local$min_guard_improvement &&
    best$rho > 0

  list(
    selected_rho = if (active) best$rho else 0,
    selected_lambda = residual_tuning$selected_lambda,
    active = active,
    relative_improvement = if (active) rel else 0,
    anchor_loss = anchor_loss,
    selected_loss = if (active) best$guard_loss else anchor_loss,
    table = tab,
    guard_residual_prediction = R_guard
  )
}

tune_state_correction <- function(
  design, fit_idx, guard_idx,
  base_fit, base_guard,
  thresholds_fit,
  cfg_local, weights, scheme,
  label = "state"
) {
  states_fit <- assign_states(design, fit_idx, thresholds_fit, cfg_local)
  states_guard <- assign_states(design, guard_idx, thresholds_fit, cfg_local)
  labels <- unique(c(
    state_labels(length(cfg_local$state_probs) + 1L),
    states_fit,
    states_guard
  ))
  labels <- labels[!is.na(labels)]

  gate_rows <- list()
  guard_correction <- matrix(
    0, nrow = length(guard_idx), ncol = design$J,
    dimnames = list(as.character(guard_idx), design$cell_names)
  )

  for (st in labels) {
    fit_st <- fit_idx[states_fit == st]
    guard_st <- guard_idx[states_guard == st]

    if (
      length(fit_st) < cfg_local$residual_min_state_fit_n ||
        length(guard_st) < cfg_local$residual_min_state_guard_n
    ) {
      gate_rows[[st]] <- tibble::tibble(
        scheme = scheme,
        layer = label,
        state = st,
        n_fit = length(fit_st),
        n_guard = length(guard_st),
        candidate_rho = NA_real_,
        selected_lambda = NA_real_,
        selected_rho = 0,
        anchor_loss = NA_real_,
        candidate_loss = NA_real_,
        relative_improvement = 0,
        active = FALSE
      )
      next
    }

    fit_pos <- match(fit_st, fit_idx)
    guard_pos <- match(guard_st, guard_idx)

    residual_fit <- design$Y[fit_st, , drop = FALSE] -
      base_fit[fit_pos, , drop = FALSE]

    residual_tuning <- tune_residual_guard(
      design, fit_st, residual_fit, guard_st,
      design$Y[guard_st, , drop = FALSE] - base_guard[guard_pos, , drop = FALSE],
      cfg_local, scheme, paste0("Residual correction ", label, " ", st)
    )
    R_guard <- residual_tuning$prediction

    anchor_loss <- weighted_rmse_all(
      design$Y[guard_st, , drop = FALSE],
      base_guard[guard_pos, , drop = FALSE],
      weights
    )^2

    cand <- purrr::map_dfr(
      cfg_local$residual_lambda_grid,
      function(rho) {
        raw <- base_guard[guard_pos, , drop = FALSE] + rho * R_guard
        pp <- project_surface_matrix(
          raw, design$cell_grid, cfg_local,
          scheme = scheme,
          model = paste0(label, "_", st),
          stage = "guard_state_correction"
        )
        tibble::tibble(
          rho = rho,
          loss = weighted_rmse_all(
            design$Y[guard_st, , drop = FALSE],
            pp,
            weights
          )^2
        )
      }
    ) |>
      dplyr::arrange(loss, rho) |>
      dplyr::slice(1)

    rel <- if (
      is.finite(anchor_loss) && anchor_loss > 0
    ) {
      (anchor_loss - cand$loss) / anchor_loss
    } else {
      0
    }

    active <- is.finite(rel) &&
      rel > cfg_local$min_guard_improvement &&
      cand$rho > 0

    rho_selected <- if (active) cand$rho else 0

    guard_correction[guard_pos, ] <- rho_selected * R_guard

    gate_rows[[st]] <- tibble::tibble(
      scheme = scheme,
      layer = label,
      state = st,
      n_fit = length(fit_st),
      n_guard = length(guard_st),
      candidate_rho = cand$rho,
      selected_lambda = residual_tuning$selected_lambda,
      selected_rho = rho_selected,
      anchor_loss = anchor_loss,
      candidate_loss = cand$loss,
      relative_improvement = if (active) rel else 0,
      active = active
    )
  }

  list(
    gate = dplyr::bind_rows(gate_rows),
    guard_correction = guard_correction,
    states_fit = states_fit,
    states_guard = states_guard
  )
}

fit_final_global_correction <- function(
  design, train_idx, new_idx,
  base_train, base_new,
  selected_rho, cfg_local, scheme, label, selected_lambda
) {
  if (!is.finite(selected_rho) || selected_rho <= 0) {
    return(matrix(
      0, nrow = length(new_idx), ncol = design$J,
      dimnames = list(as.character(new_idx), design$cell_names)
    ))
  }

  residual_train <- design$Y[train_idx, , drop = FALSE] - base_train
  R_new <- fit_residual_ridge(
    design, train_idx, residual_train, new_idx,
    cfg_local, scheme, paste0("Final global correction ", label), selected_lambda
  )

  selected_rho * R_new
}

fit_final_state_correction <- function(
  design, train_idx, new_idx,
  base_train, base_new,
  gate_table, thresholds_full,
  cfg_local, scheme, label
) {
  correction <- matrix(
    0, nrow = length(new_idx), ncol = design$J,
    dimnames = list(as.character(new_idx), design$cell_names)
  )

  states_train <- assign_states(
    design, train_idx, thresholds_full, cfg_local
  )
  states_new <- assign_states(
    design, new_idx, thresholds_full, cfg_local
  )

  for (ii in seq_len(nrow(gate_table))) {
    st <- gate_table$state[ii]
    rho <- gate_table$selected_rho[ii]
    active <- isTRUE(gate_table$active[ii])

    if (!active || !is.finite(rho) || rho <= 0) next

    train_st <- train_idx[states_train == st]
    new_pos <- which(states_new == st)

    if (
      length(train_st) < cfg_local$residual_min_state_fit_n ||
        length(new_pos) == 0
    ) next

    train_pos <- match(train_st, train_idx)
    residual_train <- design$Y[train_st, , drop = FALSE] -
      base_train[train_pos, , drop = FALSE]

    R_new <- fit_residual_ridge(
      design,
      train_st,
      residual_train,
      new_idx[new_pos],
      cfg_local,
      scheme,
      paste0("Final state correction ", label, " ", st),
      selected_lambda = gate_table$selected_lambda[ii]
    )

    correction[new_pos, ] <- rho * R_new
  }

  correction
}

# ============================================================
# FES-CF architecture + ablation
# ============================================================

fit_predict_fescf <- function(design, sp, cfg_local) {
  audit_split_leakage(design, sp, cfg_local)

  gp <- guard_partition(sp$train_idx, cfg_local)
  fit_idx <- gp$fit_idx
  guard_idx <- gp$guard_idx
  idx_all <- c(sp$train_idx, sp$cal_idx, sp$test_idx)

  # -------- Inner fit: candidate anchors for guard selection
  inner_raw <- fit_candidate_anchors_raw(
    design, fit_idx, c(fit_idx, guard_idx), cfg_local, sp$scheme
  )
  inner_proj <- project_model_list(
    inner_raw, design, cfg_local, sp$scheme, "inner_anchor"
  )

  guard_losses <- purrr::map_dfr(
    names(inner_proj),
    function(m) {
      pred_guard <- inner_proj[[m]][
        match(guard_idx, c(fit_idx, guard_idx)),
        ,
        drop = FALSE
      ]
      tibble::tibble(
        scheme = sp$scheme,
        model = m,
        guard_loss = weighted_rmse_all(
          design$Y[guard_idx, , drop = FALSE],
          pred_guard,
          design$weights
        )^2
      )
    }
  ) |>
    dplyr::arrange(guard_loss, model) |>
    dplyr::mutate(guard_rank = dplyr::row_number())

  chosen <- guard_losses$model[1]
  second <- guard_losses$model[2]
  third <- guard_losses$model[3]

  # Inner matrices in fit/guard order
  inner_index <- c(fit_idx, guard_idx)

  chosen_fit_raw <- inner_raw[[chosen]][
    match(fit_idx, inner_index), , drop = FALSE
  ]
  chosen_guard_raw <- inner_raw[[chosen]][
    match(guard_idx, inner_index), , drop = FALSE
  ]
  chosen_fit <- inner_proj[[chosen]][
    match(fit_idx, inner_index), , drop = FALSE
  ]
  chosen_guard <- inner_proj[[chosen]][
    match(guard_idx, inner_index), , drop = FALSE
  ]
  second_fit <- inner_proj[[second]][
    match(fit_idx, inner_index), , drop = FALSE
  ]
  second_guard <- inner_proj[[second]][
    match(guard_idx, inner_index), , drop = FALSE
  ]

  # -------- Top-2 blending
  blend2 <- select_top2_blend(
    design,
    guard_idx,
    chosen_guard,
    second_guard,
    cfg_local,
    design$weights,
    sp$scheme,
    chosen,
    second
  )
  lam2 <- blend2$lambda

  blend2_fit <- (1 - lam2) * chosen_fit + lam2 * second_fit
  blend2_guard <- (1 - lam2) * chosen_guard + lam2 * second_guard
  blend2_fit <- project_surface_matrix(
    blend2_fit, design$cell_grid, cfg_local,
    sp$scheme, "top2_blend", "inner_fit"
  )
  blend2_guard <- project_surface_matrix(
    blend2_guard, design$cell_grid, cfg_local,
    sp$scheme, "top2_blend", "inner_guard"
  )

  # -------- Top-3 blend sensitivity
  top3_result <- NULL
  top3_weights <- c(1, 0, 0)

  if (isTRUE(cfg_local$run_top3_sensitivity)) {
    third_fit <- inner_proj[[third]][
      match(fit_idx, inner_index), , drop = FALSE
    ]
    third_guard <- inner_proj[[third]][
      match(guard_idx, inner_index), , drop = FALSE
    ]

    top3_result <- select_top3_blend(
      design,
      guard_idx,
      list(chosen_guard, second_guard, third_guard),
      cfg_local,
      design$weights,
      sp$scheme,
      c(chosen, second, third)
    )
    top3_weights <- top3_result$weights
  }

  # -------- Thresholds for guard decisions use fit block only
  thresholds_fit <- compute_state_thresholds(
    design, fit_idx, cfg_local
  )

  # -------- Global correction ablation around selected anchor
  global_selected <- tune_global_correction(
    design,
    fit_idx,
    guard_idx,
    chosen_fit,
    chosen_guard,
    cfg_local,
    design$weights,
    sp$scheme,
    label = "selected_anchor"
  )

  # -------- State correction around selected anchor
  state_selected <- tune_state_correction(
    design,
    fit_idx,
    guard_idx,
    chosen_fit,
    chosen_guard,
    thresholds_fit,
    cfg_local,
    design$weights,
    sp$scheme,
    label = "selected_anchor"
  )

  # -------- State correction around top-2 blend = main FES-CF
  state_blend2 <- tune_state_correction(
    design,
    fit_idx,
    guard_idx,
    blend2_fit,
    blend2_guard,
    thresholds_fit,
    cfg_local,
    design$weights,
    sp$scheme,
    label = "top2_blend"
  )

  inner_full_guard_raw <- blend2_guard +
    state_blend2$guard_correction

  inner_full_guard <- project_surface_matrix(
    inner_full_guard_raw,
    design$cell_grid,
    cfg_local,
    sp$scheme,
    "FES-CF full",
    "inner_guard_final"
  )

  # Guard residuals are training-only scaling inputs, reused after guard tuning.
  # They are not independent model-validation residuals.
  guard_residuals_for_scale <- design$Y[guard_idx, , drop = FALSE] -
    inner_full_guard

  # -------- Full-training refit of anchors
  full_raw <- fit_candidate_anchors_raw(
    design, sp$train_idx, idx_all, cfg_local, sp$scheme
  )
  full_proj <- project_model_list(
    full_raw, design, cfg_local, sp$scheme, "full_anchor"
  )

  full_chosen_raw <- full_raw[[chosen]]
  full_chosen <- full_proj[[chosen]]
  full_second <- full_proj[[second]]
  full_third <- full_proj[[third]]

  rownames(full_chosen_raw) <- idx_all
  rownames(full_chosen) <- idx_all
  rownames(full_second) <- idx_all
  rownames(full_third) <- idx_all

  blend2_all <- (1 - lam2) * full_chosen + lam2 * full_second
  blend2_all <- project_surface_matrix(
    blend2_all,
    design$cell_grid,
    cfg_local,
    sp$scheme,
    "top2_blend",
    "full_refit"
  )
  rownames(blend2_all) <- idx_all

  thresholds_full <- compute_state_thresholds(
    design, sp$train_idx, cfg_local
  )

  # -------- Final correction layers
  # Global correction on selected anchor
  global_corr_all <- fit_final_global_correction(
    design,
    sp$train_idx,
    idx_all,
    full_chosen[as.character(sp$train_idx), , drop = FALSE],
    full_chosen,
    global_selected$selected_rho,
    cfg_local,
    sp$scheme,
    "selected_anchor",
    selected_lambda = global_selected$selected_lambda
  )

  # State correction on selected anchor
  state_selected_corr_all <- fit_final_state_correction(
    design,
    sp$train_idx,
    idx_all,
    full_chosen[as.character(sp$train_idx), , drop = FALSE],
    full_chosen,
    state_selected$gate,
    thresholds_full,
    cfg_local,
    sp$scheme,
    "selected_anchor"
  )

  # State correction on blended anchor = main
  state_blend2_corr_all <- fit_final_state_correction(
    design,
    sp$train_idx,
    idx_all,
    blend2_all[as.character(sp$train_idx), , drop = FALSE],
    blend2_all,
    state_blend2$gate,
    thresholds_full,
    cfg_local,
    sp$scheme,
    "top2_blend"
  )

  full_no_projection <- blend2_all + state_blend2_corr_all
  full_final <- project_surface_matrix(
    full_no_projection,
    design$cell_grid,
    cfg_local,
    sp$scheme,
    "FES-CF full",
    "final"
  )
  rownames(full_final) <- idx_all

  # -------- Top-3 full sensitivity
  top3_final <- NULL
  top3_gate <- tibble::tibble()
  top3_table <- tibble::tibble()

  if (isTRUE(cfg_local$run_top3_sensitivity) && !is.null(top3_result)) {
    w3 <- top3_weights

    top3_fit <- w3[1] * chosen_fit +
      w3[2] * second_fit +
      w3[3] * inner_proj[[third]][
        match(fit_idx, inner_index), , drop = FALSE
      ]
    top3_guard <- w3[1] * chosen_guard +
      w3[2] * second_guard +
      w3[3] * inner_proj[[third]][
        match(guard_idx, inner_index), , drop = FALSE
      ]

    top3_fit <- project_surface_matrix(
      top3_fit, design$cell_grid, cfg_local,
      sp$scheme, "top3_blend", "inner_fit"
    )
    top3_guard <- project_surface_matrix(
      top3_guard, design$cell_grid, cfg_local,
      sp$scheme, "top3_blend", "inner_guard"
    )

    top3_state <- tune_state_correction(
      design,
      fit_idx,
      guard_idx,
      top3_fit,
      top3_guard,
      thresholds_fit,
      cfg_local,
      design$weights,
      sp$scheme,
      label = "top3_blend"
    )

    top3_all <- w3[1] * full_chosen +
      w3[2] * full_second +
      w3[3] * full_third
    top3_all <- project_surface_matrix(
      top3_all, design$cell_grid, cfg_local,
      sp$scheme, "top3_blend", "full_refit"
    )
    rownames(top3_all) <- idx_all

    top3_corr <- fit_final_state_correction(
      design,
      sp$train_idx,
      idx_all,
      top3_all[as.character(sp$train_idx), , drop = FALSE],
      top3_all,
      top3_state$gate,
      thresholds_full,
      cfg_local,
      sp$scheme,
      "top3_blend"
    )

    top3_final <- project_surface_matrix(
      top3_all + top3_corr,
      design$cell_grid,
      cfg_local,
      sp$scheme,
      "FES-CF top3 sensitivity",
      "final"
    )
    rownames(top3_final) <- idx_all
    top3_gate <- top3_state$gate
    top3_table <- top3_result$table
  }

  # -------- Systematic ablation
  selected_projected <- full_chosen

  selected_plus_global <- project_surface_matrix(
    full_chosen + global_corr_all,
    design$cell_grid,
    cfg_local,
    sp$scheme,
    "Ablation global correction",
    "final"
  )
  rownames(selected_plus_global) <- idx_all

  selected_plus_state <- project_surface_matrix(
    full_chosen + state_selected_corr_all,
    design$cell_grid,
    cfg_local,
    sp$scheme,
    "Ablation state correction",
    "final"
  )
  rownames(selected_plus_state) <- idx_all

  ablations <- list(
    "Selected anchor raw" = full_chosen_raw,
    "Selected anchor + projection" = selected_projected,
    "Selected anchor + top2 blend" = blend2_all,
    "Selected anchor + global correction" = selected_plus_global,
    "Selected anchor + state correction" = selected_plus_state,
    "Blend + state correction, no final projection" = full_no_projection,
    "FES-CF full" = full_final
  )

  if (!is.null(top3_final)) {
    ablations[["FES-CF top3 sensitivity"]] <- top3_final
  }

  # Guard summary
  guard_anchor_loss <- guard_losses$guard_loss[1]
  guard_blend_loss <- blend2$best$guard_loss
  guard_full_loss <- weighted_rmse_all(
    design$Y[guard_idx, , drop = FALSE],
    inner_full_guard,
    design$weights
  )^2

  correction_summary <- tibble::tibble(
    scheme = sp$scheme,
    chosen_anchor = chosen,
    second_anchor = second,
    third_anchor = third,
    secondary_blend_active = lam2 > 0,
    secondary_blend_lambda = lam2,
    n_active_states = sum(state_blend2$gate$active, na.rm = TRUE),
    guard_loss_selected_anchor = guard_anchor_loss,
    guard_loss_after_blend = guard_blend_loss,
    guard_loss_full_fescf = guard_full_loss,
    guard_gain_blend = ifelse(
      guard_anchor_loss > 0,
      (guard_anchor_loss - guard_blend_loss) / guard_anchor_loss,
      NA_real_
    ),
    guard_gain_full = ifelse(
      guard_anchor_loss > 0,
      (guard_anchor_loss - guard_full_loss) / guard_anchor_loss,
      NA_real_
    ),
    global_rho = global_selected$selected_rho,
    global_ridge_lambda = global_selected$selected_lambda
  )

  list(
    idx_all = idx_all,
    chosen_anchor = chosen,
    second_anchor = second,
    third_anchor = third,
    anchor_selection = guard_losses,
    secondary_blend_guard = blend2$table,
    top3_blend_guard = top3_table,
    correction_summary = correction_summary,
    state_gate_selected = state_selected$gate,
    state_gate_blend = state_blend2$gate,
    state_gate_top3 = top3_gate,
    thresholds_fit = thresholds_fit,
    thresholds_full = thresholds_full,
    guard_idx = guard_idx,
    guard_residuals_for_scale = guard_residuals_for_scale,
    ablations = ablations,
    full = full_final,
    top3 = top3_final
  )
}

# ============================================================
# Conformal prediction
# ============================================================

robust_residual_scale <- function(residuals, cfg_local) {
  residuals <- as.matrix(residuals)

  apply(
    residuals,
    2,
    function(x) {
      x <- x[is.finite(x)]
      if (length(x) < 5) return(cfg_local$scale_epsilon)

      candidates <- c(
        stats::mad(x, center = stats::median(x), constant = 1 / 0.6745, na.rm = TRUE),
        stats::IQR(x, na.rm = TRUE) / 1.349,
        mean(abs(x - stats::median(x)), na.rm = TRUE),
        cfg_local$scale_epsilon
      )
      s <- max(candidates[is.finite(candidates)], na.rm = TRUE)
      max(s, cfg_local$scale_epsilon)
    }
  )
}

conformal_quantile <- function(scores, alpha, type = 8) {
  scores <- scores[is.finite(scores)]
  m <- length(scores)
  if (m == 0) return(NA_real_)

  k <- min(max(ceiling((m + 1) * (1 - alpha)), 1), m)
  sort(scores)[k]
}

interval_score <- function(y, lower, upper, alpha) {
  y <- as.matrix(y)
  lower <- as.matrix(lower)
  upper <- as.matrix(upper)

  width <- upper - lower
  under <- (2 / alpha) * (lower - y) * (y < lower)
  over <- (2 / alpha) * (y - upper) * (y > upper)

  mean(width + under + over, na.rm = TRUE)
}

surface_coverage_metrics <- function(
  Y, lower, upper, alpha,
  method, scheme, state = "All"
) {
  inside <- (Y >= lower) & (Y <= upper)
  upper_inside <- Y <= upper

  tibble::tibble(
    scheme = scheme,
    method = method,
    state = state,
    n = nrow(Y),
    surface_coverage = mean(apply(inside, 1, all)),
    pointwise_coverage = mean(inside),
    upper_surface_coverage = mean(apply(upper_inside, 1, all)),
    mean_band_width = mean(upper - lower),
    mean_upper_half_width = mean(upper - (lower + upper) / 2),
    interval_score = interval_score(Y, lower, upper, alpha)
  )
}

make_scaled_max_scores <- function(Y, pred, scale_vec) {
  apply(
    sweep(abs(as.matrix(Y) - as.matrix(pred)), 2, scale_vec, "/"),
    1,
    max,
    na.rm = TRUE
  )
}

# One-sided simultaneous upper-band nonconformity score. Unlike the
# two-sided score above, this preserves the direction of the forecast error:
# only Y - prediction matters for an upper risk band.
make_scaled_upper_scores <- function(Y, pred, scale_vec) {
  apply(
    sweep(as.matrix(Y) - as.matrix(pred), 2, scale_vec, "/"),
    1,
    max,
    na.rm = TRUE
  )
}

compute_one_sided_upper_conformal <- function(
  design, sp, pred_cal, pred_test, scale_vec,
  alpha, multiplier = 1.00, scheme, quantile_type = 8
) {
  cal_scores <- make_scaled_upper_scores(
    design$Y[sp$cal_idx, , drop = FALSE],
    pred_cal,
    scale_vec
  )

  q <- conformal_quantile(
    cal_scores, alpha, type = quantile_type
  )

  upper_add <- matrix(
    q * multiplier,
    nrow = length(sp$test_idx),
    ncol = design$J
  )
  upper_add <- sweep(upper_add, 2, scale_vec, "*")
  upper <- pred_test + upper_add

  Y_test <- design$Y[sp$test_idx, , drop = FALSE]
  inside_upper <- Y_test <= upper

  list(
    method = "One-sided upper split conformal",
    q = q,
    scale = scale_vec,
    cal_scores = cal_scores,
    upper = upper,
    metrics = tibble::tibble(
      scheme = scheme,
      method = "One-sided upper split conformal",
      n = length(sp$test_idx),
      q = q,
      upper_surface_coverage = mean(apply(inside_upper, 1, all)),
      pointwise_upper_coverage = mean(inside_upper),
      mean_upper_width = mean(upper - pred_test)
    )
  )
}

compute_global_split_conformal <- function(
  design, sp, pred_cal, pred_test, scale_vec,
  alpha, multiplier, scheme, quantile_type = 8
) {
  cal_scores <- make_scaled_max_scores(
    design$Y[sp$cal_idx, , drop = FALSE],
    pred_cal,
    scale_vec
  )

  q <- conformal_quantile(
    cal_scores, alpha, type = quantile_type
  )

  half <- matrix(
    q * multiplier,
    nrow = length(sp$test_idx),
    ncol = design$J
  )
  half <- sweep(half, 2, scale_vec, "*")

  lower <- pred_test - half
  upper <- pred_test + half

  list(
    method = "Global split conformal",
    q = q,
    scale = scale_vec,
    cal_scores = cal_scores,
    lower = lower,
    upper = upper,
    metrics = surface_coverage_metrics(
      design$Y[sp$test_idx, , drop = FALSE],
      lower, upper, alpha,
      "Global split conformal",
      scheme
    )
  )
}

compute_mondrian_conformal <- function(
  design, sp, pred_cal, pred_test, scale_vec,
  states_cal, states_test,
  alpha, multiplier, cfg_local, scheme
) {
  cal_scores <- make_scaled_max_scores(
    design$Y[sp$cal_idx, , drop = FALSE],
    pred_cal,
    scale_vec
  )
  q_global <- conformal_quantile(
    cal_scores, alpha, type = cfg_local$quantile_type
  )

  labels <- unique(c(states_cal, states_test))
  labels <- labels[!is.na(labels)]

  q_state <- setNames(rep(q_global, length(labels)), labels)
  n_state <- setNames(integer(length(labels)), labels)

  for (st in labels) {
    ss <- cal_scores[states_cal == st]
    n_state[st] <- sum(is.finite(ss))
    if (n_state[st] >= cfg_local$conformal_min_state_cal_n) {
      q_state[st] <- conformal_quantile(
        ss, alpha, type = cfg_local$quantile_type
      )
    }
  }

  qv <- q_state[states_test]
  qv[!is.finite(qv)] <- q_global

  half <- matrix(
    qv * multiplier,
    nrow = length(qv),
    ncol = design$J
  )
  half <- sweep(half, 2, scale_vec, "*")

  lower <- pred_test - half
  upper <- pred_test + half

  overall <- surface_coverage_metrics(
    design$Y[sp$test_idx, , drop = FALSE],
    lower, upper, alpha,
    "Mondrian state conformal",
    scheme
  )

  by_state <- purrr::map_dfr(
    labels,
    function(st) {
      pos <- which(states_test == st)
      if (length(pos) == 0) return(tibble::tibble())

      surface_coverage_metrics(
        design$Y[sp$test_idx[pos], , drop = FALSE],
        lower[pos, , drop = FALSE],
        upper[pos, , drop = FALSE],
        alpha,
        "Mondrian state conformal",
        scheme,
        state = st
      )
    }
  )

  list(
    method = "Mondrian state conformal",
    q_global = q_global,
    q_state = q_state,
    n_state = n_state,
    cal_scores = cal_scores,
    lower = lower,
    upper = upper,
    metrics = dplyr::bind_rows(overall, by_state)
  )
}

compute_bonferroni_empirical_bands <- function(
  design, sp, pred_cal, pred_test, alpha, scheme,
  quantile_type = 8
) {
  abs_res <- abs(
    design$Y[sp$cal_idx, , drop = FALSE] - pred_cal
  )

  per_cell_alpha <- alpha / design$J
  qj <- apply(
    abs_res,
    2,
    function(x) safe_quantile(
      x, 1 - per_cell_alpha, type = quantile_type
    )
  )

  half <- matrix(
    qj,
    nrow = length(sp$test_idx),
    ncol = design$J,
    byrow = TRUE
  )
  colnames(half) <- design$cell_names

  lower <- pred_test - half
  upper <- pred_test + half

  list(
    method = "Bonferroni empirical residual bands",
    lower = lower,
    upper = upper,
    metrics = surface_coverage_metrics(
      design$Y[sp$test_idx, , drop = FALSE],
      lower, upper, alpha,
      "Bonferroni empirical residual bands",
      scheme
    )
  )
}

compute_aci_conformal <- function(
  design, sp, pred_cal, pred_test, scale_vec,
  alpha, gamma, scheme, quantile_type = 8
) {
  cal_scores <- make_scaled_max_scores(
    design$Y[sp$cal_idx, , drop = FALSE],
    pred_cal,
    scale_vec
  )

  history <- cal_scores[is.finite(cal_scores)]
  alpha_t <- alpha

  n_test <- length(sp$test_idx)
  lower <- matrix(
    NA_real_, nrow = n_test, ncol = design$J,
    dimnames = list(NULL, design$cell_names)
  )
  upper <- lower
  q_path <- alpha_path <- error_path <- rep(NA_real_, n_test)
  score_path <- rep(NA_real_, n_test)
  feedback_released <- integer(n_test)
  history_n <- integer(n_test)
  feedback_used <- logical(n_test)

  # Delayed-feedback ACI: a test-origin score becomes eligible for the
  # adaptive history only after its complete h_max response is observable.
  # This prevents using a t+h_max label to tune a band at an earlier origin.
  for (tt in seq_len(n_test)) {
    current_idx <- sp$test_idx[tt]
    current_origin <- design$dates[current_idx]

    if (tt > 1L) {
      prior_pos <- seq_len(tt - 1L)
      matured <- prior_pos[
        !feedback_used[prior_pos] &
          design$label_end_dates[sp$test_idx[prior_pos]] < current_origin
      ]

      if (length(matured) > 0) {
        # Release feedback in chronological order.
        for (jj in matured) {
          if (is.finite(score_path[jj])) {
            history <- c(history, score_path[jj])
          }
          if (is.finite(error_path[jj])) {
            alpha_t <- alpha_t + gamma * (alpha - error_path[jj])
            alpha_t <- min(max(alpha_t, 0.001), 0.999)
          }
          feedback_used[jj] <- TRUE
        }
      }
      feedback_released[tt] <- length(matured)
    }

    q_t <- conformal_quantile(
      history,
      alpha = min(max(alpha_t, 0.001), 0.999),
      type = quantile_type
    )

    half <- q_t * scale_vec
    lower[tt, ] <- pred_test[tt, ] - half
    upper[tt, ] <- pred_test[tt, ] + half

    score_t <- max(
      abs(
        design$Y[current_idx, ] - pred_test[tt, ]
      ) / scale_vec,
      na.rm = TRUE
    )
    err_t <- as.numeric(score_t > q_t)

    q_path[tt] <- q_t
    alpha_path[tt] <- alpha_t
    error_path[tt] <- err_t
    score_path[tt] <- score_t
    history_n[tt] <- length(history)
  }

  list(
    method = "Adaptive conformal inference (delayed feedback)",
    lower = lower,
    upper = upper,
    path = tibble::tibble(
      scheme = scheme,
      date = design$dates[sp$test_idx],
      label_end_date = design$label_end_dates[sp$test_idx],
      q = q_path,
      alpha_t = alpha_path,
      error = error_path,
      score = score_path,
      feedback_released = feedback_released,
      history_n = history_n
    ),
    metrics = surface_coverage_metrics(
      design$Y[sp$test_idx, , drop = FALSE],
      lower, upper, alpha,
      "Adaptive conformal inference (delayed feedback)",
      scheme
    )
  )
}

compute_conformal_suite <- function(
  design, sp, fescf, cfg_local
) {
  pred_all <- fescf$full
  pred_cal <- pred_all[
    match(sp$cal_idx, fescf$idx_all), , drop = FALSE
  ]
  pred_test <- pred_all[
    match(sp$test_idx, fescf$idx_all), , drop = FALSE
  ]

  scale_vec <- robust_residual_scale(
    fescf$guard_residuals_for_scale,
    cfg_local
  )

  states_cal <- assign_states(
    design, sp$cal_idx, fescf$thresholds_full, cfg_local
  )
  states_test <- assign_states(
    design, sp$test_idx, fescf$thresholds_full, cfg_local
  )

  global <- compute_global_split_conformal(
    design, sp, pred_cal, pred_test, scale_vec,
    alpha = cfg_local$conformal_alpha,
    multiplier = cfg_local$primary_band_multiplier,
    scheme = sp$scheme,
    quantile_type = cfg_local$quantile_type
  )

  mondrian <- compute_mondrian_conformal(
    design, sp, pred_cal, pred_test, scale_vec,
    states_cal, states_test,
    alpha = cfg_local$conformal_alpha,
    multiplier = 1.00,
    cfg_local = cfg_local,
    scheme = sp$scheme
  )

  bonf <- compute_bonferroni_empirical_bands(
    design, sp, pred_cal, pred_test,
    alpha = cfg_local$conformal_alpha,
    scheme = sp$scheme,
    quantile_type = cfg_local$quantile_type
  )

  aci <- compute_aci_conformal(
    design, sp, pred_cal, pred_test, scale_vec,
    alpha = cfg_local$conformal_alpha,
    gamma = cfg_local$aci_gamma,
    scheme = sp$scheme,
    quantile_type = cfg_local$quantile_type
  )

  # Multiplier sensitivity for global split conformal.
  multiplier_sensitivity <- purrr::map_dfr(
    cfg_local$band_multiplier_grid,
    function(mb) {
      z <- compute_global_split_conformal(
        design, sp, pred_cal, pred_test, scale_vec,
        alpha = cfg_local$conformal_alpha,
        multiplier = mb,
        scheme = sp$scheme,
        quantile_type = cfg_local$quantile_type
      )
      z$metrics |>
        dplyr::mutate(band_multiplier = mb)
    }
  )

  # Nominal-level sensitivity.
  alpha_sensitivity <- purrr::map_dfr(
    cfg_local$conformal_alpha_grid,
    function(a) {
      z <- compute_global_split_conformal(
        design, sp, pred_cal, pred_test, scale_vec,
        alpha = a,
        multiplier = 1.00,
        scheme = sp$scheme,
        quantile_type = cfg_local$quantile_type
      )
      z$metrics |>
        dplyr::mutate(alpha = a)
    }
  )

  # Primary global bands summarized by train-only state.
  primary_state_metrics <- purrr::map_dfr(
    unique(states_test),
    function(st) {
      pos <- which(states_test == st)
      if (length(pos) == 0) return(tibble::tibble())

      surface_coverage_metrics(
        design$Y[sp$test_idx[pos], , drop = FALSE],
        global$lower[pos, , drop = FALSE],
        global$upper[pos, , drop = FALSE],
        cfg_local$conformal_alpha,
        "Global split conformal",
        sp$scheme,
        state = st
      )
    }
  )

  # Genuine one-sided upper simultaneous conformal band. The calibration
  # score is max_j (Y_j - prediction_j) / scale_j, not the upper half of a
  # two-sided absolute-residual band.
  upper_only <- compute_one_sided_upper_conformal(
    design = design,
    sp = sp,
    pred_cal = pred_cal,
    pred_test = pred_test,
    scale_vec = scale_vec,
    alpha = cfg_local$conformal_alpha,
    multiplier = 1.00,
    scheme = sp$scheme,
    quantile_type = cfg_local$quantile_type
  )

  list(
    scale = scale_vec,
    states_cal = states_cal,
    states_test = states_test,
    global = global,
    mondrian = mondrian,
    bonferroni = bonf,
    aci = aci,
    all_metrics = dplyr::bind_rows(
      global$metrics,
      primary_state_metrics,
      mondrian$metrics,
      bonf$metrics,
      aci$metrics
    ),
    multiplier_sensitivity = multiplier_sensitivity,
    alpha_sensitivity = alpha_sensitivity,
    upper_only = upper_only$metrics,
    upper_only_object = upper_only
  )
}

# ============================================================
# Evaluation
# ============================================================

split_prediction_matrix <- function(M, idx_all, target_idx) {
  M[match(target_idx, idx_all), , drop = FALSE]
}

evaluate_prediction_list <- function(
  design, sp, prediction_list, weights = design$weights
) {
  Yt <- design$Y[sp$test_idx, , drop = FALSE]

  purrr::map_dfr(
    names(prediction_list),
    function(m) {
      pred <- prediction_list[[m]]
      tibble::tibble(
        scheme = sp$scheme,
        model = m,
        RMSE = rmse_all(Yt, pred),
        MAE = mae_all(Yt, pred),
        Weighted_RMSE = weighted_rmse_all(Yt, pred, weights),
        Terminal_RMSE = terminal_rmse_all(
          Yt, pred, design$cell_grid
        ),
        High_Tau_RMSE = high_tau_rmse_all(
          Yt, pred, design$cell_grid
        )
      )
    }
  ) |>
    dplyr::arrange(Weighted_RMSE)
}

projection_effect_table <- function(
  design, sp, raw_models, projected_models
) {
  Yt <- design$Y[sp$test_idx, , drop = FALSE]

  purrr::map_dfr(
    intersect(names(raw_models), names(projected_models)),
    function(m) {
      raw <- raw_models[[m]]
      proj <- projected_models[[m]]

      pre <- surface_violation_summary(raw, design$cell_grid)
      post <- surface_violation_summary(proj, design$cell_grid, cfg$projection_tol)

      wr_raw <- weighted_rmse_all(Yt, raw, design$weights)
      wr_proj <- weighted_rmse_all(Yt, proj, design$weights)

      tibble::tibble(
        scheme = sp$scheme,
        model = m,
        pre_negative_count = pre$negative_count,
        pre_tau_violation_count = pre$tau_violation_count,
        pre_horizon_violation_count = pre$horizon_violation_count,
        pre_total_violation_magnitude =
          pre$negative_magnitude +
          pre$tau_violation_magnitude +
          pre$horizon_violation_magnitude,
        post_negative_count = post$negative_count,
        post_tau_violation_count = post$tau_violation_count,
        post_horizon_violation_count = post$horizon_violation_count,
        post_total_violation_magnitude = post$negative_magnitude +
          post$tau_violation_magnitude + post$horizon_violation_magnitude,
        post_max_raw_violation = post$max_raw_violation,
        weighted_rmse_raw = wr_raw,
        weighted_rmse_projected = wr_proj,
        projection_loss_change = wr_proj - wr_raw
      )
    }
  )
}

loss_time_series <- function(
  design, sp, prediction_list, weights = design$weights
) {
  purrr::map_dfr(
    names(prediction_list),
    function(m) {
      tibble::tibble(
        scheme = sp$scheme,
        date = design$dates[sp$test_idx],
        state = NA_character_,
        model = m,
        weighted_loss = weighted_mse_rows(
          design$Y[sp$test_idx, , drop = FALSE],
          prediction_list[[m]],
          weights
        ),
        unweighted_loss = rowMeans(
          (design$Y[sp$test_idx, , drop = FALSE] -
             prediction_list[[m]])^2,
          na.rm = TRUE
        ),
        absolute_error = rowMeans(
          abs(
            design$Y[sp$test_idx, , drop = FALSE] -
              prediction_list[[m]]
          ),
          na.rm = TRUE
        )
      )
    }
  )
}

predictive_accuracy_tables <- function(
  design, sp, predictions, proposed_name, cfg_local
) {
  stopifnot(proposed_name %in% names(predictions))

  prop_loss <- weighted_mse_rows(
    design$Y[sp$test_idx, , drop = FALSE],
    predictions[[proposed_name]],
    design$weights
  )

  benchmarks <- setdiff(names(predictions), proposed_name)

  dm <- purrr::map_dfr(
    benchmarks,
    function(bm) {
      bm_loss <- weighted_mse_rows(
        design$Y[sp$test_idx, , drop = FALSE],
        predictions[[bm]],
        design$weights
      )

      purrr::map_dfr(
        cfg_local$dm_nw_lags,
        function(lag) {
          dm_test(bm_loss, prop_loss, lag) |>
            dplyr::mutate(
              scheme = sp$scheme,
              benchmark = bm,
              proposed = proposed_name,
              .before = 1
            )
        }
      )
    }
  ) |>
    dplyr::group_by(scheme, nw_lag) |>
    dplyr::mutate(
      p_holm_two_sided = stats::p.adjust(
        p_two_sided, method = "holm"
      ),
      p_holm_one_sided = stats::p.adjust(
        p_one_sided_favors_prop, method = "holm"
      )
    ) |>
    dplyr::ungroup()

  boot <- purrr::map_dfr(
    benchmarks,
    function(bm) {
      bm_loss <- weighted_mse_rows(
        design$Y[sp$test_idx, , drop = FALSE],
        predictions[[bm]],
        design$weights
      )
      d <- bm_loss - prop_loss
      ci <- block_bootstrap_mean_ci(
        d,
        B = cfg_local$bootstrap_reps,
        block_length = cfg_local$bootstrap_block_length,
        conf = 0.95,
        seed = cfg_local$seed + match(bm, benchmarks)
      )
      ci |>
        dplyr::mutate(
          scheme = sp$scheme,
          benchmark = bm,
          proposed = proposed_name,
          block_length = cfg_local$bootstrap_block_length,
          reps = cfg_local$bootstrap_reps,
          .before = 1
        )
    }
  )

  list(dm = dm, bootstrap = boot)
}

# ============================================================
# FPCA-score stationarity / structural-stability diagnostic
# ============================================================

functional_kpss_statistic <- function(Y) {
  Y <- as.matrix(Y)
  Y <- sweep(Y, 2, colMeans(Y, na.rm = TRUE), "-")
  Y[!is.finite(Y)] <- 0
  n <- nrow(Y)
  if (n < 10) return(NA_real_)

  # Discrete Hilbert-space version of the KPSS-type partial-sum energy.
  S <- apply(Y, 2, cumsum)
  if (is.vector(S)) S <- matrix(S, ncol = ncol(Y))
  Z <- S / sqrt(n)
  mean(rowMeans(Z^2))
}

stationarity_cusum_diagnostic <- function(
  design, train_idx, cfg_local, scheme
) {
  Y <- design$Y[train_idx, , drop = FALSE]
  n <- nrow(Y)

  # ----------------------------------------------------------
  # 1) Functional KPSS-type surface-level stationarity test
  #    calibrated by moving-block bootstrap. This operationalizes
  #    the stationarity-null logic of Horvath--Kokoszka--Rice (2014)
  #    on the discretized horizon-by-tail surface.
  # ----------------------------------------------------------
  stat_kpss <- functional_kpss_statistic(Y)
  B <- min(cfg_local$bootstrap_reps, 499L)
  L <- min(cfg_local$bootstrap_block_length, n)
  starts <- seq_len(n - L + 1L)
  n_blocks <- ceiling(n / L)

  Y_center <- sweep(Y, 2, colMeans(Y, na.rm = TRUE), "-")
  Y_center[!is.finite(Y_center)] <- 0
  boot_kpss <- numeric(B)

  set.seed(cfg_local$seed + 770)
  for (b in seq_len(B)) {
    chosen <- sample(starts, n_blocks, replace = TRUE)
    idx <- unlist(
      lapply(chosen, function(s) s:(s + L - 1L)),
      use.names = FALSE
    )
    idx <- idx[seq_len(n)]
    Yb <- Y_center[idx, , drop = FALSE]
    boot_kpss[b] <- functional_kpss_statistic(Yb)
  }
  p_kpss <- mean(boot_kpss >= stat_kpss, na.rm = TRUE)

  # ----------------------------------------------------------
  # 2) FPCA-score CUSUM structural-stability diagnostic
  # ----------------------------------------------------------
  Yc <- sweep(Y, 2, colMeans(Y), "-")
  sv <- svd(Yc)
  ve <- sv$d^2 / sum(sv$d^2)

  K <- min(5L, length(sv$d), ncol(Y))
  scores <- Yc %*% sv$v[, seq_len(K), drop = FALSE]
  scores <- scale(scores, center = TRUE, scale = TRUE)
  scores[!is.finite(scores)] <- 0

  cs <- apply(scores, 2, cumsum)
  if (is.vector(cs)) cs <- matrix(cs, ncol = K)
  stat_path <- sqrt(rowSums(cs^2)) / sqrt(n)
  stat_cusum <- max(stat_path, na.rm = TRUE)

  set.seed(cfg_local$seed + 771)
  centered <- scale(scores, center = TRUE, scale = FALSE)
  boot_cusum <- numeric(B)

  for (b in seq_len(B)) {
    chosen <- sample(starts, n_blocks, replace = TRUE)
    idx <- unlist(
      lapply(chosen, function(s) s:(s + L - 1L)),
      use.names = FALSE
    )
    idx <- idx[seq_len(n)]
    sb <- centered[idx, , drop = FALSE]
    csb <- apply(sb, 2, cumsum)
    if (is.vector(csb)) csb <- matrix(csb, ncol = K)
    boot_cusum[b] <- max(
      sqrt(rowSums(csb^2)) / sqrt(n),
      na.rm = TRUE
    )
  }
  p_cusum <- mean(boot_cusum >= stat_cusum, na.rm = TRUE)

  dplyr::bind_rows(
    tibble::tibble(
      design = design$name,
      scheme = scheme,
      diagnostic = "Functional KPSS-type block bootstrap",
      null_hypothesis = "Stationary weakly dependent surface process",
      n = n,
      K = NA_integer_,
      first_pc_variance_share = NA_real_,
      first_5_pc_variance_share = NA_real_,
      statistic = stat_kpss,
      bootstrap_p = p_kpss,
      block_length = L,
      reps = B
    ),
    tibble::tibble(
      design = design$name,
      scheme = scheme,
      diagnostic = "FPCA-score CUSUM block bootstrap",
      null_hypothesis = "No structural instability in leading FPCA scores",
      n = n,
      K = K,
      first_pc_variance_share = ve[1],
      first_5_pc_variance_share = sum(ve[seq_len(K)]),
      statistic = stat_cusum,
      bootstrap_p = p_cusum,
      block_length = L,
      reps = B
    )
  )
}

# ============================================================
# Main split execution
# ============================================================

run_split_uncached <- function(
  design, sp, cfg_local,
  include_external_benchmarks = TRUE,
  include_conformal = TRUE,
  include_inference = TRUE,
  include_stationarity = FALSE
) {
  message("Running ", design$name, " / ", sp$scheme)

  idx_all <- c(sp$train_idx, sp$cal_idx, sp$test_idx)
  audit_split_leakage(design, sp, cfg_local)

  # Candidate anchors: raw and projected.
  base_raw_all <- fit_candidate_anchors_raw(
    design, sp$train_idx, idx_all, cfg_local, sp$scheme
  )
  base_proj_all <- project_model_list(
    base_raw_all, design, cfg_local, sp$scheme, "benchmark"
  )

  # External reviewer-requested benchmarks.
  ext_raw_all <- list()
  ext_proj_all <- list()

  if (isTRUE(include_external_benchmarks)) {
    ext_raw_all <- fit_external_benchmarks_raw(
      design, sp$train_idx, idx_all, cfg_local, sp$scheme
    )
    if (length(ext_raw_all) > 0) {
      ext_proj_all <- project_model_list(
        ext_raw_all, design, cfg_local, sp$scheme, "external_benchmark"
      )
    }
  }

  # Equal-weight combination of five prespecified candidate anchors.
  eq_all <- Reduce("+", base_proj_all) / length(base_proj_all)
  eq_all <- project_surface_matrix(
    eq_all,
    design$cell_grid,
    cfg_local,
    sp$scheme,
    "Equal-weight anchor combination",
    "benchmark"
  )
  rownames(eq_all) <- idx_all

  # FES-CF + ablation.
  fescf <- timed(
    "FES-CF full architecture",
    sp$scheme,
    function() fit_predict_fescf(design, sp, cfg_local)
  )

  # Main test predictions.
  benchmark_all <- c(
    base_proj_all,
    ext_proj_all,
    list("Equal-weight anchor combination" = eq_all)
  )

  benchmark_test <- purrr::map(
    benchmark_all,
    function(M) split_prediction_matrix(M, idx_all, sp$test_idx)
  )

  fescf_test <- split_prediction_matrix(
    fescf$full, fescf$idx_all, sp$test_idx
  )

  main_predictions <- c(
    benchmark_test,
    list("FES-CF full" = fescf_test)
  )

  # Top-three blending is a reviewer-requested sensitivity analysis only.
  # It is deliberately excluded from the primary ranking, DM/bootstrap
  # comparisons, and Model Confidence Set. Its performance remains available
  # through the ablation/sensitivity outputs below.

  performance <- evaluate_prediction_list(
    design, sp, main_predictions, design$weights
  )

  rank_rows <- performance |>
    dplyr::mutate(
      rank = rank(Weighted_RMSE, ties.method = "min")
    )

  # Ablation performance.
  ablation_test <- purrr::map(
    fescf$ablations,
    function(M) split_prediction_matrix(
      M, fescf$idx_all, sp$test_idx
    )
  )
  ablation_performance <- evaluate_prediction_list(
    design, sp, ablation_test, design$weights
  )

  # Projection diagnostics for all conventional benchmarks.
  base_raw_test <- purrr::map(
    base_raw_all,
    function(M) split_prediction_matrix(M, idx_all, sp$test_idx)
  )
  base_proj_test <- purrr::map(
    base_proj_all,
    function(M) split_prediction_matrix(M, idx_all, sp$test_idx)
  )
  ext_raw_test <- purrr::map(
    ext_raw_all,
    function(M) split_prediction_matrix(M, idx_all, sp$test_idx)
  )
  ext_proj_test <- purrr::map(
    ext_proj_all,
    function(M) split_prediction_matrix(M, idx_all, sp$test_idx)
  )

  fescf_raw_test <- split_prediction_matrix(
    fescf$ablations[["Blend + state correction, no final projection"]],
    fescf$idx_all,
    sp$test_idx
  )
  fescf_proj_test <- split_prediction_matrix(
    fescf$full,
    fescf$idx_all,
    sp$test_idx
  )

  projection_effect <- dplyr::bind_rows(
    projection_effect_table(
      design, sp, base_raw_test, base_proj_test
    ),
    if (length(ext_raw_test) > 0) {
      projection_effect_table(
        design, sp, ext_raw_test, ext_proj_test
      )
    } else {
      tibble::tibble()
    },
    projection_effect_table(
      design,
      sp,
      list("FES-CF full" = fescf_raw_test),
      list("FES-CF full" = fescf_proj_test)
    )
  )

  # State labels for loss decomposition.
  states_test <- assign_states(
    design, sp$test_idx, fescf$thresholds_full, cfg_local
  )

  loss_ts <- loss_time_series(
    design, sp, main_predictions, design$weights
  ) |>
    dplyr::mutate(
      state = rep(
        states_test,
        times = length(main_predictions)
      )
    )

  inference <- list(
    dm = tibble::tibble(),
    bootstrap = tibble::tibble()
  )
  if (isTRUE(include_inference)) {
    inference <- predictive_accuracy_tables(
      design,
      sp,
      main_predictions,
      proposed_name = "FES-CF full",
      cfg_local = cfg_local
    )
  }

  conformal <- NULL
  if (isTRUE(include_conformal)) {
    conformal <- compute_conformal_suite(
      design, sp, fescf, cfg_local
    )
  }

  stationarity <- tibble::tibble()
  if (isTRUE(include_stationarity) &&
      isTRUE(cfg_local$run_stationarity_diagnostic)) {
    stationarity <- stationarity_cusum_diagnostic(
      design, sp$train_idx, cfg_local, sp$scheme
    )
  }

  state_counts <- dplyr::bind_rows(
    tibble::tibble(
      scheme = sp$scheme,
      block = "train",
      state = assign_states(
        design, sp$train_idx, fescf$thresholds_full, cfg_local
      )
    ),
    tibble::tibble(
      scheme = sp$scheme,
      block = "calibration",
      state = assign_states(
        design, sp$cal_idx, fescf$thresholds_full, cfg_local
      )
    ),
    tibble::tibble(
      scheme = sp$scheme,
      block = "test",
      state = states_test
    )
  ) |>
    dplyr::count(scheme, block, state, name = "N")

  list(
    design_name = design$name,
    split = sp,
    performance = performance,
    rank_rows = rank_rows,
    ablation_performance = ablation_performance,
    projection_effect = projection_effect,
    loss_ts = loss_ts,
    inference = inference,
    conformal = conformal,
    stationarity = stationarity,
    state_counts = state_counts,
    fescf = fescf,
    main_predictions = main_predictions
  )
}

# Complete split results are persisted atomically. An interrupted split restarts;
# earlier completed splits are reused only with identical code, data and settings.
atomic_save_rds <- function(object, path) {
  tmp <- tempfile(pattern = ".fescf-", tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(object, tmp, compress = FALSE)
  if (file.exists(path)) stop("Refusing to overwrite immutable checkpoint: ", path)
  if (!file.rename(tmp, path)) stop("Cannot commit checkpoint: ", path)
  invisible(path)
}

run_split <- function(design, sp, cfg_local, include_external_benchmarks = TRUE,
                       include_conformal = TRUE, include_inference = TRUE,
                       include_stationarity = FALSE) {
  settings <- list(external = include_external_benchmarks, conformal = include_conformal,
                   inference = include_inference, stationarity = include_stationarity)
  key <- digest::digest(list(implementation_hash, runtime_signature, design, sp,
                             cfg_local, settings), algo = "sha256")
  path <- file.path(cfg_local$output_dir, "checkpoints", paste0(key, ".rds"))
  if (file.exists(path)) {
    cached <- readRDS(path)
    if (!identical(cached$key, key)) stop("Checkpoint identity mismatch: ", path)
    for (nm in names(cached$logs)) append_log(nm, cached$logs[[nm]])
    assign(".Random.seed", cached$rng_after, envir = .GlobalEnv)
    message("RESUME: ", design$name, " / ", sp$scheme, " (completed split)")
    return(cached$result)
  }
  # A key-specific seed makes a split reproducible even if execution is resumed
  # after another process or an interrupted split has consumed random numbers.
  split_seed <- as.integer(strtoi(substr(key, 1, 7), base = 16L))
  set.seed(split_seed)
  slots <- ls(audit_env)
  before <- vapply(slots, function(nm) nrow(audit_env[[nm]]), integer(1))
  result <- run_split_uncached(design, sp, cfg_local,
    include_external_benchmarks, include_conformal, include_inference, include_stationarity)
  logs <- setNames(lapply(slots, function(nm) {
    tab <- audit_env[[nm]]
    if (nrow(tab) <= before[[nm]]) tab[0, , drop = FALSE]
    else tab[seq.int(before[[nm]] + 1L, nrow(tab)), , drop = FALSE]
  }), slots)
  atomic_save_rds(list(key = key, result = result, logs = logs,
                       rng_after = get(".Random.seed", envir = .GlobalEnv)), path)
  # The small audit snapshot remains inspectable while the full run is in progress.
  for (nm in slots) readr::write_csv(audit_env[[nm]],
    file.path(cfg_local$output_dir, "audit", paste0("progress_", nm, ".csv")))
  message("SAVED: ", design$name, " / ", sp$scheme)
  result
}


# ============================================================
# Primary analysis: main holdout + 20 rolling splits
# ============================================================

primary_main_split <- make_main_split(
  primary_design, cfg, embargo = cfg$embargo
)
primary_rolling_splits <- make_rolling_splits(
  primary_design, cfg, embargo = cfg$embargo
)

primary_splits <- c(
  list(primary_main_split),
  primary_rolling_splits
)

split_summary <- purrr::map_dfr(
  primary_splits,
  function(sp) {
    tibble::tibble(
      scheme = sp$scheme,
      embargo = sp$embargo,
      n_train = length(sp$train_idx),
      n_cal = length(sp$cal_idx),
      n_test = length(sp$test_idx),
      train_start = as.character(
        primary_design$dates[min(sp$train_idx)]
      ),
      train_end = as.character(
        primary_design$dates[max(sp$train_idx)]
      ),
      cal_start = as.character(
        primary_design$dates[min(sp$cal_idx)]
      ),
      cal_end = as.character(
        primary_design$dates[max(sp$cal_idx)]
      ),
      test_start = as.character(
        primary_design$dates[min(sp$test_idx)]
      ),
      test_end = as.character(
        primary_design$dates[max(sp$test_idx)]
      )
    )
  }
)

primary_results <- vector("list", length(primary_splits))

for (i in seq_along(primary_splits)) {
  sp <- primary_splits[[i]]
  primary_results[[i]] <- run_split(
    design = primary_design,
    sp = sp,
    cfg_local = cfg,
    include_external_benchmarks = TRUE,
    include_conformal = TRUE,
    include_inference = TRUE,
    include_stationarity = identical(sp$scheme, "main_holdout")
  )
}

names(primary_results) <- vapply(
  primary_splits, `[[`, character(1), "scheme"
)

# ============================================================
# Collect primary outputs
# ============================================================

all_performance <- purrr::map_dfr(
  primary_results, "performance"
)
all_rank_rows <- purrr::map_dfr(
  primary_results, "rank_rows"
)
all_ablation_performance <- purrr::map_dfr(
  primary_results, "ablation_performance"
)
all_projection_effect <- purrr::map_dfr(
  primary_results, "projection_effect"
)
all_loss_ts <- purrr::map_dfr(
  primary_results, "loss_ts"
)
all_dm <- purrr::map_dfr(
  primary_results,
  function(x) x$inference$dm
)
all_bootstrap_ci <- purrr::map_dfr(
  primary_results,
  function(x) x$inference$bootstrap
)
all_state_counts <- purrr::map_dfr(
  primary_results, "state_counts"
)
all_stationarity <- purrr::map_dfr(
  primary_results, "stationarity"
)

all_anchor_selection <- purrr::map_dfr(
  primary_results,
  function(x) x$fescf$anchor_selection
)
all_correction_summary <- purrr::map_dfr(
  primary_results,
  function(x) x$fescf$correction_summary
)
all_state_gate <- purrr::map_dfr(
  primary_results,
  function(x) x$fescf$state_gate_blend
)
all_secondary_blend_guard <- purrr::map_dfr(
  primary_results,
  function(x) x$fescf$secondary_blend_guard
)
all_top3_blend_guard <- purrr::map_dfr(
  primary_results,
  function(x) x$fescf$top3_blend_guard
)

all_conformal_metrics <- purrr::map_dfr(
  primary_results,
  function(x) {
    if (is.null(x$conformal)) return(tibble::tibble())
    x$conformal$all_metrics
  }
)
all_conformal_multiplier <- purrr::map_dfr(
  primary_results,
  function(x) {
    if (is.null(x$conformal)) return(tibble::tibble())
    x$conformal$multiplier_sensitivity
  }
)
all_conformal_alpha <- purrr::map_dfr(
  primary_results,
  function(x) {
    if (is.null(x$conformal)) return(tibble::tibble())
    x$conformal$alpha_sensitivity
  }
)
all_conformal_upper <- purrr::map_dfr(
  primary_results,
  function(x) {
    if (is.null(x$conformal)) return(tibble::tibble())
    x$conformal$upper_only
  }
)
all_aci_path <- purrr::map_dfr(
  primary_results,
  function(x) {
    if (is.null(x$conformal)) return(tibble::tibble())
    x$conformal$aci$path
  }
)


rolling_2020_schemes <- split_summary |>
  dplyr::filter(
    grepl("^rolling_", scheme),
    as.Date(test_start) <= as.Date("2020-06-30"),
    as.Date(test_end) >= as.Date("2020-02-15")
  ) |>
  dplyr::pull(scheme)

rolling_2020_conformal <- all_conformal_metrics |>
  dplyr::filter(scheme %in% rolling_2020_schemes)

main_perf <- all_performance |>
  dplyr::filter(scheme == "main_holdout") |>
  dplyr::arrange(Weighted_RMSE)

main_ablation <- all_ablation_performance |>
  dplyr::filter(scheme == "main_holdout") |>
  dplyr::arrange(Weighted_RMSE)

rolling_rank_summary <- all_rank_rows |>
  dplyr::filter(grepl("^rolling_", scheme)) |>
  dplyr::group_by(model) |>
  dplyr::summarise(
    n_splits = dplyr::n_distinct(scheme),
    avg_rank = mean(rank),
    wins = sum(rank == 1),
    top2 = sum(rank <= 2),
    avg_weighted_rmse = mean(Weighted_RMSE),
    .groups = "drop"
  ) |>
  dplyr::arrange(avg_rank, avg_weighted_rmse)

rolling_split_wide <- all_performance |>
  dplyr::filter(grepl("^rolling_", scheme)) |>
  dplyr::select(scheme, model, Weighted_RMSE) |>
  tidyr::pivot_wider(
    names_from = model,
    values_from = Weighted_RMSE
  ) |>
  dplyr::arrange(scheme)

main_dm_primary <- all_dm |>
  dplyr::filter(
    scheme == "main_holdout",
    nw_lag == cfg$primary_dm_lag
  ) |>
  dplyr::arrange(p_holm_one_sided)

main_bootstrap_ci <- all_bootstrap_ci |>
  dplyr::filter(scheme == "main_holdout") |>
  dplyr::arrange(dplyr::desc(mean))

# ============================================================
# Reviewer-compliance pooled / daily forecast-comparison outputs
# ============================================================

# Daily main-holdout weighted-loss differences relative to FES-CF.
# Positive values favor FES-CF. The presentation table is scaled by 10,000
# because these are squared-error loss differences rather than RMSE levels.
main_daily_loss_diff_raw <- all_loss_ts |>
  dplyr::filter(scheme == "main_holdout") |>
  dplyr::select(date, model, weighted_loss) |>
  tidyr::pivot_wider(
    names_from = model,
    values_from = weighted_loss
  )

main_daily_loss_diff <- tibble::tibble(
  date = main_daily_loss_diff_raw$date
)
if ("FES-CF full" %in% names(main_daily_loss_diff_raw)) {
  daily_benchmarks <- setdiff(
    names(main_daily_loss_diff_raw),
    c("date", "FES-CF full")
  )
  for (bm in daily_benchmarks) {
    main_daily_loss_diff[[paste0(
      "Loss diff x10000: ", model_display_name(bm), " - FES-CF"
    )]] <- 10000 * (
      main_daily_loss_diff_raw[[bm]] -
        main_daily_loss_diff_raw[["FES-CF full"]]
    )
  }
}

# Pooled rolling ablation summary. Rank is computed within each rolling split
# over the ablation variants only, then summarized across the 20 schemes.
rolling_ablation_summary <- all_ablation_performance |>
  dplyr::filter(grepl("^rolling_", scheme)) |>
  dplyr::group_by(scheme) |>
  dplyr::mutate(
    ablation_rank = rank(Weighted_RMSE, ties.method = "min")
  ) |>
  dplyr::ungroup() |>
  dplyr::group_by(model) |>
  dplyr::summarise(
    n_splits = dplyr::n_distinct(scheme),
    avg_rank = mean(ablation_rank, na.rm = TRUE),
    wins = sum(ablation_rank == 1, na.rm = TRUE),
    top2 = sum(ablation_rank <= 2, na.rm = TRUE),
    avg_weighted_rmse = mean(Weighted_RMSE, na.rm = TRUE),
    median_weighted_rmse = stats::median(Weighted_RMSE, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(avg_rank, avg_weighted_rmse)

rolling_ablation_summary_presentation <- rolling_ablation_summary |>
  dplyr::transmute(
    Variant = model,
    `N splits` = n_splits,
    `Average rank` = avg_rank,
    Wins = wins,
    `Top 2` = top2,
    `Average weighted RMSE x100` = 100 * avg_weighted_rmse,
    `Median weighted RMSE x100` = 100 * median_weighted_rmse
  )

# Direct pooled rolling comparison requested by the reviewer: FES-CF versus
# the rolling-mean completed surface using the same moving-block bootstrap.
pooled_rolling_pair <- all_loss_ts |>
  dplyr::filter(
    grepl("^rolling_", scheme),
    model %in% c("FES-CF full", "Rolling mean completed surface")
  ) |>
  dplyr::select(scheme, date, model, weighted_loss) |>
  tidyr::pivot_wider(
    names_from = model,
    values_from = weighted_loss
  ) |>
  dplyr::arrange(scheme, date)

pooled_rolling_diff <-
  pooled_rolling_pair[["Rolling mean completed surface"]] -
  pooled_rolling_pair[["FES-CF full"]]

pooled_rolling_ci <- block_bootstrap_mean_ci(
  pooled_rolling_diff,
  B = cfg$bootstrap_reps,
  block_length = cfg$bootstrap_block_length,
  conf = 0.95,
  seed = cfg$seed + 902
)

pooled_rolling_fescf_vs_rolling_mean <- tibble::tibble(
  comparison = "Rolling mean completed surface - FES-CF",
  n = sum(is.finite(pooled_rolling_diff)),
  block_length = cfg$bootstrap_block_length,
  bootstrap_reps = cfg$bootstrap_reps,
  `Mean weighted-loss diff x10000` = 10000 * pooled_rolling_ci$mean,
  `95pct CI lower x10000` = 10000 * pooled_rolling_ci$lower,
  `95pct CI upper x10000` = 10000 * pooled_rolling_ci$upper,
  p_one_sided_favors_fescf = pooled_rolling_ci$p_one_sided_gt0
)

# ============================================================
# Loss decomposition: year, state, top 5%, stress windows
# ============================================================

yearly_loss <- all_loss_ts |>
  dplyr::mutate(year = format(date, "%Y")) |>
  dplyr::group_by(scheme, year, model) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean_weighted_loss = mean(weighted_loss),
    weighted_rmse = sqrt(mean(weighted_loss)),
    mean_unweighted_loss = mean(unweighted_loss),
    .groups = "drop"
  )

state_loss <- all_loss_ts |>
  dplyr::group_by(scheme, state, model) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean_weighted_loss = mean(weighted_loss),
    weighted_rmse = sqrt(mean(weighted_loss)),
    .groups = "drop"
  )

top5_gain_share <- function(loss_ts, scheme_name = "main_holdout") {
  wide <- loss_ts |>
    dplyr::filter(scheme == scheme_name) |>
    dplyr::select(date, model, weighted_loss) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = weighted_loss
    )

  if (!"FES-CF full" %in% names(wide)) return(tibble::tibble())

  prop <- wide[["FES-CF full"]]
  benchmarks <- setdiff(names(wide), c("date", "FES-CF full"))

  purrr::map_dfr(
    benchmarks,
    function(bm) {
      d <- wide[[bm]] - prop
      ok <- is.finite(d)
      d <- d[ok]
      n_top <- max(1L, ceiling(0.05 * length(d)))
      ord <- order(d, decreasing = TRUE)
      total <- sum(d)

      tibble::tibble(
        scheme = scheme_name,
        benchmark = bm,
        mean_loss_diff = mean(d),
        cumulative_loss_diff = total,
        top5pct_n = n_top,
        top5pct_gain = sum(d[ord[seq_len(n_top)]]),
        top5pct_share_of_total = ifelse(
          total > 0,
          sum(d[ord[seq_len(n_top)]]) / total,
          NA_real_
        )
      )
    }
  )
}

main_top5_gain_share <- top5_gain_share(
  all_loss_ts, "main_holdout"
)

stress_windows <- tibble::tribble(
  ~window, ~start, ~end,
  "2015-2016 stress", as.Date("2015-08-01"), as.Date("2016-03-31"),
  "2018 stress", as.Date("2018-02-01"), as.Date("2018-12-31"),
  "2020 pandemic stress", as.Date("2020-02-15"), as.Date("2020-06-30"),
  "2022 stress", as.Date("2022-01-01"), as.Date("2022-12-31"),
  "2024 window", as.Date("2024-01-01"), as.Date("2024-12-31")
)

stress_loss <- purrr::map_dfr(
  seq_len(nrow(stress_windows)),
  function(i) {
    ww <- stress_windows[i, ]
    all_loss_ts |>
      dplyr::filter(
        grepl("^rolling_", scheme),
        date >= ww$start,
        date <= ww$end
      ) |>
      dplyr::group_by(model) |>
      dplyr::summarise(
        n = dplyr::n(),
        weighted_rmse = sqrt(mean(weighted_loss)),
        mean_weighted_loss = mean(weighted_loss),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        window = ww$window,
        start = ww$start,
        end = ww$end,
        .before = 1
      )
  }
)

# Guard gain vs realized test gain.
guard_vs_realized <- purrr::map_dfr(
  primary_results,
  function(res) {
    sp <- res$split
    chosen <- res$fescf$chosen_anchor

    perf <- res$performance
    selected_wr <- perf$Weighted_RMSE[
      perf$model == chosen
    ]
    full_wr <- perf$Weighted_RMSE[
      perf$model == "FES-CF full"
    ]

    cs <- res$fescf$correction_summary

    tibble::tibble(
      scheme = sp$scheme,
      chosen_anchor = chosen,
      guard_gain = cs$guard_gain_full,
      selected_anchor_test_wrmse = selected_wr,
      fescf_test_wrmse = full_wr,
      realized_test_gain = ifelse(
        length(selected_wr) == 1 &&
          length(full_wr) == 1 &&
          selected_wr > 0,
        (selected_wr^2 - full_wr^2) / selected_wr^2,
        NA_real_
      )
    )
  }
)

# ============================================================
# Main / pooled rolling Model Confidence Sets
# ============================================================

loss_matrix_from_ts <- function(loss_ts, schemes) {
  loss_ts |>
    dplyr::filter(scheme %in% schemes) |>
    dplyr::select(scheme, date, model, weighted_loss) |>
    dplyr::mutate(row_id = paste(scheme, date, sep = "::")) |>
    dplyr::select(row_id, model, weighted_loss) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = weighted_loss
    ) |>
    dplyr::select(-row_id)
}

main_loss_matrix <- loss_matrix_from_ts(
  all_loss_ts, "main_holdout"
)
rolling_loss_matrix <- loss_matrix_from_ts(
  all_loss_ts,
  unique(
    all_loss_ts$scheme[
      grepl("^rolling_", all_loss_ts$scheme)
    ]
  )
)

main_mcs <- mcs_block_bootstrap(
  main_loss_matrix,
  alpha = cfg$mcs_alpha,
  B = cfg$mcs_reps,
  block_length = cfg$bootstrap_block_length,
  seed = cfg$seed + 900
)

rolling_mcs <- mcs_block_bootstrap(
  rolling_loss_matrix,
  alpha = cfg$mcs_alpha,
  B = cfg$mcs_reps,
  block_length = cfg$bootstrap_block_length,
  seed = cfg$seed + 901
)

mcs_survivors <- dplyr::bind_rows(
  tibble::tibble(
    sample = "main_holdout",
    model = main_mcs$survivors
  ),
  tibble::tibble(
    sample = "pooled_rolling",
    model = rolling_mcs$survivors
  )
)

mcs_path <- dplyr::bind_rows(
  main_mcs$path |>
    dplyr::mutate(sample = "main_holdout", .before = 1),
  rolling_mcs$path |>
    dplyr::mutate(sample = "pooled_rolling", .before = 1)
)

# ============================================================
# Sensitivity framework
# ============================================================

clone_cfg <- function(cfg_local) {
  unserialize(serialize(cfg_local, NULL))
}

apply_overrides <- function(cfg_local, overrides) {
  out <- clone_cfg(cfg_local)
  for (nm in names(overrides)) out[[nm]] <- overrides[[nm]]
  out
}

run_main_variant_existing_design <- function(
  design, cfg_variant, label,
  include_external = FALSE,
  include_conformal = TRUE
) {
  design_variant <- design
  design_variant$weights <- make_cell_weights(
    design_variant$cell_grid, cfg_variant
  )

  sp <- make_main_split(
    design_variant, cfg_variant, embargo = cfg_variant$embargo
  )
  res <- run_split(
    design_variant,
    sp,
    cfg_variant,
    include_external_benchmarks = include_external,
    include_conformal = include_conformal,
    include_inference = FALSE,
    include_stationarity = FALSE
  )

  perf <- res$performance
  frow <- perf |>
    dplyr::filter(model == "FES-CF full")

  cm <- if (!is.null(res$conformal)) {
    res$conformal$global$metrics
  } else {
    tibble::tibble(
      surface_coverage = NA_real_,
      mean_band_width = NA_real_,
      interval_score = NA_real_
    )
  }

  tibble::tibble(
    sensitivity = label,
    design = design_variant$name,
    embargo = cfg_variant$embargo,
    tail_weight_lambda = cfg_variant$tail_weight_lambda,
    terminal_weight_lambda = cfg_variant$terminal_weight_lambda,
    guard_frac = cfg_variant$guard_frac,
    state_terminal_weight = cfg_variant$state_terminal_weight,
    n_states = length(cfg_variant$state_probs) + 1L,
    fpca_pve = cfg_variant$fpca_pve,
    fpca_k_max = cfg_variant$fpca_k_max,
    graph_top_k = cfg_variant$graph_top_k,
    graph_min_abs_cor = cfg_variant$graph_min_abs_cor,
    quantile_type = cfg_variant$quantile_type,
    scale_epsilon = cfg_variant$scale_epsilon,
    Weighted_RMSE = frow$Weighted_RMSE,
    RMSE = frow$RMSE,
    MAE = frow$MAE,
    surface_coverage = cm$surface_coverage[1],
    mean_band_width = cm$mean_band_width[1],
    interval_score = cm$interval_score[1],
    chosen_anchor = res$fescf$chosen_anchor,
    blend_lambda = res$fescf$correction_summary$secondary_blend_lambda,
    active_states = res$fescf$correction_summary$n_active_states
  )
}

sensitivity_results <- tibble::tibble()

if (isTRUE(cfg$run_hyperparameter_sensitivity)) {
  sensitivity_specs <- list(
    list(
      label = "primary",
      overrides = list()
    ),
    list(
      label = "weights_tau2_h1",
      overrides = list(tail_weight_lambda = 2, terminal_weight_lambda = 1)
    ),
    list(
      label = "weights_tau6_h1",
      overrides = list(tail_weight_lambda = 6, terminal_weight_lambda = 1)
    ),
    list(
      label = "weights_tau4_h0",
      overrides = list(tail_weight_lambda = 4, terminal_weight_lambda = 0)
    ),
    list(
      label = "weights_tau4_h2",
      overrides = list(tail_weight_lambda = 4, terminal_weight_lambda = 2)
    ),
    list(
      label = "weights_tau2_h0",
      overrides = list(tail_weight_lambda = 2, terminal_weight_lambda = 0)
    ),
    list(
      label = "weights_tau2_h2",
      overrides = list(tail_weight_lambda = 2, terminal_weight_lambda = 2)
    ),
    list(
      label = "weights_tau6_h0",
      overrides = list(tail_weight_lambda = 6, terminal_weight_lambda = 0)
    ),
    list(
      label = "weights_tau6_h2",
      overrides = list(tail_weight_lambda = 6, terminal_weight_lambda = 2)
    ),
    list(
      label = "blend_old_grid",
      overrides = list(secondary_blend_grid = c(0, 0.05, 0.10, 0.20, 0.30))
    ),
    list(
      label = "blend_wide_coarse",
      overrides = list(secondary_blend_grid = seq(0, 0.50, by = 0.10))
    ),
    list(
      label = "residual_old_grid",
      overrides = list(residual_lambda_grid = c(0, 0.10, 0.15, 0.30, 0.50))
    ),
    list(
      label = "residual_fine_0025",
      overrides = list(residual_lambda_grid = seq(0, 0.50, by = 0.025))
    ),
    list(
      label = "residual_wide_to_1",
      overrides = list(residual_lambda_grid = seq(0, 1.00, by = 0.10))
    ),
    list(
      label = "guard_min_gain_1pct",
      overrides = list(min_guard_improvement = 0.01)
    ),
    list(
      label = "guard_min_gain_2pct",
      overrides = list(min_guard_improvement = 0.02)
    ),
    list(
      label = "guard_20pct",
      overrides = list(guard_frac = 0.20)
    ),
    list(
      label = "guard_40pct",
      overrides = list(guard_frac = 0.40)
    ),
    list(
      label = "state_terminal_025",
      overrides = list(state_terminal_weight = 0.25)
    ),
    list(
      label = "state_terminal_100",
      overrides = list(state_terminal_weight = 1.00)
    ),
    list(
      label = "two_states",
      overrides = list(state_probs = 0.50)
    ),
    list(
      label = "four_states",
      overrides = list(state_probs = c(0.25, 0.50, 0.75))
    ),
    list(
      label = "fpca_pve90_k8",
      overrides = list(fpca_pve = 0.90, fpca_k_max = 8)
    ),
    list(
      label = "fpca_pve99_k12",
      overrides = list(fpca_pve = 0.99, fpca_k_max = 12)
    ),
    list(
      label = "graph_k2_cor000",
      overrides = list(graph_top_k = 2, graph_min_abs_cor = 0.00)
    ),
    list(
      label = "graph_k5_cor010",
      overrides = list(graph_top_k = 5, graph_min_abs_cor = 0.10)
    ),
    list(
      label = "epsilon_1e10",
      overrides = list(scale_epsilon = 1e-10)
    ),
    list(
      label = "epsilon_1e6",
      overrides = list(scale_epsilon = 1e-6)
    )
  )

  sensitivity_results <- purrr::map_dfr(
    sensitivity_specs,
    function(ss) {
      cv <- apply_overrides(cfg, ss$overrides)
      # Existing design is used only for parameters that do not redefine
      # the target grid or the feature-construction sample.
      run_main_variant_existing_design(
        primary_design,
        cv,
        label = ss$label,
        include_external = FALSE,
        include_conformal = TRUE
      )
    }
  )
}

# Embargo robustness must rebuild only the split, not the surface.
embargo_robustness <- purrr::map_dfr(
  cfg$embargo_sensitivity,
  function(e) {
    cv <- clone_cfg(cfg)
    cv$embargo <- e
    cv$cv_purge <- max(max(cv$h_grid), e)

    run_main_variant_existing_design(
      primary_design,
      cv,
      label = paste0("embargo_", e),
      include_external = FALSE,
      include_conformal = TRUE
    )
  }
)

# ============================================================
# Design-level robustness: target, universe, grid, quantile type
# ============================================================

run_design_robustness <- function(
  label,
  cfg_variant,
  universe,
  score_type
) {
  des <- timed(
    paste0("build_design_", label),
    label,
    function() build_design(
      prices_xts = prices_xts_full,
      vix_xts = vix_xts,
      cfg_local = cfg_variant,
      universe_tickers = universe,
      score_type = score_type,
      design_name = label
    )
  )

  sp <- make_main_split(
    des, cfg_variant, embargo = cfg_variant$embargo
  )

  res <- run_split(
    design = des,
    sp = sp,
    cfg_local = cfg_variant,
    include_external_benchmarks = FALSE,
    include_conformal = TRUE,
    include_inference = FALSE,
    include_stationarity = FALSE
  )

  perf <- res$performance |>
    dplyr::mutate(
      robustness_design = label,
      score_type = score_type,
      n_assets = length(universe),
      h_grid = paste(cfg_variant$h_grid, collapse = ","),
      tau_grid = paste(cfg_variant$tau_grid, collapse = ","),
      quantile_type = cfg_variant$quantile_type,
      .before = 1
    )

  conf <- res$conformal$all_metrics |>
    dplyr::mutate(
      robustness_design = label,
      score_type = score_type,
      n_assets = length(universe),
      .before = 1
    )

  list(
    performance = perf,
    conformal = conf,
    design = des,
    result = res
  )
}

design_robustness_results <- list()

if (isTRUE(cfg$run_target_robustness)) {
  for (score_type in c("downside_variation", "squared_return")) {
    label <- paste0("target_", score_type)
    design_robustness_results[[label]] <- run_design_robustness(
      label = label,
      cfg_variant = cfg,
      universe = cfg$tickers,
      score_type = score_type
    )
  }
}

if (isTRUE(cfg$run_universe_robustness)) {
  etf_universe <- asset_dictionary |>
    dplyr::filter(asset_group == "ETF") |>
    dplyr::pull(ticker)

  equity_universe <- asset_dictionary |>
    dplyr::filter(asset_group == "Equity") |>
    dplyr::pull(ticker)

  design_robustness_results[["universe_etf_only"]] <-
    run_design_robustness(
      label = "universe_etf_only",
      cfg_variant = cfg,
      universe = etf_universe,
      score_type = cfg$score_type
    )

  design_robustness_results[["universe_equity_only"]] <-
    run_design_robustness(
      label = "universe_equity_only",
      cfg_variant = cfg,
      universe = equity_universe,
      score_type = cfg$score_type
    )
}

if (isTRUE(cfg$run_grid_robustness)) {
  grid_specs <- list(
    list(
      label = "grid_h_coarse",
      h_grid = c(1, 3, 5, 10),
      tau_grid = cfg$tau_grid,
      quantile_type = cfg$quantile_type
    ),
    list(
      label = "grid_tau_coarse",
      h_grid = cfg$h_grid,
      tau_grid = c(0.80, 0.90, 0.95, 0.975),
      quantile_type = cfg$quantile_type
    ),
    list(
      label = "quantile_type7_rebuilt",
      h_grid = cfg$h_grid,
      tau_grid = cfg$tau_grid,
      quantile_type = 7
    ),
    list(
      label = "quantile_type9_rebuilt",
      h_grid = cfg$h_grid,
      tau_grid = cfg$tau_grid,
      quantile_type = 9
    ),
    list(
      label = "har_windows_1_10_22",
      h_grid = cfg$h_grid,
      tau_grid = cfg$tau_grid,
      quantile_type = cfg$quantile_type,
      har_windows = c(1, 10, 22)
    ),
    list(
      label = "har_windows_1_5_10_22",
      h_grid = cfg$h_grid,
      tau_grid = cfg$tau_grid,
      quantile_type = cfg$quantile_type,
      har_windows = c(1, 5, 10, 22)
    )
  )

  for (gs in grid_specs) {
    cv <- clone_cfg(cfg)
    cv$h_grid <- gs$h_grid
    cv$tau_grid <- gs$tau_grid
    cv$quantile_type <- gs$quantile_type
    if (!is.null(gs$har_windows)) cv$har_windows <- gs$har_windows
    cv$embargo <- max(cv$h_grid)
    cv$cv_purge <- max(cv$h_grid)

    design_robustness_results[[gs$label]] <- run_design_robustness(
      label = gs$label,
      cfg_variant = cv,
      universe = cfg$tickers,
      score_type = cfg$score_type
    )
  }
}

robustness_performance <- purrr::map_dfr(
  design_robustness_results,
  "performance"
)

robustness_conformal <- purrr::map_dfr(
  design_robustness_results,
  "conformal"
)

# ============================================================
# Runtime summary
# ============================================================

runtime_summary <- audit_env$runtime |>
  dplyr::group_by(component) |>
  dplyr::summarise(
    calls = dplyr::n(),
    total_seconds = sum(elapsed_seconds, na.rm = TRUE),
    mean_seconds = mean(elapsed_seconds, na.rm = TRUE),
    median_seconds = stats::median(elapsed_seconds, na.rm = TRUE),
    max_seconds = max(elapsed_seconds, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(total_seconds))

# ============================================================
# Publication-oriented presentation tables
# ============================================================

main_perf_presentation <- main_perf |>
  dplyr::transmute(
    Model = model_display_name(model),
    `RMSE x100` = 100 * RMSE,
    `MAE x100` = 100 * MAE,
    `Weighted RMSE x100` = 100 * Weighted_RMSE,
    `Terminal RMSE x100` = 100 * Terminal_RMSE,
    `High-tail RMSE x100` = 100 * High_Tau_RMSE
  )

main_ablation_presentation <- main_ablation |>
  dplyr::transmute(
    Component = model,
    `Weighted RMSE x100` = 100 * Weighted_RMSE,
    `RMSE x100` = 100 * RMSE,
    `MAE x100` = 100 * MAE
  )

rolling_rank_summary_presentation <- rolling_rank_summary |>
  dplyr::transmute(
    Model = model_display_name(model),
    `N splits` = n_splits,
    `Average rank` = avg_rank,
    Wins = wins,
    `Top 2` = top2,
    `Average weighted RMSE x100` = 100 * avg_weighted_rmse
  )

embargo_robustness_presentation <- embargo_robustness |>
  dplyr::mutate(
    Weighted_RMSE = 100 * Weighted_RMSE,
    RMSE = 100 * RMSE,
    MAE = 100 * MAE,
    mean_band_width = 100 * mean_band_width
  ) |>
  dplyr::rename(
    `Weighted RMSE x100` = Weighted_RMSE,
    `RMSE x100` = RMSE,
    `MAE x100` = MAE,
    `Mean band width x100` = mean_band_width
  )

# Appendix accuracy tables use percentage-point scaling for RMSE/MAE levels
# wherever possible, while inference tables retain their natural loss scale.
rolling_split_wide_presentation <- rolling_split_wide
num_cols_rs <- setdiff(names(rolling_split_wide_presentation), "scheme")
rolling_split_wide_presentation[num_cols_rs] <- lapply(
  rolling_split_wide_presentation[num_cols_rs],
  function(x) 100 * x
)

all_ablation_presentation <- all_ablation_performance |>
  dplyr::transmute(
    Scheme = make_clean_label(scheme),
    Variant = model,
    `RMSE x100` = 100 * RMSE,
    `MAE x100` = 100 * MAE,
    `Weighted RMSE x100` = 100 * Weighted_RMSE,
    `Terminal RMSE x100` = 100 * Terminal_RMSE,
    `High-tail RMSE x100` = 100 * High_Tau_RMSE
  )

projection_effect_presentation <- all_projection_effect |>
  dplyr::transmute(
    Scheme = make_clean_label(scheme),
    Model = model_display_name(model),
    `Pre negative count` = pre_negative_count,
    `Pre tail violations` = pre_tau_violation_count,
    `Pre horizon violations` = pre_horizon_violation_count,
    `Pre violation magnitude x100` = 100 * pre_total_violation_magnitude,
    `Post negative count` = post_negative_count,
    `Post tail violations` = post_tau_violation_count,
    `Post horizon violations` = post_horizon_violation_count,
    `Post violation magnitude > tol` = post_total_violation_magnitude,
    `Post max raw violation` = post_max_raw_violation,
    `Raw weighted RMSE x100` = 100 * weighted_rmse_raw,
    `Projected weighted RMSE x100` = 100 * weighted_rmse_projected,
    `Projection loss change x100` = 100 * projection_loss_change
  )

yearly_loss_presentation <- yearly_loss |>
  dplyr::transmute(
    Scheme = make_clean_label(scheme),
    Year = year,
    Model = model_display_name(model),
    N = n,
    `Weighted RMSE x100` = 100 * weighted_rmse,
    `Unweighted RMSE x100` = 100 * sqrt(mean_unweighted_loss)
  )

state_loss_presentation <- state_loss |>
  dplyr::transmute(
    Scheme = make_clean_label(scheme),
    State = make_clean_label(state),
    Model = model_display_name(model),
    N = n,
    `Weighted RMSE x100` = 100 * weighted_rmse
  )

stress_loss_presentation <- stress_loss |>
  dplyr::transmute(
    Window = window,
    Start = start,
    End = end,
    Model = model_display_name(model),
    N = n,
    `Weighted RMSE x100` = 100 * weighted_rmse
  )

guard_vs_realized_presentation <- guard_vs_realized |>
  dplyr::transmute(
    Scheme = make_clean_label(scheme),
    `Chosen anchor` = model_display_name(chosen_anchor),
    `Guard gain pct` = 100 * guard_gain,
    `Selected anchor test WRMSE x100` = 100 * selected_anchor_test_wrmse,
    `FES-CF test WRMSE x100` = 100 * fescf_test_wrmse,
    `Realized test gain pct` = 100 * realized_test_gain
  )

sensitivity_results_presentation <- sensitivity_results |>
  dplyr::mutate(
    Weighted_RMSE = 100 * Weighted_RMSE,
    RMSE = 100 * RMSE,
    MAE = 100 * MAE,
    mean_band_width = 100 * mean_band_width
  ) |>
  dplyr::rename(
    `Weighted RMSE x100` = Weighted_RMSE,
    `RMSE x100` = RMSE,
    `MAE x100` = MAE,
    `Mean band width x100` = mean_band_width
  )

robustness_performance_presentation <- robustness_performance |>
  dplyr::mutate(
    RMSE = 100 * RMSE,
    MAE = 100 * MAE,
    Weighted_RMSE = 100 * Weighted_RMSE,
    Terminal_RMSE = 100 * Terminal_RMSE,
    High_Tau_RMSE = 100 * High_Tau_RMSE
  ) |>
  dplyr::rename(
    `RMSE x100` = RMSE,
    `MAE x100` = MAE,
    `Weighted RMSE x100` = Weighted_RMSE,
    `Terminal RMSE x100` = Terminal_RMSE,
    `High-tail RMSE x100` = High_Tau_RMSE
  )

# Single reviewer-facing protocol table: what was prespecified, what is
# selected inside the training/guard sample, and what is only a sensitivity.
tuning_protocol <- tibble::tribble(
  ~parameter, ~primary_value, ~role, ~selection_scope, ~sensitivity_set,
  "Horizon grid", paste(cfg$h_grid, collapse = ","), "Prespecified", "None", "Coarser horizon grid",
  "Tail grid", paste(cfg$tau_grid, collapse = ","), "Prespecified", "None", "Coarser tail grid",
  "Quantile convention", as.character(cfg$quantile_type), "Prespecified", "None", "Types 7 and 9",
  "Minimum asset coverage", as.character(cfg$min_asset_coverage), "Prespecified", "None", "Reported in manifest / cell availability",
  "Embargo", as.character(cfg$embargo), "Prespecified", "None", paste(cfg$embargo_sensitivity, collapse = ","),
  "CV purge", as.character(cfg$cv_purge), "Prespecified", "None", "Tracks h_max in rebuilt designs",
  "Ridge lambda grid", "1e-5 to 1e3, 80 log points", "Candidate grid", "Anchor CV + 1-SE; residual penalty uses training guard", "Primary grid retained",
  "glmnet numerical convergence", paste0("maxit=", cfg$glmnet_maxit), "Numerical safeguard", "Common converged lambdas only; truncated-edge CV minima/choices abort", paste0("Minimum common count ", cfg$glmnet_min_common_count, "; edge buffer ", cfg$glmnet_edge_buffer),
  "HAR windows", paste(cfg$har_windows, collapse = ","), "Prespecified", "None", "1,10,22 and 1,5,10,22",
  "FPCA PVE / K max", paste0(cfg$fpca_pve, " / ", cfg$fpca_k_max), "Train-selected dimension", "Purged fold-training basis / mean / K; full-training refit", "Alternative PVE and K max",
  "Graph top-k / min abs cor", paste0(cfg$graph_top_k, " / ", cfg$graph_min_abs_cor), "Prespecified", "Graph relearned on each purged fold-training block", "Alternative sparsity settings",
  "Guard share / minimum N", paste0(cfg$guard_frac, " / ", cfg$guard_min_n), "Prespecified", "Training block only", "20pct and 40pct guard shares",
  "Top-2 blend grid", "0 to 0.50 by 0.025", "Guard-selected", "Guard block only", "Old grid and coarse 0 to 0.50",
  "Top-3 simplex step", as.character(cfg$top3_simplex_step), "Sensitivity only", "Guard block only", "Top-3 not used in primary ranking/MCS",
  "FPCA score penalty", "One shared lambda across retained scores", "CV-selected", "Reconstructed surface MSE; purged fold-local CV + 1-SE", "PVE/K sensitivity retained",
  "Residual ridge penalty", "Same 80-point lambda grid", "Guard-selected", "Unprojected guard residual MSE; carried to final refit without CV", "Existing grid retained",
  "Residual correction rho grid", "0 to 0.50 by 0.05", "Guard-selected", "State-specific guard block", "Old/fine grids plus 0 to 1.00 by 0.10",
  "State cut probabilities", paste(cfg$state_probs, collapse = ","), "Prespecified", "Training block only", "2-state and 4-state partitions",
  "State terminal coefficient", as.character(cfg$state_terminal_weight), "Prespecified", "Training block only", "0.25 and 1.00",
  "Conformal alpha", as.character(cfg$conformal_alpha), "Prespecified", "Calibration only", paste(cfg$conformal_alpha_grid, collapse = ","),
  "Band multiplier", as.character(cfg$primary_band_multiplier), "Prespecified", "Calibration only", paste(cfg$band_multiplier_grid, collapse = ","),
  "Scale epsilon", format(cfg$scale_epsilon, scientific = TRUE), "Numerical floor", "Training residual scale", "1e-10 and 1e-6",
  "Loss weights tau / horizon", paste0(cfg$tail_weight_lambda, " / ", cfg$terminal_weight_lambda), "Prespecified", "Used for guard and evaluation", "2/4/6 by 0/1/2 grid",
  "Primary NW lag", as.character(cfg$primary_dm_lag), "Prespecified after reviewer correction", "Inference only", paste(cfg$dm_nw_lags, collapse = ","),
  "Bootstrap block / reps", paste0(cfg$bootstrap_block_length, " / ", cfg$bootstrap_reps), "Prespecified", "Inference only", "MCS uses separate prespecified reps",
  "MCS alpha / reps", paste0(cfg$mcs_alpha, " / ", cfg$mcs_reps), "Prespecified", "Inference only", "Main and pooled rolling samples"
)

# ============================================================
# Exports
# ============================================================

# Main tables
write_table_outputs(
  design_summary,
  "T0_design_summary",
  "Primary design summary.",
  digits = 3
)
write_table_outputs(
  cell_availability,
  "T0b_cell_availability",
  "Cell-level cross-sectional availability.",
  digits = 3
)
write_table_outputs(
  asset_manifest,
  "T1_asset_manifest",
  "Prespecified instrument universe and data availability.",
  digits = 3
)
write_table_outputs(
  split_summary,
  "T2_split_summary",
  "Purged chronological split design.",
  digits = 3
)
write_table_outputs(
  main_perf_presentation,
  "T3_main_holdout_performance",
  "Main-holdout forecast performance.",
  digits = 3
)
write_table_outputs(
  main_ablation_presentation,
  "T4_main_ablation",
  "Main-holdout FES-CF ablation.",
  digits = 3
)
write_table_outputs(
  main_dm_primary,
  "T5_main_dm_primary",
  "Main-holdout Diebold--Mariano comparisons at Newey--West lag 10.",
  digits = 6
)
write_table_outputs(
  main_bootstrap_ci,
  "T6_main_block_bootstrap_ci",
  "Block-bootstrap confidence intervals for average weighted-loss differences.",
  digits = 6
)
write_table_outputs(
  rolling_rank_summary_presentation,
  "T7_rolling_rank_summary",
  "Rolling-split rank summary; average weighted RMSE is multiplied by 100.",
  digits = 3
)
write_table_outputs(
  mcs_survivors,
  "T8_mcs_survivors",
  "Models retained by the block-bootstrap Model Confidence Set procedure.",
  digits = 6
)

# Appendix / reviewer diagnostics
write_table_outputs(
  rolling_split_wide_presentation,
  "A1_rolling_split_wrmse",
  "Weighted RMSE multiplied by 100 for every rolling split and model.",
  digits = 3
)
write_table_outputs(
  all_ablation_presentation,
  "A2_ablation_all_splits",
  "Ablation performance across all evaluation schemes; accuracy measures are multiplied by 100.",
  digits = 3
)
write_table_outputs(
  projection_effect_presentation,
  "A3_projection_effect",
  paste0("Pre/post projection diagnostics. Post violations count only above ",
         format(cfg$projection_tol, scientific = TRUE),
         "; the maximum raw violation is retained in the CSV. RMSE quantities are multiplied by 100."),
  digits = 3
)
write_table_outputs(
  all_conformal_metrics,
  "A4_conformal_comparators",
  "Coverage, width, and interval-score comparison of conformal procedures.",
  digits = 6
)
write_table_outputs(
  all_conformal_multiplier,
  "A5_conformal_multiplier_sensitivity",
  "Global conformal coverage-width sensitivity to the band multiplier.",
  digits = 6
)
write_table_outputs(
  all_conformal_alpha,
  "A6_conformal_alpha_sensitivity",
  "Global conformal sensitivity to the nominal miscoverage level.",
  digits = 6
)
write_table_outputs(
  all_conformal_upper,
  "A7_one_sided_upper_band",
  "One-sided upper simultaneous-band diagnostics.",
  digits = 6
)
write_table_outputs(
  all_dm,
  "A8_dm_lag_sensitivity",
  "Diebold--Mariano sensitivity to Newey--West lag choice.",
  digits = 6
)
write_table_outputs(
  all_bootstrap_ci,
  "A9_block_bootstrap_all_splits",
  "Block-bootstrap intervals for weighted-loss differences.",
  digits = 6
)
write_table_outputs(
  all_state_counts,
  "A10_state_counts",
  "Train, calibration, and test counts by train-defined risk state.",
  digits = 0
)
write_table_outputs(
  all_anchor_selection,
  "A11_anchor_selection",
  "Train-only guard ranking of candidate anchors.",
  digits = 6
)
write_table_outputs(
  all_correction_summary,
  "A12_guard_summary",
  "Guarded blend and correction diagnostics.",
  digits = 6
)
write_table_outputs(
  all_state_gate,
  "A13_state_correction_gates",
  "State-specific residual-correction gates.",
  digits = 6
)
write_table_outputs(
  all_secondary_blend_guard,
  "A14_top2_blend_grid",
  "Fine-grid top-two anchor blending diagnostics.",
  digits = 6
)
write_table_outputs(
  all_top3_blend_guard,
  "A15_top3_simplex",
  "Top-three simplex anchor-blending sensitivity.",
  digits = 6
)
write_table_outputs(
  yearly_loss_presentation,
  "A16_yearly_loss",
  "Forecast accuracy by calendar year; RMSE quantities are multiplied by 100.",
  digits = 3
)
write_table_outputs(
  state_loss_presentation,
  "A17_state_loss",
  "Forecast accuracy by train-defined risk state; weighted RMSE is multiplied by 100.",
  digits = 3
)
write_table_outputs(
  main_top5_gain_share,
  "A18_top5_gain_share",
  "Share of cumulative main-holdout loss differential attributable to the largest five percent of daily gains.",
  digits = 6
)
write_table_outputs(
  stress_loss_presentation,
  "A19_stress_window_performance",
  "Forecast performance in reviewer-requested stress windows; weighted RMSE is multiplied by 100.",
  digits = 3
)
write_table_outputs(
  guard_vs_realized_presentation,
  "A20_guard_vs_realized",
  "Guard and test gains are percentage reductions in weighted MSE against the selected anchor; RMSE levels are multiplied by 100.",
  digits = 3
)
write_table_outputs(
  all_stationarity,
  "A21_stationarity_diagnostic",
  "Functional stationarity and structural-stability diagnostics.",
  digits = 6
)
write_table_outputs(
  mcs_path,
  "A22_mcs_path",
  "Block-bootstrap Model Confidence Set elimination path.",
  digits = 6
)
write_table_outputs(
  sensitivity_results_presentation,
  "A23_hyperparameter_sensitivity",
  "One-at-a-time hyperparameter sensitivity; accuracy and band-width quantities are multiplied by 100.",
  digits = 4
)
write_table_outputs(
  embargo_robustness_presentation,
  "A24_embargo_sensitivity",
  "Embargo sensitivity with full-horizon purging; accuracy and band-width quantities are multiplied by 100.",
  digits = 4
)
write_table_outputs(
  robustness_performance_presentation,
  "A25_target_universe_grid_robustness",
  "Target, universe, quantile-convention, and grid robustness; accuracy measures are multiplied by 100.",
  digits = 3
)
write_table_outputs(
  robustness_conformal,
  "A26_robustness_conformal",
  "Conformal diagnostics across target, universe, and grid robustness designs.",
  digits = 6
)
write_table_outputs(
  tail_cell_resolution,
  "A27_tail_cell_resolution",
  "Cross-sectional resolution of the upper-tail surface cells.",
  digits = 4
)
write_table_outputs(
  tail_asset_occupancy,
  "A28_tail_asset_occupancy",
  "Instrument occupancy of the upper-tail surface cells.",
  digits = 4
)
write_table_outputs(
  rolling_2020_conformal,
  "A29_rolling_2020_conformal",
  "Conformal coverage and width for rolling splits overlapping the 2020 stress window.",
  digits = 6
)
write_table_outputs(
  audit_env$fpca,
  "A30_fpca_components",
  "Selected FPCA dimension and explained-variance diagnostics.",
  digits = 6
)
write_table_outputs(
  runtime_summary,
  "A31_runtime_summary",
  "Computational cost by pipeline component.",
  digits = 3
)
write_table_outputs(
  main_daily_loss_diff,
  "A32_main_daily_loss_differences",
  "Daily main-holdout weighted-loss differences relative to FES-CF; positive values favor FES-CF and are multiplied by 10,000.",
  digits = 4
)
write_table_outputs(
  rolling_ablation_summary_presentation,
  "A33_pooled_rolling_ablation_summary",
  "Pooled rolling-split ablation summary; weighted RMSE quantities are multiplied by 100.",
  digits = 3
)
write_table_outputs(
  pooled_rolling_fescf_vs_rolling_mean,
  "A34_pooled_rolling_fescf_vs_rolling_mean",
  "Direct pooled rolling comparison of FES-CF with the rolling-mean benchmark using a moving-block bootstrap.",
  digits = 4
)
write_table_outputs(
  tuning_protocol,
  "A35_tuning_protocol",
  "Prespecified, train-selected, guard-selected, and sensitivity-analysis quantities.",
  digits = 4
)

# Compact, tolerance-aware A3 companion (generated from current-run predictions).
projection_compact <- all_projection_effect |>
  dplyr::mutate(pre_total = pre_negative_count + pre_tau_violation_count + pre_horizon_violation_count,
                post_total = post_negative_count + post_tau_violation_count + post_horizon_violation_count) |>
  dplyr::group_by(model) |>
  dplyr::summarise(n_schemes = dplyr::n(), pre_violations = sum(pre_total),
    schemes_with_post_violations = sum(post_total > 0), post_violations = sum(post_total),
    max_raw_violation = max(post_max_raw_violation),
    mean_rmse_change_x100 = 100 * mean(projection_loss_change), .groups = "drop")
write_table_outputs(projection_compact |>
  dplyr::mutate(model = model_display_name(model),
    max_raw_violation = formatC(max_raw_violation, format = "e", digits = 3)),
  "A3_projection_effect_compact", paste0("Projection summary across evaluation schemes; post violations are counted above ",
    format(cfg$projection_tol, scientific = TRUE), "."), digits = 6)
readr::write_csv(projection_compact,
  file.path(cfg$output_dir, "audit", "projection_tolerance_numeric.csv"))

# Audit tables
readr::write_csv(audit_env$preprocessing,
  file.path(cfg$output_dir, "audit", "fold_local_preprocessing_audit.csv"))
readr::write_csv(
  audit_env$leakage,
  file.path(cfg$output_dir, "audit", "leakage_audit.csv")
)
readr::write_csv(
  audit_env$tuning,
  file.path(cfg$output_dir, "audit", "ridge_tuning_audit.csv")
)
readr::write_csv(
  audit_env$fpca,
  file.path(cfg$output_dir, "audit", "fpca_component_audit.csv")
)
readr::write_csv(
  audit_env$projection,
  file.path(cfg$output_dir, "audit", "dykstra_projection_audit.csv")
)
readr::write_csv(
  audit_env$warnings,
  file.path(cfg$output_dir, "audit", "warnings_log.csv")
)
readr::write_csv(
  audit_env$runtime,
  file.path(cfg$output_dir, "runtime", "runtime_log.csv")
)
readr::write_csv(
  all_aci_path,
  file.path(cfg$output_dir, "csv", "diagnostic_aci_path.csv")
)

# ============================================================
# Figures
# ============================================================

# F1: main holdout
fig1_df <- main_perf |>
  dplyr::mutate(
    Model = factor(
      model_display_name(model),
      levels = rev(model_display_name(model))
    )
  )

p1 <- ggplot2::ggplot(
  fig1_df,
  ggplot2::aes(x = Model, y = Weighted_RMSE)
) +
  ggplot2::geom_col(width = 0.70) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Main-holdout forecast accuracy",
    subtitle = "Lower weighted RMSE indicates smaller surface forecast error",
    x = NULL,
    y = "Weighted RMSE"
  ) +
  theme_paper()

save_figure(
  p1, "F1-main-holdout-performance",
  width = 8.5, height = 5.2
)

# F2: rolling ranks
fig2_df <- all_rank_rows |>
  dplyr::filter(grepl("^rolling_", scheme)) |>
  dplyr::mutate(Model = model_display_name(model))

p2 <- ggplot2::ggplot(
  fig2_df,
  ggplot2::aes(
    x = reorder(Model, rank, FUN = stats::median),
    y = rank
  )
) +
  ggplot2::geom_boxplot(
    outlier.alpha = 0.35,
    width = 0.65
  ) +
  ggplot2::geom_jitter(
    width = 0.12,
    alpha = 0.45,
    size = 1.2
  ) +
  ggplot2::coord_flip() +
  ggplot2::scale_y_reverse() +
  ggplot2::labs(
    title = "Rolling-split model ranks",
    subtitle = "Ranks are based on weighted RMSE; rank one is best",
    x = NULL,
    y = "Rank"
  ) +
  theme_paper()

save_figure(
  p2, "F2-rolling-rank-distribution",
  width = 8.5, height = 5.2
)

# F3: main cumulative loss difference
fig3_df <- all_loss_ts |>
  dplyr::filter(scheme == "main_holdout") |>
  dplyr::select(date, model, weighted_loss) |>
  tidyr::pivot_wider(
    names_from = model,
    values_from = weighted_loss
  )

if ("FES-CF full" %in% names(fig3_df)) {
  prop_loss <- fig3_df[["FES-CF full"]]
  fig3_long <- fig3_df |>
    dplyr::select(-`FES-CF full`) |>
    tidyr::pivot_longer(
      -date,
      names_to = "Benchmark",
      values_to = "benchmark_loss"
    ) |>
    dplyr::group_by(Benchmark) |>
    dplyr::mutate(
      cumulative_loss_difference = cumsum(
        benchmark_loss - prop_loss
      ),
      Benchmark = model_display_name(Benchmark)
    ) |>
    dplyr::ungroup()

  p3 <- ggplot2::ggplot(
    fig3_long,
    ggplot2::aes(
      x = date,
      y = cumulative_loss_difference,
      group = Benchmark
    )
  ) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::facet_wrap(~Benchmark, scales = "free_y") +
    ggplot2::labs(
      title = "Cumulative weighted-loss difference",
      subtitle = "Positive values indicate lower cumulative loss for FES-CF",
      x = "Date",
      y = "Benchmark loss minus FES-CF loss"
    ) +
    theme_paper(11)

  save_figure(
    p3, "F3-cumulative-loss-difference",
    width = 9.2, height = 6.4
  )
}

# F4: conformal coverage-width
fig4_df <- all_conformal_multiplier |>
  dplyr::filter(
    scheme == "main_holdout",
    state == "All"
  )

p4 <- ggplot2::ggplot(
  fig4_df,
  ggplot2::aes(
    x = mean_band_width,
    y = surface_coverage
  )
) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_hline(
    yintercept = 1 - cfg$conformal_alpha,
    linetype = "dashed"
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = sprintf("%.2f", band_multiplier)
    ),
    vjust = -0.6,
    size = 3.2
  ) +
  ggplot2::labs(
    title = "Conformal coverage-width sensitivity",
    subtitle = "Global split conformal; dashed line marks nominal simultaneous coverage",
    x = "Mean band width",
    y = "Surface coverage"
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme_paper()

save_figure(
  p4, "F4-conformal-coverage-width",
  width = 7.4, height = 5.0
)

# F5: high-risk state label, no OOD / no version text
fig5_df <- all_conformal_metrics |>
  dplyr::filter(
    scheme == "main_holdout",
    method == "Global split conformal",
    state != "All"
  ) |>
  dplyr::mutate(State = make_clean_label(state))

p5 <- ggplot2::ggplot(
  fig5_df,
  ggplot2::aes(x = State, y = surface_coverage)
) +
  ggplot2::geom_col(width = 0.65) +
  ggplot2::geom_hline(
    yintercept = 1 - cfg$conformal_alpha,
    linetype = "dashed"
  ) +
  ggplot2::labs(
    title = "State-specific conformal coverage",
    subtitle = "Primary simultaneous-band specification on the main holdout",
    x = NULL,
    y = "Surface coverage"
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.05)
  ) +
  theme_paper()

save_figure(
  p5, "F5-state-specific-conformal-coverage",
  width = 7.2, height = 5.0
)

# F6: ablation
fig6_df <- main_ablation |>
  dplyr::mutate(
    Component = reorder(make_clean_label(model), Weighted_RMSE)
  )

p6 <- ggplot2::ggplot(
  fig6_df,
  ggplot2::aes(x = Component, y = Weighted_RMSE)
) +
  ggplot2::geom_col(width = 0.70) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "FES-CF ablation on the main holdout",
    subtitle = "The comparison isolates blending, residual correction, and projection",
    x = NULL,
    y = "Weighted RMSE"
  ) +
  theme_paper()

save_figure(
  p6, "F6-main-ablation",
  width = 8.5, height = 5.4
)

# F7: guard gain vs realized gain
fig7_df <- guard_vs_realized |>
  dplyr::mutate(Scheme = make_clean_label(scheme))

p7 <- ggplot2::ggplot(
  fig7_df,
  ggplot2::aes(x = guard_gain, y = realized_test_gain)
) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_text(
    ggplot2::aes(label = Scheme),
    nudge_y = 0.002,
    size = 2.8,
    check_overlap = TRUE
  ) +
  ggplot2::labs(
    title = "Guard-block gain and realized test gain",
    subtitle = "Both axes use relative weighted-MSE reduction against the selected anchor",
    x = "Guard relative weighted-MSE gain",
    y = "Test relative weighted-MSE gain"
  ) +
  theme_paper()

save_figure(
  p7, "F7-guard-vs-realized-gain",
  width = 8.0, height = 5.2
)

# F8: daily main-holdout loss differential requested by the reviewer.
# Positive values favor FES-CF; labels are manuscript-safe and contain no
# underscore-delimited internal identifiers.
if (ncol(main_daily_loss_diff) > 1L) {
  fig8_df <- main_daily_loss_diff |>
    tidyr::pivot_longer(
      -date,
      names_to = "Comparison",
      values_to = "loss_diff_x10000"
    ) |>
    dplyr::mutate(
      Comparison = sub(
        "^Loss diff x10000: ", "", Comparison
      )
    )

  p8 <- ggplot2::ggplot(
    fig8_df,
    ggplot2::aes(x = date, y = loss_diff_x10000)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_line(linewidth = 0.45) +
    ggplot2::facet_wrap(~Comparison, scales = "free_y") +
    ggplot2::labs(
      title = "Daily weighted-loss difference on the main holdout",
      subtitle = "Positive values favor FES-CF; loss differences are multiplied by 10,000",
      x = "Date",
      y = "Benchmark loss minus FES-CF loss"
    ) +
    theme_paper(10)

  save_figure(
    p8, "F8-daily-loss-difference",
    width = 9.4, height = 7.0
  )
}

# ============================================================
# Reproducibility metadata, checksums, audit
# ============================================================

saveRDS(
  cfg,
  file.path(cfg$output_dir, "rds", "config.rds")
)
saveRDS(
  primary_results,
  file.path(cfg$output_dir, "rds", "primary_split_results.rds")
)
saveRDS(
  c(primary_design, list(cell_weights = primary_design$weights)),
  file.path(cfg$output_dir, "rds", "primary_design_matrices.rds")
)

saveRDS(all_loss_ts, file.path(cfg$output_dir, "rds", "all_daily_loss_series.rds"))
readr::write_csv(all_loss_ts, file.path(cfg$output_dir, "csv", "all_daily_loss_series.csv"))

jsonlite::write_json(
  cfg,
  file.path(cfg$output_dir, "meta", "config.json"),
  pretty = TRUE,
  auto_unbox = TRUE
)
writeLines(
  capture.output(str(cfg)),
  file.path(cfg$output_dir, "meta", "config.txt")
)
writeLines(
  capture.output(sessionInfo()),
  file.path(cfg$output_dir, "meta", "session-info.txt")
)

download_metadata <- tibble::tibble(
  downloaded_at = asset_dl$downloaded_at,
  cache_used = asset_dl$cache_used,
  requested_start = cfg$start_date,
  requested_end = cfg$end_date,
  yahoo_source = "Yahoo Finance via quantmod"
)
readr::write_csv(
  download_metadata,
  file.path(cfg$output_dir, "meta", "download_metadata.csv")
)

# ============================================================
# Final publication audit
# ============================================================

leakage_ok <- nrow(audit_env$leakage) > 0 &&
  all(audit_env$leakage$zero_overlap)

projection_ok <- nrow(audit_env$projection) > 0 &&
  all(
    is.finite(audit_env$projection$convergence_rate) &
      audit_env$projection$convergence_rate == 1 &
      audit_env$projection$max_raw_violation <= audit_env$projection$tolerance
  )

primary_method_ok <- identical(
  cfg$primary_conformal_method,
  "global_split"
) &&
  identical(cfg$primary_band_multiplier, 1.00)

no_ood_in_core_outputs <- !any(
  grepl(
    "OOD|out-of-distribution",
    c(
      colnames(all_state_counts),
      unique(all_state_counts$state),
      unique(all_conformal_metrics$state)
    ),
    ignore.case = TRUE
  )
)

fixed_universe_ok <- length(asset_dl$failed) == 0L &&
  identical(colnames(prices_xts_full), cfg$tickers)

vix_required_ok <- isTRUE(cfg$use_vix_feature) &&
  isTRUE(cfg$require_vix_feature) &&
  !is.null(vix_xts) &&
  identical(colnames(vix_xts), "VIX") &&
  any(is.finite(as.numeric(vix_xts)))

latex_table_files <- list.files(
  cfg$output_dir, pattern = "\\.tex$", recursive = TRUE, full.names = TRUE
)
latex_tables_ok <- length(latex_table_files) >= 10L

one_sided_upper_ok <- nrow(all_conformal_upper) > 0L &&
  all(all_conformal_upper$method == "One-sided upper split conformal")

top3_excluded_from_primary_ok <- !any(
  all_performance$model == "FES-CF top3 sensitivity"
) && !any(
  all_loss_ts$model == "FES-CF top3 sensitivity"
)

residual_wide_sensitivity_ok <- any(
  sensitivity_results$sensitivity == "residual_wide_to_1"
)

glmnet_tuning_rows <- audit_env$tuning |>
  dplyr::filter(family %in% c("mgaussian_ridge", "gaussian_ridge", "residual_ridge_guard"))
glmnet_numerical_safe_ok <- cfg$glmnet_maxit >= 1000000L &&
  nrow(glmnet_tuning_rows) > 0L &&
  all(glmnet_tuning_rows$common_lambda_count >= cfg$glmnet_min_common_count, na.rm = TRUE) &&
  all(
    glmnet_tuning_rows$truncated_cv_folds == 0L |
      (
        glmnet_tuning_rows$cv_min_loss_edge_rank > cfg$glmnet_edge_buffer &
        glmnet_tuning_rows$selected_lambda_edge_rank > cfg$glmnet_edge_buffer
      ),
    na.rm = TRUE
  )

reviewer_output_suite_ok <- all(file.exists(file.path(
  cfg$output_dir, "tex",
  c(
    "A32_main_daily_loss_differences.tex",
    "A33_pooled_rolling_ablation_summary.tex",
    "A34_pooled_rolling_fescf_vs_rolling_mean.tex",
    "A35_tuning_protocol.tex"
  )
)))

publication_audit <- tibble::tibble(
  check = c(
    "Primary embargo is at least h_max",
    "Purged CV is at least h_max",
    "All leakage boundary audits pass",
    "Dykstra projection converges to tolerance",
    "Primary conformal multiplier equals 1.00",
    "Primary conformal method is global split",
    "High-risk terminology contains no OOD label",
    "Asset manifest written",
    "Runtime log written",
    "Target/universe/grid robustness completed when requested",
    "CV fold date-based leakage audits are present",
    "MCS procedure completed",
    "Functional stationarity diagnostics completed",
    "Prespecified primary universe is complete",
    "Required VIX series is available and enabled",
    "Publication tables exported as LaTeX",
    "One-sided upper band uses directional conformal scores",
    "Top-three sensitivity excluded from primary ranking and MCS inputs",
    "Residual-correction rho sensitivity reaches 1.00",
    "glmnet path convergence safeguards pass",
    "Reviewer-compliance output tables are written",
    "FPCA and graph transforms are learned inside purged CV folds",
    "Residual ridge uses guard-selected penalties without full-refit CV",
    "A3 post-projection violations above tolerance are zero"
  ),
  pass = c(
    cfg$embargo >= max(cfg$h_grid),
    cfg$cv_purge >= max(cfg$h_grid),
    leakage_ok,
    projection_ok,
    identical(cfg$primary_band_multiplier, 1.00),
    identical(cfg$primary_conformal_method, "global_split"),
    no_ood_in_core_outputs,
    file.exists(file.path(cfg$output_dir, "csv", "asset_manifest.csv")),
    file.exists(file.path(cfg$output_dir, "runtime", "runtime_log.csv")),
    if (
      cfg$run_target_robustness ||
        cfg$run_universe_robustness ||
        cfg$run_grid_robustness
    ) {
      nrow(robustness_performance) > 0
    } else {
      TRUE
    },
    any(grepl("^cv_", audit_env$leakage$boundary)),
    nrow(mcs_survivors) > 0 && nrow(mcs_path) > 0,
    if (isTRUE(cfg$run_stationarity_diagnostic)) nrow(all_stationarity) >= 2 else TRUE,
    fixed_universe_ok,
    vix_required_ok,
    latex_tables_ok,
    one_sided_upper_ok,
    top3_excluded_from_primary_ok,
    residual_wide_sensitivity_ok,
    glmnet_numerical_safe_ok,
    reviewer_output_suite_ok,
    nrow(audit_env$preprocessing) > 0L &&
      all(c("fpca", "graph") %in% audit_env$preprocessing$component) &&
      all(audit_env$preprocessing$zero_overlap),
    any(audit_env$tuning$family == "residual_ridge_guard") &&
      all(audit_env$tuning$cv_folds[audit_env$tuning$family %in%
        c("residual_ridge_guard", "residual_ridge_fixed_guard")] == 0L),
    all(all_projection_effect$post_negative_count == 0 &
        all_projection_effect$post_tau_violation_count == 0 &
        all_projection_effect$post_horizon_violation_count == 0)
  )
)

readr::write_csv(
  publication_audit,
  file.path(cfg$output_dir, "audit", "publication_audit.csv")
)

if (!all(publication_audit$pass)) {
  audit_path <- file.path(cfg$output_dir, "audit", "publication_audit.csv")
  msg <- paste0(
    "One or more publication-audit checks failed. Inspect ", audit_path
  )
  if (isTRUE(cfg$strict_publication_audit)) stop(msg) else warning(msg)
}

# Hash all CSV/TEX/figure/meta artifacts after generation.
artifact_files <- list.files(
  cfg$output_dir,
  recursive = TRUE,
  full.names = TRUE
)
artifact_files <- artifact_files[
  file.info(artifact_files)$isdir %in% FALSE &
  basename(artifact_files) != "artifact_checksums_sha256.csv"
]
artifact_files_abs <- normalizePath(
  artifact_files, winslash = "/", mustWork = TRUE
)
output_root_abs <- normalizePath(
  cfg$output_dir, winslash = "/", mustWork = TRUE
)

checksums <- tibble::tibble(
  file = substring(
    artifact_files_abs,
    nchar(output_root_abs) + 2
  ),
  sha256 = vapply(
    artifact_files_abs,
    function(f) digest::digest(
      file = f, algo = "sha256", serialize = FALSE
    ),
    character(1)
  )
)

readr::write_csv(
  checksums,
  file.path(cfg$output_dir, "meta", "artifact_checksums_sha256.csv")
)


message("============================================================")
message("FES-CF V3.2.0 fold-local anchors / guard residual ridge FULL pipeline completed.")
message(
  "Output directory: ",
  normalizePath(cfg$output_dir, winslash = "/")
)
message("Run profile: ", cfg$run_profile)
message("Primary embargo: ", cfg$embargo)
message("Primary NW lag: ", cfg$primary_dm_lag)
message("Primary conformal multiplier: ", cfg$primary_band_multiplier)
message("Main-holdout ranking:")
print(
  main_perf |>
    dplyr::select(model, Weighted_RMSE) |>
    dplyr::arrange(Weighted_RMSE)
)
message("Rolling rank summary:")
print(rolling_rank_summary)
message("Publication audit:")
print(publication_audit)
message("============================================================")

})()
