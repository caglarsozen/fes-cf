# CLI wrapper around the unchanged canonical V3.2.0 source.
(function() {
  usage <- paste(
    "Usage: Rscript scripts/run_pipeline.R INPUT_DIR OUTPUT_DIR [--check-only]",
    "INPUT_DIR must contain the original frozen cache and rds/config.rds.",
    "--check-only validates paths, configuration and dependencies without fitting.",
    sep = "\n"
  )
  args <- commandArgs(trailingOnly = TRUE)
  if (any(args %in% c("--help", "-h"))) { cat(usage, "\n"); return(invisible(NULL)) }
  check_only <- "--check-only" %in% args
  args <- args[args != "--check-only"]
  if (length(args) != 2L || any(startsWith(args, "--"))) stop(usage, call. = FALSE)
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) != 1L) stop("Use Rscript for this wrapper; see README for RStudio usage.", call. = FALSE)
  script <- normalizePath(sub("^--file=", "", file_arg), winslash = "/", mustWork = TRUE)
  root <- dirname(dirname(script))
  main <- file.path(root, "analysis", "FES_CF_V3_2_0_FULL.R")
  input <- normalizePath(path.expand(args[1L]), winslash = "/", mustWork = TRUE)
  if (!dir.exists(input)) stop("INPUT_DIR is not a directory.", call. = FALSE)
  output_arg <- path.expand(args[2L])
  absolute <- grepl("^(/|[A-Za-z]:[/\\]|\\\\)", output_arg)
  if (!absolute) output_arg <- file.path(getwd(), output_arg)
  output <- normalizePath(output_arg, winslash = "/", mustWork = FALSE)
  if (identical(input, output)) stop("INPUT_DIR and OUTPUT_DIR must differ.", call. = FALSE)
  if (file.exists(output) && !dir.exists(output)) stop("OUTPUT_DIR points to a file.", call. = FALSE)
  rel <- readLines(file.path(root, "config", "frozen_input_files.txt"), warn = FALSE)
  paths <- file.path(input, rel)
  ok <- file.exists(paths) & !is.na(file.info(paths)$size) & file.info(paths)$size > 0
  if (!all(ok)) stop("Missing or empty frozen input(s): ", paste(rel[!ok], collapse = ", "), call. = FALSE)

  # Parse the config expression only. Do not execute the pipeline to inspect it.
  parsed <- parse(main, encoding = "UTF-8")
  pipeline_function <- eval(parsed[[1L]][[1L]])
  nodes <- as.list(body(pipeline_function))[-1L]
  is_cfg <- vapply(nodes, function(x) is.call(x) && identical(x[[1L]], as.name("<-")) && identical(x[[2L]], as.name("cfg")), logical(1))
  if (sum(is_cfg) != 1L) stop("Canonical configuration could not be located.", call. = FALSE)
  expected <- eval(nodes[[which(is_cfg)]][[3L]], envir = baseenv())
  prior <- readRDS(paths[match("rds/config.rds", rel)])
  if (!is.list(prior)) stop("Frozen config.rds must contain a configuration list.", call. = FALSE)
  for (name in c("tickers", "start_date", "end_date", "vix_symbol")) {
    if (!identical(prior[[name]], expected[[name]])) stop("Frozen configuration mismatch: ", name, call. = FALSE)
  }
  packages <- readLines(file.path(root, "config", "required_packages.txt"), warn = FALSE)
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Missing packages: ", paste(missing, collapse = ", "), ". Run scripts/install_dependencies.R first.", call. = FALSE)
  message("Basic input/configuration/dependency checks passed.")
  message("Input: ", input, "\nOutput: ", output)
  if (check_only) {
    message("Check only: no output created and no models fitted. The canonical pipeline performs further cache/hash/runtime checks.")
    return(invisible(NULL))
  }
  old_options <- options(fescf.previous_output_dir = input, fescf.output_dir = output)
  on.exit(options(old_options), add = TRUE)
  source(main, encoding = "UTF-8")
})()
