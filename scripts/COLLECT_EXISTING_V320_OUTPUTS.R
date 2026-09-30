# Collect an ALREADY FINISHED V3.2.0 run. No model fitting or downloading.
# In R: source(file.choose(), encoding = "UTF-8") and select this file.
# Optional: options(fescf.collect_dir = "C:/path/out_fescf_v3_2_0_foldlocal_guardridge")
(function() {
  src <- getOption("fescf.collect_dir", file.path(getwd(), "out_fescf_v3_2_0_foldlocal_guardridge"))
  if (!dir.exists(src)) {
    if (.Platform$OS.type == "windows" && interactive()) {
      src <- utils::choose.dir(caption = "Select the FINISHED V3.2.0 output folder")
    } else if (interactive()) {
      src <- readline("Full path to the finished V3.2.0 output folder: ")
    } else stop("Set options(fescf.collect_dir = '/path/to/finished/output') first.")
  }
  if (is.na(src) || !dir.exists(src)) stop("No valid output folder selected.")
  src <- normalizePath(src, winslash = "/", mustWork = TRUE)
  groups <- intersect(c("audit", "meta", "csv", "runtime", "rds"), list.dirs(src, recursive = FALSE, full.names = FALSE))
  if (!all(c("audit", "meta") %in% groups)) stop("This folder lacks audit/meta. Select the completed run output, not the revision package.")
  paths <- unlist(lapply(groups, function(d) list.files(file.path(src, d), recursive = TRUE, full.names = TRUE)))
  paths <- paths[file.info(paths)$isdir %in% FALSE]
  if (!length(paths)) stop("No files found.")
  relative <- substring(paths, nchar(src) + 2L)
  archive <- file.path(dirname(src), paste0("FES_CF_V320_EXISTING_OUTPUTS_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"))
  message("Collecting ", length(paths), " existing files, ", round(sum(file.info(paths)$size)/1024^2, 1), " MiB before compression.")
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zipr(archive, files = relative, root = src, include_directories = FALSE)
  } else {
    old <- getwd();on.exit(setwd(old), add = TRUE);setwd(src)
    if (!nzchar(Sys.which("zip"))) stop("No ZIP utility found. Compress audit, meta, csv, runtime and rds folders manually, or install the R 'zip' package. No analysis was run.")
    utils::zip(archive, relative, flags = "-q")
  }
  if (!file.exists(archive) || file.info(archive)$size == 0) stop("Archive creation did not complete.")
  message("Saved: ", archive)
  message("No forecasts were refitted. Cache and checkpoint folders were not included.")
  message("Archive creation does not mean the underlying audit checks passed; those files still need review.")
})()
