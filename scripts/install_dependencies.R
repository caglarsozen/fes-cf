# Install missing direct dependencies only; do not execute the financial study.
(function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) != 1L) stop("Run this file with Rscript.", call. = FALSE)
  script <- normalizePath(sub("^--file=", "", file_arg), winslash = "/", mustWork = TRUE)
  root <- dirname(dirname(script))
  packages <- readLines(file.path(root, "config", "required_packages.txt"), warn = FALSE)
  packages <- packages[nzchar(packages)]
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
  absent <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(absent)) stop("Installation incomplete: ", paste(absent, collapse = ", "), call. = FALSE)
  versions <- vapply(packages, function(p) as.character(utils::packageVersion(p)), character(1))
  print(data.frame(package = packages, installed_version = unname(versions)), row.names = FALSE)
  message("Dependencies are available. No forecasts were fitted.")
})()
