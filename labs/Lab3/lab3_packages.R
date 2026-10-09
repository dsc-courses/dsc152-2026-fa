# Runtime package versions verified against CRAN on 2026-10-08.
# The student setup works offline when these versions are already available.
lab3_ensure_packages <- function(mode = c("student", "grader"),
                                 manifest = "lab3-package-versions.csv",
                                 update = FALSE, lib = NULL) {
  mode <- match.arg(mode)
  if (getRversion() < "4.1.0") {
    stopifnot(getRversion() >= "4.1.0")
  }
  if (is.null(lib)) {
    r_series <- paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1],
                      sep = ".")
    lib <- Sys.getenv("LAB3_R_LIBRARY", unset = file.path(path.expand("~"), "R",
                                                         "lab3-library", r_series))
  }
  if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))
  required <- read.csv(manifest, stringsAsFactors = FALSE)
  stopifnot(all(c("Package", "Version", "Use") %in% names(required)),
            !anyDuplicated(required$Package))
  if (mode == "student") required <- required[required$Use == "student", ]
  installed_version <- function(package) {
    tryCatch(utils::packageVersion(package, lib.loc = .libPaths()),
             error = function(e) package_version("0.0.0"))
  }
  behind <- function(packages, versions) {
    vapply(seq_along(packages), function(i)
      installed_version(packages[i]) < package_version(versions[i]), logical(1))
  }
  needs_update <- required$Package[behind(required$Package, required$Version)]
  if (update || length(needs_update)) {
    repos <- c(CRAN = "https://cloud.r-project.org")
    available <- utils::available.packages(repos = repos, type = "source")
    roots <- c("ottr", "testthat")
    if (mode == "grader") roots <- c(roots, "IRkernel", "rmarkdown")
    dependencies <- tools::package_dependencies(
      roots, db = available, which = c("Depends", "Imports", "LinkingTo"),
      recursive = TRUE)
    packages <- sort(intersect(unique(c(roots, unlist(dependencies))), rownames(available)))
    targets <- packages[behind(packages, available[packages, "Version"])]
    targets <- unique(c(targets, needs_update))
    if (length(targets)) {
      dir.create(lib, recursive = TRUE, showWarnings = FALSE)
      if (!dir.exists(lib) || file.access(lib, 2) != 0) {
        stopifnot(dir.exists(lib), file.access(lib, 2) == 0L)
      }
      .libPaths(c(lib, .libPaths()))
      message("Updating the lab's R packages. This may take a few minutes.")
      # Prefer ready-made binaries on Windows/macOS. Compile only when CRAN's
      # binary release lags the current source release; never ask an interactive
      # compilation question inside a notebook or Gradescope setup.
      old_compile <- options(install.packages.compile.from.source = "always",
                             install.packages.check.source = "yes")
      on.exit(options(old_compile), add = TRUE)
      package_type <- if (mode == "grader" || .Platform$pkgType == "source")
        "source" else "both"
      utils::install.packages(targets, repos = repos, lib = lib, dependencies = NA,
                              type = package_type)
    }
    # Catch failed downloads/builds; install.packages can otherwise only warn.
    remaining <- packages[behind(packages, available[packages, "Version"])]
    if (length(remaining)) {
      stopifnot(length(remaining) == 0L)
    }
  }
  remaining <- required$Package[behind(required$Package, required$Version)]
  if (length(remaining)) {
    stopifnot(length(remaining) == 0L)
  }
  # IRkernel loads dependencies before this setup cell runs. Their installed
  # versions are checked above; R checks the loaded versions against each
  # package's actual import requirements when ottr/testthat are loaded.
  # Request a student restart only for an outdated grader/testthat namespace.
  runtime_required <- if (mode == "student")
    required[required$Package %in% c("ottr", "testthat"), ] else required
  loaded_old <- runtime_required$Package[vapply(seq_len(nrow(runtime_required)), function(i) {
    package <- runtime_required$Package[i]
    package %in% loadedNamespaces() &&
      getNamespaceVersion(package) < package_version(runtime_required$Version[i])
  }, logical(1))]
  if (length(loaded_old)) {
    if (mode == "student") {
      message("The required package versions are installed. Restart the R kernel ",
              "(Kernel > Restart Kernel), then run this setup cell again before continuing.")
      return(invisible(FALSE))
    }
    stopifnot(length(loaded_old) == 0L)
  }
  if (!all(c("expect_all_true", "expect_all_false") %in% getNamespaceExports("testthat"))) {
    stopifnot(all(c("expect_all_true", "expect_all_false") %in% getNamespaceExports("testthat")))
  }
  runtime_version <- function(package) {
    if (package %in% loadedNamespaces()) getNamespaceVersion(package) else
      installed_version(package)
  }
  message("Lab packages ready: ottr ", runtime_version("ottr"),
          "; testthat ", runtime_version("testthat"), ".")
  invisible(TRUE)
}
