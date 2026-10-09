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
    # Use a library R searches before IRkernel starts, so a restart picks up
    # updated dependencies instead of loading the same older versions again.
    user_libraries <- strsplit(Sys.getenv("R_LIBS_USER"), .Platform$path.sep,
                               fixed = TRUE)[[1]]
    user_libraries <- path.expand(user_libraries[nzchar(user_libraries) &
                                                   user_libraries != "NULL"])
    stopifnot(length(user_libraries) > 0L)
    for (candidate in user_libraries) {
      if (!dir.exists(candidate)) dir.create(candidate, recursive = TRUE, showWarnings = FALSE)
      if (dir.exists(candidate) && file.access(candidate, 2) == 0L) {
        lib <- candidate
        break
      }
    }
    stopifnot(!is.null(lib))
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
  # Allow compatible dependencies preloaded by IRkernel, but detect actual
  # Depends/Imports conflicts before trying to load ottr or testthat.
  runtime_required <- if (mode == "student")
    required[required$Package %in% c("ottr", "testthat"), ] else required
  loaded_old <- runtime_required$Package[vapply(seq_len(nrow(runtime_required)), function(i) {
    package <- runtime_required$Package[i]
    package %in% loadedNamespaces() &&
      getNamespaceVersion(package) < package_version(runtime_required$Version[i])
  }, logical(1))]
  pending <- c("ottr", "testthat")
  if (mode == "grader") pending <- c(pending, "IRkernel", "rmarkdown")
  inspected <- character()
  while (length(pending)) {
    package <- pending[1]
    pending <- pending[-1]
    if (package %in% inspected) next
    inspected <- c(inspected, package)
    # An already-loaded namespace has already validated its own imports.
    # Its version is checked by its parent's requirement (or the root baseline).
    if (package %in% loadedNamespaces()) next
    description <- utils::packageDescription(package, lib.loc = .libPaths(),
                                             fields = c("Depends", "Imports"))
    declarations <- as.character(unlist(description, use.names = FALSE))
    declarations <- declarations[!is.na(declarations)]
    for (declaration in unlist(strsplit(declarations, ",", fixed = TRUE))) {
      entry <- trimws(declaration)
      match <- regmatches(entry, regexec(
        "^([[:alnum:].]+)[[:space:]]*(?:\\(([<>=!]+)[[:space:]]*([^()[:space:]]+)\\))?$",
        entry, perl = TRUE))[[1]]
      stopifnot(length(match) >= 2L)
      dependency <- match[2]
      if (dependency == "R") next
      pending <- c(pending, dependency)
      if (length(match) == 4L && nzchar(match[3]) &&
          dependency %in% loadedNamespaces()) {
        operator <- match[3]
        stopifnot(operator %in% c(">=", ">", "<=", "<", "==", "=", "!="))
        if (operator == "=") operator <- "=="
        compatible <- do.call(operator, list(getNamespaceVersion(dependency),
                                              package_version(match[4])))
        if (!isTRUE(compatible)) loaded_old <- c(loaded_old, dependency)
      }
    }
  }
  loaded_old <- unique(loaded_old)
  if (length(loaded_old)) {
    if (mode == "student") {
      message("Updated packages are installed, but this session needs a restart (",
              paste(loaded_old, collapse = ", "), "). Choose Kernel > Restart Kernel, ",
              "then run this setup cell again before continuing.")
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
