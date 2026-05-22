local({

  # the requested version of renv
  version <- "1.0.7"

  # the project directory
  project <- getwd()

  # use start-up diagnostics if enabled
  diagnostics <- Sys.getenv("RENV_STARTUP_DIAGNOSTICS", unset = "FALSE")
  if (diagnostics) {
    start <- Sys.time()
    profile <- tempfile("renv-startup-", fileext = ".Rprof")
    utils::Rprof(profile)
    on.exit({
      utils::Rprof(NULL)
      elapsed <- signif(difftime(Sys.time(), start, units = "auto"), digits = 2L)
      writeLines(sprintf("- renv took %s to run the autoloader.", format(elapsed)))
      writeLines(sprintf("- Profile: %s", profile))
      print(summaryRprof(profile))
    }, add = TRUE)
  }

  # bail if renv is already installed and active
  if (!is.null(renv <- .getNamespace("renv"))) {
    if (renv$active(project = project)) {
      return(invisible(TRUE))
    }
  }

  message(
    "* This project uses renv but the lockfile has not been generated. ",
    "Run `renv::init()` to bootstrap the library, then `renv::snapshot()` ",
    "to commit the lockfile."
  )

  invisible(NULL)
})
