# Precompiled vignettes
# Adapted from bcgov/bcdata/vignettes/precompile.R under Apache License 2.0
#
# This file DEFINES the precompilation helpers and runs nothing. Sourcing it is
# safe: it cannot render a vignette or rewrite README.md as a side effect.
#
# To rebuild, run `vignettes/rebuild-all.R`, which is the explicit driver.
# Rendering one vignette is `precompile("teaching1.Rmd.orig")` after sourcing
# this file.
#
# The vignettes knit against the INSTALLED goldfish, not the working tree, so
# whoever runs the driver installs first. Rendering against a stale install
# produces numbers that look right and are not -- an earlier rebuild shipped a
# BIC computed by a previous version, and nothing about the output said so.

# Precompile vignettes -------------------------------------------------------

precompile <- function(vignette_to_run = NULL) {
  orig_files <- file.path(
    tools::list_files_with_exts(
      dir = "vignettes",
      exts = "Rmd\\.orig",
      full.names = TRUE
    )
  )

  if (!is.null(vignette_to_run)) {
    orig_files <- orig_files[basename(orig_files) %in% vignette_to_run]

    if (rlang::is_empty(orig_files)) stop("Not a vignette!")
  }

  ## Convert *.orig to *.Rmd -------------------------------------------------
  purrr::walk(
    orig_files,
    \(x) {
      rmdFile <- tools::file_path_sans_ext(x)
      knitr::knit(x, rmdFile, envir = globalenv())
      purlFile <- paste0(tools::file_path_sans_ext(rmdFile), ".R")
      knitr::purl(x, purlFile)
    }
  )

  ## Move .png files into correct directory so they render -------------------
  # Each vignette's `fig.path` is relative to the knit working directory (the
  # package root), so plots land in a top-level dir named after it; move each
  # into vignettes/ so the shipped .Rmd's relative img src resolves at build.
  fig_dirs <- c("teaching", "two-mode", "diagnostics")
  for (fig_dir in fig_dirs) {
    if (!dir.exists(fig_dir)) {
      next
    }
    images <- list.files(fig_dir, pattern = "\\.png$")
    if (length(images) == 0) {
      next
    }
    dest <- file.path("vignettes", fig_dir)
    dir.create(dest, showWarnings = FALSE, recursive = TRUE)
    success <- file.copy(
      from = file.path(fig_dir, images),
      to = file.path(dest, images),
      overwrite = TRUE
    )
    if (!all(success)) {
      stop(
        "Image files were not successfully transferred to vignettes directory"
      )
    }
    unlink(fig_dir, recursive = TRUE)
  }
}

# Report vignettes whose rendered output contains an error ---------------------
#
# A knitted chunk that failed leaves the message in the `.Rmd`, where it is easy
# to miss: the render itself succeeds. `Std. Error` is excluded because every
# coefficient table contains it.
check_rendered_errors <- function() {
  lapply(
    file.path(
      tools::list_files_with_exts(
        dir = "vignettes",
        exts = "Rmd",
        full.names = TRUE
      )
    ),
    \(x) {
      text <- readLines(x)
      haveErrors <- grepl("Error", text) & !grepl("Std\\. Error", text)
      if (any(haveErrors)) {
        paste(
          "Error in",
          x,
          " on line",
          paste(which(haveErrors), collapse = ", ")
        )
      }
    }
  )
}
