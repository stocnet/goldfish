# Precompiled vignettes
# Adapted from bcgov/bcdata/vignettes/precompile.R under Apache License 2.0

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
  fig_dirs <- c("teaching", "two-mode")
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

## Run all vignettes
precompile()


# check if the RMD files contain errors
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

# render README.Rmd
rmarkdown::render(
  "README.Rmd",
  output_format = "md_document",
  envir = globalenv()
)
