# Guards on the descriptor's two standing rules: one name per behavior
# (this file's first half) and no re-derivation from model / sub_model.
# They grep the package source, so they only run from a source checkout.

package_r_dir <- function() {
  path <- test_path("..", "..", "R")
  skip_if_not(dir.exists(path), "not running from a source checkout")
  path
}

source_lines <- function() {
  dir <- package_r_dir()
  files <- list.files(dir, pattern = "[.]R$", full.names = TRUE)
  unlist(lapply(files, function(f) {
    paste0(basename(f), ":", seq_along(readLines(f)), ": ", readLines(f))
  }))
}

test_that("neither retired flag names a behavior any more", {
  # `right_censored` and `intercept_scalars` were one fact under two names,
  # and neither named the fact: both were TRUE exactly when the sub-model
  # models waiting times. What survives is the unrelated, data-descriptive
  # use -- which VALUES are censored, and which fids an event censors -- so
  # the guard allows those by name and nothing else.
  allowed <- paste(
    "right_censored_event", # routing a censored event to its consumers
    "right_censored_events", # the fit's per-event censoring vector
    "right_censored_intervals", # the censored elapsed times
    "right_censored_stats_change", # their statistic updates
    "pointer_temp_right_censored", # a counter over censored events
    "n_right_censored_events", # how many there are
    "i_right_censored", # a commented-out counter
    "rightCensoredStatChange", # the legacy spelling of the same data
    sep = "|"
  )
  hits <- grep("right_censored|intercept_scalars", source_lines(), value = TRUE)
  hits <- hits[!grepl(allowed, hits)]

  # The remaining legitimate uses are the residual span attribute and the
  # multivariate route partition, both of which describe data rather than the
  # sub-model. Listed by file so a NEW site has to be justified here.
  data_uses <- paste0(
    "^(methods_residuals|diagnostic_tables|preprocess_multivariate)",
    "[.]R:"
  )
  hits <- hits[!grepl(data_uses, hits)]
  expect_identical(hits, character(0))
})

test_that("the preprocessed object no longer copies has_intercept", {
  # The object stored `right_censored = has_intercept` directly beneath
  # `has_intercept = has_intercept` -- one value under two names, which is how
  # a reader ends up unable to tell whether two knobs are one knob.
  prep <- compute_statistics(
    depNetwork ~ 1 + indeg,
    data = dataTest,
    model = "DyNAM",
    sub_model = "rate",
    output = "preprocessed"
  )
  expect_false("right_censored" %in% names(prep))
  expect_true(prep$is_exact_time)
  # The surviving pair answers two different questions and is allowed to
  # agree; what is forbidden is a second name for the same question.
  expect_true(prep$has_intercept)
})

test_that("is_exact_time follows the sub-model on every output form", {
  forms <- c("preprocessed", "gather")
  for (output in forms) {
    timed <- compute_statistics(
      depNetwork ~ 1 + indeg,
      data = dataTest,
      model = "DyNAM",
      sub_model = "rate",
      output = output
    )
    ordinal <- compute_statistics(
      depNetwork ~ indeg,
      data = dataTest,
      model = "DyNAM",
      sub_model = "rate_ordered",
      output = output
    )
    expect_true(timed$is_exact_time, info = output)
    expect_false(ordinal$is_exact_time, info = output)
    expect_false("right_censored" %in% names(timed), info = output)
  }
})
