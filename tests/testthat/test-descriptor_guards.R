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

test_that("dispatch survives only where implementations differ", {
  # Nine variants, six likelihood methods, and no method registered as an
  # alias of another: a registered method contains an implementation.
  ns <- asNamespace("goldfish")
  methods <- grep(
    "^compute_event_contribution[.]",
    ls(ns, all.names = TRUE),
    value = TRUE
  )
  bodies <- lapply(methods, function(m) body(get(m, envir = ns)))
  expect_identical(anyDuplicated(bodies), 0L)

  # `estimate_int` still dispatches on the axis it loops over, and
  # preprocessing dispatches on nothing per variant.
  expect_setequal(
    as.character(methods(estimate_int)),
    c("estimate_int.goldfishAxisSender", "estimate_int.goldfishAxisDyad")
  )
  expect_identical(
    as.character(methods(preprocess)),
    "preprocess.goldfishKind"
  )
})

test_that("a DyNAM-i spec dispatches to the DyNAM method, with no alias", {
  pairs <- list(
    c("rate", "rate"),
    c("rate_ordered", "rate_ordered"),
    c("choice", "choice")
  )
  for (p in pairs) {
    dynami <- new_model_spec("DyNAMi", p[1], nodes = "actors")
    dynam <- new_model_spec("DyNAM", p[2], nodes = "actors")
    # The same class, so necessarily the same method -- which is why deleting
    # the three aliases removed code rather than behavior.
    expect_identical(class(dynami)[1], class(dynam)[1], info = p[1])
    expect_identical(
      utils::getS3method("compute_event_contribution", class(dynami)[1]),
      utils::getS3method("compute_event_contribution", class(dynam)[1]),
      info = p[1]
    )
  }
  # And no method is registered under any retired variant name.
  ns <- asNamespace("goldfish")
  expect_false(any(grepl(
    "^compute_event_contribution[.]goldfishKind(Dn|Dni|Rem)",
    ls(ns, all.names = TRUE)
  )))
})

test_that("no component re-derives behavior from model or sub_model", {
  # The rule is that a site holding a SPEC reads the descriptor. It cannot
  # apply to a site that runs before a spec exists: during parsing and
  # specification building, `model` and `sub_model` are the user's arguments
  # and there is nothing to read them off. So the exception list is by phase,
  # documented per file with the reason, and never pattern-matched.
  exceptions <- c(
    # The constructor itself, which is where the mapping lives.
    "model_spec.R" = "builds the descriptor",
    # Validators checking a user-supplied value against its allowed set.
    "class_checks.R" = "validates a user-supplied model / sub_model",
    "formula_validate.R" = "validates a formula against the requested pair",
    # Pre-spec: the arguments have not been resolved into a spec yet.
    "model_estimate.R" = "estimator entry, before new_model_spec()",
    "make_specification.R" = "builds a specification from user arguments",
    "make_joint_specification.R" = "builds a joint specification",
    "formula_parser.R" = "parses a formula against the requested pair",
    "preprocess_joint.R" = "plans the merged walk from requested pairs",
    "preprocess_export.R" = "labels an export from the requested pair",
    # The legacy loops take the two-value sub-model as a string argument and
    # never see a spec.
    "model_preprocess.R" = "monolith loop, takes the legacy sub_model string",
    "model_preprocess_group.R" = "group loop, same legacy string argument",
    "dynami_bridge.R" = "DyNAM-i bridge, same legacy string argument",
    "intercept_only_rate.R" = "prose only, no branch"
  )

  hits <- grep(
    "sub_model *(==|%in%)|(?<![_$\\\\w])model *(==|%in%)",
    source_lines(),
    value = TRUE,
    perl = TRUE
  )
  hits <- hits[!grepl("^\\\\s*#", sub("^[^ ]+ ", "", hits))]
  offenders <- hits[!(sub(":.*", "", hits) %in% names(exceptions))]
  expect_identical(offenders, character(0))
})
