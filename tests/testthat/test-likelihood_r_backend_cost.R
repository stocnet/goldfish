# The r backend keeps its statistics in one buffer per evaluation and writes
# each event's updates into it in place, so an event allocates the vectors its
# contribution forms, not copies of the statistics. Measured in units of the
# statistics array on a 300-actor generated fixture, where one array is about
# 2 MB and a copy per event stands out. Before the in-place rewrite an event
# allocated about 10 arrays on REM and 4 on choice.

r_backend_cost_prep <- function(sub_model) {
  data <- constraint_cost_data(n_actors = 300L, n_calls = 60L, n_emails = 10L)
  spec <- suppressMessages(make_specification(
    rate = switch(
      sub_model,
      rem = ~ 1 + inertia + recip,
      rate_ordered = ~ indeg + outdeg
    ),
    choice = if (sub_model == "choice") ~ inertia + recip else NULL,
    model = if (sub_model == "rem") "REM" else "DyNAM",
    layer = "calls",
    data = data
  ))
  suppressWarnings(suppressMessages(
    if (sub_model == "rem") {
      estimate_rem(spec, preprocessing_only = TRUE)
    } else {
      estimate_dynam(spec, sub_model = sub_model, preprocessing_only = TRUE)
    }
  ))
}

r_backend_cost_pass <- function(prep) {
  n_statistics <- if (is.matrix(prep$initial_stats)) {
    ncol(prep$initial_stats)
  } else {
    dim(prep$initial_stats)[3]
  }
  at <- c(if (isTRUE(prep$has_intercept)) -5, rep(0.1, n_statistics))
  needs <- list(
    scores = FALSE,
    ranks = FALSE,
    margins = FALSE,
    total_rate = FALSE,
    probabilities = FALSE,
    availability = FALSE,
    conditional_scores = FALSE,
    information_trace = FALSE
  )
  evaluate_engine_once(prep$model_spec, prep, at, "r", needs)
}

test_that("an r-backend event allocates no copy of the statistics", {
  skip_on_cran()
  skip_if_not_installed("bench")
  withr::local_options(lifecycle_verbosity = "quiet")
  # Arrays per event: the contribution's own vectors and one weighted
  # cross-product on REM; a sender's rows on choice.
  limits <- c(rem = 4, choice = 0.25)
  for (sub_model in names(limits)) {
    prep <- r_backend_cost_prep(sub_model)
    r_backend_cost_pass(prep)
    allocated <- as.numeric(
      bench::bench_memory(r_backend_cost_pass(prep))$mem_alloc
    )
    n1 <- dim(prep$initial_stats)[1]
    n2 <- dim(prep$initial_stats)[2]
    array_bytes <- 8 *
      n1 *
      n2 *
      (dim(prep$initial_stats)[3] + isTRUE(prep$has_intercept))
    per_event <- allocated / length(prep$is_dependent) / array_bytes
    expect_lt(
      per_event,
      limits[[sub_model]],
      label = paste("statistics arrays allocated per", sub_model, "event")
    )
  }
})

test_that("a constraint that excludes nothing costs the r backend nothing", {
  skip_on_cran()
  skip_if_not_installed("bench")
  withr::local_options(lifecycle_verbosity = "quiet")
  # A constrained object carries its availability buffers even when they
  # hold no flip, as 2 x 0 matrices rather than NULL. An empty buffer is no
  # filter: were it read as one, every event would take the filtered path
  # and read the kept rows of the statistics, a copy per event that changes
  # no number.
  data <- constraint_cost_data(n_actors = 300L, n_calls = 60L, n_emails = 10L)
  data$nodes$allow <- 1
  noop_prep <- function(sub_model, constraint) {
    spec <- suppressMessages(make_specification(
      rate = if (sub_model == "rem") ~ 1 + inertia + recip else NULL,
      choice = if (sub_model == "choice") ~ inertia + recip else NULL,
      model = if (sub_model == "rem") "REM" else "DyNAM",
      layer = "calls",
      data = data,
      support_constraint = constraint
    ))
    suppressWarnings(suppressMessages(
      if (sub_model == "rem") {
        estimate_rem(spec, preprocessing_only = TRUE)
      } else {
        estimate_dynam(spec, sub_model = "choice", preprocessing_only = TRUE)
      }
    ))
  }
  for (sub_model in c("rem", "choice")) {
    free <- noop_prep(sub_model, NULL)
    noop <- noop_prep(sub_model, ~ alter(allow) > 0)
    estimates <- function(prep) {
      r_backend_cost_pass(prep)[c(
        "logLikelihood",
        "score",
        "informationMatrix"
      )]
    }
    expect_equal(estimates(noop), estimates(free), tolerance = 1e-12)
    allocated <- vapply(
      list(free = free, noop = noop),
      function(prep) {
        as.numeric(bench::bench_memory(r_backend_cost_pass(prep))$mem_alloc)
      },
      numeric(1)
    )
    # Both evaluations walk the same events with the same statistics, so
    # the allocations match; a filtered path adds a copy per event, a third
    # more on REM and a twentieth on choice at this size.
    expect_lt(
      allocated[["noop"]] / allocated[["free"]],
      1.005,
      label = paste("no-op over unconstrained allocation,", sub_model)
    )
  }
})

test_that("an r-backend evaluation leaves the preprocessed object untouched", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  # The in-place writers would corrupt the preprocessed object if a buffer
  # they write were shared with it. A serialized round trip is a copy no
  # in-place write can reach. Formulas, functions and environments are left
  # out: a round trip rebuilds their environments, and no writer touches
  # them. One fixture per loop shape: the dyad loop, the choice loop and the
  # sender loop.
  data_fields <- function(prep) {
    keep <- vapply(
      prep,
      function(x) {
        !is.function(x) && !is.environment(x) && !inherits(x, "formula")
      },
      logical(1)
    )
    unclass(prep)[keep]
  }
  for (sub_model in c("rem", "choice", "rate_ordered")) {
    prep <- r_backend_cost_prep(sub_model)
    before <- data_fields(unserialize(serialize(prep, NULL)))
    r_backend_cost_pass(prep)
    expect_identical(data_fields(prep), before, info = sub_model)
  }
})

test_that("the coordination contribution runs without a per-row closure", {
  # The r backend computes a contribution as staged linear algebra; a closure
  # run once per row or slice is what the coordination path used to carry (the
  # row maximum of its softmax, the expected statistics of its derivative).
  # Read from each function's parsed body, so a comment cannot satisfy it.
  closures <- c("apply", "sapply", "vapply", "lapply", "mapply", "Map")
  call_path <- mget(
    c(
      "compute_event_contribution.goldfishLikCoordination",
      "getMultinomialProbabilities",
      "stable_softmax",
      "getLikelihoodMM",
      "compute_first_derivative_choice_coord",
      "getMultinomialInformationMatrix"
    ),
    envir = asNamespace("goldfish")
  )
  for (name in names(call_path)) {
    called <- intersect(closures, all.names(body(call_path[[name]])))
    expect_identical(called, character(0), info = name)
  }
})
