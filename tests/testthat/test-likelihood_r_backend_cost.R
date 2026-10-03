# The r backend keeps its statistics in one buffer per evaluation and writes
# each event's updates into it in place, so an event allocates the vectors its
# contribution forms, not copies of the statistics. Measured in units of the
# statistics array on a 300-actor generated fixture, where one array is about
# 2 MB and a copy per event stands out. Before the in-place rewrite an event
# allocated about 10 arrays on REM and 4 on choice; before the contributions
# read the statistics where they are, 3.5 on REM and 9.8 on the ordinal REM.

# 300 actors of which only the first `n_present` ever call, and the others
# leave at time 0.5 and stay away: an outer-encoded availability that
# excludes most of the grid at nearly every event. Their departure is 200
# sender and 200 receiver flips at one change, each writing the row or the
# column it reaches; 120 calls spread that one-off write the way the other
# fixtures' 60 events spread their evaluation's fixed allocations.
r_backend_cost_absent_data <- function(
  n_actors = 300L,
  n_present = 100L,
  n_calls = 120L
) {
  withr::local_seed(2)
  calls <- data.frame(
    from = sample.int(n_present, n_calls, replace = TRUE),
    to = sample.int(n_present, n_calls, replace = TRUE)
  )
  calls <- calls[calls$from != calls$to, ]
  calls$time <- sort(stats::runif(nrow(calls), 1, 100))
  absent <- (n_present + 1L):n_actors
  changes <- data.frame(time = 0.5, node = absent, var = "active")
  changes$value <- rep(list(list(FALSE)), length(absent))
  list(
    info = list(
      name = "absent actors",
      focal = "calls",
      update = c(calls = "increment"),
      directed = c(calls = TRUE),
      observation = c(calls = "event")
    ),
    nodes = data.frame(
      label = sprintf("A%04d", seq_len(n_actors)),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = cbind(calls, layer = "calls"),
    changes = changes
  )
}

r_backend_cost_prep <- function(sub_model) {
  data <- if (endsWith(sub_model, "_absent")) {
    r_backend_cost_absent_data()
  } else {
    constraint_cost_data(n_actors = 300L, n_calls = 60L, n_emails = 10L)
  }
  model <- if (startsWith(sub_model, "rem")) "REM" else "DyNAM"
  rem_sub_model <- if (startsWith(sub_model, "rem_ordered")) {
    "rate_ordered"
  } else {
    "rate"
  }
  spec <- suppressMessages(make_specification(
    rate = switch(
      sub_model,
      rem = ,
      rem_point = ,
      rem_absent = ~ 1 + inertia + recip,
      rem_ordered = ,
      rem_ordered_absent = ~ inertia + recip,
      rate_ordered = ~ indeg + outdeg
    ),
    choice = if (sub_model == "choice") ~ inertia + recip else NULL,
    model = model,
    rate_sub_model = rem_sub_model,
    layer = "calls",
    data = data,
    support_constraint = if (sub_model == "rem_point") ~ !tie(emails)
  ))
  suppressWarnings(suppressMessages(
    if (model == "REM") {
      estimate_rem(spec, sub_model = rem_sub_model, preprocessing_only = TRUE)
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
  # A REM event allocates one weighted statistics array, `S * p` in the
  # information's cross-product, and three vectors of the risk set's size
  # (the linear predictor, its exponential and the probabilities): at most
  # 1 + 3 / q arrays, q the array's columns, which is 2 on the exact-time
  # fixture (an intercept and two effects) and 2.5 on the ordinal one (two
  # effects). A point constraint changes nothing, its mask being added within
  # the predictor's buffer. The 0.1 above the bound is the evaluation's own
  # fixed allocations (its copy of the initial statistics, the accumulators)
  # spread over the fixture's 60 events. Two thirds of the actors absent
  # change nothing either: an outer filter is a mask kept in step with the
  # composition changes, so no event rebuilds the cells it excludes. A
  # choice event reads a sender's rows.
  rem_limit <- function(prep) 1 + 3 / statistics_columns(prep) + 0.1
  statistics_columns <- function(prep) {
    dim(prep$initial_stats)[3] + isTRUE(prep$has_intercept)
  }
  sub_models <- c(
    "rem",
    "rem_point",
    "rem_absent",
    "rem_ordered",
    "rem_ordered_absent",
    "choice"
  )
  for (sub_model in sub_models) {
    prep <- r_backend_cost_prep(sub_model)
    if (sub_model == "rem_point") {
      expect_identical(prep$active_dyad_encoding, "point")
    }
    if (endsWith(sub_model, "_absent")) {
      expect_identical(prep$active_dyad_encoding, "outer")
    }
    r_backend_cost_pass(prep)
    allocated <- as.numeric(
      bench::bench_memory(r_backend_cost_pass(prep))$mem_alloc
    )
    array_bytes <- 8 *
      prod(dim(prep$initial_stats)[1:2]) *
      statistics_columns(prep)
    per_event <- allocated / length(prep$is_dependent) / array_bytes
    expect_lt(
      per_event,
      if (sub_model == "choice") 0.25 else rem_limit(prep),
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
