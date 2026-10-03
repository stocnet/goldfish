# The compiled dyad kernels keep their risk set in one buffer, set once per
# evaluation and rewritten in the cells each availability flip reaches. The r
# backend rebuilds its mask from the availability at every event, so agreement
# of the two, event by event, on fixtures whose availability flips (a point
# constraint, a vector constraint at the outer encoding, composition changes,
# two node sets), is agreement of the maintained buffer with a from-scratch
# mask: a cell the buffer missed changes that event's rates, and with them its
# interval log-likelihood, score row and availability.

# A dyadic constraint on a network that grows while the events run: each
# email excludes its dyad from that point on, so the point-encoded
# availability flips cells during the sequence (several hundred flips here),
# not only in its initial value.
risk_set_point_flip_prep <- function(sub_model) {
  data <- constraint_cost_data(n_actors = 120L, n_calls = 400L, n_emails = 400L)
  spec <- suppressMessages(make_specification(
    rate = if (sub_model == "rate") {
      ~ 1 + inertia + recip
    } else {
      ~ inertia + recip
    },
    model = "REM",
    rate_sub_model = sub_model,
    layer = "calls",
    data = data,
    support_constraint = ~ !tie(emails)
  ))
  suppressWarnings(suppressMessages(
    estimate_rem(spec, sub_model = sub_model, preprocessing_only = TRUE)
  ))
}

risk_set_fixtures <- function() {
  fisheries <- likelihood_pin_data()["fisheries"]
  grid <- baselines_model_grid()
  list(
    rem_point = constraint_atoms_prep("rem"),
    rem_point_flips = risk_set_point_flip_prep("rate"),
    rem_ordered_point_flips = risk_set_point_flip_prep("rate_ordered"),
    coord_point = constraint_atoms_prep("coord"),
    rem_alter = availability_vector_run("rem_alter"),
    rem_ego = availability_vector_run("rem_ego"),
    rem_global = availability_vector_run("rem_global"),
    coord_alter = availability_vector_run("coord_alter"),
    rem_composition = likelihood_pin_prep(grid$fish_rem, fisheries),
    rem_ordered_composition = likelihood_pin_prep(
      grid$fish_rem_ordered,
      fisheries
    ),
    coord_composition = likelihood_pin_prep(
      grid$fish_dynam_choice_coord,
      fisheries
    ),
    rem_two_mode = suppressWarnings(suppressMessages(estimate_rem(
      make_specification(
        rate = list(modeled ~ 1 + inertia(support) + indeg(support)),
        model = "REM",
        data = as_goldfish(readRDS(test_path(
          "fixtures",
          "irps_nuclear_subset.rds"
        )))
      ),
      preprocessing_only = TRUE
    )))
  )
}

risk_set_pass <- function(prep, at, backend) {
  needs <- list(
    scores = TRUE,
    ranks = FALSE,
    margins = FALSE,
    total_rate = FALSE,
    probabilities = FALSE,
    availability = TRUE,
    conditional_scores = FALSE,
    information_trace = FALSE
  )
  res <- evaluate_engine_once(prep$model_spec, prep, at, backend, needs)
  common <- normalize_engine_pass(res, backend)
  list(
    interval_loglik = as.numeric(common$interval_loglik),
    event_scores = unname(as.matrix(res$event_scores)),
    availability = lapply(
      evaluate_availability(res, prep$model_spec, prep, backend),
      function(x) as.numeric(unlist(x, use.names = FALSE))
    )
  )
}

test_that("the point-flip fixtures flip cells during the sequence", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  # Without flips during the sequence the comparison below cannot see a
  # maintained cell that stopped following the availability.
  for (sub_model in c("rate", "rate_ordered")) {
    prep <- risk_set_point_flip_prep(sub_model)
    expect_identical(prep$active_dyad_encoding, "point", info = sub_model)
    expect_gt(NCOL(prep$active_dyad_update), 100)
  }
})

test_that("the maintained risk set gives the from-scratch mask's events", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  for (name in names(fixtures <- risk_set_fixtures())) {
    prep <- fixtures[[name]]
    n_parameters <- if (is.matrix(prep$initial_stats)) {
      ncol(prep$initial_stats)
    } else {
      dim(prep$initial_stats)[3]
    }
    n_parameters <- n_parameters + isTRUE(prep$has_intercept)
    # A fixed, unremarkable vector: the comparison is between two
    # implementations at the same point, not at an optimum.
    at <- rep(0.1, n_parameters)
    if (isTRUE(prep$has_intercept)) {
      at[1] <- -5
    }
    cpp <- risk_set_pass(prep, at, "cpp")
    r <- risk_set_pass(prep, at, "r")
    expect_equal(cpp, r, tolerance = 1e-10, info = name)
  }
})
