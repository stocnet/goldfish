# The labeled margins and the uniform accessor over them: one tibble schema for
# every model family, so a calibration comparison never branches on the family.
# The identities the two expected columns satisfy differ -- the probability
# scale totals the event count at ANY parameter vector, the compensator scale
# only at the MLE -- and the tests below pin that difference rather than
# asserting one tolerance for both.

margin_table_fit <- function(key, ...) {
  data_list <- list(social_evolution = baselines_social_evolution_data())
  spec <- baselines_model_grid()[[key]]
  ctrl <- do.call(
    set_algorithm_newton,
    c(list(diagnostics = c("loglik", "margins")), list(...))
  )
  args <- list(
    x = spec$formula,
    data = data_list[[spec$dataset]],
    control_algo = ctrl,
    progress = FALSE,
    verbose = FALSE
  )
  if (spec$model == "DyNAM") {
    args$sub_model <- spec$sub_model
    suppressWarnings(do.call(estimate_dynam, args))
  } else {
    if (!is.null(spec$sub_model)) {
      args$sub_model <- spec$sub_model
    }
    suppressWarnings(do.call(estimate_rem, args))
  }
}

test_that("margin_table returns one schema on every family", {
  skip_on_cran()
  roles <- list(
    se_dynam_rate = "sender",
    se_dynam_choice = "receiver",
    se_dynam_choice_coord = "endpoint",
    se_rem = c("sender", "receiver")
  )
  for (key in names(roles)) {
    mt <- margin_table(margin_table_fit(key))
    expect_s3_class(mt, "margin_table")
    expect_s3_class(mt, "tbl_df")
    expect_named(
      mt,
      c("actor", "role", "observed", "expected_probability", "expected_count")
    )
    expect_equal(unique(mt$role), roles[[key]], info = key)
    # Actor labels come from the node set, never from a positional index.
    expect_true(all(grepl("^Actor ", mt$actor)), info = key)
  }
})

test_that("a REM fit contributes a sender and a receiver row per actor", {
  skip_on_cran()
  mt <- margin_table(margin_table_fit("se_rem"))
  senders <- mt$actor[mt$role == "sender"]
  receivers <- mt$actor[mt$role == "receiver"]
  expect_equal(senders, receivers)
  expect_equal(nrow(mt), 2 * length(senders))
})

test_that("each event credits both endpoints of a coordination tie", {
  skip_on_cran()
  fit <- margin_table_fit("se_dynam_choice_coord")
  mt <- margin_table(fit)
  # The handshake identity: a multinomial family has no right-censored
  # intervals, so `n_events` is the dependent-event count and each event is
  # credited to both members of the pair.
  expect_equal(sum(mt$observed), 2 * fit$n_events)
  expect_equal(
    sum(mt$expected_probability),
    sum(mt$observed),
    tolerance = 1e-8
  )
})

test_that("a scale the family does not define is NA, not absent", {
  skip_on_cran()
  mt <- margin_table(margin_table_fit("se_dynam_choice"))
  expect_true(all(is.na(mt$expected_count)))
  expect_false(anyNA(mt$expected_probability))
  expect_equal(
    attr(mt, "context")$defined_scales,
    "expected_probability"
  )

  exact <- margin_table(margin_table_fit("se_dynam_rate"))
  expect_false(anyNA(exact$expected_count))
  expect_equal(
    attr(exact, "context")$defined_scales,
    c("expected_probability", "expected_count")
  )
})

test_that("probability margins total the events at any parameter vector", {
  skip_on_cran()
  # Pinned away from the MLE: the probability scale still totals the observed
  # count exactly, while the compensator scale is off -- that gap IS the
  # calibration information the compensator carries.
  off_mle <- margin_table(
    margin_table_fit(
      "se_dynam_rate",
      initial_parameters = rep(0.3, 4),
      max_iterations = 0
    )
  )
  expect_equal(
    sum(off_mle$expected_probability),
    sum(off_mle$observed),
    tolerance = 1e-8
  )
  expect_false(
    isTRUE(all.equal(
      sum(off_mle$expected_count),
      sum(off_mle$observed),
      tolerance = 1e-3
    ))
  )

  at_mle <- margin_table(margin_table_fit("se_dynam_rate"))
  expect_equal(
    sum(at_mle$expected_count),
    sum(at_mle$observed),
    tolerance = 1e-4
  )
  expect_equal(
    sum(at_mle$expected_probability),
    sum(at_mle$observed),
    tolerance = 1e-8
  )
})

test_that("margins carry the same labels and scales on every backend", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  for (key in c("se_dynam_choice", "se_rem")) {
    reference <- margin_table_fit(key)
    for (backend in c("r", "gather")) {
      other <- margin_table_fit(
        key,
        backend = backend,
        initial_parameters = reference$parameters,
        max_iterations = 0
      )
      expect_named(other$margins, names(reference$margins), info = backend)
      for (component in names(reference$margins)) {
        expect_identical(
          names(other$margins[[component]]),
          names(reference$margins[[component]]),
          info = paste(key, backend, component)
        )
        expect_identical(
          attr(other$margins[[component]], "scale"),
          attr(reference$margins[[component]], "scale"),
          info = paste(key, backend, component)
        )
      }
      mt_other <- margin_table(other)
      mt_reference <- margin_table(reference)
      expect_identical(mt_other$actor, mt_reference$actor, info = backend)
      expect_identical(mt_other$role, mt_reference$role, info = backend)
      expect_equal(
        mt_other$expected_probability,
        mt_reference$expected_probability,
        tolerance = 1e-10,
        info = paste(key, backend)
      )
    }
  }
})

test_that("the class and its metadata survive subsetting", {
  skip_on_cran()
  mt <- margin_table(margin_table_fit("se_dynam_choice"))
  head_rows <- mt[1:3, ]
  expect_s3_class(head_rows, "margin_table")
  expect_equal(attr(head_rows, "context"), attr(mt, "context"))
  expect_equal(attr(head_rows, "diagnostic"), "margin_table")

  active <- mt[mt$observed > 0, ]
  expect_s3_class(active, "margin_table")
  expect_equal(attr(active, "version"), attr(mt, "version"))
})

test_that("the conditional component decomposes the exact-time loglik", {
  skip_on_cran()
  # The which/when split at the MLE: on a dependent interval the exact-time
  # log-likelihood is log p(observed moves next) + log T - dt * T, so the stored
  # conditional component and total rate reassemble interval_log_lik. Asserted
  # near the MLE only -- the reverse identity loses digits away from it, which
  # is why the kernel computes the component directly.
  data_list <- list(social_evolution = baselines_social_evolution_data())
  spec <- baselines_model_grid()[["se_dynam_rate"]]
  fit <- suppressWarnings(estimate_dynam(
    spec$formula,
    sub_model = spec$sub_model,
    data = data_list[[spec$dataset]],
    control_algo = set_algorithm_newton(diagnostics = "loglik")
  ))
  prep <- suppressWarnings(estimate_dynam(
    spec$formula,
    sub_model = spec$sub_model,
    data = data_list[[spec$dataset]],
    preprocessing_only = TRUE
  ))
  dependent <- prep$is_dependent == 1
  expect_gt(sum(!dependent), 0)
  expect_true(all(is.na(fit$conditional_logl[!dependent])))
  expect_equal(
    fit$interval_log_lik[dependent],
    fit$conditional_logl[dependent] +
      log(fit$total_rate[dependent]) -
      prep$intervals[dependent] * fit$total_rate[dependent],
    tolerance = 1e-10
  )
})

# A state-consistent alternating stream over one mutually exclusive increment
# layer: creations only ever target an absent tie and dissolutions only a
# present one, and it is sized so the per-flavor fits are well conditioned.
margin_flavored_fixture <- function(n_actors = 12L, n_events = 80L, seed = 3L) {
  withr::local_seed(seed)
  state <- matrix(0L, n_actors, n_actors)
  hist_from <- seq_len(n_actors)
  hist_to <- (hist_from %% n_actors) + 1L
  state[cbind(hist_from, hist_to)] <- 1L

  from <- integer(0)
  to <- integer(0)
  weight <- numeric(0)
  for (e in seq_len(n_events)) {
    creating <- (e %% 2L) == 1L
    candidates <- which(
      if (creating) state == 0L else state == 1L,
      arr.ind = TRUE
    )
    candidates <- candidates[candidates[, 1] != candidates[, 2], , drop = FALSE]
    pick <- candidates[sample.int(nrow(candidates), 1L), ]
    from <- c(from, as.integer(pick[[1]]))
    to <- c(to, as.integer(pick[[2]]))
    weight <- c(weight, if (creating) 1 else -1)
    state[pick[[1]], pick[[2]]] <- if (creating) 1L else 0L
  }

  add_flavor(
    list(
      info = list(
        name = "toy",
        focal = "calls",
        update = c(calls = "increment"),
        directed = c(calls = TRUE),
        observation = c(calls = "event")
      ),
      nodes = data.frame(
        label = paste0("N", seq_len(n_actors)),
        mode = "p",
        stringsAsFactors = FALSE
      ),
      ties = data.frame(
        from = c(hist_from, from),
        to = c(hist_to, to),
        time = c(rep(NA_real_, n_actors), seq_along(from)),
        layer = "calls",
        weight = c(rep(1, n_actors), weight),
        stringsAsFactors = FALSE
      )
    ),
    layer = "calls",
    values_equivalence = c(creation = 1, dissolution = -1)
  )
}

margin_flavored_fit <- function() {
  suppressWarnings(estimate_dynam(
    make_specification(
      rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg),
      choice = list(creation ~ trans, dissolution ~ trans),
      model = "DyNAM",
      data = margin_flavored_fixture()
    ),
    control_algo = set_algorithm_newton(diagnostics = c("loglik", "margins"))
  ))
}

test_that("a flavored fit gains flavor and family columns", {
  skip_on_cran()
  fit <- margin_flavored_fit()
  mt <- margin_table(fit)
  expect_s3_class(mt, "margin_table")
  expect_named(
    mt,
    c(
      "actor",
      "role",
      "observed",
      "expected_probability",
      "expected_count",
      "flavor",
      "family"
    )
  )
  map <- fit$process_map
  expect_setequal(unique(mt$flavor), map$flavor)
  expect_setequal(unique(mt$family), map$family)
  # One block of rows per process, each covering the whole node set.
  expect_equal(nrow(mt), nrow(map) * 12)
  expect_setequal(attr(mt, "context")$flavor, map$flavor)
  # The rate processes define the compensator scale and the choice ones do not,
  # so the combined table carries both and the NA is per row, not per table.
  expect_equal(
    attr(mt, "context")$defined_scales,
    c("expected_probability", "expected_count")
  )
  expect_false(anyNA(mt$expected_count[mt$family == "rate"]))
  expect_true(all(is.na(mt$expected_count[mt$family == "choice"])))
})

test_that("printing reports the fit and the flavors behind the table", {
  skip_on_cran()
  withr::local_options(cli.width = 80, cli.num_colors = 1)
  # The header is cli output (the condition stream); the tibble body below it
  # is the ordinary tibble print.
  single <- capture_messages(print(
    margin_table(margin_table_fit("se_dynam_choice"))
  ))
  expect_match(single, "margin_table", all = FALSE)
  expect_match(single, "DyNAM", all = FALSE)
  expect_match(single, "receiver", all = FALSE)
  # The documented not-defined scale is announced rather than left as an NA
  # column a reader has to interpret.
  expect_match(single, "expected_count is not defined", all = FALSE)

  flavored <- capture_messages(print(margin_table(margin_flavored_fit())))
  expect_match(flavored, "2 flavors", all = FALSE)
  expect_match(flavored, "creation", all = FALSE)
  expect_match(flavored, "4 processes", all = FALSE)
})

test_that("margin_table errors without the stored primitive", {
  fit <- estimate_wrapper(
    depNetwork ~ inertia + recip,
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest
  )
  expect_snapshot(margin_table(fit), error = TRUE)
  expect_snapshot(margin_table(1:3), error = TRUE)
})
