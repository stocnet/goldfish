# Consistency tests for the internal process-state evaluators + materializer:
# materialize the dense state at each observed event index and assert the
# evaluators reproduce the estimation path's per-event interval log-likelihood
# at 1e-10, for every sub-model on both datasets.

# (model grid key, model_type vocabulary, has_intercept) for each baseline cell.
pse_cells <- list(
  list("se_dynam_rate", "DyNAM-M-Rate", TRUE),
  list("se_dynam_rate_ordered", "DyNAM-M-Rate-ordered", FALSE),
  list("se_dynam_choice", "DyNAM-M", FALSE),
  list("se_dynam_choice_coord", "DyNAM-MM", FALSE),
  list("se_rem", "REM", TRUE),
  list("se_rem_ordered", "REM-ordered", FALSE),
  list("fish_dynam_rate", "DyNAM-M-Rate", TRUE),
  list("fish_dynam_rate_ordered", "DyNAM-M-Rate-ordered", FALSE),
  list("fish_dynam_choice", "DyNAM-M", FALSE),
  list("fish_dynam_choice_coord", "DyNAM-MM", FALSE),
  list("fish_rem", "REM", TRUE),
  list("fish_rem_ordered", "REM-ordered", FALSE)
)

pse_data_list <- function() {
  list(
    social_evolution = baselines_social_evolution_data(),
    fisheries = baselines_fisheries_data()
  )
}

# Fit the cell, then re-evaluate the estimation path at exactly the fitted
# coefficients (pinned via fixed_parameters) so the returned intervalLogL is the
# reference the evaluators must reproduce. Returns coefs, the preprocessed
# statsList, and the per-event reference intervalLogL.
pse_reference <- function(cell, data_list) {
  spec <- baselines_model_grid()[[cell[[1]]]]
  data <- data_list[[spec$dataset]]
  fit <- suppressWarnings(baselines_fit(spec, "default_c", data_list))
  coefs <- coef(fit)

  withr::local_options(lifecycle_verbosity = "quiet")
  ctrl_args <- c(
    list(
      fixed_parameters = coefs,
      return_interval_loglik = TRUE,
      engine = "default_c"
    ),
    spec$estimation_args
  )
  args <- list(
    x = spec$formula,
    data = data,
    control_estimation = do.call(set_estimation_opt, ctrl_args),
    progress = FALSE,
    verbose = FALSE
  )
  if (spec$model == "DyNAM") {
    args$sub_model <- spec$sub_model
    fit_ref <- suppressWarnings(do.call(estimate_dynam, args))
  } else {
    if (!is.null(spec$sub_model)) {
      args$sub_model <- spec$sub_model
    }
    fit_ref <- suppressWarnings(do.call(estimate_rem, args))
  }

  pre <- suppressWarnings(estimate_wrapper(
    x = spec$formula,
    model = spec$model,
    sub_model = spec$sub_model,
    data = data,
    preprocessing_only = TRUE
  ))
  list(
    coefs = coefs,
    pre = pre,
    interval_logL = as.numeric(fit_ref$intervalLogL)
  )
}

test_that("evaluators reproduce the estimation-path intervalLogL at 1e-10", {
  data_list <- pse_data_list()
  for (cell in pse_cells) {
    ref <- pse_reference(cell, data_list)
    model_type <- cell[[2]]
    has_intercept <- cell[[3]]
    n_events <- length(ref$pre$is_dependent)
    is_timed <- model_type %in% c("DyNAM-M-Rate", "REM")

    ev_ill <- rep(NA_real_, n_events)
    for (k in seq_len(n_events)) {
      state <- materialize_process_state(
        ref$pre,
        model_type,
        k,
        has_intercept = has_intercept
      )
      ev_ill[k] <- evaluate_process_state(state, ref$coefs)$interval_logL
    }

    # Timed models score every interval; the multinomial models only the
    # dependent (observed) events.
    keep <- if (is_timed) {
      seq_len(n_events)
    } else {
      which(ref$pre$is_dependent == 1L)
    }
    expect_equal(
      ev_ill[keep],
      ref$interval_logL[keep],
      tolerance = 1e-10,
      label = paste0("interval_logL[", cell[[1]], "]")
    )
  }
})

test_that("multinomial evaluators return exact-zero excluded probabilities", {
  data_list <- pse_data_list()
  multinomial <- Filter(
    function(cell) {
      cell[[2]] %in%
        c("DyNAM-M", "DyNAM-M-Rate-ordered", "REM-ordered", "DyNAM-MM")
    },
    pse_cells
  )
  for (cell in multinomial) {
    ref <- pse_reference(cell, data_list)
    k <- which(ref$pre$is_dependent == 1L)[[1]]
    state <- materialize_process_state(
      ref$pre,
      cell[[2]],
      k,
      has_intercept = cell[[3]]
    )
    out <- evaluate_process_state(state, ref$coefs)
    active <- out$value > 0
    expect_equal(
      sum(out$value),
      1,
      tolerance = 1e-10,
      label = paste0("prob sum [", cell[[1]], "]")
    )
    expect_true(all(out$value[!active] == 0))
    expect_identical(nrow(out$index), length(out$value))
  }
})

test_that("timed evaluators return exact-zero excluded hazards", {
  data_list <- pse_data_list()
  timed <- Filter(
    function(cell) cell[[2]] %in% c("DyNAM-M-Rate", "REM"),
    pse_cells
  )
  for (cell in timed) {
    ref <- pse_reference(cell, data_list)
    k <- which(ref$pre$is_dependent == 1L)[[1]]
    state <- materialize_process_state(
      ref$pre,
      cell[[2]],
      k,
      has_intercept = cell[[3]]
    )
    out <- evaluate_process_state(state, ref$coefs)
    if (cell[[2]] == "REM") {
      allowed <- .pse_allowed_dyads(state)
    } else {
      allowed <- state$active_sender == 1
    }
    expect_true(all(out$value[!allowed] == 0))
    expect_true(all(out$value >= 0))
    expect_identical(nrow(out$index), length(out$value))
  }
})

test_that("no process-state evaluator or materializer is exported", {
  exported <- getNamespaceExports("goldfish")
  internal <- c(
    "materialize_process_state",
    "evaluate_process_state",
    ".pse_allowed_dyads",
    ".pse_eval_choice",
    ".pse_eval_rate",
    ".pse_eval_rate_ordered",
    ".pse_eval_rem",
    ".pse_eval_rem_ordered",
    ".pse_eval_coordination"
  )
  expect_length(intersect(internal, exported), 0)
})
