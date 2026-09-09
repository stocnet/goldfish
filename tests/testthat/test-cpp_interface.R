test_that("DyNAM-rate", {
  skip_on_cran()
  model <- "DyNAM"
  sub_model <- "rate"
  # endogenous and right-censored events
  formula <- depNetwork ~ 1 + indeg + outdeg(networkExog, weighted = TRUE)
  modR <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(
      backend = "r",
      return_interval_loglik = TRUE
    ),
    progress = FALSE,
    verbose = FALSE
  )
  modCd <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(
      backend = "cpp",
      return_interval_loglik = TRUE
    )
  )
  modCgc <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(backend = "gather")
  )
  expect_equal(coef(modR), coef(modCd))
  expect_equal(coef(modR), coef(modCgc))

  expect_equal(vcov(modR), vcov(modCd))
  expect_equal(vcov(modR), vcov(modCgc))
})
test_that("DyNAM-rate ordered", {
  skip_on_cran()
  model <- "DyNAM"
  sub_model <- "rate_ordered"
  # endogenous and right-censored events
  formula <- depNetwork ~ indeg + outdeg(networkExog, weighted = TRUE)
  modR <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(backend = "r")
  )
  modCd <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(backend = "cpp")
  )
  modCgc <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(backend = "gather")
  )
  expect_equal(coef(modR), coef(modCd))
  expect_equal(coef(modR), coef(modCgc))

  expect_equal(vcov(modR), vcov(modCd))
  expect_equal(vcov(modR), vcov(modCgc))
})
test_that("DyNAM-choice", {
  skip_on_cran()
  model <- "DyNAM"
  sub_model <- "choice"
  # endogenous and right-censored events
  formula <- depNetwork ~ inertia + indeg + outdeg(networkExog, weighted = TRUE)
  modR <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(backend = "r")
  )
  modCd <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(backend = "cpp")
  )
  modCgc <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(backend = "gather")
  )
  expect_equal(coef(modR), coef(modCd))
  expect_equal(coef(modR), coef(modCgc))

  expect_equal(vcov(modR), vcov(modCd))
  expect_equal(vcov(modR), vcov(modCgc))
})
test_that("REM", {
  skip_on_cran()
  model <- "REM"
  sub_model <- "rate"
  # endogenous and right-censored events
  formula <- depNetwork ~ 1 +
    inertia +
    indeg +
    outdeg(networkExog, type = "ego", weighted = TRUE)
  modR <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(backend = "r")
  )
  modCd <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(backend = "cpp")
  )
  modCgc <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(backend = "gather")
  )
  expect_equal(coef(modR), coef(modCd))
  expect_equal(coef(modR), coef(modCgc))

  expect_equal(vcov(modR), vcov(modCd))
  expect_equal(vcov(modR), vcov(modCgc))
})
test_that("REM ordered", {
  skip_on_cran()
  model <- "REM"
  sub_model <- "rate_ordered"
  # endogenous and right-censored events
  formula <- depNetwork ~ inertia +
    indeg +
    outdeg(networkExog, type = "ego", weighted = TRUE)
  modR <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(backend = "r")
  )
  modCd <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(backend = "cpp")
  )
  modCgc <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(backend = "gather")
  )
  expect_equal(coef(modR), coef(modCd))
  expect_equal(coef(modR), coef(modCgc))

  expect_equal(vcov(modR), vcov(modCd))
  expect_equal(vcov(modR), vcov(modCgc))
})
test_that("DyNAM-choice_coordination", {
  skip_on_cran()
  model <- "DyNAM"
  sub_model <- "choice_coordination"
  # endogenous and right-censored events
  # NB: choice_coordination rejects an ego-perspective main effect, so this
  # engine-consistency vector uses the default alter
  # perspective — the effect only needs to be valid and non-trivial here.
  formula <- depNetwork ~ inertia +
    indeg +
    indeg(networkExog, weighted = TRUE)
  modR <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(backend = "r")
  )
  modCd <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(backend = "cpp")
  )
  modCgc <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(backend = "gather")
  )
  expect_equal(coef(modR), coef(modCd))
  expect_equal(coef(modR), coef(modCgc))

  expect_equal(vcov(modR), vcov(modCd))
  expect_equal(vcov(modR), vcov(modCgc))
})

test_that("folding a stat buffer never duplicates it", {
  # The fold helpers return the cells to write instead of writing them, because
  # passing the buffer to a helper binds it to a second name and the helper's
  # own subassignment then copies the whole matrix, every event. At 1899 actors
  # that copy is 27 MB per event and close to the entire preprocessing cost.
  skip_if_not(capabilities("profmem"))

  n1 <- 40L
  n2 <- 40L
  n_effects <- 3L
  updates <- rbind(
    c(0L, 1L, 2L, 3L),
    c(1L, 2L, 3L, 0L),
    c(0L, 1L, 2L, 0L),
    c(1, 2, 3, 4)
  )
  broadcasts <- rbind(c(1L, 3L), c(2L, 1L), c(0L, 2L), c(7, 8))

  stat_mat <- matrix(0, n1 * n2, n_effects)
  traced <- capture.output(
    {
      invisible(tracemem(stat_mat))
      for (event in seq_len(4L)) {
        cells <- .gather_stat_cells(updates, 0L, 4L, n2)
        stat_mat[cells$idx] <- cells$value
        for (block in .gather_broadcast_blocks(
          broadcasts,
          0L,
          2L,
          n1,
          n2,
          FALSE
        )) {
          stat_mat[block$rows, block$col] <- block$value
        }
      }
      untracemem(stat_mat)
    },
    type = "output"
  )

  expect_equal(traced, character(0))
})

test_that("the fold applies stat cells with the later write winning", {
  # Duplicate cells stay in column order, so one subassignment leaves the last
  # of them standing, as the sequential C++ assignment does.
  updates <- rbind(c(0L, 0L), c(1L, 1L), c(0L, 0L), c(5, 9))
  stat_mat <- matrix(0, 4L, 2L)

  cells <- .gather_stat_cells(updates, 0L, 2L, 2L)
  stat_mat[cells$idx] <- cells$value

  expect_equal(stat_mat[2, 1], 9)
})

test_that("the fold skips the reflexive cell in a one-mode broadcast", {
  # Kind 1 fans a value over every sender holding one alter; the sender that is
  # that alter is dropped unless the model is two-mode or reflexive.
  broadcasts <- rbind(1L, 1L, 0L, 6)

  restricted <- .gather_broadcast_blocks(broadcasts, 0L, 1L, 3L, 3L, FALSE)
  reflexive <- .gather_broadcast_blocks(broadcasts, 0L, 1L, 3L, 3L, TRUE)

  expect_length(restricted[[1]]$rows, 2L)
  expect_length(reflexive[[1]]$rows, 3L)
})
