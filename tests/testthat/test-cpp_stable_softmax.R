# Cross-engine agreement for the shared C++ stable-softmax helper (design D7).
# The converged-coefficient cross-engine checks in test-cpp_interface.R only
# exercise benign predictors at the optimum; here we drive the exported
# default_c multinomial estimators at a FIXED extreme parameter and confirm they
# agree with the R stable-softmax contribution path (finite logL / score /
# information where the pre-D7 code overflowed to NaN or underflowed to -Inf).

choice_cpp <- getFromNamespace("estimate_DyNAM_choice", "goldfish")
rate_ordered_cpp <- getFromNamespace("estimate_DyNAM_rate_ordered", "goldfish")
choice_r <- getFromNamespace(
  "compute_event_contribution.dynam_choice_spec",
  "goldfish"
)
rate_ordered_r <- getFromNamespace(
  "compute_event_contribution.dynam_rate_ordered_spec",
  "goldfish"
)

# 1-event fixtures with no updates: apply_flat_updates / apply_broadcast_updates
# are no-ops when the end pointer is 0, so the empty coded matrices are unread.
empty_update <- matrix(numeric(0), 4, 0)
empty_presence_update <- matrix(numeric(0), 2, 0)
zero_pointer <- 0

test_that("default_c DyNAM-choice matches R stable softmax at extreme beta", {
  skip_on_cran()
  n1 <- 4L
  n2 <- 4L
  id_sender <- 2L
  id_receiver <- 1L
  parameters <- 1

  # sender block (rows (id_sender-1)*n2 + 1:n2) holds the receiver predictors;
  # one dominant receiver (1000) forces the observed receiver's probability to
  # underflow, so the old log(exp/normalizer) was -Inf.
  stat_mat_init <- matrix(0, n1 * n2, 1L)
  block <- (id_sender - 1L) * n2
  stat_mat_init[block + 3L, 1L] <- 1000 # receiver j = 2 dominates

  res <- choice_cpp(
    parameters,
    matrix(c(id_sender, id_receiver), 2, 1),
    stat_mat_init,
    empty_update,
    zero_pointer,
    empty_update,
    zero_pointer,
    rep(1, n2),
    empty_presence_update,
    zero_pointer,
    n1,
    n2,
    twomode_or_reflexive = FALSE,
    impute = FALSE,
    active_dyad_is_point = FALSE
  )

  sender_block <- stat_mat_init[(block + 1L):(block + n2), , drop = FALSE]
  ref <- choice_r(
    spec = NULL,
    sender_block,
    activeDyad = c(id_sender, id_receiver),
    parameters = parameters,
    isRightCensored = FALSE,
    timespan = 1,
    allowReflexive = FALSE,
    is_two_mode = FALSE
  )

  expect_true(is.finite(res$intervalLogL[1]))
  expect_equal(res$intervalLogL[1], ref$logLikelihood, tolerance = 1e-8)
  expect_equal(
    as.numeric(res$derivative),
    as.numeric(ref$score),
    tolerance = 1e-8
  )
  expect_equal(res$fisher, ref$informationMatrix, tolerance = 1e-8)
  expect_true(all(is.finite(res$fisher)))
})

test_that("default_c rate-ordered matches R stable softmax at extreme beta", {
  skip_on_cran()
  n1 <- 4L
  n2 <- 4L
  id_sender <- 1L
  parameters <- 1

  # constant sender blocks so reduce_mat_to_vector yields the sender predictor
  # directly; sender 2 dominates, the observed sender 1 underflows.
  sender_values <- c(0, 1000, 5, 2)
  stat_mat_init <- matrix(0, n1 * n2, 1L)
  for (i in seq_len(n1)) {
    stat_mat_init[((i - 1L) * n2 + 1L):(i * n2), 1L] <- sender_values[i]
  }

  res <- rate_ordered_cpp(
    parameters,
    matrix(c(id_sender, 1L), 2, 1),
    stat_mat_init,
    empty_update,
    zero_pointer,
    empty_update,
    zero_pointer,
    rep(1, n1),
    empty_presence_update,
    zero_pointer,
    rep(1, n2),
    empty_presence_update,
    zero_pointer,
    n1,
    n2,
    twomode_or_reflexive = TRUE,
    impute = FALSE
  )

  reduced <- matrix(sender_values, n1, 1L)
  ref <- rate_ordered_r(
    spec = NULL,
    reduced,
    activeDyad = c(id_sender, NA),
    parameters = parameters,
    isRightCensored = FALSE,
    timespan = 1,
    allowReflexive = TRUE,
    is_two_mode = FALSE
  )

  expect_true(is.finite(res$intervalLogL[1]))
  expect_equal(res$intervalLogL[1], ref$logLikelihood, tolerance = 1e-8)
  expect_equal(
    as.numeric(res$derivative),
    as.numeric(ref$score),
    tolerance = 1e-8
  )
  expect_equal(res$fisher, ref$informationMatrix, tolerance = 1e-8)
  expect_true(all(is.finite(res$fisher)))
})
