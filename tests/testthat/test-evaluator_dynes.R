# The prototype-path pool evaluator behind the ABMCEM loop's evaluator contract.
# A pool is a set of endpoint-hitting sequences: each is preprocessed once, on
# entry, through the existing estimation pipeline, and its per-sub-model
# statistics are stored so that re-evaluation at any parameter vector reuses them
# without re-preprocessing. These tests cover the entry path (structure,
# preprocess-once, reference-record attachment, the serial map seam).

# A minimal single-sub-model pool over the bundled social_evolution data. Both
# sequences share the same data object, which is enough to observe the
# preprocess-once bookkeeping: preprocessing is deterministic, so identical data
# still exercises "one preprocess per pooled sequence".
make_pool_fixture <- function(n_sequences = 2L) {
  data("social_evolution", envir = environment())
  sub_models <- list(
    new_pool_submodel(
      calls ~ inertia + recip + trans,
      model = "DyNAM",
      sub_model = "choice",
      theta_index = 1:3
    )
  )
  sequences <- lapply(seq_len(n_sequences), function(i) {
    list(
      data = social_evolution,
      reference = new_reference_record(
        theta_ref = c(0, 0, 0),
        loglik_ref = -100 - i,
        log_proposal = -100 - i
      )
    )
  })
  list(sub_models = sub_models, sequences = sequences)
}

test_that("the pool preprocesses once per sequence on entry", {
  fx <- make_pool_fixture(n_sequences = 2L)

  # Count invocations of the per-sequence preprocessing seam, delegating to the
  # real implementation so the stored statistics are genuine.
  preprocess_calls <- 0L
  real_preprocess <- goldfish:::preprocess_pool_sequence
  local_mocked_bindings(
    preprocess_pool_sequence = function(...) {
      preprocess_calls <<- preprocess_calls + 1L
      real_preprocess(...)
    }
  )

  pool <- build_sequence_pool(
    fx$sub_models,
    fx$sequences,
    theta_length = 3L
  )

  # Exactly one preprocessing pass per pooled sequence, on entry.
  expect_identical(preprocess_calls, length(fx$sequences))
})

test_that("pool entry stores reusable per-sub-model statistics", {
  fx <- make_pool_fixture(n_sequences = 2L)
  pool <- build_sequence_pool(fx$sub_models, fx$sequences, theta_length = 3L)

  expect_s3_class(pool, "goldfishDynesPool")
  expect_length(pool$sequences, 2L)
  # One stored goldfishStat per sub-model, kept with the sequence for reuse.
  stored <- pool$sequences[[1]]$stats
  expect_length(stored, length(fx$sub_models))
  expect_s3_class(stored[[1]], "goldfishStat")
  expect_identical(pool$theta_length, 3L)
})

test_that("pool entry attaches each sequence's reference record", {
  fx <- make_pool_fixture(n_sequences = 2L)
  pool <- build_sequence_pool(fx$sub_models, fx$sequences, theta_length = 3L)

  # Reference records travel with their sequence, in pool order.
  expect_equal(pool$sequences[[1]]$reference$loglik_ref, -101)
  expect_equal(pool$sequences[[2]]$reference$loglik_ref, -102)
  # The pool exposes the stored log proposal densities in sequence order (the
  # importance-weight denominators the E-step reads).
  expect_equal(pool_references(pool) |> pool_log_proposal(), c(-101, -102))
})

test_that("per-sequence entry runs through the injectable map seam", {
  fx <- make_pool_fixture(n_sequences = 3L)

  # A custom map seam (the serial lapply default is replaceable, e.g. by a
  # parallel backend) is used for the per-sequence entry loop.
  mapped <- 0L
  counting_map <- function(x, f) {
    mapped <<- mapped + length(x)
    lapply(x, f)
  }
  pool <- build_sequence_pool(
    fx$sub_models,
    fx$sequences,
    theta_length = 3L,
    map_fn = counting_map
  )
  expect_identical(mapped, 3L)
  expect_length(pool$sequences, 3L)
})
