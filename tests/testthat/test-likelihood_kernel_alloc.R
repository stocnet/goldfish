# The compiled kernels allocate their event-sized work buffers once per
# evaluation, never once per event. Armadillo's allocations are invisible to
# R, so this reads a counter that only the probe build routes them through:
#
#   PKG_CPPFLAGS="-O2 -include $PWD/src/alloc_probe.h \
#     -DARMA_ALIEN_MEM_ALLOC_FUNCTION=goldfish_counted_malloc \
#     -DARMA_ALIEN_MEM_FREE_FUNCTION=goldfish_counted_free" \
#     Rscript -e 'pkgbuild::clean_dll(); devtools::load_all(recompile = TRUE,
#       debug = FALSE); testthat::test_file(
#       "tests/testthat/test-likelihood_kernel_alloc.R")'
#
# Elsewhere it skips; the estimation budget's time cells carry the guard.

# Event-sized: one value per alternative of the family's risk set.
kernel_alloc_event_bytes <- function(prep) {
  stats <- prep$initial_stats
  alternatives <- switch(
    risk_set_axis(prep$model_spec),
    sender = nrow(stats),
    receiver_given_sender = dim(stats)[2],
    dim(stats)[1] * dim(stats)[2]
  )
  8 * alternatives
}

test_that("each kernel allocates nothing event-sized per event", {
  skip_on_cran()
  skip_if_not(alloc_probe_enabled(), "needs the allocation probe build")
  withr::local_options(lifecycle_verbosity = "quiet")
  references <- readRDS(test_path(
    "_references",
    "likelihood_v1",
    "likelihood_references.rds"
  ))
  grid <- baselines_model_grid()
  data_list <- likelihood_pin_data()
  default_diagnostics <- list(
    scores = TRUE,
    ranks = FALSE,
    margins = FALSE,
    total_rate = TRUE,
    probabilities = FALSE,
    availability = FALSE,
    conditional_scores = FALSE,
    information_trace = FALSE
  )
  for (name in grep("^se_", names(grid), value = TRUE)) {
    prep <- likelihood_pin_prep(grid[[name]], data_list)
    n_intervals <- length(prep$is_dependent)
    alloc_probe_start(kernel_alloc_event_bytes(prep))
    evaluate_engine_once(
      prep$model_spec,
      prep,
      references[[name]]$at,
      "cpp",
      default_diagnostics
    )
    n_allocations <- alloc_probe_stop()
    # A per-evaluation buffer set is a handful of allocations; one per event
    # is hundreds here.
    expect_lt(
      n_allocations,
      n_intervals / 10,
      label = paste("event-sized allocations in one", name, "evaluation")
    )
  }
})
