# A gather build allocates in proportion to the stack it produces: each
# event's candidate index is written into a buffer sized once and trimmed,
# never grown sender by sender, and the point availability grid is updated in
# its cells. Measured on a constrained REM whose stack is about 30 MB; the
# grown index allocated about 22 times the stack.

test_that("a gather build allocates in proportion to its stack", {
  skip_on_cran()
  skip_if_not_installed("bench")
  withr::local_options(lifecycle_verbosity = "quiet")
  data <- constraint_cost_data(n_actors = 120L, n_calls = 30L, n_emails = 60L)
  spec <- suppressMessages(make_specification(
    rate = ~ 1 + inertia + recip,
    model = "REM",
    layer = "calls",
    data = data,
    support_constraint = ~ !tie(emails)
  ))
  prep <- suppressWarnings(suppressMessages(
    estimate_rem(spec, preprocessing_only = TRUE)
  ))
  build <- function(backend) {
    make_engine_evaluator(
      prep$model_spec,
      prep,
      numeric(0),
      backend,
      has_intercept = isTRUE(prep$has_intercept),
      is_rate_model = FALSE,
      seed_intercept = FALSE
    )
  }
  gathered <- build("gather")$gathered
  build("cpp")
  # The compiled engine's preparation is the part the two builds share, so
  # the difference is the gather build itself.
  gather_bytes <- as.numeric(bench::bench_memory(build("gather"))$mem_alloc)
  shared_bytes <- as.numeric(bench::bench_memory(build("cpp"))$mem_alloc)
  stack_bytes <- 8 * length(gathered$stat_all_events)
  # The stack itself, its per-event pieces before they are bound, and the
  # per-row index vocabulary: a few stacks' worth.
  expect_lt((gather_bytes - shared_bytes) / stack_bytes, 8)
})
