# The from-result form `set_init_param(spec, result)` -- the fit -> re-simulate
# round-trip. It reconstructs the per-fid vectors from the fitted result's own
# coef_layout() (which keeps the fid grouping the flat coef() vector discards),
# after asserting the result was fit against that same spec. A flat, free-only
# coef() vector is not accepted directly (its per-parameter names collide across
# fids).

# A two-process DyNAM join (calls, emails); the calls choice carries an
# inline-coef offset, so `tie(friendship)` is the one fixed coefficient.
result_join <- function() {
  nodes <- data.frame(
    label = paste0("N", 1:6),
    mode = "p",
    stringsAsFactors = FALSE
  )
  ties <- rbind(
    data.frame(
      from = c(1L, 2L, 3L, 4L),
      to = c(2L, 3L, 4L, 5L),
      time = c(1, 2, 3, 4),
      layer = "friendship"
    ),
    data.frame(
      from = c(1L, 2L, 3L, 4L, 5L),
      to = c(2L, 3L, 4L, 5L, 1L),
      time = c(1, 2, 3, 4, 5),
      layer = "calls"
    ),
    data.frame(
      from = c(2L, 3L, 4L),
      to = c(1L, 2L, 3L),
      time = c(1, 2, 3),
      layer = "emails"
    )
  )
  info <- list(
    name = "toy",
    focal = "calls",
    update = c(
      friendship = "increment",
      calls = "increment",
      emails = "increment"
    ),
    directed = c(friendship = TRUE, calls = TRUE, emails = TRUE),
    observation = c(friendship = "panel", calls = "event", emails = "event")
  )
  data <- list(info = info, nodes = nodes, ties = ties)
  calls_spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + offset(tie(friendship), coef = -0.5),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  emails_spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    layer = "emails",
    model = "DyNAM",
    data = data
  )
  make_joint_specification(calls_spec, emails_spec, data = data)
}

# There is no live joint-estimate path on this branch (estimate_dynes() lands in
# abmcem), so a faithful flavored_result.goldfish is fabricated from the spec's
# own coef_layout(): each fid carries the spec's coefficient names, its fixed
# mask, the fixed value at fixed slots, and `estimate` at every free slot -- the
# exact skeleton a real fit over this spec would surface. This is the round-trip
# target `set_init_param(spec, result)` must invert.
fabricate_joint_result <- function(
  spec,
  estimate = seq(0.11, by = 0.11, length.out = 6L)
) {
  layout <- coef_layout(spec)
  free_seen <- 0L
  results <- list()
  for (fid in unique(layout$fid)) {
    rows <- layout[layout$fid == fid, ]
    values <- rows$value
    free <- !rows$fixed
    n_free <- sum(free)
    values[free] <- estimate[free_seen + seq_len(n_free)]
    free_seen <- free_seen + n_free
    description <- data.frame(
      .coef_name = rows$name,
      fixed = rows$fixed,
      row.names = rows$name,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    results[[as.character(fid)]] <- list(
      parameters = stats::setNames(values, rows$name),
      standard_errors = rep(0.05, n_free),
      names = description
    )
  }
  structure(
    list(
      results = results,
      process_map = spec$process_map,
      model = "DyNAM"
    ),
    class = "flavored_result.goldfish"
  )
}

test_that("a fitted result round-trips into a complete parameter object", {
  spec <- result_join()
  fit <- fabricate_joint_result(spec)

  p <- set_init_param(spec, fit)

  # Every free slot pinned from the fit -> a complete object simulate() drives.
  expect_true(p$complete)
  expect_false(anyNA(p$free))
  # The free values land at the same slots, in the canonical order.
  expect_equal(unname(p$free), seq(0.11, by = 0.11, length.out = 6L))
  # The offset slot keeps the specification's fixed value, not a re-supplied one,
  # and does not warn on the way through.
  expect_identical(
    unname(p$full[["calls › choice"]]["tie(friendship)"]),
    -0.5
  )
})

test_that("the round-trip warns nothing about the fixed slot", {
  spec <- result_join()
  fit <- fabricate_joint_result(spec)
  # The reconstruction leaves fixed slots NA so the spec resolves them silently.
  expect_no_warning(set_init_param(spec, fit))
})

test_that("a result fit against another specification aborts", {
  fit <- fabricate_joint_result(result_join())
  local_cli_context <- function(env = parent.frame()) {
    withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
  }
  local_cli_context()

  # A spec whose calls rate is intercept-only has a different coefficient count,
  # so its layout cannot match the result's.
  nodes <- data.frame(label = paste0("N", 1:6), mode = "p")
  ties <- rbind(
    data.frame(
      from = c(1L, 2L, 3L, 4L, 5L),
      to = c(2L, 3L, 4L, 5L, 1L),
      time = c(1, 2, 3, 4, 5),
      layer = "calls"
    ),
    data.frame(
      from = c(2L, 3L, 4L),
      to = c(1L, 2L, 3L),
      time = c(1, 2, 3),
      layer = "emails"
    )
  )
  info <- list(
    name = "toy",
    focal = "calls",
    update = c(calls = "increment", emails = "increment"),
    directed = c(calls = TRUE, emails = TRUE),
    observation = c(calls = "event", emails = "event")
  )
  data <- list(info = info, nodes = nodes, ties = ties)
  other <- make_joint_specification(
    make_specification(
      rate = ~1,
      choice = ~inertia,
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~ 1 + indeg,
      choice = ~inertia,
      layer = "emails",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
  expect_snapshot(set_init_param(other, fit), error = TRUE)
})
