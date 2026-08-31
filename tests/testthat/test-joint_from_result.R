# The from-result form `set_init_param(spec, result)` -- the fit -> re-simulate
# round-trip. It reconstructs the per-fid vectors from the fitted result's own
# coef_layout() (which keeps the fid grouping the flat coef() vector discards),
# after asserting the result was fit against that same spec. A flat, free-only
# coef() vector is not accepted directly (its per-parameter names collide across
# fids).

# A two-process DyNAM join (calls, emails); the calls choice carries an
# inline-coef offset, so `tie/friendship [Fx]` is the one fixed coefficient.
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

# A flavored join: a mutually-exclusive `calls` layer (creation/dissolution)
# joined with a plain `emails` layer. Every choice sub-model carries `inertia`
# (the same coefficient name on all three fids -- the proposal's motivating
# collision), and each calls flavor carries its own inline-coef offset on the
# SAME term at two distinct values (`-0.3` creation, `0.4` dissolution). So the
# round-trip must land each flavor's free `inertia` at its own fid despite the
# shared name, and keep each flavor's fixed offset at the specification's value.
flavored_result_join <- function() {
  nodes <- data.frame(
    label = paste0("N", 1:6),
    mode = "p",
    stringsAsFactors = FALSE
  )
  calls <- data.frame(
    from = c(1L, 3L, 2L, 1L, 4L, 3L),
    to = c(2L, 4L, 3L, 2L, 5L, 4L),
    time = c(NA, NA, 1, 2, 3, 4),
    layer = "calls",
    weight = c(1, 1, 1, -1, 1, -1),
    stringsAsFactors = FALSE
  )
  friendship <- data.frame(
    from = c(1L, 2L, 3L),
    to = c(2L, 3L, 4L),
    time = c(1, 2, 3),
    layer = "friendship",
    weight = 1,
    stringsAsFactors = FALSE
  )
  emails <- data.frame(
    from = c(2L, 3L, 4L),
    to = c(1L, 2L, 3L),
    time = c(1, 2, 3),
    layer = "emails",
    weight = 1,
    stringsAsFactors = FALSE
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
  data <- add_flavor(
    list(info = info, nodes = nodes, ties = rbind(calls, friendship, emails)),
    layer = "calls",
    values_equivalence = c(creation = 1, dissolution = -1),
    flavor_style = "mutually_exclusive"
  )
  calls_spec <- make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg),
    choice = list(
      creation ~ inertia + offset(tie(friendship), coef = -0.3),
      dissolution ~ inertia + offset(tie(friendship), coef = 0.4)
    ),
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
  bundles <- joint_fid_bundles(spec)
  free_seen <- 0L
  results <- list()
  for (fid in unique(layout$fid)) {
    rows <- layout[layout$fid == fid, ]
    values <- rows$value
    free <- !rows$fixed
    n_free <- sum(free)
    values[free] <- estimate[free_seen + seq_len(n_free)]
    free_seen <- free_seen + n_free
    # The real effect-description matrix a fit carries -- object/flag columns and
    # all -- so `compact_term_strings(names, "console")` re-renders it to the same
    # console names the spec side produces. A degenerate frame with the rendered
    # name as its own rowname would double-append the `[Fx]` token at fixed slots.
    parsed <- bundles[[as.character(fid)]]$bundle$parsed
    description <- fid_effect_description(parsed, rows$fixed)
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
    unname(p$full[["calls › choice"]]["tie/friendship [Fx]"]),
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

# ---- Flavored round-trip: same-name free slots and per-flavor fixed values ---

test_that("a flavored result round-trips per fid, keeping each flavor's fixed value", {
  spec <- flavored_result_join()
  # Nine free slots across the six fids (rate: Intercept + indeg; choice: the
  # free inertia), each fabricated at a distinct estimate.
  fit <- fabricate_joint_result(
    spec,
    estimate = seq(0.11, by = 0.11, length.out = 9L)
  )

  p <- expect_no_warning(set_init_param(spec, fit))

  # Every free slot pinned from the fit, in the canonical order.
  expect_true(p$complete)
  expect_equal(unname(p$free), seq(0.11, by = 0.11, length.out = 9L))

  # The free `inertia/calls` slots land at their own flavor's fid despite the
  # shared coefficient name -- each matches that fid's fabricated estimate, and
  # the two differ.
  lay <- coef_layout(fit)
  fid_of <- function(label) unique(lay$fid[lay$process == label])
  for (label in c(
    "calls › creation › choice",
    "calls › dissolution › choice"
  )) {
    fid <- fid_of(label)
    expect_identical(
      unname(p$full[[label]]["inertia/calls"]),
      unname(fit$results[[as.character(fid)]]$parameters["inertia/calls"])
    )
  }
  expect_false(identical(
    unname(p$full[["calls › creation › choice"]]["inertia/calls"]),
    unname(p$full[["calls › dissolution › choice"]]["inertia/calls"])
  ))

  # Each flavor's fixed slot keeps the specification's own offset value, not a
  # value read off the result -- and the two stay distinct.
  expect_identical(
    unname(p$full[["calls › creation › choice"]]["tie/friendship [Fx]"]),
    -0.3
  )
  expect_identical(
    unname(p$full[["calls › dissolution › choice"]]["tie/friendship [Fx]"]),
    0.4
  )
})
