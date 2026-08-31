# set_init_param() builds a self-validating goldfishParams over a joint
# specification: it resolves each `...` key by membership against the rendered
# process label (never by parsing the label back into components), validates
# each per-fid vector against the process's coefficient layout, and classifies
# every slot free/fixed from the offset mask projected into coefficient space --
# an `offset()` term keeps the specification's value, a value supplied there is
# warned about and dropped.

# cli error/warning snapshots are pinned to a reproducible width/no-color
# context so the rendered bullets stay stable across machines.
local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# A two-process DyNAM join (calls, emails), each with a rate (intercept + indeg)
# and a choice sub-model. The calls choice carries an inline-coef offset, so its
# `tie/friendship [Fx]` slot is the fixed coefficient the classifier must resolve
# from the specification; every other slot is free. Non-flavored, so the
# rendered labels elide the flavor segment (`calls > rate`, not
# `calls > NA > rate`).
parameters_join <- function() {
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
# built with add_flavor(), joined with a plain `emails` layer. Every choice
# sub-model carries `inertia` -- the two `calls` flavors both surface
# `inertia/calls`, reproducing the proposal's motivating collision (one name,
# two fids of the same layer) that the plain fixtures above cannot. Each
# flavor's choice also carries its own inline-coef offset on the SAME term at
# two distinct values (`-0.3` for creation, `0.4` for dissolution), while emails
# offset-free -- the minimum that catches a fixed value resolved from the wrong
# fid. Flavored, so the calls labels carry a flavor segment
# (`calls > creation > rate`) while emails elides it (`emails > rate`).
flavored_parameters_join <- function() {
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

# The flavored join with an argument-bearing effect: each `calls` flavor's
# choice carries `inertia(calls, window = "1 hour")` rather than a bare
# `inertia`. This is the minimum that makes the deparse and console naming
# schemes differ -- the deparse form is `inertia(calls, window = "1 hour")`, the
# console form `inertia/calls [1h]` -- so a test can tell the joint vocabulary
# (console) apart from the raw formula term. Every other slot matches
# `flavored_parameters_join()`.
windowed_flavored_join <- function() {
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
      creation ~ inertia(calls, window = "1 hour") +
        offset(tie(friendship), coef = -0.3),
      dissolution ~ inertia(calls, window = "1 hour") +
        offset(tie(friendship), coef = 0.4)
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

test_that("set_init_param() returns a goldfishParams, not the retired parameters.goldfish", {
  p <- set_init_param(parameters_join())
  expect_s3_class(p, "goldfishParams")
  expect_false(inherits(p, "parameters.goldfish"))
})

test_that("set_init_param is exported and the retired set_parameters is not", {
  expect_true(exists("set_init_param", where = asNamespace("goldfish")))
  expect_true("set_init_param" %in% getNamespaceExports("goldfish"))
  expect_false("set_parameters" %in% getNamespaceExports("goldfish"))
})

test_that("labels elide the flavor segment for a non-flavored process", {
  p <- set_init_param(parameters_join())
  expect_identical(
    names(p$full),
    c("calls › rate", "calls › choice", "emails › rate", "emails › choice")
  )
})

test_that("an omitted process key leaves every non-fixed slot free", {
  p <- set_init_param(parameters_join())

  # Nothing supplied: every free slot is NA and the object is incomplete, but
  # the offset slot already carries the specification's fixed value.
  expect_false(p$complete)
  expect_true(anyNA(p$free))
  expect_identical(
    names(p$free),
    c(
      "calls › rate: Intercept",
      "calls › rate: indeg/calls",
      "calls › choice: inertia/calls",
      "emails › rate: Intercept",
      "emails › rate: indeg/emails",
      "emails › choice: inertia/emails"
    )
  )

  # The calls-choice offset is the one fixed slot, held at the formula's value.
  choice <- p$fids[["2"]]
  expect_identical(unname(choice$fixed), c(FALSE, TRUE))
  expect_identical(choice$values[choice$fixed], -0.5)
})

test_that("a per-fid vector resolves by rendered-label membership", {
  p <- set_init_param(
    parameters_join(),
    `calls › rate` = c(0.1, 0.2),
    `emails › choice` = c(`inertia/emails` = 0.7)
  )

  expect_identical(
    p$full[["calls › rate"]],
    c(Intercept = 0.1, `indeg/calls` = 0.2)
  )
  expect_identical(unname(p$full[["emails › choice"]]["inertia/emails"]), 0.7)
  # A process whose key is omitted stays all-free.
  expect_true(anyNA(p$full[["emails › rate"]]))
})

test_that("named and positional per-fid vectors agree", {
  join <- parameters_join()
  positional <- set_init_param(join, `calls › rate` = c(0.1, 0.2))
  # Fully named, supplied out of coefficient order -- the names fix the slots.
  named <- set_init_param(
    join,
    `calls › rate` = c(`indeg/calls` = 0.2, Intercept = 0.1)
  )
  expect_identical(
    named$full[["calls › rate"]],
    positional$full[["calls › rate"]]
  )
})

test_that("pinning every free slot marks the object complete", {
  p <- set_init_param(
    parameters_join(),
    `calls › rate` = c(0.1, 0.2),
    `calls › choice` = c(0.3, NA), # NA at offset slot: silent, spec prevails
    `emails › rate` = c(0.4, 0.5),
    `emails › choice` = c(0.6)
  )
  expect_true(p$complete)
  expect_false(anyNA(p$free))
})

test_that("a value supplied for a fixed coefficient warns and is ignored", {
  local_cli_context()
  join <- parameters_join()
  expect_snapshot(
    p <- set_init_param(
      join,
      `calls › choice` = c(`inertia/calls` = 0.3, `tie/friendship [Fx]` = 9)
    )
  )
  # Offset prevails: the supplied 9 is dropped, the spec's -0.5 stands.
  expect_identical(
    unname(p$full[["calls › choice"]]["tie/friendship [Fx]"]),
    -0.5
  )
})

test_that("a key matching no process aborts naming the valid labels", {
  local_cli_context()
  join <- parameters_join()
  expect_snapshot(
    set_init_param(join, `calls:rate` = c(0.1, 0.2)),
    error = TRUE
  )
})

test_that("the same process given twice aborts", {
  local_cli_context()
  join <- parameters_join()
  expect_snapshot(
    set_init_param(
      join,
      `calls › rate` = c(0.1, 0.2),
      `calls › rate` = c(0.3, 0.4)
    ),
    error = TRUE
  )
})

test_that("a wrong-length per-fid vector aborts naming the fid", {
  local_cli_context()
  join <- parameters_join()
  expect_snapshot(
    set_init_param(join, `calls › rate` = c(0.1, 0.2, 0.3)),
    error = TRUE
  )
})

test_that("a partly named per-fid vector is rejected", {
  local_cli_context()
  join <- parameters_join()
  expect_snapshot(
    set_init_param(join, `calls › rate` = c(Intercept = 0.1, 0.2)),
    error = TRUE
  )
})

test_that("a per-fid vector naming an unknown coefficient aborts", {
  local_cli_context()
  join <- parameters_join()
  expect_snapshot(
    set_init_param(join, `calls › rate` = c(Intercept = 0.1, wrong = 0.2)),
    error = TRUE
  )
})

# ---- Flavored join: label grammar and same-name resolution ------------------

test_that("flavor-inclusion labels sit alongside an elided one on one object", {
  p <- set_init_param(flavored_parameters_join())
  # The calls flavors carry a flavor segment; emails elides it -- all on one
  # object, the mirror of the non-flavored elision test above.
  expect_identical(
    names(p$full),
    c(
      "calls › creation › rate",
      "calls › creation › choice",
      "calls › dissolution › rate",
      "calls › dissolution › choice",
      "emails › rate",
      "emails › choice"
    )
  )

  # Each is a valid key: a flavor-inclusion label and an elided label both
  # resolve on the same call.
  p2 <- set_init_param(
    flavored_parameters_join(),
    `calls › creation › rate` = c(0.1, 0.2),
    `emails › rate` = c(0.3, 0.4)
  )
  expect_identical(
    p2$full[["calls › creation › rate"]],
    c(Intercept = 0.1, `indeg/calls` = 0.2)
  )
  expect_identical(
    p2$full[["emails › rate"]],
    c(Intercept = 0.3, `indeg/emails` = 0.4)
  )
})

test_that("same-name free slots resolve per flavor without leaking", {
  # `inertia/calls` is the same coefficient name on both calls flavors -- the
  # proposal's motivating collision. Each flavor's key pins its own slot.
  p <- set_init_param(
    flavored_parameters_join(),
    `calls › creation › choice` = c(
      `inertia/calls` = 0.5,
      `tie/friendship [Fx]` = NA
    ),
    `calls › dissolution › choice` = c(
      `inertia/calls` = 0.7,
      `tie/friendship [Fx]` = NA
    )
  )
  expect_identical(
    unname(p$full[["calls › creation › choice"]]["inertia/calls"]),
    0.5
  )
  expect_identical(
    unname(p$full[["calls › dissolution › choice"]]["inertia/calls"]),
    0.7
  )
  # The sibling name in the plain layer is untouched by either flavor's key.
  expect_true(is.na(p$full[["emails › choice"]]["inertia/emails"]))
})

# ---- Flavored join: per-flavor fixed classification (extends D4) -------------

test_that("each flavor's fixed offset resolves to its own value on one call", {
  p <- set_init_param(flavored_parameters_join())
  # Two offsets on the SAME term (tie(friendship)), one per flavor, at distinct
  # values -- the fixed value must be keyed per fid, never shared across a join.
  expect_identical(
    unname(p$full[["calls › creation › choice"]]["tie/friendship [Fx]"]),
    -0.3
  )
  expect_identical(
    unname(p$full[["calls › dissolution › choice"]]["tie/friendship [Fx]"]),
    0.4
  )
  # And each choice fid's fixed mask marks only the offset slot fixed.
  expect_identical(unname(p$fids[["2"]]$fixed), c(FALSE, TRUE))
  expect_identical(unname(p$fids[["4"]]$fixed), c(FALSE, TRUE))
})

test_that("a value at one flavor's fixed slot warns only that flavor", {
  local_cli_context()
  join <- flavored_parameters_join()
  # A single value supplied at creation's fixed slot: the warning names only
  # creation's slot (never dissolution's), and fires once.
  expect_snapshot(
    p <- set_init_param(
      join,
      `calls › creation › choice` = c(
        `inertia/calls` = 0.5,
        `tie/friendship [Fx]` = 9
      )
    )
  )
  # Offset prevails on creation; dissolution's own offset value is untouched.
  expect_identical(
    unname(p$full[["calls › creation › choice"]]["tie/friendship [Fx]"]),
    -0.3
  )
  expect_identical(
    unname(p$full[["calls › dissolution › choice"]]["tie/friendship [Fx]"]),
    0.4
  )
  expect_identical(unname(p$fids[["4"]]$fixed), c(FALSE, TRUE))
})

test_that("omitting one flavor's key leaves only that flavor's slots free", {
  p <- set_init_param(
    flavored_parameters_join(),
    `calls › dissolution › choice` = c(
      `inertia/calls` = 0.7,
      `tie/friendship [Fx]` = NA
    ),
    `emails › choice` = c(`inertia/emails` = 0.2)
  )
  # creation's choice was omitted: its free slot stays NA, its fixed slot still
  # carries the spec's value.
  expect_true(is.na(p$full[["calls › creation › choice"]]["inertia/calls"]))
  expect_identical(
    unname(p$full[["calls › creation › choice"]]["tie/friendship [Fx]"]),
    -0.3
  )
  # The sibling flavor and the plain layer are unaffected by the omission.
  expect_identical(
    unname(p$full[["calls › dissolution › choice"]]["inertia/calls"]),
    0.7
  )
  expect_identical(
    unname(p$full[["emails › choice"]]["inertia/emails"]),
    0.2
  )
})

test_that("a colon-grammar key aborts against a flavored spec", {
  local_cli_context()
  join <- flavored_parameters_join()
  # `calls:creation:rate` is the retired colon grammar. A naive string-split
  # would resolve it against this flavored spec (creation is a real flavor here),
  # but membership resolution against the rendered ` › ` labels rejects it --
  # proving the abort non-vacuously, where a split would have matched.
  expect_snapshot(
    set_init_param(join, `calls:creation:rate` = c(0.1, 0.2)),
    error = TRUE
  )
})

# ---- Console vocabulary (design D12 correction) ------------------------------

# A joint spec whose calls choice carries an inertia:indeg interaction, so the
# effect-description matrix's interaction row and rate intercept can be checked
# against the pre-switch deparse strings -- the no-regression guard for the two
# slot kinds the D12 correction says the console switch does not touch (inertia
# and indeg surface as operand-only, fixed-at-0 main effects alongside the
# interaction column, per D9).
interaction_join <- function() {
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
    choice = ~ inertia:indeg + offset(tie(friendship), coef = -0.5),
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

# A faithful `flavored_result.goldfish`, mirroring `fabricate_joint_result()` in
# test-joint_from_result.R: each fid's coefficient names come from the spec's
# own effect-description matrix, so the console re-render on the result side
# reproduces the spec side exactly -- what the name-column agreement test below
# needs.
fabricate_windowed_result <- function(
  spec,
  estimate = seq(0.11, by = 0.11, length.out = 9L)
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
    parsed <- bundles[[as.character(fid)]]$bundle$parsed
    description <- fid_effect_description(parsed, rows$fixed)
    results[[as.character(fid)]] <- list(
      parameters = stats::setNames(values, rows$name),
      standard_errors = rep(0.05, n_free),
      names = description
    )
  }
  structure(
    list(results = results, process_map = spec$process_map, model = "DyNAM"),
    class = "flavored_result.goldfish"
  )
}

test_that("the windowed slot's coef_layout name is the console form, not the deparse string", {
  lay <- coef_layout(windowed_flavored_join())
  windowed <- lay$name[lay$process == "calls › creation › choice" & !lay$fixed]
  expect_identical(windowed, "inertia/calls [1h]")
  expect_false(any(grepl('window = "1 hour"', lay$name, fixed = TRUE)))
})

test_that("set_init_param() resolves the console name and rejects the old deparse string", {
  local_cli_context()
  join <- windowed_flavored_join()
  p <- set_init_param(
    join,
    `calls › creation › choice` = c(
      `inertia/calls [1h]` = 0.6,
      `tie/friendship [Fx]` = NA
    )
  )
  expect_identical(
    unname(p$full[["calls › creation › choice"]]["inertia/calls [1h]"]),
    0.6
  )
  # The retired deparse string is not a valid key -- it names no coefficient of
  # the resolved fid, so it aborts as an unknown name, pinning the vocabulary.
  expect_snapshot(
    set_init_param(
      join,
      `calls › creation › choice` = c(
        `inertia(calls, window = "1 hour")` = 0.6,
        `tie/friendship [Fx]` = NA
      )
    ),
    error = TRUE
  )
})

test_that("coef_layout(spec) and coef_layout(result) agree on the windowed slot's name", {
  join <- windowed_flavored_join()
  fit <- fabricate_windowed_result(join)
  expect_identical(coef_layout(join)$name, coef_layout(fit)$name)
})

test_that("Intercept and interaction rows render identically to the pre-switch strings", {
  lay <- coef_layout(windowed_flavored_join())
  # Three rate fids (calls-creation, calls-dissolution, emails), each with an
  # Intercept row unaffected by the console switch.
  expect_identical(
    lay$name[lay$sub_model == "rate" & lay$name == "Intercept"],
    rep("Intercept", 3)
  )

  interaction_lay <- coef_layout(interaction_join())
  expect_identical(
    interaction_lay$name[interaction_lay$process == "calls › rate"][1],
    "Intercept"
  )
  expect_identical(
    interaction_lay$name[
      interaction_lay$process == "calls › choice" & !interaction_lay$fixed
    ],
    "inertia:indeg"
  )
})
