# coef_layout() tabulates the coefficient-space layout of a joint parameter
# surface: one row per coefficient slot (an intercept when the sub-model carries
# one, the effects, the interaction columns), keyed by the rendered process
# label. The joint_specification method is completion-aware -- a raw (authored)
# spec spans authored fids only, a completed spec also renders the autocompleted
# fids' rows as fixed. The goldfishParams method spans the authored fids
# and carries the supplied values; the fitted-result method carries theta-hat
# and SE.

# A two-process DyNAM join (calls, emails), each with a rate (Intercept + indeg)
# and a choice; the calls choice carries an inline-coef offset, so its
# `tie(friendship)` slot is the one fixed coefficient. Non-flavored, so the
# rendered labels elide the flavor segment.
authored_join <- function() {
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

# A join whose `calls` process is rate-only, so completing the spec synthesizes
# a zero-free-parameter uniform choice for it -- the autocompleted fid the
# completion-aware layout must surface as fixed.
rate_only_join <- function() {
  join <- authored_join()
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

test_that("the raw-spec layout has one row per authored coefficient slot", {
  layout <- coef_layout(authored_join())

  # One row per coefficient (n_params), not per effect: 2 + 2 + 2 + 1 rows.
  expect_identical(nrow(layout), 7L)
  expect_identical(
    names(layout),
    c(
      "fid",
      "process",
      "sub_model",
      "flavor",
      "name",
      "fixed",
      "value",
      "index"
    )
  )
  expect_identical(
    unique(layout$process),
    c("calls › rate", "calls › choice", "emails › rate", "emails › choice")
  )

  # The offset is the one fixed slot, carrying the specification's value; every
  # free slot gets a running index into the flat theta and no value.
  offset_row <- layout[layout$name == "tie(friendship)", ]
  expect_true(offset_row$fixed)
  expect_identical(offset_row$value, -0.5)
  expect_identical(offset_row$index, NA_integer_)
  expect_identical(layout$index[!layout$fixed], 1:6)
  expect_true(all(is.na(layout$value[!layout$fixed])))
})

test_that("the raw-spec layout omits autocompleted rows", {
  layout <- coef_layout(rate_only_join())

  # calls is rate-only; nothing is completed, so no calls-choice row appears.
  expect_false(any(layout$process == "calls › choice"))
  expect_setequal(
    unique(layout$process),
    c("calls › rate", "emails › rate", "emails › choice")
  )
})

test_that("the completed-spec layout renders autocompleted fids as fixed", {
  cc <- suppressWarnings(complete_generative_spec(
    rate_only_join(),
    consumer = "estimate_dynes"
  ))

  # Still a goldfishJointSpec -- the same method reads its populated
  # `completed` column rather than dispatching on a new class.
  expect_s3_class(cc, "goldfishJointSpec")
  layout <- coef_layout(cc)

  # The autocompleted uniform choice now appears, as a single fixed placeholder
  # row with the "1" name and no free index -- the pre-fit walked layout.
  choice <- layout[layout$process == "calls › choice", ]
  expect_identical(nrow(choice), 1L)
  expect_identical(choice$name, "1")
  expect_true(choice$fixed)
  expect_identical(choice$index, NA_integer_)

  # Its fid is exactly the one the process_map marks completed.
  completed_fid <- cc$process_map$fid[cc$process_map$completed]
  expect_identical(unique(choice$fid), completed_fid)
})

test_that("a completed timed rate carries its frozen pinned value", {
  # A panel friendship layer makes the join timed, so friendship's missing rate
  # (it is choice-only) completes to a pinned intercept-only rate.
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
    )
  )
  base_info <- list(
    name = "toy",
    focal = "calls",
    update = c(friendship = "increment", calls = "increment"),
    directed = c(friendship = TRUE, calls = TRUE)
  )
  # The join is timed (friendship observed as panel), but the friendship spec is
  # authored against event-observed friendship so it clears the panel-focal
  # guard -- the same split the completion fixtures use.
  event_data <- list(
    info = c(
      base_info,
      list(observation = c(friendship = "event", calls = "event"))
    ),
    nodes = nodes,
    ties = ties
  )
  panel_data <- list(
    info = c(
      base_info,
      list(observation = c(friendship = "panel", calls = "event"))
    ),
    nodes = nodes,
    ties = ties
  )
  friendship <- make_specification(
    choice = ~inertia,
    layer = "friendship",
    model = "DyNAM",
    data = event_data
  )
  calls <- make_specification(
    rate = ~ 1 + tie(friendship),
    choice = ~inertia,
    layer = "calls",
    model = "DyNAM",
    data = panel_data
  )
  js <- make_joint_specification(friendship, calls, data = panel_data)
  cc <- suppressWarnings(complete_generative_spec(
    js,
    consumer = "estimate_dynes",
    wave_times = c(0, 2, 5)
  ))

  layout <- coef_layout(cc)
  rate_fid <- cc$process_map$fid[
    cc$process_map$completed & cc$process_map$family == "rate"
  ]
  row <- layout[layout$fid == rate_fid, ]
  expect_identical(nrow(row), 1L)
  expect_true(row$fixed)
  # The frozen value is the primitive's first-period pinned log-hazard.
  pinned <- cc$completed_rates[[as.character(rate_fid)]]$intercept[[1L]]
  expect_identical(row$value, pinned)
})

test_that("the goldfishParams layout carries supplied values (authored)", {
  p <- set_parameters(authored_join(), `calls › rate` = c(0.1, 0.2))
  layout <- coef_layout(p)

  # Authored fids only -- no autocompleted rows on a parameter object.
  expect_identical(nrow(layout), 7L)
  rate <- layout[layout$process == "calls › rate", ]
  expect_identical(rate$value, c(0.1, 0.2))
  expect_true(all(!rate$fixed))

  # A free slot the user left unpinned stays NA; the offset keeps its value.
  free_unpinned <- layout[layout$name == "inertia(calls)", ]
  expect_identical(free_unpinned$value, NA_real_)
  offset <- layout[layout$name == "tie(friendship)", ]
  expect_true(offset$fixed)
  expect_identical(offset$value, -0.5)
})

test_that("the fitted-result layout groups estimates and standard errors", {
  skip_if_not(exists("flavored_container_fit"))
  fit <- flavored_container_fit()
  layout <- coef_layout(fit)

  expect_true("se" %in% names(layout))
  # Rows run in canonical process_map fid order, grouping the flat coefficient
  # vector back into per-process blocks.
  expect_identical(unique(layout$fid), fit$process_map$fid)
  # Each block's values match that per-process fit's own parameters.
  for (fid in fit$process_map$fid) {
    block <- layout[layout$fid == fid, ]
    expect_equal(
      block$value,
      unname(fit$results[[as.character(fid)]]$parameters)
    )
  }
  expect_true(all(is.na(layout$se[layout$fixed])))
})

test_that("the empty layout guides authoring of set_parameters()", {
  spec <- authored_join()
  layout <- coef_layout(spec)

  # Author set_parameters() purely from what the layout reveals -- its own
  # fid grouping, coefficient order, and fixed flag -- rather than from prior
  # knowledge of the specification. A fixed slot is authored as NA so the
  # specification's own value (already visible in the layout) prevails.
  by_fid <- split(layout, layout$fid)
  args <- lapply(by_fid, function(rows) {
    values <- rep(NA_real_, nrow(rows))
    values[!rows$fixed] <- seq_len(sum(!rows$fixed)) / 10
    values
  })
  labels <- vapply(by_fid, function(rows) rows$process[[1L]], "")
  p <- do.call(
    set_parameters,
    c(list(spec), stats::setNames(args, labels))
  )

  # The authored free values land at the same fid/slot the layout described;
  # the fixed slots keep the value the layout already showed.
  for (fid in names(by_fid)) {
    rows <- by_fid[[fid]]
    expect_identical(
      unname(p$full[[rows$process[[1L]]]]),
      ifelse(rows$fixed, rows$value, args[[fid]])
    )
  }
})

test_that("a stub summary() groups the flat coefficients via coef_layout()", {
  skip_if_not(exists("flavored_container_fit"))
  fit <- flavored_container_fit()

  # summary.goldfishFlavFit() itself is out of scope for this change
  # (design D6); this stub stands in for it, grouping the result's coef_layout()
  # rows into the per-process blocks a real summary() would render.
  summary_stub <- function(result) {
    layout <- coef_layout(result)
    split(layout, layout$process)
  }

  blocks <- summary_stub(fit)

  expect_setequal(names(blocks), coef_layout(fit)$process)
  for (label in names(blocks)) {
    fid <- blocks[[label]]$fid[[1L]]
    expect_equal(
      blocks[[label]]$value,
      unname(fit$results[[as.character(fid)]]$parameters)
    )
  }
})
