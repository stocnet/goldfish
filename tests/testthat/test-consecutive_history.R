# `trans(history = "consecutive")` counts a two-path only when its two ties
# arrive at adjacent events, so it reads the per-event order counter the
# preprocessing walk maintains. That counter is a difference of two counters
# which advance together only for a routed dependent event, so a burn-in event
# -- replayed before `start_time`, never routed -- used to leave a gap that made
# every pair of events look non-adjacent and zeroed the statistic across the
# fold.
#
# The walk below sends each event from the receiver of the event before it, so
# every event after the first closes exactly one two-path. No tie repeats, so
# each update takes the tie-creation branch the consecutive rule reads.

consecutive_walk_data <- function() {
  sender <- c(1, 2, 3, 4, 5, 1, 4, 2, 5, 3, 1, 5)
  receiver <- c(2, 3, 4, 5, 1, 4, 2, 5, 3, 1, 5, 4)
  as_goldfish(list(
    info = list(
      name = "walk",
      focal = "closure",
      update = c(closure = "increment"),
      directed = c(closure = TRUE),
      observation = c(closure = "event")
    ),
    nodes = data.frame(label = sprintf("Actor %d", seq_len(5))),
    ties = data.frame(
      from = sender,
      to = receiver,
      time = seq_along(sender),
      layer = "closure"
    )
  ))
}

# The statistic the likelihood reads, rebuilt per dependent event from the
# initial matrix and the changes the walk recorded. No accessor returns it, and
# the recorded changes alone would not show the burn-in, which is folded into
# `initial_stats`.
running_statistic <- function(prep, effect_pos) {
  changes <- ReducePreprocess(prep, "withTime")[[effect_pos]]
  statistic <- prep$initial_stats[,, effect_pos]
  out <- vector("list", length(prep$event_time))
  for (i in seq_along(prep$event_time)) {
    rows <- if (is.null(changes)) {
      NULL
    } else {
      changes[changes[, "time"] == prep$event_time[i], , drop = FALSE]
    }
    if (!is.null(rows) && nrow(rows) > 0) {
      statistic[cbind(rows[, "node1"], rows[, "node2"])] <- rows[, "replace"]
    }
    out[[i]] <- statistic
  }
  names(out) <- prep$event_time
  out
}

test_that("the consecutive statistic carries across the burn-in", {
  walk <- consecutive_walk_data()
  formula <- closure ~
    inertia(closure) + trans(closure, history = "consecutive")
  trans_pos <- 2L

  full <- compute_statistics(
    formula,
    model = "DyNAM",
    sub_model = "choice",
    data = walk,
    output = "preprocessed"
  )
  windowed <- compute_statistics(
    formula,
    model = "DyNAM",
    sub_model = "choice",
    data = walk,
    output = "preprocessed",
    control_prep = set_preprocessing(start_time = 7)
  )

  expect_equal(windowed$event_time, as.numeric(7:12))

  # The burn-in replayed five closures, so the fold cannot be empty. This is
  # what the counter drift zeroed.
  expect_false(all(windowed$initial_stats[,, trans_pos] == 0))

  full_statistic <- running_statistic(full, trans_pos)
  windowed_statistic <- running_statistic(windowed, trans_pos)

  # A dependent event records the updates made since the event before it, so
  # the full fit's statistic at the first in-window event is exactly what the
  # burn-in has to fold into `initial_stats`.
  expect_equal(
    windowed$initial_stats[,, trans_pos],
    full_statistic[["7"]]
  )

  # The two-way equality. Both fits have seen the same event history at every
  # in-window event -- modeled in one, replayed in the other -- so the
  # order-dependent statistic must agree at each of them.
  for (event in as.character(7:12)) {
    expect_equal(
      windowed_statistic[[event]],
      full_statistic[[event]],
      info = paste("event at time", event)
    )
  }
})

test_that("consecutive closure is estimated on an unwindowed fit", {
  # The control for the ordinary case: `history = "consecutive"` is never
  # estimated anywhere else in the suite -- it appears only inside a
  # name-formatting call -- so the counter change needs pinning where there is
  # no burn-in as well as where there is one.
  walk <- consecutive_walk_data()
  formula <- closure ~ trans(closure, history = "consecutive")

  prep <- compute_statistics(
    formula,
    model = "DyNAM",
    sub_model = "choice",
    data = walk,
    output = "preprocessed"
  )
  statistic <- running_statistic(prep, 1L)

  # One closure per event, recorded at the event after the one that closed it,
  # so the total grows by one from the third event on and never by more.
  expect_equal(
    vapply(statistic, sum, numeric(1)),
    c(0, 0, seq_len(10)),
    ignore_attr = TRUE
  )
  expect_equal(
    statistic[["12"]],
    matrix(
      # fmt: skip
      c(
        0, 1, 1, 0, 0,
        0, 0, 1, 1, 0,
        0, 0, 0, 0, 2,
        1, 0, 0, 0, 1,
        1, 0, 0, 1, 0
      ),
      nrow = 5,
      ncol = 5,
      byrow = TRUE
    )
  )

  fit <- estimate_dynam(formula, sub_model = "choice", data = walk)
  expect_true(fit$convergence$is_converged)
  expect_equal(unname(coef(fit)), -1.0986123, tolerance = 1e-6)
})

test_that("consecutive closure differs from pooled closure", {
  # Guards the walk itself: if every pair of events closed a two-path under
  # either rule, the tests above would hold for a statistic that had stopped
  # reading event adjacency at all.
  walk <- consecutive_walk_data()

  consecutive <- compute_statistics(
    closure ~ trans(closure, history = "consecutive"),
    model = "DyNAM",
    sub_model = "choice",
    data = walk,
    output = "preprocessed"
  )
  pooled <- compute_statistics(
    closure ~ trans(closure, history = "pooled"),
    model = "DyNAM",
    sub_model = "choice",
    data = walk,
    output = "preprocessed"
  )

  consecutive_final <- running_statistic(consecutive, 1L)[["12"]]
  pooled_final <- running_statistic(pooled, 1L)[["12"]]
  expect_false(isTRUE(all.equal(consecutive_final, pooled_final)))
  expect_lt(sum(consecutive_final), sum(pooled_final))
})
