# The batch-versus-replay oracle.
#
# `materialize_process_state()` rebuilds the dense process state at one event
# index from a stored `goldfishStat`, by replaying the preprocessed init and
# update streams up to that index -- the same buffer assembly `estimate_c_int()`
# performs once for a whole fit. It is the reference the live walk handle is
# compared against: `walk_evaluate()` at event k must equal
# `evaluate_process_state()` of this materialization at event k, which is what
# makes the stepping substrate trustworthy.
#
# It lives here, with the tests, rather than in R/. It was written as a
# building block for the generative consumers; `simulate()` has since landed on
# the live handle and does not call it, and the augmenters re-preprocess each
# pooled sequence rather than materialize one. Nothing in the package calls it,
# so keeping it in R/ would have been carrying a test fixture as package code.

materialize_process_state <- function(
  statsList,
  model_type = c(
    "DyNAM-M",
    "DyNAM-M-Rate",
    "DyNAM-M-Rate-ordered",
    "REM",
    "REM-ordered",
    "DyNAM-MM"
  ),
  event_index,
  has_intercept = FALSE,
  allow_reflexive = FALSE,
  is_two_mode = FALSE
) {
  model_type <- match.arg(model_type)
  is_rate <- model_type %in% c("DyNAM-M-Rate", "DyNAM-M-Rate-ordered")

  sl <- prepare_statslist(
    statsList = statsList,
    addInterceptEffect = has_intercept,
    is_sender = is_rate
  )

  if (is_rate) {
    n_parameters <- ncol(sl$initial_stats)
    n_actors1 <- nrow(sl$initial_stats)
    n_actors2 <- 1L
    twomode_or_reflexive <- TRUE
  } else {
    n_parameters <- dim(sl$initial_stats)[3]
    n_actors1 <- dim(sl$initial_stats)[1]
    n_actors2 <- dim(sl$initial_stats)[2]
    twomode_or_reflexive <- allow_reflexive || is_two_mode
  }

  # Flatten the initial statistics to the sender-major layout the compiled
  # estimators use (dyad (i, j) at row (i - 1) * n2 + j); rate stats stay n1xp.
  if (is_rate) {
    stat_mat <- sl$initial_stats
  } else {
    stat_mat <- matrix(0, n_actors1 * n_actors2, n_parameters)
    for (i in seq_len(n_parameters)) {
      stat_mat[, i] <- t(sl$initial_stats[,, i])
    }
  }

  stat_mat_update <- statsList$stat_mat_update
  stat_mat_update_pointer <- statsList$stat_mat_pointer
  stat_mat_broadcast <- statsList$stat_mat_broadcast
  stat_mat_broadcast_pointer <- statsList$stat_mat_broadcast_pointer
  if (is.null(stat_mat_broadcast)) {
    stat_mat_broadcast <- matrix(0, 4L, 0L)
    stat_mat_broadcast_pointer <- numeric(length(stat_mat_update_pointer))
  }
  # The intercept sits at effect position 1, so every stored effect index shifts
  # by one (mirrors estimate_c_int()).
  if (has_intercept) {
    stat_mat_update[3, ] <- stat_mat_update[3, ] + 1
    if (ncol(stat_mat_broadcast) > 0L) {
      stat_mat_broadcast[3, ] <- stat_mat_broadcast[3, ] + 1
    }
  }

  active_sender <- statsList$active_sender_init
  active_sender_update <- statsList$active_sender_update
  active_sender_update_pointer <- statsList$active_sender_update_pointer
  has_cc1 <- !is.null(active_sender_update) && length(active_sender_update) > 0

  active_dyad <- statsList$active_dyad_init
  active_dyad_update <- statsList$active_dyad_update
  active_dyad_update_pointer <- statsList$active_dyad_update_pointer
  has_cc2 <- !is.null(active_dyad_update) && length(active_dyad_update) > 0
  active_dyad_encoding <- if (is.null(statsList$active_dyad_encoding)) {
    "alter"
  } else {
    statsList$active_dyad_encoding
  }
  is_point <- identical(active_dyad_encoding, "point")

  # Replay every event's update / broadcast / presence slice up to and including
  # `event_index`, leaving the state exactly as the estimator holds it just
  # before computing that event's contribution.
  update_id <- 0L
  bc_id <- 0L
  p1_id <- 0L
  p2_id <- 0L
  for (e in seq_len(event_index)) {
    ptr <- stat_mat_update_pointer[e]
    cells <- .gather_stat_cells(stat_mat_update, update_id, ptr, n_actors2)
    if (!is.null(cells)) {
      stat_mat[cells$idx] <- cells$value
    }
    update_id <- ptr
    bc_ptr <- stat_mat_broadcast_pointer[e]
    blocks <- .gather_broadcast_blocks(
      stat_mat_broadcast,
      bc_id,
      bc_ptr,
      n_actors1,
      n_actors2,
      twomode_or_reflexive
    )
    for (block in blocks) {
      stat_mat[block$rows, block$col] <- block$value
    }
    bc_id <- bc_ptr
    if (has_cc1) {
      ptr1 <- active_sender_update_pointer[e]
      active_sender <- .gather_apply_presence(
        active_sender,
        active_sender_update,
        p1_id,
        ptr1
      )
      p1_id <- ptr1
    }
    if (has_cc2) {
      ptr2 <- active_dyad_update_pointer[e]
      active_dyad <- if (is_point) {
        .gather_apply_presence_point(
          active_dyad,
          active_dyad_update,
          p2_id,
          ptr2
        )
      } else {
        .gather_apply_presence(active_dyad, active_dyad_update, p2_id, ptr2)
      }
      p2_id <- ptr2
    }
  }

  list(
    model_type = model_type,
    stat_mat = stat_mat,
    active_sender = active_sender,
    active_dyad = active_dyad,
    active_dyad_encoding = active_dyad_encoding,
    n_actors1 = n_actors1,
    n_actors2 = n_actors2,
    n_parameters = n_parameters,
    twomode_or_reflexive = twomode_or_reflexive,
    is_rate = is_rate,
    event_sender = statsList$event_sender[[event_index]],
    event_receiver = statsList$event_receiver[[event_index]],
    is_dependent = statsList$is_dependent[[event_index]] == 1L,
    timespan = if (is.null(statsList$intervals)) {
      NA_real_
    } else {
      statsList$intervals[[event_index]]
    }
  )
}
