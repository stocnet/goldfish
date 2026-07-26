# Internal process-state evaluators + state materializer.
#
# These are `goldfish:::` building blocks for a future `simulate()` method and
# for DyNES data augmentation; they are deliberately NOT exported and commit to
# no user-facing signature. Given a materialized process state (the dense
# statistics state plus the active/presence sets at one event index) and a
# parameter vector, each evaluator returns the per-event probability or rate the
# estimation path computes for that event, over the FULL candidate space, with
# excluded alternatives as exact zeros. Every return is labelled with the shared
# sanitized-index vocabulary (`index_i` / `index_j`) used by the gather/export
# long formats, so residuals and diagnostics share the same join keys.
#
# The per-event math mirrors the compiled estimators one-for-one (choice /
# rate / rate-ordered / REM / REM-ordered / coordination), and the consistency
# tests assert the reconstructed per-event interval log-likelihood reproduces
# the estimator's `interval_log_lik` at 1e-10 — that gate guards the buffer
# assembly here against drift from `estimate_c_int()`'s sibling assembly.

# Reproduce, for a single event index, the buffer assembly that
# `estimate_c_int()` performs once for a whole fit: prepare the statistics list
# (optional intercept column), null-fill the optional update buffers, derive the
# actor counts / encoding, and materialize the dense state by replaying the
# preprocessed init + update streams up to `event_index` with the shared
# `.gather_apply_*` helpers. No preprocessing output format is changed.
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
    excludeParameters = NULL,
    addInterceptEffect = has_intercept,
    is_sender = is_rate
  )

  if (is_rate) {
    n_parameters <- ncol(sl$initialStats)
    n_actors1 <- nrow(sl$initialStats)
    n_actors2 <- 1L
    twomode_or_reflexive <- TRUE
  } else {
    n_parameters <- dim(sl$initialStats)[3]
    n_actors1 <- dim(sl$initialStats)[1]
    n_actors2 <- dim(sl$initialStats)[2]
    twomode_or_reflexive <- allow_reflexive || is_two_mode
  }

  # Flatten the initial statistics to the sender-major layout the compiled
  # estimators use (dyad (i, j) at row (i - 1) * n2 + j); rate stats stay n1xp.
  if (is_rate) {
    stat_mat <- sl$initialStats
  } else {
    stat_mat <- matrix(0, n_actors1 * n_actors2, n_parameters)
    for (i in seq_len(n_parameters)) {
      stat_mat[, i] <- t(sl$initialStats[,, i])
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
    stat_mat <- .gather_apply_stat(
      stat_mat,
      stat_mat_update,
      update_id,
      ptr,
      n_actors2
    )
    update_id <- ptr
    bc_ptr <- stat_mat_broadcast_pointer[e]
    stat_mat <- .gather_apply_broadcast(
      stat_mat,
      stat_mat_broadcast,
      bc_id,
      bc_ptr,
      n_actors1,
      n_actors2,
      twomode_or_reflexive
    )
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

# Per-sender / per-dyad availability mask over the sender-major dyad grid, as a
# length n1*n2 logical vector (dyad (i, j) at (i - 1) * n2 + j). A dyad is at
# risk iff its sender is active, its receiver is available (read through the
# active-dyad encoding), and — unless reflexive dyads are allowed — i != j.
.pse_allowed_dyads <- function(state) {
  n1 <- state$n_actors1
  n2 <- state$n_actors2
  allowed <- logical(n1 * n2)
  is_point <- identical(state$active_dyad_encoding, "point")
  for (i in seq_len(n1)) {
    if (state$active_sender[i] != 1) {
      next
    }
    avail <- if (is_point) {
      state$active_dyad[i, ] == 1
    } else {
      state$active_dyad == 1
    }
    if (!state$twomode_or_reflexive) {
      avail[i] <- FALSE
    }
    allowed[(i - 1L) * n2 + seq_len(n2)] <- avail
  }
  allowed
}

# Evaluate the per-event probability / rate for a materialized state and
# parameters, dispatching on the state's model type. Returns a list with an
# `index` data frame (`index_i` / `index_j` in the shared vocabulary), the
# per-alternative `value` (probability for the multinomial sub-models, hazard
# for the timed sub-models; exact zero for excluded alternatives), and observed
# event's `interval_logL` reconstructed with the estimator's stable formula.
evaluate_process_state <- function(state, parameters) {
  switch(
    state$model_type,
    "DyNAM-M" = .pse_eval_choice(state, parameters),
    "DyNAM-M-Rate" = .pse_eval_rate(state, parameters),
    "DyNAM-M-Rate-ordered" = .pse_eval_rate_ordered(state, parameters),
    "REM" = .pse_eval_rem(state, parameters),
    "REM-ordered" = .pse_eval_rem_ordered(state, parameters),
    "DyNAM-MM" = .pse_eval_coordination(state, parameters)
  )
}

# DyNAM-choice: P(sender -> j) over the sender's active receivers.
.pse_eval_choice <- function(state, parameters) {
  n2 <- state$n_actors2
  s <- state$event_sender
  rows <- state$stat_mat[(s - 1L) * n2 + seq_len(n2), , drop = FALSE]
  lin_pred <- as.numeric(rows %*% parameters)
  is_point <- identical(state$active_dyad_encoding, "point")
  avail <- if (is_point) state$active_dyad[s, ] == 1 else state$active_dyad == 1
  if (!state$twomode_or_reflexive) {
    avail[s] <- FALSE
  }
  lin_pred[!avail] <- -Inf
  sm <- stable_softmax(lin_pred)
  prob <- sm$probabilities
  prob[!avail] <- 0
  list(
    model_type = state$model_type,
    index = data.frame(index_i = rep(s, n2), index_j = seq_len(n2)),
    value = prob,
    interval_logL = if (state$is_dependent) {
      sm$logProbabilities[state$event_receiver]
    } else {
      NA_real_
    }
  )
}

# DyNAM-rate: the per-actor hazard exp(beta^T s_i) on the absolute (timed)
# scale; excluded senders get exact-zero hazard.
.pse_eval_rate <- function(state, parameters) {
  n1 <- state$n_actors1
  lin_pred <- as.numeric(state$stat_mat %*% parameters)
  active <- state$active_sender == 1
  hazard <- numeric(n1)
  hazard[active] <- exp(lin_pred[active])
  normalizer <- sum(hazard)
  interval_logL <- -state$timespan * normalizer
  if (state$is_dependent) {
    interval_logL <- interval_logL + lin_pred[state$event_sender]
  }
  list(
    model_type = state$model_type,
    index = data.frame(index_i = seq_len(n1), index_j = NA_integer_),
    value = hazard,
    interval_logL = interval_logL
  )
}

# DyNAM-rate-ordered: P(actor i is the next sender), a softmax over active
# senders.
.pse_eval_rate_ordered <- function(state, parameters) {
  n1 <- state$n_actors1
  lin_pred <- as.numeric(state$stat_mat %*% parameters)
  active <- state$active_sender == 1
  lin_pred[!active] <- -Inf
  sm <- stable_softmax(lin_pred)
  prob <- sm$probabilities
  prob[!active] <- 0
  list(
    model_type = state$model_type,
    index = data.frame(index_i = seq_len(n1), index_j = NA_integer_),
    value = prob,
    interval_logL = if (state$is_dependent) {
      sm$logProbabilities[state$event_sender]
    } else {
      NA_real_
    }
  )
}

# Sender-major (index_i, index_j) grid for the dyad-indexed sub-models.
.pse_dyad_index <- function(n1, n2) {
  data.frame(
    index_i = rep(seq_len(n1), each = n2),
    index_j = rep(seq_len(n2), times = n1)
  )
}

# REM: the per-dyad hazard exp(beta^T s_ij) on the absolute (timed) scale,
# masked to the risk set.
.pse_eval_rem <- function(state, parameters) {
  n1 <- state$n_actors1
  n2 <- state$n_actors2
  lin_pred <- as.numeric(state$stat_mat %*% parameters)
  allowed <- .pse_allowed_dyads(state)
  hazard <- numeric(n1 * n2)
  hazard[allowed] <- exp(lin_pred[allowed])
  normalizer <- sum(hazard)
  interval_logL <- -state$timespan * normalizer
  if (state$is_dependent) {
    obs <- (state$event_sender - 1L) * n2 + state$event_receiver
    interval_logL <- interval_logL + lin_pred[obs]
  }
  list(
    model_type = state$model_type,
    index = .pse_dyad_index(n1, n2),
    value = hazard,
    interval_logL = interval_logL
  )
}

# REM-ordered: P(dyad ij is the next event), a softmax over the risk set.
.pse_eval_rem_ordered <- function(state, parameters) {
  n1 <- state$n_actors1
  n2 <- state$n_actors2
  lin_pred <- as.numeric(state$stat_mat %*% parameters)
  allowed <- .pse_allowed_dyads(state)
  lin_pred[!allowed] <- -Inf
  sm <- stable_softmax(lin_pred)
  prob <- sm$probabilities
  prob[!allowed] <- 0
  list(
    model_type = state$model_type,
    index = .pse_dyad_index(n1, n2),
    value = prob,
    interval_logL = if (state$is_dependent) {
      obs <- (state$event_sender - 1L) * n2 + state$event_receiver
      sm$logProbabilities[obs]
    } else {
      NA_real_
    }
  )
}

# DyNAM-coordination: the dyad softmax over unordered pairs {a, b} (a > b), with
# log-weight log p(a -> b) + log p(b -> a) from the two per-sender softmaxes
# (design's dyad-triangle form). Returns one row per unordered dyad with
# index_i = a (the larger sanitized id), index_j = b.
.pse_eval_coordination <- function(state, parameters) {
  n1 <- state$n_actors1
  n2 <- state$n_actors2
  lin_pred <- as.numeric(state$stat_mat %*% parameters)
  allowed <- .pse_allowed_dyads(state)

  # Per-sender log-probabilities log p(i -> j); excluded receivers are -Inf.
  log_p <- matrix(-Inf, n1, n2)
  for (i in seq_len(n1)) {
    idx <- (i - 1L) * n2 + seq_len(n2)
    lp_i <- lin_pred[idx]
    lp_i[!allowed[idx]] <- -Inf
    if (any(is.finite(lp_i))) {
      log_p[i, ] <- stable_softmax(lp_i)$logProbabilities
    }
  }

  # Enumerate unordered pairs (a > b) in the same order the kernel uses.
  pairs <- which(lower.tri(matrix(0, n1, n1)), arr.ind = TRUE)
  a <- pairs[, 1]
  b <- pairs[, 2]
  logw <- log_p[cbind(a, b)] + log_p[cbind(b, a)]
  logw[!is.finite(logw)] <- -Inf
  sm <- stable_softmax(logw)
  prob <- sm$probabilities
  prob[!is.finite(logw)] <- 0

  obs_logL <- NA_real_
  if (state$is_dependent) {
    a_obs <- max(state$event_sender, state$event_receiver)
    b_obs <- min(state$event_sender, state$event_receiver)
    pos <- which(a == a_obs & b == b_obs)
    obs_logL <- sm$logProbabilities[pos]
  }
  list(
    model_type = state$model_type,
    index = data.frame(index_i = a, index_j = b),
    value = prob,
    interval_logL = obs_logL
  )
}
