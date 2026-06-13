#' Preprocessing output writers
#'
#' A writer decouples the recipe event loops (design D22) from the output
#' format they produce (design D15). Every writer exposes three hooks:
#'
#' \describe{
#'   \item{`init(spec, dims)`}{called once before the event loop with the
#'     model spec and a `dims` list describing the problem
#'     (`nEffects`, `n1`, `n2`, `is_sender`, `buf_capacity`, `max_store`,
#'     `has_intercept`, `twomode_or_reflexive`, `initial_stats_fn` — a thunk
#'     returning the recipe's `initialStats` once pre-start updates are
#'     applied).}
#'   \item{`write_event(event_updates, event_info)`}{called once per stored
#'     event. `event_updates` is a 4 x k matrix of flat updates (rows
#'     `node1`, `node2`, `effect`, `replace`; all 0-indexed) accumulated
#'     since the previous stored event; `event_info` is a list with
#'     `is_dependent`, `interval`, `time`, `sender`, `receiver`.}
#'   \item{`finalize(tail)`}{called once after the loop with a `tail` list of
#'     recipe-computed assembly inputs (`initialStats`,
#'     `active_mode1_init` / `active_mode1_changes`,
#'     `active_mode2_init` / `active_mode2_changes`, `startTime`, `endTime`,
#'     `intercept_scalars`). Returns the writer's output.}
#' }
#'
#' Recipe methods emit output exclusively through these hooks; they never
#' branch on the output format. `compute_stats(output = ...)` selects the
#' writer.
#'
#' @section Future extension points (documented, not implemented):
#' The writer contract is the seam for three planned output strategies that
#' are deliberately out of scope for this change (design D15/D17):
#' \describe{
#'   \item{Alternatives-sampling gather writer}{a `writer_gather()` variant
#'     that keeps gather rows for only a sample of the alternatives per
#'     event. It changes the likelihood contribution (sampled denominators),
#'     so the estimation procedure must be adapted (sampling design and
#'     estimator correction) before it can be used; the writer obligation is
#'     to record, per event, which alternatives were sampled alongside the
#'     selected one.}
#'   \item{Parallel chunk preprocessing}{chunk the event sequence by time
#'     points, warm-start each chunk's `init()` from the state at its first
#'     event, run recipe + writer per chunk in parallel, and merge the
#'     per-chunk outputs in a coordinating `finalize()`. The obligation is
#'     that `write_event` is associative across a contiguous chunk boundary
#'     and `finalize` can stitch ordered chunk results.}
#'   \item{Per-event simulation hook}{a hook on the recipe loop (not a
#'     writer, design D17) invoked after the stats update for event i and
#'     before advancing: it sees the visible state and may append to the
#'     event stream, reserved for a future `simulate()` goodness-of-fit
#'     method. Its interface obligation is the position in the loop, the
#'     visible-state snapshot it receives, and the event-stream append
#'     semantics.}
#' }
#'
#' @name preprocess_writers
#' @keywords internal
NULL

#' @describeIn preprocess_writers default flat-buffer writer producing the
#'   `preprocessed.goldfish` object consumed by both estimation engines.
#' @noRd
writer_default <- function() {
  buf_capacity <- NULL
  stat_mat_buf <- NULL
  buf_n <- 0L
  stat_mat_pointer <- NULL
  intervals <- NULL
  is_dependent <- NULL
  event_time <- NULL
  event_sender <- NULL
  event_receiver <- NULL
  n_stored <- 0L
  initial_stats_fn <- NULL

  structure(
    list(
      output = "default",
      init = function(spec, dims) {
        buf_capacity <<- dims$buf_capacity
        stat_mat_buf <<- matrix(0, 4L, buf_capacity)
        buf_n <<- 0L
        stat_mat_pointer <<- integer(dims$max_store)
        intervals <<- numeric(dims$max_store)
        is_dependent <<- integer(dims$max_store)
        event_time <<- numeric(dims$max_store)
        event_sender <<- integer(dims$max_store)
        event_receiver <<- integer(dims$max_store)
        n_stored <<- 0L
        initial_stats_fn <<- dims$initial_stats_fn
        invisible(NULL)
      },
      write_event = function(event_updates, event_info) {
        n_cols <- ncol(event_updates)
        if (n_cols > 0L) {
          while (buf_n + n_cols > buf_capacity) {
            buf_capacity <<- buf_capacity * 2L
            new_buf <- matrix(0, 4L, buf_capacity)
            if (buf_n > 0L) {
              new_buf[, seq_len(buf_n)] <- stat_mat_buf[, seq_len(buf_n)]
            }
            stat_mat_buf <<- new_buf
          }
          stat_mat_buf[, buf_n + seq_len(n_cols)] <<- event_updates
          buf_n <<- buf_n + n_cols
        }
        n_stored <<- n_stored + 1L
        stat_mat_pointer[n_stored] <<- buf_n
        intervals[n_stored] <<- event_info$interval
        is_dependent[n_stored] <<- event_info$is_dependent
        event_time[n_stored] <<- event_info$time
        event_sender[n_stored] <<- event_info$sender
        event_receiver[n_stored] <<- event_info$receiver
        invisible(NULL)
      },
      finalize = function(tail) {
        stat_mat_update <- stat_mat_buf[, seq_len(buf_n), drop = FALSE]
        keep <- seq_len(n_stored)
        stat_mat_pointer <- stat_mat_pointer[keep]
        intervals <- intervals[keep]
        is_dependent <- is_dependent[keep]
        event_time <- event_time[keep]
        event_sender <- event_sender[keep]
        event_receiver <- event_receiver[keep]

        assemble_default_output(
          initialStats = tail$initialStats,
          stat_mat_update = stat_mat_update,
          stat_mat_pointer = stat_mat_pointer,
          intervals = intervals,
          is_dependent = is_dependent,
          event_time = event_time,
          event_sender = event_sender,
          event_receiver = event_receiver,
          n_stored = n_stored,
          active_mode1_init = tail$active_mode1_init,
          active_mode1_changes = tail$active_mode1_changes,
          active_mode2_init = tail$active_mode2_init,
          active_mode2_changes = tail$active_mode2_changes,
          startTime = tail$startTime,
          endTime = tail$endTime,
          intercept_scalars = tail$intercept_scalars
        )
      }
    ),
    class = c("writer_default", "preprocess_writer")
  )
}

#' @describeIn preprocess_writers gather-stack writer producing the
#'   `gather_model_data()`-compatible output (one row per event x alternative).
#'   It reuses the default writer's per-event accumulation and, in
#'   `finalize()`, builds the gather stack from the assembled flat buffer.
#'
#'   Note (design D15, staged delivery): the gather expansion is currently
#'   produced by `gather_()` (the existing C++ routine) from the flat buffer
#'   rather than row-by-row inside `write_event()`. The native in-loop
#'   expansion that retires `gather_()` lands with the C++ removal task; this
#'   writer establishes the `output = "gather"` contract and byte-for-byte
#'   compatibility with `gather_model_data()` first.
#' @noRd
writer_gather <- function() {
  base <- writer_default()
  structure(
    list(
      output = "gather",
      init = base$init,
      write_event = base$write_event,
      finalize = function(tail) {
        prep <- base$finalize(tail)
        gather_from_prep(prep, tail$spec)
      }
    ),
    class = c("writer_gather", "preprocess_writer")
  )
}

#' Build the gather stack from an assembled flat preprocessing object
#'
#' Mirrors the gather-input construction of the `gather_compute` estimation
#' path (`estimate_c_int`): intercept prepend, presence C-format, flattened
#' `stat_mat_init`, effect-index shift, and the `gather_()` expansion. The
#' `twomode_or_reflexive` flag follows `gather_model_data()` (`is_two_mode`),
#' not the rate-model override used at estimation time, so the output matches
#' the legacy `gather_model_data()` result. Naming (`namesEffects`,
#' `effectDescription`) and label resolution are added by the caller, which
#' holds the parsed formula and node sets.
#'
#' @noRd
gather_from_prep <- function(prep, spec) {
  modelTypeCall <- legacy_model_type(spec)
  has_intercept <- modelTypeCall %in% c("DyNAM-M-Rate", "REM")
  is_rate_model <- modelTypeCall %in%
    c("DyNAM-M-Rate", "DyNAM-M-Rate-ordered")
  is_two_mode <- isTRUE(spec$is_two_mode)
  # Rate models reduce over a single receiver column; the estimation
  # gather_compute path forces twomode_or_reflexive = TRUE there (avoiding the
  # n_actors2 - 1 == 0 reduction that makes the legacy gather_model_data()
  # error on one-mode rate). Dyad models follow gather_model_data().
  twomode_or_reflexive <- if (is_rate_model) TRUE else is_two_mode

  statsList <- prepare_statslist(
    statsList = prep,
    excludeParameters = NULL,
    addInterceptEffect = has_intercept
  )

  presence1_update <- statsList$presence1_update
  presence1_update_pointer <- statsList$presence1_update_pointer
  if (is.null(presence1_update)) {
    presence1_update <- matrix(0, 0, 0)
    presence1_update_pointer <- numeric(1)
  }
  presence2_update <- statsList$presence2_update
  presence2_update_pointer <- statsList$presence2_update_pointer
  if (is.null(presence2_update)) {
    presence2_update <- matrix(0, 0, 0)
    presence2_update_pointer <- numeric(1)
  }
  presence1_init <- statsList$active_mode1_init
  presence2_init <- statsList$active_mode2_init

  if (is_rate_model) {
    n_parameters <- ncol(statsList$initialStats)
    n_actors1 <- nrow(statsList$initialStats)
    n_actors2 <- 1L
  } else {
    n_parameters <- dim(statsList$initialStats)[3]
    n_actors1 <- dim(statsList$initialStats)[1]
    n_actors2 <- dim(statsList$initialStats)[2]
  }

  stat_mat_update <- statsList$stat_mat_update
  stat_mat_update_pointer <- statsList$stat_mat_pointer
  if (has_intercept) {
    stat_mat_update[3, ] <- stat_mat_update[3, ] + 1
  }

  if (modelTypeCall %in% c("DyNAM-M-Rate", "REM", "DyNAM-MM")) {
    is_dependent <- as.logical(statsList$is_dependent)
    timespan <- if (modelTypeCall != "DyNAM-MM") {
      statsList$intervals
    } else {
      numeric(length(is_dependent))
    }
  } else {
    is_dependent <- as.logical(statsList$is_dependent)
    timespan <- NA
  }

  event_mat <- rbind(statsList$event_sender, statsList$event_receiver)

  if (is_rate_model) {
    stat_mat_init <- statsList$initialStats
  } else {
    stat_mat_init <- matrix(0, n_actors1 * n_actors2, n_parameters)
    for (i in seq_len(n_parameters)) {
      stat_mat_init[, i] <- t(statsList$initialStats[, , i])
    }
  }

  gathered_data <- gather_(
    modelTypeCall = modelTypeCall,
    event_mat = event_mat,
    timespan = timespan,
    is_dependent = is_dependent,
    stat_mat_init = stat_mat_init,
    stat_mat_update = stat_mat_update,
    stat_mat_update_pointer = stat_mat_update_pointer,
    presence1_init = presence1_init,
    presence1_update = presence1_update,
    presence1_update_pointer = presence1_update_pointer,
    presence2_init = presence2_init,
    presence2_update = presence2_update,
    presence2_update_pointer = presence2_update_pointer,
    n_actors1 = n_actors1,
    n_actors2 = n_actors2,
    twomode_or_reflexive = twomode_or_reflexive,
    verbose = FALSE,
    impute = FALSE
  )

  gathered_data$selected <- gathered_data$selected +
    if (has_intercept) (1 * is_dependent) else 1
  gathered_data$has_intercept <- has_intercept
  attr(gathered_data, "event_sender") <- prep$event_sender
  attr(gathered_data, "event_receiver") <- prep$event_receiver
  attr(gathered_data, "is_dependent") <- is_dependent
  attr(gathered_data, "timespan") <- timespan
  attr(gathered_data, "model_type_call") <- modelTypeCall
  gathered_data
}

#' Assemble the flat-buffer `preprocessed.goldfish` object
#'
#' Shared output assembly for the default writer: computes the intercept
#' scalars (`n_dep_events`, `total_time`, `avg_active_actors`) and the
#' composition-change C-format presence matrices, then wraps the per-event
#' fields produced by the writer into a `preprocessed.goldfish` object.
#'
#' @noRd
assemble_default_output <- function(
  initialStats, stat_mat_update, stat_mat_pointer, intervals, is_dependent,
  event_time, event_sender, event_receiver, n_stored,
  active_mode1_init, active_mode1_changes, active_mode2_init,
  active_mode2_changes, startTime, endTime, intercept_scalars
) {
  n_dep_events <- NULL
  total_time <- NULL
  avg_active_actors <- NULL
  if (intercept_scalars) {
    n_dep_events <- sum(is_dependent == 1L)
    total_time <- sum(intervals)
    nActors <- sum(active_mode1_init)
    if (length(active_mode1_changes) > 0 && n_stored > 0) {
      changesTime <- vapply(active_mode1_changes, `[[`, double(1), "time")
      changesReplace <- vapply(
        active_mode1_changes, `[[`, logical(1), "replace"
      )
      timeAcc <- startTime
      previousTime <- -Inf
      activeAcc <- 0
      for (i in seq_len(n_stored)) {
        timeAcc <- timeAcc + intervals[i]
        changesAt <- changesTime > previousTime & changesTime <= timeAcc
        nActors <- nActors +
          sum(changesReplace[changesAt]) - sum(!changesReplace[changesAt])
        activeAcc <- activeAcc + nActors
        previousTime <- timeAcc
      }
      avg_active_actors <- activeAcc / n_stored
    } else {
      avg_active_actors <- nActors
    }
  }

  presence1_update <- NULL
  presence1_update_pointer <- NULL
  presence2_update <- NULL
  presence2_update_pointer <- NULL
  if (length(active_mode1_changes) > 0) {
    compChange1 <- data.frame(
      time = vapply(active_mode1_changes, `[[`, double(1), "time"),
      node = vapply(active_mode1_changes, `[[`, integer(1), "node"),
      replace = vapply(active_mode1_changes, `[[`, logical(1), "replace")
    )
    temp <- C_convert_composition_change(compChange1, event_time)
    presence1_update <- temp$presenceUpdate
    presence1_update_pointer <- temp$presenceUpdatePointer
  }
  if (length(active_mode2_changes) > 0) {
    compChange2 <- data.frame(
      time = vapply(active_mode2_changes, `[[`, double(1), "time"),
      node = vapply(active_mode2_changes, `[[`, integer(1), "node"),
      replace = vapply(active_mode2_changes, `[[`, logical(1), "replace")
    )
    temp <- C_convert_composition_change(compChange2, event_time)
    presence2_update <- temp$presenceUpdate
    presence2_update_pointer <- temp$presenceUpdatePointer
  }

  structure(
    list(
      initialStats = initialStats,
      stat_mat_update = stat_mat_update,
      stat_mat_pointer = stat_mat_pointer,
      intervals = intervals,
      is_dependent = is_dependent,
      event_time = event_time,
      event_sender = event_sender,
      event_receiver = event_receiver,
      event_pos = seq_len(n_stored),
      active_mode1_init = active_mode1_init,
      active_mode1_changes = active_mode1_changes,
      active_mode2_init = active_mode2_init,
      active_mode2_changes = active_mode2_changes,
      startTime = startTime,
      endTime = endTime,
      n_dep_events = n_dep_events,
      total_time = total_time,
      avg_active_actors = avg_active_actors,
      presence1_update = presence1_update,
      presence1_update_pointer = presence1_update_pointer,
      presence2_update = presence2_update,
      presence2_update_pointer = presence2_update_pointer,
      version = PREPROCESSED_GOLDFISH_VERSION
    ),
    class = "preprocessed.goldfish"
  )
}
