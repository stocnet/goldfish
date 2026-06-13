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
