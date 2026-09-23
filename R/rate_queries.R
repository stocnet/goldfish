# =========================================================================== #
# Rate queries over a candidate sequence.
#
# A proposal density for an augmented event needs the model's frozen intensity
# for a named pair at a position inside a candidate (not yet accepted)
# sequence. The candidate is bound to a completed specification, a walk handle
# is opened over it, and the handle's own schedule -- the candidate's
# dependent rows and their realizations, interleaved with the exogenous rows --
# is replayed by row position up to the queried point.
#
# Replay is driven by schedule position, not by time: the batch walk scores a
# dependent event before the covariate rows its schedule orders after it at
# the same stamp, and a query can address "after the first of two tied
# events", which no time cutoff can.
# =========================================================================== #

# Apply the handle's schedule rows in order up to, not including, `row`:
# `walk_replay_to(handle, k)` leaves the state row k is read at. It resumes
# from where the previous call stopped, so the handle only moves forward.
#
# * An exogenous row (not dependent, not on a focal layer) is applied alone.
# * A focal-layer covariate row is a dependent event's realization, injected
#   through walk_inject().
# * A dependent row writes nothing; the node composition in force at its stamp
#   is brought in, since a composition change at an event's own stamp applies
#   to that event.
walk_replay_to <- function(handle, row, call = rlang::caller_env()) {
  walk_assert_open(handle, call)
  schedule <- handle$schedule
  done <- handle$replay_cursor %||% 0L
  last <- schedule$n + 1L
  if (length(row) != 1L || is.na(row) || row < 1L || row > last) {
    cli::cli_abort(
      c(
        "{.arg row} must be one schedule row in {.val {1L}}:{.val {last}}.",
        "x" = "Got {.val {row}}."
      ),
      call = call,
      class = "goldfish_walk_bad_row"
    )
  }
  if (row - 1L < done) {
    cli::cli_abort(
      c(
        "{.fn walk_replay_to} cannot move the replay backward.",
        "x" = "Rows up to {.val {done}} are applied; asked to stop before row
               {.val {row}}."
      ),
      call = call,
      class = "goldfish_walk_out_of_order"
    )
  }

  for (k in seq_len(row - 1L - done) + done) {
    if (schedule$dependent[k]) {
      walk_advance_presence(handle, schedule$time[k])
    } else if (schedule$layer[k] %in% handle$focal_layers) {
      event <- list(
        layer = schedule$layer[k],
        sender = schedule$sender[k],
        receiver = schedule$receiver[k],
        time = schedule$time[k]
      )
      event[[schedule$semantics[k]]] <- schedule$value[[k]]
      walk_inject(handle, event, call = call)
    } else {
      walk_replay_exogenous_row(handle, k, call)
    }
    handle$replay_cursor <- k
  }
  invisible(handle)
}

# Apply exogenous schedule row `k` and nothing else. walk_advance() applies
# every exogenous row stamped at or before a time, which at a shared stamp
# includes rows the schedule orders after `k`; these are the steps it runs
# for one row. A row an earlier walk_advance() already applied is skipped.
walk_replay_exogenous_row <- function(handle, k, call) {
  cursor <- handle$exo_cursor
  if (k %in% handle$exo_rows[seq_len(cursor)]) {
    return(invisible(NULL))
  }
  if (!identical(handle$exo_rows[cursor + 1L], k)) {
    cli::cli_abort(
      "Exogenous row {.val {k}} is not the handle's next exogenous row.",
      .internal = TRUE,
      call = call
    )
  }
  schedule <- handle$schedule
  t <- schedule$time[k]
  oid <- schedule$target[k]
  walk_advance_presence(handle, t)
  event_args <- merged_build_event_args(
    schedule,
    k,
    oid,
    handle$props,
    handle$state
  )
  walk_apply_object_event(handle, oid, schedule$shape[k], event_args, t)
  handle$exo_cursor <- cursor + 1L
  invisible(NULL)
}

# Rebind a completed joint specification to a candidate sequence: the candidate
# replaces the data at the joint level and on every process specification, and
# nothing else is recomputed. Flavor style, the mode map, the process map's
# coupling and the completed-rate pins are all fixed by `info` and `nodes` at
# construction, so a candidate differing there would silently run against a
# specification built for another dataset; only the ties may differ. Completion
# must not re-run per candidate either: a pin that moved with the candidate
# would shift the likelihood between proposals with no parameter to explain it.
bind_candidate_data <- function(spec, candidate, call = rlang::caller_env()) {
  differing <- c("info", "nodes")[
    !c(
      identical(candidate$info, spec$data$info),
      identical(candidate$nodes, spec$data$nodes)
    )
  ]
  if (length(differing) > 0L) {
    cli::cli_abort(
      c(
        "The candidate sequence does not match the specification's data in
         {.field {differing}}.",
        "i" = "A candidate may differ from the observed data only in its
               ties."
      ),
      call = call,
      class = "goldfish_candidate_mismatch"
    )
  }
  spec$data <- candidate
  spec$specifications <- lapply(spec$specifications, function(part) {
    part$data <- candidate
    part
  })
  spec
}
