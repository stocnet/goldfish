#' Preprocessing output writers
#'
#' A writer decouples the recipe event loops from the output
#' format they produce. Every writer exposes three hooks:
#'
#' \describe{
#'   \item{`init(spec, dims)`}{called once before the event loop with the
#'     model spec and a `dims` list describing the problem
#'     (`nEffects`, `n1`, `n2`, `is_sender`, `buf_capacity`, `max_store`,
#'     `has_intercept`, `twomode_or_reflexive`, `initial_stats_fn` — a thunk
#'     returning the recipe's `initialStats` once pre-start updates are
#'     applied).}
#'   \item{`write_event(event_updates, event_info, event_broadcast)`}{called
#'     once per stored event. `event_updates` is a 4 x k matrix of flat point
#'     updates (rows `node1`, `node2`, `effect`, `replace`; all 0-indexed)
#'     accumulated since the previous stored event; `event_info` is a list with
#'     `is_dependent`, `interval`, `time`, `sender`, `receiver`;
#'     `event_broadcast` is the optional 4 x b matrix of compact broadcast
#'     entries (rows `kind`, `fixed`, `effect`, `replace`; see the
#'     broadcast-update entry format documented on `writer_default()`),
#'     defaulting to an empty 4 x 0 matrix.}
#'   \item{`finalize(tail)`}{called once after the loop with a `tail` list of
#'     recipe-computed assembly inputs (`initialStats`,
#'     `active_sender_init` / `active_sender_changes`,
#'     `active_dyad_init` / `active_dyad_changes`, `startTime`, `endTime`,
#'     `intercept_scalars`). Returns the writer's output.}
#' }
#'
#' Recipe methods emit output exclusively through these hooks; they never
#' branch on the output format. `compute_stats(output = ...)` selects the
#' writer.
#'
#' @section Future extension points (documented, not implemented):
#' The writer contract is the seam for three planned output strategies that
#' are deliberately out of scope for this change:
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
#'     writer) invoked after the stats update for event i and
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

#' Broadcast-update entry format
#'
#' Constant-value fan-out updates (an `alter()` / `ego()` / degree projection,
#' or a `global()` change) are stored as a single coded entry in the
#' `stat_mat_broadcast` buffer instead of one duplicate column per affected cell
#' in the point buffer `stat_mat_update`. The buffer is a 4 x M matrix with rows
#'
#' \describe{
#'   \item{`kind`}{`1` = broadcast over `node1` (senders) holding the alter
#'     index `fixed`; `2` = broadcast over `node2` (alters) holding the ego
#'     index `fixed`; `3` = broadcast to all actors (`fixed` ignored).}
#'   \item{`fixed`}{the held index (the changed actor) for kinds 1/2; ignored
#'     for kind 3. **0-indexed**.}
#'   \item{`effect`}{the 0-indexed effect column the value is written to.}
#'   \item{`replace`}{the broadcast value.}
#' }
#'
#' `stat_mat_broadcast_pointer` (length = number of stored events) records the
#' end-column index in `stat_mat_broadcast` after each stored event, exactly as
#' `stat_mat_pointer` does for the point buffer. Sender-indexed (rate) models
#' emit only `kind = 3`. Decode mirrors `to_alter()` (kind 1) / `to_ego()`
#' (kind 2) / `fillChanges()` (kind 3), including the reflexive-diagonal
#' exclusion for one-mode dyad models.
#' @name broadcast_format
#' @keywords internal
#' @noRd
NULL

#' @describeIn preprocess_writers default flat-buffer writer producing the
#'   `preprocessed.goldfish` object consumed by both estimation engines.
#' @noRd
writer_default <- function() {
  buf_capacity <- NULL
  stat_mat_buf <- NULL
  buf_n <- 0
  stat_mat_pointer <- NULL
  bc_capacity <- NULL
  stat_mat_bc_buf <- NULL
  bc_n <- 0
  stat_mat_bc_pointer <- NULL
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
        buf_capacity <<- min(as.double(dims$buf_capacity), .Machine$integer.max)
        stat_mat_buf <<- matrix(0, 4L, buf_capacity)
        buf_n <<- 0
        stat_mat_pointer <<- numeric(dims$max_store)
        bc_capacity <<- min(1000, .Machine$integer.max)
        stat_mat_bc_buf <<- matrix(0, 4L, bc_capacity)
        bc_n <<- 0
        stat_mat_bc_pointer <<- numeric(dims$max_store)
        intervals <<- numeric(dims$max_store)
        is_dependent <<- integer(dims$max_store)
        event_time <<- numeric(dims$max_store)
        event_sender <<- integer(dims$max_store)
        event_receiver <<- integer(dims$max_store)
        n_stored <<- 0L
        initial_stats_fn <<- dims$initial_stats_fn
        invisible(NULL)
      },
      write_event = function(
        event_updates,
        event_info,
        event_broadcast = matrix(0, 4L, 0L)
      ) {
        n_cols <- ncol(event_updates)
        if (n_cols > 0L) {
          if (buf_n + n_cols > .Machine$integer.max) {
            cli::cli_abort(c(
              "The preprocessing statistics buffer would exceed R's matrix
               column limit.",
              "x" = "Need {.val {buf_n + n_cols}} update columns but a matrix
                     can have at most {.val {.Machine$integer.max}}.",
              "i" = "This model produces too many statistic updates for
                     in-memory preprocessing. Stream them with
                     {.code compute_stats(output = \"db\")}, or reduce the
                     number of effects or window effects."
            ))
          }
          while (buf_n + n_cols > buf_capacity) {
            buf_capacity <<- min(buf_capacity * 2, .Machine$integer.max)
            new_buf <- matrix(0, 4L, buf_capacity)
            if (buf_n > 0) {
              new_buf[, seq_len(buf_n)] <- stat_mat_buf[, seq_len(buf_n)]
            }
            stat_mat_buf <<- new_buf
          }
          stat_mat_buf[, buf_n + seq_len(n_cols)] <<- event_updates
          buf_n <<- buf_n + n_cols
        }
        n_bc <- ncol(event_broadcast)
        if (n_bc > 0L) {
          if (bc_n + n_bc > .Machine$integer.max) {
            cli::cli_abort(c(
              "The preprocessing broadcast buffer would exceed R's matrix
               column limit.",
              "x" = "Need {.val {bc_n + n_bc}} broadcast columns but a matrix
                     can have at most {.val {.Machine$integer.max}}.",
              "i" = "This model produces too many broadcast updates for
                     in-memory preprocessing. Stream them with
                     {.code compute_stats(output = \"db\")}, or reduce the
                     number of effects or window effects."
            ))
          }
          while (bc_n + n_bc > bc_capacity) {
            bc_capacity <<- min(bc_capacity * 2, .Machine$integer.max)
            new_bc <- matrix(0, 4L, bc_capacity)
            if (bc_n > 0) {
              new_bc[, seq_len(bc_n)] <- stat_mat_bc_buf[, seq_len(bc_n)]
            }
            stat_mat_bc_buf <<- new_bc
          }
          stat_mat_bc_buf[, bc_n + seq_len(n_bc)] <<- event_broadcast
          bc_n <<- bc_n + n_bc
        }
        n_stored <<- n_stored + 1L
        stat_mat_pointer[n_stored] <<- buf_n
        stat_mat_bc_pointer[n_stored] <<- bc_n
        intervals[n_stored] <<- event_info$interval
        is_dependent[n_stored] <<- event_info$is_dependent
        event_time[n_stored] <<- event_info$time
        event_sender[n_stored] <<- event_info$sender
        event_receiver[n_stored] <<- event_info$receiver
        invisible(NULL)
      },
      finalize = function(tail) {
        stat_mat_update <- stat_mat_buf[, seq_len(buf_n), drop = FALSE]
        stat_mat_broadcast <- stat_mat_bc_buf[, seq_len(bc_n), drop = FALSE]
        keep <- seq_len(n_stored)
        stat_mat_pointer <- stat_mat_pointer[keep]
        stat_mat_broadcast_pointer <- stat_mat_bc_pointer[keep]
        intervals <- intervals[keep]
        is_dependent <- is_dependent[keep]
        event_time <- event_time[keep]
        event_sender <- event_sender[keep]
        event_receiver <- event_receiver[keep]

        assemble_default_output(
          initialStats = tail$initialStats,
          stat_mat_update = stat_mat_update,
          stat_mat_pointer = stat_mat_pointer,
          stat_mat_broadcast = stat_mat_broadcast,
          stat_mat_broadcast_pointer = stat_mat_broadcast_pointer,
          intervals = intervals,
          is_dependent = is_dependent,
          event_time = event_time,
          event_sender = event_sender,
          event_receiver = event_receiver,
          n_stored = n_stored,
          active_sender_init = tail$active_sender_init,
          active_sender_changes = tail$active_sender_changes,
          active_dyad_init = tail$active_dyad_init,
          active_dyad_changes = tail$active_dyad_changes,
          active_dyad_encoding = active_dyad_encoding_for(tail$spec),
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
#'   Note (staged delivery): the gather expansion is currently
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

#' @describeIn preprocess_writers DBI streaming writer producing the same
#'   gather rows as `writer_gather()` but persisting them to a database table
#'   (`set_preprocessing_opt(db = , db_table = )`) instead of holding the full
#'   stack in memory. The connection is validated up front (fail fast before
#'   the event loop); the actual event-aligned batched append happens after
#'   `finalize()` via `write_gather_to_db()` once the effect names are
#'   resolved. Reuses `writer_gather()`'s accumulation and finalize.
#'
#'   Note (staged delivery): like `writer_gather()`, the gather
#'   stack is currently assembled from the flat buffer in `finalize()` (not
#'   row-by-row during the loop), so the full stack is materialised transiently
#'   before being written out in event-aligned batches. True per-event
#'   streaming lands with the native in-loop gather expansion.
#' @noRd
writer_db <- function(db = NULL, db_table = "stats") {
  if (is.null(db)) {
    cli::cli_abort(c(
      "A DBI connection is required for {.code output = \"db\"}.",
      "i" = "Configure one with {.code set_preprocessing_opt(db =
             <DBIConnection>, db_table = ...)}."
    ))
  }
  if (!inherits(db, "DBIConnection")) {
    cli::cli_abort("{.arg db} must be a {.cls DBIConnection} object.")
  }
  if (!is.character(db_table) || length(db_table) != 1L) {
    cli::cli_abort("{.arg db_table} must be a single character string.")
  }
  base <- writer_gather()
  structure(
    list(
      output = "db",
      db = db,
      db_table = db_table,
      init = base$init,
      write_event = base$write_event,
      finalize = base$finalize
    ),
    class = c("writer_db", "writer_gather", "preprocess_writer")
  )
}

#' Stream a named gather stack to a DBI table in event-aligned batches
#'
#' Writes the gather long table (one row per event x alternative) produced by
#' `finalize_gather_output()` to `db_table` on `db`. Rows are appended in
#' batches aligned to event boundaries, so on a mid-stream failure the last
#' fully written event index is known and reported. The returned descriptor
#' omits `stat_all_events` (now persisted in the table) and keeps the per-event
#' metadata. The long table has an `event_id` column, an `is_selected` flag
#' (1 for the chosen alternative of an event, 0 otherwise), and one
#' `stat_<i>` column per effect statistic.
#'
#' @noRd
write_gather_to_db <- function(gathered, db, db_table, batch_events = 1000L) {
  stat <- gathered$stat_all_events
  n_candidates <- gathered$n_candidates
  n_events <- length(n_candidates)
  n_parameters <- ncol(stat)
  n_rows <- nrow(stat)

  event_id <- rep.int(seq_len(n_events), n_candidates)
  ev_starts <- cumsum(c(0L, n_candidates[-n_events]))
  selected <- gathered$selected
  has_sel <- selected > 0
  is_selected <- integer(n_rows)
  is_selected[ev_starts[has_sel] + selected[has_sel]] <- 1L

  stat_df <- as.data.frame(stat)
  names(stat_df) <- paste0("stat_", seq_len(n_parameters))
  # Row identity in SQL: without index_i/index_j the long table
  # (event_id / is_selected / stat_<i>) leaves each candidate row unidentifiable
  # once the risk set is filtered. The index columns decode to the sanitized
  # actor ids (index_j is NA for sender-set rate rows).
  id_df <- data.frame(event_id = event_id, is_selected = is_selected)
  if (!is.null(gathered$index_i)) {
    id_df$index_i <- gathered$index_i
    id_df$index_j <- gathered$index_j
  }
  long_df <- cbind(id_df, stat_df)

  row_end <- cumsum(n_candidates)
  row_start <- c(1L, utils::head(row_end, -1L) + 1L)
  first <- TRUE
  last_event <- 0L
  e <- 1L
  while (e <= n_events) {
    e_to <- min(e + batch_events - 1L, n_events)
    rows <- if (row_end[e_to] >= row_start[e]) {
      row_start[e]:row_end[e_to]
    } else {
      integer(0)
    }
    batch <- long_df[rows, , drop = FALSE]
    res <- tryCatch(
      {
        if (first) {
          DBI::dbWriteTable(db, db_table, batch, overwrite = TRUE)
          first <- FALSE
        } else {
          DBI::dbAppendTable(db, db_table, batch)
        }
        TRUE
      },
      error = function(cnd) cnd
    )
    if (!isTRUE(res)) {
      cli::cli_abort(c(
        "Failed to write gather rows to table {.val {db_table}}.",
        "x" = "Last successfully written event index: {.val {last_event}}.",
        "i" = conditionMessage(res)
      ))
    }
    last_event <- e_to
    e <- e_to + 1L
  }

  gathered$stat_all_events <- NULL
  gathered$db <- db
  gathered$db_table <- db_table
  gathered$n_rows <- n_rows
  gathered$n_parameters <- n_parameters
  structure(gathered, class = "preprocessed_db.goldfish")
}

#' Build the gather stack from an assembled flat preprocessing object
#'
#' Mirrors the gather-input construction of the `gather_compute` estimation
#' path (`estimate_c_int`): intercept prepend, presence C-format, flattened
#' `stat_mat_init`, effect-index shift, and the `gather_()` expansion. The
#' `twomode_or_reflexive` flag follows `gather_model_data()` (`is_two_mode`),
#' not the rate-model override used at estimation time, so the output matches
#' the legacy `gather_model_data()` result. Naming (`namesEffects`,
#' `effect_description`) and label resolution are added by the caller, which
#' holds the parsed formula and node sets.
#'
#' @noRd
gather_from_prep <- function(prep, spec) {
  has_intercept <- identical(risk_set_normalizer(spec), "poisson")
  is_rate_model <- identical(risk_set_axis(spec), "sender")
  is_two_mode <- isTRUE(spec$is_two_mode)
  # Rate models reduce over a single receiver column; the estimation
  # gather_compute path forces twomode_or_reflexive = TRUE there (avoiding the
  # n_actors2 - 1 == 0 reduction that makes the legacy gather_model_data()
  # error on one-mode rate). Dyad models follow gather_model_data().
  twomode_or_reflexive <- if (is_rate_model) TRUE else is_two_mode

  statsList <- prepare_statslist(
    statsList = prep,
    excludeParameters = NULL,
    addInterceptEffect = has_intercept,
    is_sender = is_rate_model
  )

  active_sender_update <- statsList$active_sender_update
  active_sender_update_pointer <- statsList$active_sender_update_pointer
  if (is.null(active_sender_update)) {
    active_sender_update <- matrix(0, 0, 0)
    active_sender_update_pointer <- numeric(1)
  }
  active_dyad_update <- statsList$active_dyad_update
  active_dyad_update_pointer <- statsList$active_dyad_update_pointer
  if (is.null(active_dyad_update)) {
    active_dyad_update <- matrix(0, 0, 0)
    active_dyad_update_pointer <- numeric(1)
  }
  active_sender_init <- statsList$active_sender_init
  active_dyad_init <- statsList$active_dyad_init

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
  stat_mat_broadcast <- statsList$stat_mat_broadcast
  stat_mat_broadcast_pointer <- statsList$stat_mat_broadcast_pointer
  if (is.null(stat_mat_broadcast)) {
    stat_mat_broadcast <- matrix(0, 4L, 0L)
    stat_mat_broadcast_pointer <- numeric(length(stat_mat_update_pointer))
  }
  if (has_intercept) {
    stat_mat_update[3, ] <- stat_mat_update[3, ] + 1
    if (ncol(stat_mat_broadcast) > 0L) {
      stat_mat_broadcast[3, ] <- stat_mat_broadcast[3, ] + 1
    }
  }

  # Poisson (rate / standard REM) and coordination carry per-event timespans;
  # coordination weights them to zero, the others take the intervals; the
  # remaining multinomial families read no timespan.
  if (risk_set_normalizer(spec) %in% c("poisson", "coordination")) {
    is_dependent <- as.logical(statsList$is_dependent)
    timespan <- if (!identical(risk_set_normalizer(spec), "coordination")) {
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
      stat_mat_init[, i] <- t(statsList$initialStats[,, i])
    }
  }

  gathered_data <- gather_(
    spec = spec,
    event_mat = event_mat,
    timespan = timespan,
    is_dependent = is_dependent,
    stat_mat_init = stat_mat_init,
    stat_mat_update = stat_mat_update,
    stat_mat_update_pointer = stat_mat_update_pointer,
    stat_mat_broadcast = stat_mat_broadcast,
    stat_mat_broadcast_pointer = stat_mat_broadcast_pointer,
    active_sender_init = active_sender_init,
    active_sender_update = active_sender_update,
    active_sender_update_pointer = active_sender_update_pointer,
    active_dyad_init = active_dyad_init,
    active_dyad_update = active_dyad_update,
    active_dyad_update_pointer = active_dyad_update_pointer,
    n_actors1 = n_actors1,
    n_actors2 = n_actors2,
    twomode_or_reflexive = twomode_or_reflexive,
    verbose = FALSE,
    impute = FALSE
  )

  gathered_data$selected <- gathered_data$selected +
    if (has_intercept) (1 * is_dependent) else 1
  # `sender_of_row` / `dyad_partner` are the coordination kernel's internal
  # consumption structures; the export surfaces only the shared
  # index vocabulary (index_i / index_j), so drop them here.
  gathered_data$sender_of_row <- NULL
  gathered_data$dyad_partner <- NULL
  gathered_data$has_intercept <- has_intercept
  attr(gathered_data, "event_sender") <- prep$event_sender
  attr(gathered_data, "event_receiver") <- prep$event_receiver
  attr(gathered_data, "is_dependent") <- is_dependent
  attr(gathered_data, "timespan") <- timespan
  gathered_data
}

#' Decide the `active_dyad` minimal encoding
#'
#' The dyad-loop availability object is stored at its minimal encoding, decided
#' statically at spec time from the model family's folded presences, the
#' support constraint's axis-union `mask_kind`, and the
#' presence of an opportunity list. The receiver presence (col
#' axis) is folded by every dyad-loop family, so only three encodings arise:
#' \describe{
#'   \item{`"point"`}{a genuinely dyadic (point) support atom or an opportunity
#'     list is present — a dense `n1 x n2` init plus point flips.}
#'   \item{`"outer"`}{both axes are dynamic but separable — the risk set is
#'     dyadic (REM / REM-ordered / DyNAM-MM fold both presences) or a pure
#'     ego-kind support atom adds a sender-axis factor; stored as two factor
#'     vectors with cell `(i, j) = f1[i] & f2[j]`.}
#'   \item{`"alter"`}{only the receiver axis is dynamic (DyNAM-M choice/rate
#'     with no constraint, or a pure alter-/scalar-kind support atom) — one
#'     length-n2 vector.}
#' }
#' `mask_kind` codes: `0` point, `1` alter, `2` ego, `3` scalar; `NULL` when
#' unconstrained. `base_encoding` is the spec descriptor's base `active_dyad`
#' encoding (`"outer"` for the dyadic risk sets that fold both presences,
#' `"alter"` for choice); with `mask_kind = NULL` and `has_opportunity = FALSE`
#' this reproduces the pre-fold assignment (dyadic outer, otherwise alter).
#' @noRd
active_dyad_encoding_decide <- function(
  base_encoding,
  mask_kind = NULL,
  has_opportunity = FALSE
) {
  base_row <- identical(base_encoding, "outer")
  has_point <- (!is.null(mask_kind) && mask_kind == 0L) ||
    isTRUE(has_opportunity)
  row_from_atom <- !is.null(mask_kind) && mask_kind == 2L
  if (has_point) {
    "point"
  } else if (base_row || row_from_atom) {
    "outer"
  } else {
    "alter"
  }
}

#' @noRd
active_dyad_encoding_for <- function(spec) {
  active_dyad_encoding_decide(risk_set_encoding(spec))
}

#' `active_dyad` read accessors
#'
#' Consumers read per-event availability through these helpers and never branch
#' on the encoding. At `"alter"` `active_dyad` is the receiver-axis logical
#' vector; at `"outer"` cell `(i, j) = active_sender[i] & active_dyad[j]` (two
#' factor vectors); at `"point"` `active_dyad` is the dense `n1 x n2` logical
#' and the cell is read directly. `active_dyad_row()` returns the length-n2
#' availability for a given sender, `active_dyad_cell()` a single dyad, and
#' `active_dyad_count()` the number of available dyads (the intercept
#' denominator's per-event TRUE-count).
#' @noRd
active_dyad_row <- function(
  encoding,
  sender_i,
  active_dyad,
  active_sender = NULL
) {
  if (identical(encoding, "point")) {
    active_dyad[sender_i, ]
  } else if (identical(encoding, "outer")) {
    if (isTRUE(active_sender[sender_i] == 1)) {
      active_dyad
    } else {
      active_dyad & FALSE
    }
  } else {
    active_dyad
  }
}

#' @noRd
active_dyad_cell <- function(
  encoding,
  sender_i,
  receiver_j,
  active_dyad,
  active_sender = NULL
) {
  if (identical(encoding, "point")) {
    return(as.logical(active_dyad[sender_i, receiver_j]))
  }
  avail_j <- active_dyad[receiver_j]
  if (identical(encoding, "outer")) {
    avail_j & isTRUE(active_sender[sender_i] == 1)
  } else {
    avail_j
  }
}

#' @noRd
active_dyad_count <- function(encoding, active_dyad, active_sender = NULL) {
  if (identical(encoding, "point")) {
    sum(active_dyad == 1)
  } else if (identical(encoding, "outer")) {
    sum(active_sender == 1) * sum(active_dyad == 1)
  } else {
    sum(active_dyad == 1)
  }
}

#' Assemble the flat-buffer `preprocessed.goldfish` object
#'
#' Shared output assembly for the default writer: computes the intercept
#' scalars (`n_dep_events`, `total_time`, `avg_active_entity`) and the
#' composition-change C-format presence matrices, then wraps the per-event
#' fields produced by the writer into a `preprocessed.goldfish` object.
#'
#' @noRd
assemble_default_output <- function(
  initialStats,
  stat_mat_update,
  stat_mat_pointer,
  intervals,
  is_dependent,
  event_time,
  event_sender,
  event_receiver,
  n_stored,
  active_sender_init,
  active_sender_changes,
  active_dyad_init,
  active_dyad_changes,
  active_dyad_encoding,
  startTime,
  endTime,
  intercept_scalars,
  stat_mat_broadcast = matrix(0, 4L, 0L),
  stat_mat_broadcast_pointer = numeric(n_stored)
) {
  n_dep_events <- NULL
  total_time <- NULL
  avg_active_entity <- NULL
  if (intercept_scalars) {
    n_dep_events <- sum(is_dependent == 1L)
    total_time <- sum(intervals)
    nActors <- sum(active_sender_init)
    if (length(active_sender_changes) > 0 && n_stored > 0) {
      changesTime <- vapply(active_sender_changes, `[[`, double(1), "time")
      changesReplace <- vapply(
        active_sender_changes,
        `[[`,
        logical(1),
        "replace"
      )
      timeAcc <- startTime
      previousTime <- -Inf
      activeAcc <- 0
      for (i in seq_len(n_stored)) {
        timeAcc <- timeAcc + intervals[i]
        changesAt <- changesTime > previousTime & changesTime <= timeAcc
        nActors <- nActors +
          sum(changesReplace[changesAt]) -
          sum(!changesReplace[changesAt])
        activeAcc <- activeAcc + nActors
        previousTime <- timeAcc
      }
      avg_active_entity <- activeAcc / n_stored
    } else {
      avg_active_entity <- nActors
    }
  }

  active_sender_update <- NULL
  active_sender_update_pointer <- NULL
  active_dyad_update <- NULL
  active_dyad_update_pointer <- NULL
  if (length(active_sender_changes) > 0) {
    compChange1 <- data.frame(
      time = vapply(active_sender_changes, `[[`, double(1), "time"),
      node = vapply(active_sender_changes, `[[`, integer(1), "node"),
      replace = vapply(active_sender_changes, `[[`, logical(1), "replace")
    )
    temp <- C_convert_composition_change(compChange1, event_time)
    active_sender_update <- temp$presenceUpdate
    active_sender_update_pointer <- temp$presenceUpdatePointer
  }
  if (length(active_dyad_changes) > 0) {
    compChange2 <- data.frame(
      time = vapply(active_dyad_changes, `[[`, double(1), "time"),
      node = vapply(active_dyad_changes, `[[`, integer(1), "node"),
      replace = vapply(active_dyad_changes, `[[`, logical(1), "replace")
    )
    temp <- C_convert_composition_change(compChange2, event_time)
    active_dyad_update <- temp$presenceUpdate
    active_dyad_update_pointer <- temp$presenceUpdatePointer
  }

  structure(
    list(
      initialStats = initialStats,
      stat_mat_update = stat_mat_update,
      stat_mat_pointer = stat_mat_pointer,
      stat_mat_broadcast = stat_mat_broadcast,
      stat_mat_broadcast_pointer = stat_mat_broadcast_pointer,
      intervals = intervals,
      is_dependent = is_dependent,
      event_time = event_time,
      event_sender = event_sender,
      event_receiver = event_receiver,
      event_pos = seq_len(n_stored),
      active_sender_init = active_sender_init,
      active_sender_changes = active_sender_changes,
      active_dyad_init = active_dyad_init,
      active_dyad_changes = active_dyad_changes,
      active_dyad_encoding = active_dyad_encoding,
      startTime = startTime,
      endTime = endTime,
      n_dep_events = n_dep_events,
      total_time = total_time,
      avg_active_entity = avg_active_entity,
      active_sender_update = active_sender_update,
      active_sender_update_pointer = active_sender_update_pointer,
      active_dyad_update = active_dyad_update,
      active_dyad_update_pointer = active_dyad_update_pointer,
      version = PREPROCESSED_GOLDFISH_VERSION
    ),
    class = "preprocessed.goldfish"
  )
}
