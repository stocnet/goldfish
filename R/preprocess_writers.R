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
#'     returning the recipe's `initial_stats` once pre-start updates are
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
#'     recipe-computed assembly inputs (`initial_stats`,
#'     `active_sender_init` / `active_sender_changes`,
#'     `active_dyad_init` / `active_dyad_changes`, `start_time`, `end_time`,
#'     `intercept_scalars`). Returns the writer's output.}
#' }
#'
#' Recipe methods emit output exclusively through these hooks; they never
#' branch on the output format. `compute_statistics(output = ...)` selects the
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
  has_intercept <- FALSE

  structure(
    list(
      output = "default",
      init = function(spec, dims) {
        # One flag, two consumer-facing names: the exact-time (poisson-
        # normalized) sub-models are exactly those that carry a time intercept
        # AND store right-censored rows -- two halves of the same waiting-time
        # likelihood. `init_consumers()` already passes a single value for
        # both, so reporting them separately renames rather than re-derives.
        has_intercept <<- isTRUE(dims$has_intercept)
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
                     {.code compute_statistics(output = \"db\")}, or reduce the
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
                     {.code compute_statistics(output = \"db\")}, or reduce the
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
          initial_stats = tail$initial_stats,
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
          start_time = tail$start_time,
          end_time = tail$end_time,
          intercept_scalars = tail$intercept_scalars,
          has_intercept = has_intercept
        )
      },
      # The render stage turns the assembled object into the writer's product.
      # It runs AFTER the support constraint is realized and folded (see
      # `finalize_consumers()`), so a product that enumerates candidates -- the
      # gather stack -- expands the post-fold risk set rather than the full one.
      # For the default writer the product is the assembled object itself.
      render = function(out, spec) out
    ),
    class = c("goldfishWriterDefault", "goldfishWriter")
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
      # `finalize()` returns the assembled default shape so the constraint can be
      # realized and folded on an object that still carries `event_time` and the
      # availability encoding; the gather expansion happens in `render()`, after
      # the fold, so it enumerates only the candidates the constraint allows.
      finalize = base$finalize,
      render = function(out, spec) gather_from_prep(out, spec)
    ),
    class = c("goldfishWriterGather", "goldfishWriter")
  )
}

#' @describeIn preprocess_writers DBI streaming writer producing the same
#'   gather rows as `writer_gather()` but persisting them to a database table
#'   (`set_preprocessing(db = , db_table = )`) instead of holding the full
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
  validate_db_target(db, db_table)
  base <- writer_gather()
  structure(
    list(
      output = "db",
      db = db,
      db_table = db_table,
      init = base$init,
      write_event = base$write_event,
      # Persistence stays in `write_gather_to_db()` after the effect names
      # resolve; the writer's product is the same rendered gather stack as
      # `writer_gather()`, so both finalize and render are inherited from it.
      finalize = base$finalize,
      render = base$render
    ),
    class = c("goldfishWriterDB", "goldfishWriterGather", "goldfishWriter")
  )
}

#' Check the export target of `output = "db"` before any work happens
#'
#' Called where the db route is decided — when the db writer is constructed on
#' the single-process path, and before the shared preprocessing pass on the
#' flavored one, which renders and persists per process afterwards. A run that
#' cannot write must fail before the event loop, not after it.
#'
#' @noRd
validate_db_target <- function(db, db_table) {
  if (is.null(db)) {
    cli::cli_abort(c(
      "A DBI connection is required for {.code output = \"db\"}.",
      "i" = "Configure one with {.code set_preprocessing(db =
             <DBIConnection>, db_table = ...)}."
    ))
  }
  if (!inherits(db, "DBIConnection")) {
    cli::cli_abort("{.arg db} must be a {.cls DBIConnection} object.")
  }
  if (!is.character(db_table) || length(db_table) != 1L) {
    cli::cli_abort("{.arg db_table} must be a single character string.")
  }
  invisible(NULL)
}

# The identity columns every exported long table reserves, whatever the model
# family: a rate export leaves `index_j` unwritten but the name stays reserved,
# so the same effect resolves to the same column name across families.
DB_RESERVED_COLUMNS <- c("event_id", "is_selected", "index_i", "index_j")

# The same, for the ready-to-estimate frame, which carries labels and exposure
# alongside the indices.
FRAME_RESERVED_COLUMNS <- c(
  "event",
  "chosen",
  "sender",
  "receiver",
  "index_i",
  "index_j",
  "timespan",
  "is_dependent"
)

#' Resolve the statistics column names of an export
#'
#' The exported statistics columns are named by their effect (`names_effects`)
#' rather than by position, so a table read without the producing session still
#' says which effect each column holds. The `reserved` identity columns take
#' part in the uniqueness pass: an effect whose short name collides with one of
#' them is disambiguated instead of overwriting it.
#'
#' @noRd
stat_column_names <- function(names_effects, reserved) {
  resolved <- make.unique(c(reserved, names_effects), sep = "_")
  utils::tail(resolved, length(names_effects))
}

#' Stream a named gather stack to a DBI table in event-aligned batches
#'
#' Writes the gather long table (one row per event x alternative) produced by
#' `finalize_gather_output()` to the process table `<db_table>_<fid>` on `db`.
#' Rows are appended in batches aligned to event boundaries, so on a mid-stream
#' failure the last fully written event index is known and reported, together
#' with the process whose table failed (an event index alone is ambiguous once
#' an export holds several processes). The returned descriptor omits
#' `stat_all_events` (now persisted in the table) and keeps the per-event
#' metadata. The long table has an `event_id` column, an `is_selected` flag
#' (1 for the chosen alternative of an event, 0 otherwise), the identity columns
#' `index_i`/`index_j`, and one column per effect statistic named by that
#' effect.
#'
#' @noRd
write_gather_to_db <- function(
  gathered,
  db,
  db_table,
  fid = 1L,
  batch_events = 1000L
) {
  stats_table <- paste0(db_table, "_", fid)
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
  names(stat_df) <- stat_column_names(
    gathered$names_effects,
    DB_RESERVED_COLUMNS
  )
  # Row identity in SQL: without index_i/index_j the long table
  # (event_id / is_selected / statistics) leaves each candidate row
  # unidentifiable once the risk set is filtered. The index columns decode to
  # the sanitized actor ids (index_j is NA for sender-set rate rows).
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
          DBI::dbWriteTable(db, stats_table, batch, overwrite = TRUE)
          first <- FALSE
        } else {
          DBI::dbAppendTable(db, stats_table, batch)
        }
        TRUE
      },
      error = function(cnd) cnd
    )
    if (!isTRUE(res)) {
      cli::cli_abort(c(
        "Failed to write gather rows to table {.val {stats_table}}.",
        "x" = "Process {.val {fid}}, last successfully written event index:
               {.val {last_event}}.",
        "i" = conditionMessage(res)
      ))
    }
    last_event <- e_to
    e <- e_to + 1L
  }

  gathered$stat_all_events <- NULL
  gathered$db <- db
  gathered$db_table <- db_table
  gathered$db_tables <- c(stats = stats_table)
  gathered$fid <- fid
  gathered$n_rows <- n_rows
  gathered$n_parameters <- n_parameters
  structure(gathered, class = "preprocessed_db.goldfish")
}

#' Complete a db export with its map and node tables
#'
#' A db export is always K processes, K >= 1: every export writes one statistics
#' table per process plus the two tables that make it readable without the
#' session that produced it — `<db_table>_map` (the `process_map` identity
#' columns and each process's table name) and `<db_table>_nodes` (the
#' `(side, local, global, label)` lookup the `index_i`/`index_j` columns join
#' to). The node lookup is one layer's, shared by every process, so it is
#' written once; the legacy environment path carries none and writes no node
#' table.
#'
#' The map is the authority for which tables belong to this export. Tables a
#' previous export with more processes left behind are NOT dropped: a
#' prefix-matching drop would be a destructive guess against tables the
#' connection may own for other reasons.
#'
#' @param descriptors fid-keyed list of `preprocessed_db.goldfish` descriptors,
#'   one per written process table.
#' @param process_map the identity table of the export, one row per fid.
#' @noRd
finish_db_export <- function(descriptors, db, db_table, process_map) {
  map_table <- paste0(db_table, "_map")
  nodes_table <- paste0(db_table, "_nodes")
  written <- vapply(descriptors, function(d) unname(d$db_tables["stats"]), "")
  map_df <- process_map
  map_df$table_name <- unname(written[as.character(process_map$fid)])

  node_lookup <- descriptors[[1L]]$node_lookup
  has_nodes <- !is.null(node_lookup)
  if (has_nodes) {
    DBI::dbWriteTable(db, nodes_table, node_lookup, overwrite = TRUE)
  }
  DBI::dbWriteTable(db, map_table, map_df, overwrite = TRUE)

  tables <- c(map = map_table, if (has_nodes) c(nodes = nodes_table))
  lapply(descriptors, function(d) {
    d$db_tables <- c(d$db_tables, tables)
    d$process_map <- map_df
    d
  })
}

#' The one-row `process_map` of a single-process export
#'
#' The db schema is the same whether or not the specification is flavored, so a
#' single process is fid 1 under exactly the numbering flavoring uses. Its map
#' row is synthesized from the model itself: there is no flavor to name, but
#' the layer, family and intercept of the process whose table it points at are
#' what a later reader needs.
#'
#' @noRd
single_process_map <- function(model, sub_model, layer, has_intercept) {
  data.frame(
    fid = 1L,
    layer = layer %||% NA_character_,
    flavor = NA_character_,
    family = sub_model,
    stat_block = paste(model, sub_model, sep = ":"),
    has_intercept = has_intercept,
    constraint_id = NA_integer_,
    stringsAsFactors = FALSE
  )
}

#' Export one gather stack as a complete single-process db export
#'
#' Writes `<db_table>_1`, the one-row map and the node table, and returns that
#' process's descriptor. The return is deliberately asymmetric: a single
#' process gives back its descriptor, a flavored specification a fid-keyed
#' list, because the caller of the first knows there is only one.
#'
#' @noRd
export_single_process_db <- function(
  gathered,
  db,
  db_table,
  model,
  sub_model,
  layer,
  has_intercept
) {
  descriptors <- finish_db_export(
    list("1" = write_gather_to_db(gathered, db, db_table, fid = 1L)),
    db,
    db_table,
    single_process_map(model, sub_model, layer, has_intercept)
  )
  descriptors[[1L]]
}

#' Build the gather stack from an assembled flat preprocessing object
#'
#' Mirrors the gather-input construction of the `gather` backend's estimation
#' path (`estimate_c_int`): intercept prepend, presence C-format, flattened
#' `stat_mat_init`, effect-index shift, and the `gather_()` expansion. The
#' `twomode_or_reflexive` flag follows `gather_model_data()` (`is_two_mode`),
#' not the rate-model override used at estimation time, so the output matches
#' the legacy `gather_model_data()` result. Naming (`names_effects`,
#' `effect_description`) and label resolution are added by the caller, which
#' holds the parsed formula and node sets.
#'
#' @noRd
gather_from_prep <- function(prep, spec) {
  has_intercept <- identical(risk_set_normalizer(spec), "poisson")
  is_rate_model <- identical(risk_set_axis(spec), "sender")
  is_two_mode <- isTRUE(spec$is_two_mode)
  # Rate models reduce over a single receiver column; the estimation
  # gather path forces twomode_or_reflexive = TRUE there (avoiding the
  # n_actors2 - 1 == 0 reduction that makes the legacy gather_model_data()
  # error on one-mode rate). Dyad models follow gather_model_data().
  twomode_or_reflexive <- if (is_rate_model) TRUE else is_two_mode

  statsList <- prepare_statslist(
    statsList = prep,
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
  # Folding a support constraint upgrades the availability to the `point`
  # encoding (a dense n1 x n2 mask). The gather expansion must read that
  # encoding, exactly as `estimate_c_int()` does, or it flattens the matrix as a
  # length-n2 vector and indexes past the statistics matrix.
  active_dyad_encoding <- if (is.null(statsList$active_dyad_encoding)) {
    "alter"
  } else {
    statsList$active_dyad_encoding
  }

  if (is_rate_model) {
    n_parameters <- ncol(statsList$initial_stats)
    n_actors1 <- nrow(statsList$initial_stats)
    n_actors2 <- 1L
  } else {
    n_parameters <- dim(statsList$initial_stats)[3]
    n_actors1 <- dim(statsList$initial_stats)[1]
    n_actors2 <- dim(statsList$initial_stats)[2]
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
    stat_mat_init <- statsList$initial_stats
  } else {
    stat_mat_init <- matrix(0, n_actors1 * n_actors2, n_parameters)
    for (i in seq_len(n_parameters)) {
      stat_mat_init[, i] <- t(statsList$initial_stats[,, i])
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
    impute = FALSE,
    active_dyad_encoding = active_dyad_encoding
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
  initial_stats,
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
  start_time,
  end_time,
  intercept_scalars,
  has_intercept = intercept_scalars,
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
      timeAcc <- start_time
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
      initial_stats = initial_stats,
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
      start_time = start_time,
      end_time = end_time,
      n_dep_events = n_dep_events,
      total_time = total_time,
      avg_active_entity = avg_active_entity,
      active_sender_update = active_sender_update,
      active_sender_update_pointer = active_sender_update_pointer,
      active_dyad_update = active_dyad_update,
      active_dyad_update_pointer = active_dyad_update_pointer,
      has_intercept = has_intercept,
      right_censored = has_intercept,
      prep_version = PREP_VERSION
    ),
    class = "preprocessed.goldfish"
  )
}
