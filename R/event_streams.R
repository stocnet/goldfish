# =========================================================================== #
# Event ordering and component splitting for stocnet conversion.
#
# stocnet coercions arrange ties by from/to and drop the incoming row order, so
# conversion imposes a deterministic schedule (D2) and splits the components into
# the per-layer / per-variable streams the existing fetch_plan multi-stream walk
# consumes (D15 — no monolithic stacked copy).
# =========================================================================== #

component_rank <- c(ties = 1L, changes = 2L, global = 3L)

#' Order events by the deterministic D2 sort key
#'
#' Sort precedence: `time`, then dependent (focal-layer) events before
#' exogenous, then component order (`ties` < `changes` < `global`), then `layer`,
#' then the final tie-break. When an integer `order` column is present it is the
#' final tie-break in place of `from`/`to` (or `node`). Multiple `replace` events
#' targeting the same cell / node-variable at the same time with no `order`
#' column are genuinely ambiguous and abort; same-time increments commute and
#' pass silently.
#'
#' @param events a data frame with a numeric `time` column and any of the
#'   optional key columns `is_dependent`, `component`, `layer`, `from`/`to`,
#'   `node`, `order`, and an `update` column (`"increment"`/`"replace"`) driving
#'   the ambiguity check.
#' @param call calling environment for error reporting.
#'
#' @return `events` reordered; aborts on an unresolved same-time replace tie.
#' @noRd
order_events <- function(events, call = rlang::caller_env()) {
  n <- nrow(events)
  if (n == 0) {
    return(events)
  }
  time <- as.numeric(events$time)
  dep_rank <- if ("is_dependent" %in% names(events)) {
    as.integer(!events$is_dependent)
  } else {
    rep(0L, n)
  }
  comp_rank <- if ("component" %in% names(events)) {
    unname(component_rank[events$component])
  } else {
    rep(1L, n)
  }
  layer_key <- if ("layer" %in% names(events)) events$layer else rep("", n)

  has_order <- "order" %in% names(events)
  if (has_order) {
    tie1 <- events$order
    tie2 <- rep(0L, n)
  } else if (all(c("from", "to") %in% names(events))) {
    tie1 <- events$from
    tie2 <- events$to
  } else if ("node" %in% names(events)) {
    tie1 <- events$node
    tie2 <- rep(0L, n)
  } else {
    tie1 <- seq_len(n)
    tie2 <- rep(0L, n)
  }

  check_replace_ambiguity(events, time, has_order, call = call)

  ord <- order(time, dep_rank, comp_rank, layer_key, tie1, tie2)
  events[ord, , drop = FALSE]
}

# A same-time collision is unrecoverable only for replace events on the same
# target with no disambiguating order value; increments commute.
check_replace_ambiguity <- function(events, time, has_order, call) {
  if (!"update" %in% names(events)) {
    return(invisible(TRUE))
  }
  is_replace <- events$update == "replace" & !is.na(time)
  if (!any(is_replace)) {
    return(invisible(TRUE))
  }
  target <- event_target(events)
  layer_key <- if ("component" %in% names(events)) {
    paste(events$component, events[["layer"]] %||% "", sep = "\r")
  } else {
    rep("", nrow(events))
  }
  group <- paste(layer_key, target, time, sep = "\r")[is_replace]
  order_vals <- if (has_order) events$order[is_replace] else NA

  dup_groups <- unique(group[duplicated(group)])
  if (length(dup_groups) == 0) {
    return(invisible(TRUE))
  }
  # An order column resolves a collision only when its values are distinct
  # within the colliding group.
  unresolved <- vapply(
    dup_groups,
    function(g) {
      idx <- which(group == g)
      if (!has_order) {
        return(TRUE)
      }
      anyDuplicated(order_vals[idx]) > 0
    },
    logical(1)
  )
  if (!any(unresolved)) {
    return(invisible(TRUE))
  }
  colliding <- which(is_replace)[group %in% dup_groups[unresolved]]
  cli::cli_abort(
    c(
      "Ambiguous same-time {.field replace} events on the same target.",
      "x" = "{length(colliding)} row{?s} collide with no {.field order} \\
             tie-break: rows {.val {colliding}}.",
      "i" = "Add an integer {.field order} column to fix their sequence."
    ),
    call = call
  )
}

# Target key identifying the cell (ties) or node-variable (changes) / variable
# (global) a replace event writes.
event_target <- function(events) {
  if (all(c("from", "to") %in% names(events))) {
    paste(events$from, events$to, sep = "\r")
  } else if (all(c("node", "var") %in% names(events))) {
    paste(events$node, events$var, sep = "\r")
  } else if ("node" %in% names(events)) {
    as.character(events$node)
  } else if ("var" %in% names(events)) {
    as.character(events$var)
  } else {
    rep("", nrow(events))
  }
}
