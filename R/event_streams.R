# =========================================================================== #
# Event ordering and component splitting for stocnet conversion.
#
# stocnet coercions arrange ties by from/to and drop the incoming row order, so
# conversion imposes a deterministic schedule and splits the components into the
# per-layer / per-variable streams the existing fetch_plan multi-stream walk
# already consumes -- a single stacked copy would duplicate what the fetch plan
# organizes anyway.
# =========================================================================== #

component_rank <- c(ties = 1L, changes = 2L, global = 3L)

#' Order events by the deterministic sort key
#'
#' Sort precedence: `time`, then dependent (focal-layer) events before
#' exogenous, then component order (`ties` < `changes` < `global`), then
#' `layer`, then the final tie-break. When an integer `order` column is present
#' it is the final tie-break in place of `from`/`to` (or `node`). Multiple
#' `replace` events targeting the same cell / node-variable at the same time
#' with no `order` column are genuinely ambiguous and abort; same-time
#' increments commute and pass silently.
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

#' Split stocnet components into per-layer / per-variable event streams
#'
#' Maps the stocnet components onto the per-object streams the recipe loop's
#' `fetch_plan` walk consumes, remapping node references through the mode map
#' to local index spaces and converting `time` to a numeric axis:
#'
#' - `ties`: one stream per layer carrying every row (`time = NA` history rows
#'   fold into the initial state; timed rows are updates). The tie value is the
#'   `weight` column (default 1) applied with the layer's `info$update`
#'   semantics.
#' - focal layer: a `dependent` stream of the modeled rows -- filtered to
#'   `modeled_flavor` when the specification keys one (non-matching / `NA`
#'   flavor rows stay state-only in the network stream), otherwise all timed
#'   focal rows.
#' - `changes`: one stream per `var`; `var == "active"` routes to per-side
#'   composition (`mode1`/`mode2`) via the focal layer's mode map.
#' - `global`: one stream per `var`.
#'
#' @param x a validated stocnet object.
#' @param mode_map the [build_mode_map()] result.
#' @param focal focal layer name (defaults to `info$focal`).
#' @param modeled_flavor optional single flavor value selecting the dependent
#'   rows on a flavored focal layer.
#'
#' @return a list of streams: `network` (per layer), `dependent`, `attribute`
#'   (per var), `composition` (`mode1`/`mode2`), `global` (per var), and
#'   `focal`.
#' @noRd
split_stocnet_streams <- function(
  x,
  mode_map,
  focal = NULL,
  modeled_flavor = NULL
) {
  info <- x$info %||% list()
  nodes <- as.data.frame(x$nodes)
  ties <- as.data.frame(x$ties)
  focal <- focal %||% info$focal
  layers <- unique(ties$layer)

  weight <- if ("weight" %in% names(ties)) ties$weight else rep(1, nrow(ties))
  tie_time <- as.numeric(ties$time)
  flavor <- if ("flavor" %in% names(ties)) {
    ties$flavor
  } else {
    rep(NA_character_, nrow(ties))
  }
  ord_col <- if ("order" %in% names(ties)) ties$order else NULL

  network <- lapply(layers, function(layer) {
    sel <- ties$layer == layer
    rl <- remap_layer_refs(mode_map, layer, ties$from[sel], ties$to[sel], nodes)
    stream <- data.frame(
      time = tie_time[sel],
      from = rl$from,
      to = rl$to,
      value = weight[sel],
      update = unname(info$update[layer]),
      layer = layer,
      flavor = flavor[sel],
      stringsAsFactors = FALSE
    )
    if (!is.null(ord_col)) {
      stream$order <- ord_col[sel]
    }
    stream
  })
  names(network) <- layers

  dependent <- NULL
  if (!is.null(focal)) {
    fstream <- network[[focal]]
    timed <- !is.na(fstream$time)
    if (!is.null(modeled_flavor)) {
      modeled <- timed &
        !is.na(fstream$flavor) &
        fstream$flavor == modeled_flavor
    } else {
      modeled <- timed
    }
    dependent <- fstream[modeled, , drop = FALSE]
  }

  attribute <- list()
  composition <- list()
  if (!is.null(x$changes) && nrow(x$changes) > 0) {
    ch <- as.data.frame(x$changes)
    node_global <- to_global_id(ch$node, nodes)
    values <- unwrap_values(ch$value)
    ch_time <- as.numeric(ch$time)
    for (v in unique(ch$var)) {
      sel <- ch$var == v
      stream <- data.frame(
        time = ch_time[sel],
        node = node_global[sel],
        stringsAsFactors = FALSE
      )
      stream$value <- values[sel]
      if (identical(v, "active")) {
        composition <- split_composition(stream, mode_map, focal)
      } else {
        attribute[[v]] <- stream
      }
    }
  }

  global <- list()
  if (!is.null(x$global) && nrow(x$global) > 0) {
    g <- as.data.frame(x$global)
    g_values <- unwrap_values(g$value)
    g_time <- as.numeric(g$time)
    for (v in unique(g$var)) {
      sel <- g$var == v
      stream <- data.frame(time = g_time[sel], stringsAsFactors = FALSE)
      stream$value <- g_values[sel]
      global[[v]] <- stream
    }
  }

  list(
    network = network,
    dependent = dependent,
    attribute = attribute,
    composition = composition,
    global = global,
    focal = focal
  )
}

# Composition (active) changes split by the focal layer's side membership into
# the mode1/mode2 streams the support-constraint active_1/active_2 factors
# consume; node references become per-side local indices.
split_composition <- function(df, mode_map, focal) {
  if (is.null(focal)) {
    focal <- names(mode_map$layers)[1]
  }
  lm <- mode_map$layers[[focal]]
  side1_local <- match(df$node, lm$side1)
  in1 <- !is.na(side1_local)
  res <- list()
  d1 <- df[in1, , drop = FALSE]
  d1$node <- side1_local[in1]
  res$mode1 <- d1
  if (lm$is_two_mode) {
    side2_local <- match(df$node, lm$side2)
    in2 <- !is.na(side2_local)
    d2 <- df[in2, , drop = FALSE]
    d2$node <- side2_local[in2]
    res$mode2 <- d2
  }
  res
}
