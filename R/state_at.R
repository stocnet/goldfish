# =========================================================================== #
# State-at-time helpers: evaluate a layer's network or the nodes'
# attribute values at a time point, sharing the initial-state materializer that
# preprocessing uses. Honors time = NA history and per-layer info$update
# semantics.
# =========================================================================== #

# Accept numeric / POSIXct / Date / character times, returning a numeric axis.
coerce_time <- function(time) {
  if (is.character(time)) {
    time <- as.POSIXct(time, tz = "GMT")
  }
  as.numeric(time)
}

#' Network state of a layer at a time point
#'
#' `r lifecycle::badge("experimental")`
#'
#' Evaluates a stocnet layer's network as the adjacency matrix implied by all of
#' its `time = NA` history rows plus every timed update strictly before `time`,
#' applied under the layer's `info$update` semantics (last `replace` wins;
#' `increment`s aggregate). This is exactly the initial state preprocessing would
#' use for `start_time` at `time`.
#'
#' @param data a `stocnet` object (or a [as_goldfish()]-stamped one).
#' @param layer character string naming the layer.
#' @param time the time point; the returned state reflects updates strictly
#'   before it. Numeric, `POSIXct`, `Date`, or a character timestamp.
#' @param start_time optional lower bound; earlier timed rows fold into the
#'   initial state. `time = NA` history rows are always included.
#'
#' @return a numeric adjacency matrix with `label` dimnames.
#'
#' @seealso [nodes_state_at()]
#' @export
#' @examples
#' nodes <- data.frame(label = c("A", "B", "C"))
#' ties <- data.frame(
#'   from = c(1L, 2L, 3L),
#'   to = c(2L, 3L, 1L),
#'   time = c(NA, 1, 2),
#'   layer = "calls"
#' )
#' info <- list(
#'   focal = "calls", update = c(calls = "increment"),
#'   directed = c(calls = TRUE), observation = c(calls = "event")
#' )
#' x <- list(info = info, nodes = nodes, ties = ties)
#' network_state_at(x, "calls", time = 2)
network_state_at <- function(data, layer, time, start_time = -Inf) {
  validate_goldfish_data(data)
  info <- data$info %||% list()
  nodes <- as.data.frame(data$nodes)
  layers <- unique(data$ties$layer)
  if (!layer %in% layers) {
    cli::cli_abort(c(
      "{.arg layer} must name a layer in {.arg data}.",
      "x" = "{.val {layer}} is not among {.val {layers}}."
    ))
  }
  mode_map <- build_mode_map(info, nodes, layers)
  streams <- split_stocnet_streams(data, mode_map)
  lm <- mode_map$layers[[layer]]
  directed <- isTRUE(unname(info$directed[layer]))

  mat <- materialize_network_state(
    streams$network[[layer]],
    n1 = lm$n1,
    n2 = lm$n2,
    directed = directed,
    start_time = coerce_time(start_time),
    time = coerce_time(time)
  )
  labels <- nodes$label
  dimnames(mat) <- list(labels[lm$side1], labels[lm$side2])
  mat
}

#' Node attribute values at a time point
#'
#' `r lifecycle::badge("experimental")`
#'
#' Returns the `nodes` data frame with each dynamic attribute updated to its
#' value at `time`, folding in all `changes` rows strictly before `time` (last
#' value per node wins). Attributes with no `changes` rows are returned as-is.
#'
#' @inheritParams network_state_at
#' @param time the time point; the returned values reflect changes strictly
#'   before it.
#'
#' @return the `nodes` data frame with dynamic attributes updated to `time`.
#'
#' @seealso [network_state_at()]
#' @export
#' @examples
#' nodes <- data.frame(label = c("A", "B"), gdp = c(1, 2))
#' changes <- data.frame(time = c(1, 3), node = c(1L, 2L), var = "gdp")
#' changes$value <- list(list(10), list(20))
#' info <- list(
#'   focal = "l", update = c(l = "replace"),
#'   directed = c(l = TRUE), observation = c(l = "event")
#' )
#' ties <- data.frame(from = 1L, to = 2L, time = 2, layer = "l")
#' x <- list(info = info, nodes = nodes, ties = ties, changes = changes)
#' nodes_state_at(x, time = 2)
nodes_state_at <- function(data, time, start_time = -Inf) {
  validate_goldfish_data(data)
  nodes <- as.data.frame(data$nodes)
  time_num <- coerce_time(time)
  start_num <- coerce_time(start_time)
  if (is.null(data$changes) || nrow(data$changes) == 0) {
    return(nodes)
  }
  ch <- as.data.frame(data$changes)
  node_global <- to_global_id(ch$node, nodes)
  values <- unwrap_values(ch$value)
  ch_time <- as.numeric(ch$time)
  for (v in unique(ch$var)) {
    if (!v %in% names(nodes)) {
      next
    }
    sel <- ch$var == v
    stream <- data.frame(time = ch_time[sel], node = node_global[sel])
    stream$value <- values[sel]
    nodes[[v]] <- materialize_attribute_state(
      stream,
      nodes[[v]],
      start_time = start_num,
      time = time_num,
      update = "replace"
    )
  }
  nodes
}
