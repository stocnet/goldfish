################################## ###
#
# Goldfish package
#
# S3 update methods: as.data.frame.nodes.goldfish, as.matrix.network.goldfish
#
################################## ###

#' Methods to update a nodes or network object
#'
#' Methods to create a data frame from an object of class `nodes.goldfish`
#' (see [make_nodes()]) or a matrix from an object of class
#' `network.goldfish` (see [make_network()]) with the attributes
#' or the network ties updated according with the events linked to the object
#' using the [link_events()]) function.
#' @param x an object of class `nodes.goldfish` for `as.data.frame()`
#' method or `network.goldfish` for `as.matrix()` method.
#' @param time a numeric value or a calendar date value (see [as.Date()])
#' to update the state of the object `x` until this time value
#' (event time < time).
#' @param startTime a numeric `as.Date` format value; prior events are
#' disregarded.
#' @param ... Not further arguments are required.
#' @param envir an `environment` where the nodes and linked events
#'   objects are available.
#' @return The respective object updated accordingly to the events link to it.
#' For `nodes.goldfish` object the attributes are updated according to the
#' events linked to them.
#' For `network.goldfish` object the network ties are updated according to the
#' events linked to it.
#' @seealso [network_state_at()] and [nodes_state_at()], which supersede these
#'   methods for the single stocnet data object; [make_network()], [make_nodes()],
#'   [link_events()]
#' @examples
#' # These S3 methods act on the legacy `nodes.goldfish` / `network.goldfish`
#' # objects. For the single stocnet data object, evaluate a layer's network or
#' # the node attributes at a time point with the superseding helpers:
#' data("fisheries_treaties")
#' update_net <- network_state_at(
#'   fisheries_treaties, "treaties",
#'   time = as.POSIXct("1965-12-31", tz = "GMT")
#' )
#' update_states <- nodes_state_at(
#'   fisheries_treaties,
#'   time = as.POSIXct("1965-12-31", tz = "GMT")
#' )
#'
#' @name update-method
NULL

#' @export
#' @rdname update-method
as.data.frame.nodes.goldfish <- function(
  x,
  ...,
  time = -Inf,
  startTime = -Inf,
  envir = new.env()
) {
  df <- x
  dynamic_attributes <- attr(df, "dynamic_attributes")
  eventNames <- attr(df, "events")
  if (is.character(time)) {
    time <- as.POSIXct(time)
  }
  time <- as.numeric(time)
  startTime <- as.numeric(startTime)
  if (length(eventNames) == 0) {
    return(df)
  }
  for (i in seq_along(eventNames)) {
    events <- get(eventNames[i], envir = envir)
    events <- sanitizeEvents(events, df, envir = envir)
    events <- events[events$time >= startTime & events$time < time, ]
    if (nrow(events) == 0) {
      next
    }

    has_replace <- !is.null(events$replace)
    has_increment <- !is.null(events$increment)

    if (has_replace && has_increment) {
      cli::cli_abort(c(
        "x" = "Event list {.val {eventNames[i]}} contains both {.field replace}
               and {.field increment} columns.",
        "i" = "An event list may only update attribute values by replacement
               or by increment, not both simultaneously."
      ))
    }

    if (has_replace) {
      df[[dynamic_attributes[i]]][events$node] <- events$replace
    }

    if (has_increment) {
      totals <- tapply(events$increment, events$node, sum)
      nodes_idx <- as.integer(names(totals))
      df[[dynamic_attributes[i]]][nodes_idx] <-
        df[[dynamic_attributes[i]]][nodes_idx] + as.numeric(totals)
    }
  }
  df
}

#' @export
#' @rdname update-method
as.matrix.network.goldfish <- function(
  x,
  ...,
  time = -Inf,
  startTime = -Inf,
  envir = new.env()
) {
  net <- x
  if (is.character(time)) {
    time <- as.POSIXct(time)
  }
  time <- as.numeric(time)
  startTime <- as.numeric(startTime)
  dim_net <- dim(net)
  isDirected <- attr(net, "directed")
  eventNames <- attr(net, "events")
  nodeNames <- attr(net, "nodes")
  nodes <- nodeNames[1]
  nodes2 <- if (length(nodeNames) == 2) nodeNames[2] else nodes
  if (is.null(eventNames)) {
    return(net[1:dim_net[1], 1:dim_net[2]])
  }

  events <- lapply(
    lapply(eventNames, get, envir = envir),
    sanitizeEvents,
    nodes = nodes,
    nodes2 = nodes2,
    envir = envir
  )

  # merge all event lists and restrict to window once
  all_events <- do.call(
    rbind,
    lapply(events, function(ev) {
      ev[ev$time >= startTime & ev$time < time, , drop = FALSE]
    })
  )

  if (is.null(all_events) || nrow(all_events) == 0) {
    return(net[1:dim_net[1], 1:dim_net[2]])
  }

  has_replace <- !is.null(all_events$replace)
  has_increment <- !is.null(all_events$increment)

  if (has_replace && has_increment) {
    cli::cli_abort(c(
      "x" = "One or more event lists linked to this network contain both
             {.field replace} and {.field increment} columns.",
      "i" = "An event list may only update network ties by replacement
             or by increment, not both simultaneously."
    ))
  }

  if (has_replace) {
    all_events <- all_events[order(all_events$time), ]
    last <- !duplicated(all_events[c("sender", "receiver")], fromLast = TRUE)
    df_rep <- all_events[last, ]
    net[cbind(df_rep$sender, df_rep$receiver)] <- df_rep$replace
    if (!isDirected) {
      net[cbind(df_rep$receiver, df_rep$sender)] <- df_rep$replace
    }
  }

  if (has_increment) {
    key <- paste(all_events$sender, all_events$receiver, sep = ",")
    totals <- tapply(all_events$increment, key, sum)
    parts <- do.call(rbind, strsplit(names(totals), ","))
    s <- as.integer(parts[, 1])
    r <- as.integer(parts[, 2])
    net[cbind(s, r)] <- net[cbind(s, r)] + as.numeric(totals)
    if (!isDirected) {
      net[cbind(r, s)] <- net[cbind(r, s)] + as.numeric(totals)
    }
  }

  net[1:dim_net[1], 1:dim_net[2]]
}
