# =========================================================================== #
# Initial-state materializer.
#
# The vectorized update engine (methods_update.R: dedup-last replace, tapply
# increment aggregation -- no event loop) is generalized here to work on the
# component streams from split_stocnet_streams() rather than on goldfish
# objects.
# Given a stream and a [start_time, time) window it produces the state matrix /
# attribute vector in one vectorized pass. `time = NA` history rows always fold
# into the initial state (they precede every timed event); timed rows before
# start_time fold in too.
#
# Chunked-parallel preprocessing seam (documented, not built): because the fold
# is vectorized and associative per window, a long stream can be split at time
# points and each chunk's initial state materialized independently from the
# prior chunk's end state -- the entry point a future parallel preprocessing
# change would use. No machinery is built here.
# =========================================================================== #

# Rows active in [start_time, time): NA-time history always, plus timed rows in
# the half-open window.
in_window <- function(time, start_time, end_time) {
  is.na(time) | (time >= start_time & time < end_time)
}

# Sort so NA-time history precedes timed rows, then ascending time.
history_first_order <- function(time) {
  order(!is.na(time), time)
}

#' Materialize a network layer's state matrix over [start_time, time)
#'
#' @param stream a network stream (`time`, `from`, `to`, `value`, `update`) with
#'   local indices, from [split_stocnet_streams()].
#' @param n1,n2 row/column dimensions (from the mode map).
#' @param directed whether the layer is directed; when `FALSE`, updates mirror
#'   across the diagonal.
#' @param start_time,time half-open observation window `[start_time, time)`.
#'
#' @return an `n1 x n2` numeric matrix.
#' @noRd
materialize_network_state <- function(
  stream,
  n1,
  n2,
  directed = TRUE,
  start_time = -Inf,
  time = Inf
) {
  mat <- matrix(0, n1, n2)
  if (is.null(stream) || nrow(stream) == 0) {
    return(mat)
  }
  keep <- in_window(stream$time, start_time, time)
  s <- stream[keep, , drop = FALSE]
  if (nrow(s) == 0) {
    return(mat)
  }
  update <- s$update[1]
  if (identical(update, "replace")) {
    s <- s[history_first_order(s$time), , drop = FALSE]
    last <- !duplicated(s[c("from", "to")], fromLast = TRUE)
    d <- s[last, , drop = FALSE]
    mat[cbind(d$from, d$to)] <- d$value
    if (!directed) {
      mat[cbind(d$to, d$from)] <- d$value
    }
  } else {
    key <- paste(s$from, s$to, sep = ",")
    totals <- tapply(s$value, key, sum)
    parts <- do.call(rbind, strsplit(names(totals), ",", fixed = TRUE))
    fr <- as.integer(parts[, 1])
    to <- as.integer(parts[, 2])
    mat[cbind(fr, to)] <- mat[cbind(fr, to)] + as.numeric(totals)
    if (!directed) {
      mat[cbind(to, fr)] <- mat[cbind(to, fr)] + as.numeric(totals)
    }
  }
  mat
}

#' Materialize an attribute vector over [start_time, time)
#'
#' @param stream an attribute / composition stream (`time`, `node`, `value`).
#' @param initial the attribute's initial vector (e.g. a `nodes` column).
#' @param start_time,time half-open observation window.
#' @param update `"replace"` (last value per node wins) or `"increment"`.
#'
#' @return the updated attribute vector.
#' @noRd
materialize_attribute_state <- function(
  stream,
  initial,
  start_time = -Inf,
  time = Inf,
  update = "replace"
) {
  vals <- initial
  if (is.null(stream) || nrow(stream) == 0) {
    return(vals)
  }
  keep <- in_window(stream$time, start_time, time)
  s <- stream[keep, , drop = FALSE]
  if (nrow(s) == 0) {
    return(vals)
  }
  scalars <- if (is.list(s$value)) {
    unlist(s$value, use.names = FALSE)
  } else {
    s$value
  }
  if (identical(update, "replace")) {
    o <- history_first_order(s$time)
    s <- s[o, , drop = FALSE]
    scalars <- scalars[o]
    last <- !duplicated(s$node, fromLast = TRUE)
    vals[s$node[last]] <- scalars[last]
  } else {
    totals <- tapply(scalars, s$node, sum)
    idx <- as.integer(names(totals))
    vals[idx] <- vals[idx] + as.numeric(totals)
  }
  vals
}

# Observation window: explicit start_time/end_time, else the focal
# dependent-event span. end_time is inclusive of the last dependent event, so
# the materializer's half-open [start, time) is called with time = end_time for
# strict "state before t".
resolve_observation_window <- function(
  streams,
  start_time = NULL,
  end_time = NULL
) {
  dep <- streams$dependent
  span <- if (!is.null(dep) && nrow(dep) > 0) {
    range(dep$time, na.rm = TRUE)
  } else {
    c(-Inf, Inf)
  }
  list(
    start_time = start_time %||% span[1],
    end_time = end_time %||% span[2]
  )
}
