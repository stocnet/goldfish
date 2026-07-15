# =========================================================================== #
# Data-resolution seam for the builder layer.
#
# The builders need four things from a formula's object names: a layer's matrix,
# a node set's rows, an attribute vector, and a layer's direction flag. The
# legacy representation answers all four with get(name, envir) over an
# environment of goldfish objects; the single data object answers them from its
# ties/nodes/global components.
#
# Both answer through this seam so the builders carry no branch of their own.
# The environment implementation exists only for the deprecation cycle in which
# the legacy constructors still produce environments, and is removed with them —
# each resolver's `is_stocnet` branch is then the whole function.
#
# Plain branching rather than S3: the generics would be internal, so every
# method would need an explicit S3method() registration to dispatch under
# load_all(), and half of them get deleted with the legacy path anyway.
#
# Node sets are identified by NAME, not carried as data: downstream code decides
# one-mode by identical(nodes, nodes2) and routes attributes by comparing an
# entry's node set against those names. The stocnet source keeps that
# contract by naming the focal layer's two sides, so a one-mode layer names
# both sides identically and the existing comparisons keep deciding correctly.
# =========================================================================== #

# Side identifiers for a stocnet-backed source. A one-mode layer answers the
# same name for both sides.
stocnet_side_names <- function(mode_map, focal) {
  lm <- mode_map$layers[[focal]]
  if (isTRUE(lm$is_two_mode)) {
    c("nodes_side1", "nodes_side2")
  } else {
    rep("nodes", 2)
  }
}

#' Build the resolution seam over a data input
#'
#' @param data a validated stocnet object, or `NULL` on the legacy path.
#' @param envir the legacy `data.goldfish` environment, or `NULL`.
#' @param focal focal layer name, defaulting to `info$focal`.
#'
#' @return a `data_source` object.
#' @noRd
new_data_source <- function(data = NULL, envir = NULL, focal = NULL) {
  if (is.null(data)) {
    return(structure(
      list(is_stocnet = FALSE, envir = envir),
      class = "data_source"
    ))
  }
  info <- data$info %||% list()
  nodes <- as.data.frame(data$nodes)
  layers <- unique(as.data.frame(data$ties)$layer)
  focal <- focal %||% info$focal
  mode_map <- build_mode_map(info, nodes, layers)
  structure(
    list(
      is_stocnet = TRUE,
      data = data,
      info = info,
      nodes = nodes,
      layers = layers,
      focal = focal,
      mode_map = mode_map,
      streams = split_stocnet_streams(data, mode_map, focal = focal),
      side_names = stocnet_side_names(mode_map, focal)
    ),
    class = "data_source"
  )
}

# Node-set names of the modeled sides: the focal layer's side pair (D7).
ds_side_names <- function(src) {
  src$side_names
}

# Global node ids making up a node set, in local index order.
ds_side_ids <- function(src, nodeset) {
  lm <- src$mode_map$layers[[src$focal]]
  if (identical(nodeset, src$side_names[2]) && isTRUE(lm$is_two_mode)) {
    lm$side2
  } else {
    lm$side1
  }
}

#' Resolve a name to its network matrix
#'
#' @param src a data source.
#' @param name layer / network object name.
#' @param start_time,time window bounding the initial state. The stocnet source
#'   materializes the layer's state over `[start_time, time)`; the legacy source
#'   returns the object's own already-folded matrix and ignores the window.
#' @noRd
ds_network <- function(src, name, start_time = -Inf, time = Inf) {
  if (!src$is_stocnet) {
    mat <- get(name, envir = src$envir)
    if (!is.matrix(mat)) {
      cli::cli_abort("Object {.val {name}} must be a matrix network.")
    }
    attributes(mat) <- attributes(mat)[c("dim", "dimnames")]
    return(mat)
  }
  lm <- src$mode_map$layers[[name]]
  mat <- materialize_network_state(
    src$streams$network[[name]],
    n1 = lm$n1,
    n2 = lm$n2,
    directed = ds_is_directed(src, name),
    start_time = start_time,
    time = time
  )
  labels <- src$nodes$label
  dimnames(mat) <- list(labels[lm$side1], labels[lm$side2])
  mat
}

# A layer's direction flag. Vacuous on two-mode layers (no symmetry concept in
# an n1 x n2 matrix), where the validator notes and ignores the declaration.
ds_is_directed <- function(src, name) {
  if (!src$is_stocnet) {
    obj <- get(name, envir = src$envir)
    return(!inherits(obj, "network.goldfish") || isTRUE(attr(obj, "directed")))
  }
  lm <- src$mode_map$layers[[name]]
  if (isTRUE(lm$is_two_mode)) {
    return(TRUE)
  }
  isTRUE(unname(src$info$directed[name]))
}

# Number of rows of a node set.
ds_n_nodes <- function(src, nodeset) {
  if (!src$is_stocnet) {
    return(nrow(get(nodeset, envir = src$envir)))
  }
  length(ds_side_ids(src, nodeset))
}

# Does `nodeset` name the global-attribute container rather than a node set?
ds_is_global <- function(src, nodeset) {
  if (!src$is_stocnet) {
    return(inherits(get(nodeset, envir = src$envir), "global.goldfish"))
  }
  !is.null(src$data$global) &&
    nodeset %in% unique(as.data.frame(src$data$global)$var)
}

# Event-stream keys a layer or attribute contributes to the fetch plan.
#
# The legacy path names one stream per linked event object; the stocnet path
# carries exactly one stream per layer / per attribute variable (the conversion
# already merged and ordered the rows), keyed by the layer or variable name.
ds_object_streams <- function(src, name) {
  if (!src$is_stocnet) {
    return(attr(get(name, envir = src$envir), "events"))
  }
  if (name %in% src$layers) {
    stream <- src$streams$network[[name]]
    return(if (any(!is.na(stream$time))) name else character(0))
  }
  if (name %in% names(src$streams$attribute)) name else character(0)
}

# Translate one converted stream into the event-table shape the recipe loop's
# multi-stream walk consumes (D15): numeric time, local integer indices, and a
# single increment/replace value column named for the layer's update semantics.
# Node references are already remapped by the conversion module, so these tables
# need no sanitizing. Ordering happens on the stream's own columns (the D2 key,
# including the replace-ambiguity abort) before renaming to the event shape, and
# NA-time history belongs to the initial state, so it never enters the schedule.
ds_fetch_stream <- function(src, key) {
  if (!src$is_stocnet) {
    return(get(key, envir = src$envir))
  }
  if (key %in% src$layers) {
    stream <- src$streams$network[[key]]
    timed <- order_events(stream[!is.na(stream$time), , drop = FALSE])
    events <- data.frame(
      time = timed$time,
      sender = timed$from,
      receiver = timed$to,
      stringsAsFactors = FALSE
    )
    events[[tie_value_column(src, key)]] <- timed$value
    return(events)
  }
  stream <- src$streams$attribute[[key]]
  timed <- order_events(stream[!is.na(stream$time), , drop = FALSE])
  data.frame(
    time = timed$time,
    node = timed$node,
    replace = unlist(timed$value, use.names = FALSE),
    stringsAsFactors = FALSE
  )
}

# The value column a layer's events carry, named for its update semantics.
tie_value_column <- function(src, layer) {
  if (identical(unname(src$info$update[layer]), "replace")) {
    "replace"
  } else {
    "increment"
  }
}

# An attribute's initial vector, sliced to the node set's rows.
ds_attribute <- function(src, nodeset, attribute) {
  if (!src$is_stocnet) {
    return(get(nodeset, envir = src$envir)[[attribute]])
  }
  if (ds_is_global(src, nodeset)) {
    g <- as.data.frame(src$data$global)
    vals <- unwrap_values(g$value)[g$var == nodeset]
    return(vals[length(vals)])
  }
  src$nodes[[attribute]][ds_side_ids(src, nodeset)]
}
