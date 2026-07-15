# =========================================================================== #
# Data-resolution seam for the builder layer.
#
# The builders resolve a formula's object names to a handful of facts: a layer's
# matrix, its direction, whether it spans two modes, a node set's size, an
# attribute vector, and the event streams an object contributes. The legacy
# representation answers all of them with get(name, envir) over an environment
# of goldfish objects; the single data object answers them from its ties/nodes/
# global components plus the mode map.
#
# Both answer through this seam so the builders carry no branch of their own.
# The `data_source_envir` methods exist only for the deprecation cycle in which
# the legacy constructors still produce environments, and are deleted with them
# — the file then holds a single implementation.
#
# The generics are internal, so each method carries @exportS3Method: that emits
# an S3method() directive (no user-visible export). Without registration
# UseMethod() finds a method only lexically from the caller, which breaks the
# moment a generic is called from outside the namespace.
#
# Node sets are identified by NAME rather than carried as data, because the
# builders route attributes by comparing an entry's node set against these
# names.
# =========================================================================== #

#' Build the resolution seam over a data input
#'
#' @param data a validated stocnet object, or `NULL` on the legacy path.
#' @param envir the legacy `data.goldfish` environment, or `NULL`.
#' @param focal focal layer name, defaulting to `info$focal`.
#'
#' @return a `data_source_stocnet` or `data_source_envir` object.
#' @noRd
new_data_source <- function(data = NULL, envir = NULL, focal = NULL) {
  if (is.null(data)) {
    return(structure(
      list(envir = envir),
      class = c("data_source_envir", "data_source")
    ))
  }
  info <- data$info %||% list()
  nodes <- as.data.frame(data$nodes)
  layers <- unique(as.data.frame(data$ties)$layer)
  focal <- focal %||% info$focal
  mode_map <- build_mode_map(info, nodes, layers)
  structure(
    list(
      data = data,
      info = info,
      nodes = nodes,
      layers = layers,
      focal = focal,
      mode_map = mode_map,
      streams = split_stocnet_streams(data, mode_map, focal = focal)
    ),
    class = c("data_source_stocnet", "data_source")
  )
}

# Mode-ness -------------------------------------------------------------------
#
# Resolved once per layer when the mode map is built, rather than re-derived
# downstream by comparing node-set names or counting a network object's `nodes`
# attribute.

#' Does a layer span two modes?
#'
#' @param src a data source.
#' @param name layer / network object name.
#' @noRd
ds_layer_is_two_mode <- function(src, name) UseMethod("ds_layer_is_two_mode")

#' @exportS3Method
ds_layer_is_two_mode.data_source_envir <- function(src, name) {
  length(attr(get(name, envir = src$envir), "nodes")) > 1
}

#' @exportS3Method
ds_layer_is_two_mode.data_source_stocnet <- function(src, name) {
  isTRUE(src$mode_map$layers[[name]]$is_two_mode)
}

#' Is the model two-mode? (the focal layer's side pair)
#'
#' @param src a data source.
#' @param nodes,nodes2 node-set names; the legacy source has no mode map and
#'   answers from them.
#' @noRd
ds_model_is_two_mode <- function(src, nodes = NULL, nodes2 = NULL) {
  UseMethod("ds_model_is_two_mode")
}

#' @exportS3Method
ds_model_is_two_mode.data_source_envir <- function(
  src,
  nodes = NULL,
  nodes2 = NULL
) {
  !identical(nodes, nodes2)
}

#' @exportS3Method
ds_model_is_two_mode.data_source_stocnet <- function(
  src,
  nodes = NULL,
  nodes2 = NULL
) {
  ds_layer_is_two_mode(src, src$focal)
}

# Node-set identifiers of the modeled sides. A one-mode focal layer answers the
# same name for both sides, so a name comparison still resolves both.
ds_side_names <- function(src) UseMethod("ds_side_names")

#' @exportS3Method
ds_side_names.data_source_stocnet <- function(src) {
  if (ds_model_is_two_mode(src)) {
    c("nodes_side1", "nodes_side2")
  } else {
    rep("nodes", 2)
  }
}

# Global node ids making up a node set, in local index order.
ds_side_ids <- function(src, nodeset) UseMethod("ds_side_ids")

#' @exportS3Method
ds_side_ids.data_source_stocnet <- function(src, nodeset) {
  lm <- src$mode_map$layers[[src$focal]]
  if (identical(nodeset, ds_side_names(src)[2]) && isTRUE(lm$is_two_mode)) {
    lm$side2
  } else {
    lm$side1
  }
}

# Networks --------------------------------------------------------------------

#' Resolve a name to its network matrix
#'
#' @param src a data source.
#' @param name layer / network object name.
#' @param start_time,time window bounding the state. The stocnet source
#'   materializes the layer over `[start_time, time)`; the legacy source returns
#'   the object's own already-folded matrix and ignores the window.
#' @noRd
ds_network <- function(src, name, start_time = -Inf, time = Inf) {
  UseMethod("ds_network")
}

#' @exportS3Method
ds_network.data_source_envir <- function(
  src,
  name,
  start_time = -Inf,
  time = Inf
) {
  mat <- get(name, envir = src$envir)
  if (!is.matrix(mat)) {
    cli::cli_abort("Object {.val {name}} must be a matrix network.")
  }
  attributes(mat) <- attributes(mat)[c("dim", "dimnames")]
  mat
}

#' @exportS3Method
ds_network.data_source_stocnet <- function(
  src,
  name,
  start_time = -Inf,
  time = Inf
) {
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

# A layer's direction flag. Vacuous on a two-mode layer (no symmetry concept in
# an n1 x n2 matrix), where the validator notes and ignores the declaration.
ds_is_directed <- function(src, name) UseMethod("ds_is_directed")

#' @exportS3Method
ds_is_directed.data_source_envir <- function(src, name) {
  obj <- get(name, envir = src$envir)
  !inherits(obj, "network.goldfish") || isTRUE(attr(obj, "directed"))
}

#' @exportS3Method
ds_is_directed.data_source_stocnet <- function(src, name) {
  if (ds_layer_is_two_mode(src, name)) {
    return(TRUE)
  }
  isTRUE(unname(src$info$directed[name]))
}

# Nodes and attributes --------------------------------------------------------

ds_n_nodes <- function(src, nodeset) UseMethod("ds_n_nodes")

#' @exportS3Method
ds_n_nodes.data_source_envir <- function(src, nodeset) {
  nrow(get(nodeset, envir = src$envir))
}

#' @exportS3Method
ds_n_nodes.data_source_stocnet <- function(src, nodeset) {
  length(ds_side_ids(src, nodeset))
}

# Does `nodeset` name the global-attribute container rather than a node set?
ds_is_global <- function(src, nodeset) UseMethod("ds_is_global")

#' @exportS3Method
ds_is_global.data_source_envir <- function(src, nodeset) {
  inherits(get(nodeset, envir = src$envir), "global.goldfish")
}

#' @exportS3Method
ds_is_global.data_source_stocnet <- function(src, nodeset) {
  !is.null(src$data$global) &&
    nodeset %in% unique(as.data.frame(src$data$global)$var)
}

# An attribute's initial vector, sliced to the node set's rows.
ds_attribute <- function(src, nodeset, attribute) UseMethod("ds_attribute")

#' @exportS3Method
ds_attribute.data_source_envir <- function(src, nodeset, attribute) {
  get(nodeset, envir = src$envir)[[attribute]]
}

#' @exportS3Method
ds_attribute.data_source_stocnet <- function(src, nodeset, attribute) {
  if (ds_is_global(src, nodeset)) {
    g <- as.data.frame(src$data$global)
    vals <- unwrap_values(g$value)[g$var == nodeset]
    return(vals[length(vals)])
  }
  src$nodes[[attribute]][ds_side_ids(src, nodeset)]
}

# Event streams ---------------------------------------------------------------

# Event-stream keys a layer or attribute contributes to the fetch plan. The
# legacy path names one stream per linked event object; the stocnet path carries
# exactly one stream per layer / per attribute variable (conversion already
# merged the rows), keyed by the layer or variable name.
ds_object_streams <- function(src, name) UseMethod("ds_object_streams")

#' @exportS3Method
ds_object_streams.data_source_envir <- function(src, name) {
  attr(get(name, envir = src$envir), "events")
}

#' @exportS3Method
ds_object_streams.data_source_stocnet <- function(src, name) {
  if (name %in% src$layers) {
    stream <- src$streams$network[[name]]
    return(if (any(!is.na(stream$time))) name else character(0))
  }
  if (name %in% names(src$streams$attribute)) name else character(0)
}

#' Fetch one event stream in the shape the recipe loop's walk consumes
#'
#' Numeric time, local integer indices, and a single value column named for the
#' layer's update semantics. Node references are already remapped by the
#' conversion module, so these tables need no sanitizing. Ordering runs on the
#' stream's own columns (the deterministic sort key, including the
#' replace-ambiguity abort) before renaming, and `time = NA` history belongs to
#' the initial state, so it never enters the schedule.
#'
#' @param src a data source.
#' @param key a stream key from [ds_object_streams()].
#' @noRd
ds_fetch_stream <- function(src, key) UseMethod("ds_fetch_stream")

#' @exportS3Method
ds_fetch_stream.data_source_envir <- function(src, key) {
  get(key, envir = src$envir)
}

#' @exportS3Method
ds_fetch_stream.data_source_stocnet <- function(src, key) {
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
