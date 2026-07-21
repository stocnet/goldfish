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

# The fetch-plan key of the dependent event stream on the stocnet path. Legacy
# keys it by the dependent-events object's name, which is always distinct from
# the network it updates; the focal layer is both, so it needs a reserved key.
# See ds_dependent_key().
DEPENDENT_STREAM <- ".dependent"

#' Build the resolution seam over a data input
#'
#' @param data a validated stocnet object, or `NULL` on the legacy path.
#' @param envir the legacy `data.goldfish` environment, or `NULL`.
#' @param focal focal layer name, defaulting to `info$focal`.
#' @param modeled_flavor optional `ties$flavor` value(s) selecting which focal
#'   rows are modeled; the rest update state only.
#'
#' @return a `data_source_stocnet` or `data_source_envir` object.
#' @noRd
new_data_source <- function(
  data = NULL,
  envir = NULL,
  focal = NULL,
  modeled_flavor = NULL
) {
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
      modeled_flavor = modeled_flavor,
      mode_map = mode_map,
      streams = split_stocnet_streams(
        data,
        mode_map,
        focal = focal,
        modeled_flavor = modeled_flavor
      ),
      # Derived (windowed) layers and the imputed values that shadow them are
      # registered on the source itself: the legacy path realizes and imputes by
      # assigning into its environment, and the stocnet path has none to mutate.
      derived = list(),
      derived_streams = list(),
      net_override = list(),
      att_override = list()
    ),
    class = c("data_source_stocnet", "data_source")
  )
}

# Do this source's event streams still carry node labels? Legacy streams are
# user-supplied and may name nodes by label; the conversion module already
# remapped stocnet references to local indices.
ds_needs_sanitize <- function(src) UseMethod("ds_needs_sanitize")

#' @exportS3Method
ds_needs_sanitize.data_source_envir <- function(src) TRUE

#' @exportS3Method
ds_needs_sanitize.data_source_stocnet <- function(src) FALSE

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
  isTRUE(ds_layer_map(src, name)$is_two_mode)
}

# A layer's mode pair as the mode names of each side, for messages that must
# name what the data actually says (e.g. "actors -> clubs"). Answers NULL where
# no mode map exists, so callers fall back to naming the layer alone.
ds_layer_mode_pair <- function(src, name) UseMethod("ds_layer_mode_pair")

#' @exportS3Method
ds_layer_mode_pair.data_source_envir <- function(src, name) NULL

#' @exportS3Method
ds_layer_mode_pair.data_source_stocnet <- function(src, name) {
  lm <- ds_layer_map(src, name)
  if (is.null(lm)) {
    return(NULL)
  }
  modes <- src$mode_map$nodes_lookup$mode
  if (all(is.na(modes))) {
    return(NULL)
  }
  list(
    sender = unique(modes[lm$side1]),
    receiver = unique(modes[lm$side2])
  )
}

# A derived (windowed) layer inherits its structural metadata -- dimensions,
# sides, direction -- from the layer it was derived from, so both resolve
# through the same map entry.
ds_layer_map <- function(src, name) {
  if (!is.null(src$derived[[name]])) {
    name <- src$derived[[name]]$source
  }
  src$mode_map$layers[[name]]
}

#' Is the network an effect reads two-mode?
#'
#' The parser resolves this per effect to auto-set the `is_two_mode` formal.
#' `value` is only forced on the legacy path, which has to evaluate the
#' argument to read the object's node sets; the stocnet path answers from the
#' mode map, which resolved it once per layer.
#'
#' @param src a data source.
#' @param name the network argument as written in the formula, or `NULL`.
#' @param value the evaluated network argument (lazy).
#' @noRd
ds_arg_is_two_mode <- function(src, name, value) UseMethod("ds_arg_is_two_mode")

#' @exportS3Method
ds_arg_is_two_mode.data_source_envir <- function(src, name, value) {
  length(attr(value, "nodes")) > 1
}

#' @exportS3Method
ds_arg_is_two_mode.data_source_stocnet <- function(src, name, value) {
  # A `list(a, b)` argument holds several layers and has no single mode-ness;
  # the legacy object carries no node sets there either, so both read one-mode.
  if (is.null(name) || grepl("^list\\(", name)) {
    return(FALSE)
  }
  isTRUE(ds_layer_is_two_mode(src, name))
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
# The last node-set-name comparison, and now unreachable from make_data(): every
# assemblable legacy bundle becomes a stocnet, so nothing mints the environment
# this method serves. Kept until the DyNAMi engine stops reading the envir seam,
# so that seam is removed in one pass rather than dismantled piecemeal.
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
#' This is the state the walk STARTS from, which is the layer's history alone:
#' the recipe replays every timed event from the beginning of the schedule, so
#' folding timed rows in here would count them twice. Legacy returns the
#' object's own matrix, whose events were never folded in either; the stocnet
#' source returns its `time = NA` rows, which is what a legacy constructor's
#' matrix becomes. Events before `start_time` are folded by the loop's own
#' start-time logic, not here.
#' @noRd
ds_network <- function(src, name) UseMethod("ds_network")

#' @exportS3Method
ds_network.data_source_envir <- function(src, name) {
  mat <- get(name, envir = src$envir)
  if (!is.matrix(mat)) {
    cli::cli_abort("Object {.val {name}} must be a matrix network.")
  }
  attributes(mat) <- attributes(mat)[c("dim", "dimnames")]
  mat
}

#' @exportS3Method
ds_network.data_source_stocnet <- function(src, name) {
  if (!is.null(src$net_override[[name]])) {
    return(src$net_override[[name]])
  }
  lm <- ds_layer_map(src, name)
  labels <- src$nodes$label
  # A derived layer starts empty: its rows are the dissolve pseudo-events the
  # window emits, so there is no state to materialize from the source stream.
  mat <- if (!is.null(src$derived[[name]])) {
    matrix(0, nrow = lm$n1, ncol = lm$n2)
  } else {
    stream <- src$streams$network[[name]]
    materialize_network_state(
      stream[is.na(stream$time), , drop = FALSE],
      n1 = lm$n1,
      n2 = lm$n2,
      directed = ds_is_directed(src, name)
    )
  }
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
  if (!is.null(src$derived[[name]])) {
    name <- src$derived[[name]]$source
  }
  isTRUE(unname(src$info$directed[name]))
}

# The node-set identifiers a layer's two sides carry. Consumed by the fetch
# plan, which records per-stream node sets to resolve labels against.
ds_layer_sides <- function(src, name) UseMethod("ds_layer_sides")

#' @exportS3Method
ds_layer_sides.data_source_envir <- function(src, name) {
  attr(get(name, envir = src$envir), "nodes")
}

#' @exportS3Method
ds_layer_sides.data_source_stocnet <- function(src, name) {
  if (ds_layer_is_two_mode(src, name)) {
    c("nodes_side1", "nodes_side2")
  } else {
    "nodes"
  }
}

# Nodes and attributes --------------------------------------------------------

#' A node set's rows, as the data frame estimation reports on
#'
#' @param src a data source.
#' @param nodeset a node-set name.
#' @noRd
ds_nodes_frame <- function(src, nodeset) UseMethod("ds_nodes_frame")

#' @exportS3Method
ds_nodes_frame.data_source_envir <- function(src, nodeset) {
  get(nodeset, envir = src$envir)
}

#' @exportS3Method
ds_nodes_frame.data_source_stocnet <- function(src, nodeset) {
  src$nodes[ds_side_ids(src, nodeset), , drop = FALSE]
}

ds_n_nodes <- function(src, nodeset) UseMethod("ds_n_nodes")

#' @exportS3Method
ds_n_nodes.data_source_envir <- function(src, nodeset) {
  nrow(get(nodeset, envir = src$envir))
}

#' @exportS3Method
ds_n_nodes.data_source_stocnet <- function(src, nodeset) {
  length(ds_side_ids(src, nodeset))
}

# The (side, local index, global id, label) lookup for the focal layer's modeled
# sides, attached to results/exports so their index_i/index_j local indices
# resolve back to the original nodes row and label without re-deriving the mode
# map. The legacy environment carries no mode map -- its identity is the node
# frame itself -- so it exposes none.
ds_node_lookup <- function(src) UseMethod("ds_node_lookup")

#' @exportS3Method
ds_node_lookup.data_source_envir <- function(src) NULL

#' @exportS3Method
ds_node_lookup.data_source_stocnet <- function(src) {
  layer_node_lookup(src$mode_map, src$focal)
}

# Does `nodeset` name the global-attribute container rather than a node set?
ds_is_global <- function(src, nodeset) UseMethod("ds_is_global")

#' @exportS3Method
ds_is_global.data_source_envir <- function(src, nodeset) {
  inherits(get(nodeset, envir = src$envir), "global.goldfish")
}

#' @exportS3Method
ds_is_global.data_source_stocnet <- function(src, nodeset) {
  identical(nodeset, GLOBAL_NODESET)
}

# A global variable's initial value: its history rows, for the same reason
# ds_network() takes only history -- the timed rows are replayed by the walk.
# Globals carry no node reference, so the attribute materializer's per-node
# dedup does not apply: under `replace` semantics the last history row wins.
# A global with no history has no value until its first event, which is an NA
# of the variable's own type.
ds_global_value <- function(src, var) {
  stream <- src$streams$global[[var]]
  if (is.null(stream) || nrow(stream) == 0) {
    return(NULL)
  }
  values <- unlist(stream$value, use.names = FALSE)
  history <- which(is.na(stream$time))
  if (length(history) == 0) {
    return(values[NA_integer_])
  }
  values[history[length(history)]]
}

# An attribute's initial vector, sliced to the node set's rows.
ds_attribute <- function(src, nodeset, attribute) UseMethod("ds_attribute")

#' @exportS3Method
ds_attribute.data_source_envir <- function(src, nodeset, attribute) {
  get(nodeset, envir = src$envir)[[attribute]]
}

#' @exportS3Method
ds_attribute.data_source_stocnet <- function(src, nodeset, attribute) {
  key <- att_override_key(nodeset, attribute)
  if (!is.null(src$att_override[[key]])) {
    return(src$att_override[[key]])
  }
  if (ds_is_global(src, nodeset)) {
    return(ds_global_value(src, attribute))
  }
  src$nodes[[attribute]][ds_side_ids(src, nodeset)]
}

att_override_key <- function(nodeset, attribute) {
  paste(nodeset, attribute, sep = "$")
}

#' Resolve a data-object table to the objects themselves
#'
#' The values behind an object table's rows, in row order: a matrix per network
#' entry, a vector per attribute entry. Effect-cache initialization classifies
#' them by class, so both paths return bare matrices and vectors.
#'
#' @param src a data source.
#' @param obj_table a `get_data_objects()` table.
#' @noRd
ds_objects_from_table <- function(src, obj_table) {
  UseMethod("ds_objects_from_table")
}

#' @exportS3Method
ds_objects_from_table.data_source_envir <- function(src, obj_table) {
  get_element_from_data_object_table(obj_table, envir = src$envir)
}

#' Does a formula object name resolve?
#'
#' @param src a data source.
#' @param name an object name as written in a formula.
#' @noRd
ds_object_exists <- function(src, name) UseMethod("ds_object_exists")

#' @exportS3Method
ds_object_exists.data_source_envir <- function(src, name) {
  exists(name, envir = src$envir)
}

#' @exportS3Method
ds_object_exists.data_source_stocnet <- function(src, name) {
  # Names were resolved against the components before this point, and an
  # unresolvable one already aborted listing the candidates.
  name %in% src$layers || !is.null(src$derived[[name]])
}

#' Which of an object table's rows are networks?
#'
#' @param src a data source.
#' @param obj_table a `get_data_objects()` table.
#' @noRd
ds_table_is_network <- function(src, obj_table) {
  UseMethod("ds_table_is_network")
}

#' @exportS3Method
ds_table_is_network.data_source_envir <- function(src, obj_table) {
  vapply(
    ds_objects_from_table(src, obj_table),
    FUN = inherits,
    FUN.VALUE = logical(1),
    what = "network.goldfish"
  )
}

#' @exportS3Method
ds_table_is_network.data_source_stocnet <- function(src, obj_table) {
  # A layer reference fills the table's `object` slot; an attribute reference
  # fills `nodeset`/`attribute` instead. The resolver already settled which is
  # which, so the shape of the row answers this without reading any data.
  !is.na(obj_table$object)
}

#' @exportS3Method
ds_objects_from_table.data_source_stocnet <- function(src, obj_table) {
  lapply(seq_len(nrow(obj_table)), function(i) {
    entry <- obj_table[i, ]
    if (!is.na(entry$object)) {
      ds_network(src, entry$object)
    } else {
      ds_attribute(src, entry$nodeset, entry$attribute)
    }
  })
}

# Event-stream keys carrying a nodal or global attribute's changes over time.
ds_attribute_streams <- function(src, nodeset, attribute) {
  UseMethod("ds_attribute_streams")
}

#' @exportS3Method
ds_attribute_streams.data_source_envir <- function(src, nodeset, attribute) {
  obj <- get(nodeset, envir = src$envir)
  if (inherits(obj, "global.goldfish")) {
    return(attr(obj, "events") %||% character(0))
  }
  streams <- attr(obj, "events")
  streams[which(attr(obj, "dynamic_attributes") == attribute)]
}

#' @exportS3Method
ds_attribute_streams.data_source_stocnet <- function(src, nodeset, attribute) {
  pool <- if (ds_is_global(src, nodeset)) {
    names(src$streams$global)
  } else {
    names(src$streams$attribute)
  }
  if (attribute %in% pool) attribute else character(0)
}

# The dependent process ------------------------------------------------------

#' Check that a formula's dependent name designates a dependent process
#'
#' @param src a data source.
#' @param dep_name the formula's left-hand-side name.
#' @param call calling environment for error reporting.
#' @noRd
ds_check_dependent <- function(src, dep_name, call = rlang::caller_env()) {
  UseMethod("ds_check_dependent")
}

#' @exportS3Method
ds_check_dependent.data_source_envir <- function(
  src,
  dep_name,
  call = rlang::caller_env()
) {
  obj <- tryCatch(get(dep_name, envir = src$envir), error = function(e) NULL)
  if (!inherits(obj, "dependent.goldfish")) {
    stop(
      "The left hand side of the formula should contain dependent events",
      " (check the function 'make_dependent_events()').",
      call. = FALSE
    )
  }
  invisible(dep_name)
}

#' @exportS3Method
ds_check_dependent.data_source_stocnet <- function(
  src,
  dep_name,
  call = rlang::caller_env()
) {
  if (!dep_name %in% src$layers) {
    cli::cli_abort(
      c(
        "The dependent process {.val {dep_name}} is not a layer of the data.",
        "i" = "Available layer{?s}: {.val {src$layers}}."
      ),
      call = call
    )
  }
  invisible(dep_name)
}

#' The network a dependent process updates by default
#'
#' Effects written without an object (`inertia`) read it. A stocnet focal layer
#' is its own default network: the events and the ties they update are the same
#' rows, which is exactly what the legacy pairing of a dependent-events object
#' with its `default_network` encoded.
#'
#' @inheritParams ds_check_dependent
#' @noRd
ds_default_network <- function(src, dep_name) UseMethod("ds_default_network")

#' @exportS3Method
ds_default_network.data_source_envir <- function(src, dep_name) {
  attr(get(dep_name, envir = src$envir), "default_network")
}

#' @exportS3Method
ds_default_network.data_source_stocnet <- function(src, dep_name) {
  dep_name
}

#' The fetch-plan key of the dependent event stream
#'
#' Legacy names the dependent-events object and the network it updates
#' differently, so the two streams already have distinct keys. On stocnet both
#' are the focal layer, but they are not the same rows -- the dependent stream
#' carries only the modeled rows, while the network stream carries every timed
#' row, all of which update state. A reserved key keeps them apart.
#'
#' @inheritParams ds_check_dependent
#' @noRd
ds_dependent_key <- function(src, dep_name) UseMethod("ds_dependent_key")

#' @exportS3Method
ds_dependent_key.data_source_envir <- function(src, dep_name) dep_name

#' @exportS3Method
ds_dependent_key.data_source_stocnet <- function(src, dep_name) {
  DEPENDENT_STREAM
}

# Composition -----------------------------------------------------------------

#' A node set's composition: who is present initially, and when that changes
#'
#' @param src a data source.
#' @param nodeset a node-set name.
#' @param n the node set's size, used when no composition is recorded.
#' @return a list with `init` (logical, length `n`) and `changes` (a list of
#'   `time`/`node`/`replace` entries with local node indices).
#' @noRd
ds_composition <- function(src, nodeset, n) UseMethod("ds_composition")

#' @exportS3Method
ds_composition.data_source_envir <- function(src, nodeset, n) {
  nodes_obj <- get(nodeset, envir = src$envir)
  init <- nodes_obj$present %||% rep(TRUE, n)
  streams <- attr(nodes_obj, "events")[
    attr(nodes_obj, "dynamic_attribute") == "present"
  ]
  if (length(streams) == 0 || is.na(streams[1])) {
    return(list(init = init, changes = list()))
  }
  cc <- get(streams[1], envir = src$envir)
  node_idx <- if (is.character(cc$node)) {
    match(cc$node, nodes_obj$label)
  } else {
    as.integer(cc$node)
  }
  list(
    init = init,
    changes = lapply(seq_len(nrow(cc)), function(i) {
      list(time = cc$time[i], node = node_idx[i], replace = cc$replace[i])
    })
  )
}

#' @exportS3Method
ds_composition.data_source_stocnet <- function(src, nodeset, n) {
  ids <- ds_side_ids(src, nodeset)
  init <- if (is.null(src$nodes$active)) {
    rep(TRUE, n)
  } else {
    as.logical(src$nodes$active[ids])
  }
  # `active` changes were split per side at conversion, so the stream already
  # carries this side's local node indices.
  side <- if (
    identical(nodeset, ds_side_names(src)[2]) && ds_model_is_two_mode(src)
  ) {
    "mode2"
  } else {
    "mode1"
  }
  cc <- src$streams$composition[[side]]
  if (is.null(cc) || nrow(cc) == 0) {
    return(list(init = init, changes = list()))
  }
  cc <- order_events(cc[!is.na(cc$time), , drop = FALSE])
  values <- unlist(cc$value, use.names = FALSE)
  list(
    init = init,
    changes = lapply(seq_len(nrow(cc)), function(i) {
      list(time = cc$time[i], node = cc$node[i], replace = values[i])
    })
  )
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
  if (!is.null(src$derived[[name]])) {
    return(src$derived[[name]]$streams)
  }
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
  if (!is.null(src$derived_streams[[key]])) {
    return(src$derived_streams[[key]])
  }
  if (key %in% names(src$streams$global)) {
    stream <- src$streams$global[[key]]
    timed <- order_events(stream[!is.na(stream$time), , drop = FALSE])
    return(data.frame(
      time = timed$time,
      replace = unlist(timed$value, use.names = FALSE),
      stringsAsFactors = FALSE
    ))
  }
  if (identical(key, DEPENDENT_STREAM)) {
    return(dyadic_stream_events(
      src,
      src$streams$dependent,
      src$streams$focal,
      # Competing processes only: the walk routes each dependent event to its
      # own process, so it needs the per-event flavor. A single modeled flavor
      # (or none) has nothing to route, and its stream stays as it was.
      keep_flavor = length(src$modeled_flavor) > 1
    ))
  }
  if (key %in% src$layers) {
    return(dyadic_stream_events(src, src$streams$network[[key]], key))
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

# Shape a dyadic stream (a layer's ties, or the focal layer's modeled rows) the
# way the walk consumes it. `layer` names the layer whose update semantics the
# value column is named for -- the dependent stream carries the focal layer's
# rows, so it takes the focal layer's semantics.
dyadic_stream_events <- function(src, stream, layer, keep_flavor = FALSE) {
  timed <- order_events(stream[!is.na(stream$time), , drop = FALSE])
  events <- data.frame(
    time = timed$time,
    sender = timed$from,
    receiver = timed$to,
    stringsAsFactors = FALSE
  )
  events[[tie_value_column(src, layer)]] <- timed$value
  if (keep_flavor) {
    events$flavor <- timed$flavor
  }
  events
}

# The value column a layer's events carry, named for its update semantics.
tie_value_column <- function(src, layer) {
  if (identical(unname(src$info$update[layer]), "replace")) {
    "replace"
  } else {
    "increment"
  }
}

# Derived inputs --------------------------------------------------------------

#' Realize the plan's derived inputs onto the source
#'
#' Both paths materialize each derived (windowed) object before the recipe loop
#' reads it, but they own different state: the legacy path assigns the derived
#' network and its dissolve streams into its environment, so its source is
#' unchanged; the stocnet path has no environment to mutate and registers them
#' on the returned source instead.
#'
#' @param src a data source.
#' @param derivations the `plan$derivations` registry.
#' @return the source, with derived inputs resolvable through the accessors.
#' @noRd
ds_realize_derivations <- function(src, derivations) {
  UseMethod("ds_realize_derivations")
}

#' @exportS3Method
ds_realize_derivations.data_source_envir <- function(src, derivations) {
  realize_derivations(derivations, src$envir)
  src
}

#' @exportS3Method
ds_realize_derivations.data_source_stocnet <- function(src, derivations) {
  for (d in derivations) {
    if (!identical(d$kind, "window")) {
      next
    }
    # A panel layer is a change-list of wave snapshots: a window's expiry would
    # reset every panel tie at once (the wave value is the current state, not an
    # event that decays), so windowing a panel-layer effect is ill-defined.
    if (identical(unname(src$info$observation[d$source]), "panel")) {
      cli::cli_abort(c(
        "A {.arg window} cannot be used on the panel layer {.val {d$source}}.",
        "x" = "Window expiry would reset every panel tie, not just decay one
               event.",
        "i" = "Panel layers enter as change-list covariates updated at their
               wave times; drop the {.arg window} argument."
      ))
    }
    # The dissolve streams keep the legacy naming (`<stream>_<window>`) so the
    # fetch plan, built from metadata alone, addresses them the same way on
    # both paths.
    stream_names <- character(0)
    for (key in ds_object_streams(src, d$source)) {
      windowed_name <- paste(key, d$params$window, sep = "_")
      src$derived_streams[[windowed_name]] <- create_windowed_events(
        ds_fetch_stream(src, key),
        d$params$window
      )
      stream_names <- c(stream_names, windowed_name)
    }
    src$derived[[d$derived_name]] <- list(
      source = d$source,
      streams = stream_names
    )
  }
  src
}

# Missing data ----------------------------------------------------------------

#' Impute missing values in the objects the effects read
#'
#' Zero for networks, the mean for numeric attributes, the mode for categorical
#' ones. As with derivations, the legacy path writes the imputed objects back
#' into its environment while the stocnet path shadows them on the source.
#'
#' @param src a data source.
#' @param objects_effects_link matrix from `get_objects_effects_link()`.
#' @return the source, with imputed values resolvable through the accessors.
#' @noRd
ds_impute_missing <- function(src, objects_effects_link) {
  UseMethod("ds_impute_missing")
}

#' @exportS3Method
ds_impute_missing.data_source_envir <- function(src, objects_effects_link) {
  impute_missing_data(objects_effects_link, envir = src$envir)
  src
}

#' @exportS3Method
ds_impute_missing.data_source_stocnet <- function(src, objects_effects_link) {
  objects_table <- get_data_objects(
    list(rownames(objects_effects_link)),
    remove_first = FALSE
  )
  for (i in seq_len(nrow(objects_table))) {
    entry <- objects_table[i, ]
    if (!is.na(entry$object)) {
      mat <- ds_network(src, entry$object)
      if (anyNA(mat)) {
        mat[is.na(mat)] <- 0
        src$net_override[[entry$object]] <- mat
      }
      next
    }
    value <- ds_attribute(src, entry$nodeset, entry$attribute)
    if (!anyNA(value)) {
      next
    }
    src$att_override[[att_override_key(entry$nodeset, entry$attribute)]] <-
      impute_attribute(value)
  }
  src
}

# Shared imputation rule for a nodal/global attribute vector, matching the
# legacy per-object treatment (and its warning) exactly.
impute_attribute <- function(value) {
  if (is.numeric(value)) {
    cli::cli_warn(c(
      "i" = "Missing data has been detected. Mean is used to impute for
             numerical values"
    ))
    value[is.na(value)] <- mean(value, na.rm = TRUE)
  } else {
    cli::cli_warn(c(
      "i" = "Missing data has been detected. Mode is used to impute for
             categorical values"
    ))
    value[is.na(value)] <- names(which.max(table(value)))
  }
  value
}
