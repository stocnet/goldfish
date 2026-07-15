# =========================================================================== #
# Mode map: global node id + label  <->  (side, local id) per layer.
#
# The engine keeps its two local index spaces (n1 x n2 matrices, per-mode
# composition) rather than re-indexing to stocnet's global node ids. Conversion
# owns the translation: for each layer, info$sender/info$receiver declare sets
# of nodes$mode values that define the row/column node spaces. The map is a
# reusable structure carried onto results and gather/db exports so local indices
# always resolve back to the original nodes row and label -- original identity
# is never lost to the local index spaces.
# =========================================================================== #

# Normalize a sender/receiver declaration to per-layer sets of nodes$mode
# values.
#
# Two encodings mean the same thing. manynet's validate_info() type-checks these
# entries as character pooled against the mode names, so a list aborts upstream;
# the repeated-name character vector is the shape that survives make_stocnet()
# today, while the list is the readable form for hand-built objects (goldfish
# reads components structurally rather than trusting manynet):
#
#   c(survey = "employees", survey = "supervisor", report = "employees")
#   list(survey = c("employees", "supervisor"), report = "employees")
#
# An unnamed vector predates per-layer declarations and applies to every layer.
normalize_mode_sets <- function(decl, layers) {
  if (is.null(decl) || length(decl) == 0) {
    return(NULL)
  }
  if (is.list(decl)) {
    return(lapply(decl, as.character))
  }
  # Read the names first: as.character() drops them.
  nms <- names(decl)
  decl <- as.character(decl)
  if (is.null(nms) || all(nms == "")) {
    return(stats::setNames(rep(list(decl), length(layers)), layers))
  }
  split(unname(decl), nms)
}

# Normalize ties$from / ties$to / changes$node references (integer global ids or
# character labels) to integer global node ids (row indices into nodes).
to_global_id <- function(refs, nodes) {
  if (is.character(refs)) {
    match(refs, nodes$label)
  } else {
    as.integer(refs)
  }
}

#' Build the per-layer mode map for a stocnet object
#'
#' Assumes `x` has passed [validate_goldfish_data()] (identical-or-disjoint mode
#' sets, side purity already enforced). Each layer resolves its own sender-side
#' and receiver-side node spaces from its entry in
#' `info$sender`/`info$receiver`, so one object may mix one-mode and two-mode
#' layers:
#'
#' - undeclared -> one-mode over all nodes (global id == local id);
#' - identical mode sets -> one-mode over that subset of nodes;
#' - disjoint mode sets -> two-mode, distinct sender/receiver local spaces.
#'
#' `is_two_mode` is resolved here, once, and carried on each layer's entry: it
#' is the single answer downstream consumes rather than re-deriving it by
#' comparing node-set names.
#'
#' @param info the stocnet `info` list.
#' @param nodes the stocnet `nodes` data frame.
#' @param layers character vector of distinct layer names.
#'
#' @return a list with `nodes_lookup` (global id, label, mode) and a `layers`
#'   list, each entry carrying `is_two_mode`, `side1`/`side2` (global ids of the
#'   row/column node spaces), and `n1`/`n2`.
#' @noRd
build_mode_map <- function(info, nodes, layers) {
  n_nodes <- nrow(nodes)
  all_ids <- seq_len(n_nodes)
  mode_col <- if ("mode" %in% names(nodes)) nodes$mode else rep(NA, n_nodes)

  nodes_lookup <- data.frame(
    global = all_ids,
    label = nodes$label,
    mode = mode_col,
    stringsAsFactors = FALSE
  )

  sender_sets <- normalize_mode_sets(info$sender, layers)
  receiver_sets <- normalize_mode_sets(info$receiver, layers)

  layer_maps <- lapply(layers, function(layer) {
    sender_set <- sender_sets[[layer]]
    receiver_set <- receiver_sets[[layer]]
    if (is.null(sender_set) || is.null(receiver_set)) {
      return(list(
        is_two_mode = FALSE,
        side1 = all_ids,
        side2 = all_ids,
        n1 = n_nodes,
        n2 = n_nodes
      ))
    }
    identical_sets <- setequal(sender_set, receiver_set)
    side1 <- all_ids[mode_col %in% sender_set]
    side2 <- if (identical_sets) side1 else all_ids[mode_col %in% receiver_set]
    list(
      is_two_mode = !identical_sets,
      side1 = side1,
      side2 = side2,
      n1 = length(side1),
      n2 = length(side2)
    )
  })
  names(layer_maps) <- layers

  list(nodes_lookup = nodes_lookup, layers = layer_maps)
}

# Remap a layer's global from/to references to that layer's local index spaces
# (NA for a reference outside the declared side, which validation rejects).
remap_layer_refs <- function(mode_map, layer, from, to, nodes) {
  lm <- mode_map$layers[[layer]]
  from_global <- to_global_id(from, nodes)
  to_global <- to_global_id(to, nodes)
  list(
    from = match(from_global, lm$side1),
    to = match(to_global, lm$side2)
  )
}

# Reusable (side, local index, global id, label) lookup for one layer, attached
# to results/exports so index_i/index_j resolve to original node identity.
layer_node_lookup <- function(mode_map, layer) {
  lm <- mode_map$layers[[layer]]
  labels <- mode_map$nodes_lookup$label
  side1 <- data.frame(
    side = 1L,
    local = seq_along(lm$side1),
    global = lm$side1,
    label = labels[lm$side1],
    stringsAsFactors = FALSE
  )
  if (!lm$is_two_mode) {
    return(side1)
  }
  side2 <- data.frame(
    side = 2L,
    local = seq_along(lm$side2),
    global = lm$side2,
    label = labels[lm$side2],
    stringsAsFactors = FALSE
  )
  rbind(side1, side2)
}
