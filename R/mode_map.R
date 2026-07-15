# =========================================================================== #
# Mode map: global node id + label  <->  (side, local id) per layer.
#
# The engine keeps its two local index spaces (n1 x n2 matrices, per-mode
# composition) rather than re-indexing to stocnet's global node ids. Conversion
# owns the translation: for each layer, info$sender/info$receiver declare sets of
# nodes$mode values (D7) that define the row/column node spaces. The map is a
# reusable structure carried onto results and gather/db exports so local indices
# always resolve back to the original nodes row and label (D7 identity bullet).
# =========================================================================== #

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
#' sets, side purity already enforced). For each layer, resolves the sender-side
#' and receiver-side node spaces from `info$sender`/`info$receiver`:
#'
#' - undeclared -> one-mode over all nodes (global id == local id);
#' - identical mode sets -> one-mode over that subset of nodes;
#' - disjoint mode sets -> two-mode, distinct sender/receiver local spaces.
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

  sender_set <- if (!is.null(info$sender)) as.character(info$sender) else NULL
  receiver_set <- if (!is.null(info$receiver)) {
    as.character(info$receiver)
  } else {
    NULL
  }

  layer_maps <- lapply(layers, function(layer) {
    declared <- !is.null(sender_set) && !is.null(receiver_set)
    if (!declared) {
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
