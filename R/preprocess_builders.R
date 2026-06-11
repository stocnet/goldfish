#' Build the recipe state container
#'
#' Assembles the named-list state container that recipe loops own and update
#' in place (design D20): evolving networks as plain matrices in a named
#' sub-list, nodal attributes as one data frame per node set, and global
#' attributes as a one-row data frame. The mapping from each formula object
#' name to its container location is attached as the `object_keys` attribute
#' (columns `name`, `component`, `key`), consumed by `build_update_plan()`.
#'
#' @param object_names character vector with the formula object names,
#'   typically `rownames(objectsEffectsLink)`.
#' @param nodes,nodes2 names of the node sets of the dependent events.
#' @param envir environment where the data objects live.
#'
#' @return a list with components `networks`, `nodal`, `nodal2` (NULL for
#'   one-mode), and `globals`.
#' @noRd
build_state_container <- function(
    object_names, nodes, nodes2 = nodes, envir = new.env()) {
  objects_table <- getDataObjects(list(object_names), removeFirst = FALSE)
  n1 <- nrow(get(nodes, envir = envir))
  n2 <- nrow(get(nodes2, envir = envir))
  is_one_mode <- identical(nodes, nodes2)

  networks <- list()
  nodal_cols <- list()
  nodal2_cols <- list()
  global_cols <- list()
  components <- character(nrow(objects_table))
  keys <- character(nrow(objects_table))

  for (i in seq_len(nrow(objects_table))) {
    entry <- objects_table[i, ]
    if (!is.na(entry$object)) {
      mat <- get(entry$object, envir = envir)
      if (!is.matrix(mat)) {
        cli::cli_abort(
          "Object {.val {entry$object}} must be a matrix network."
        )
      }
      attributes(mat) <- attributes(mat)[c("dim", "dimnames")]
      networks[[entry$object]] <- mat
      components[i] <- "networks"
      keys[i] <- entry$object
    } else {
      container_obj <- get(entry$nodeset, envir = envir)
      value <- container_obj[[entry$attribute]]
      if (is.null(value)) {
        cli::cli_abort(
          "Attribute {.val {entry$attribute}} not found in
           {.val {entry$nodeset}}."
        )
      }
      if (inherits(container_obj, "global.goldfish")) {
        global_cols[[entry$attribute]] <- value
        components[i] <- "globals"
      } else if (entry$nodeset == nodes) {
        nodal_cols[[entry$attribute]] <- value
        components[i] <- "nodal"
      } else if (entry$nodeset == nodes2) {
        nodal2_cols[[entry$attribute]] <- value
        components[i] <- "nodal2"
      } else {
        cli::cli_abort(
          "Attribute {.val {entry$name}} belongs to node set
           {.val {entry$nodeset}}, which is neither {.val {nodes}} nor
           {.val {nodes2}}."
        )
      }
      keys[i] <- entry$attribute
    }
  }

  state <- list(
    networks = networks,
    nodal = data.frame(nodal_cols, row.names = NULL),
    nodal2 = if (is_one_mode) {
      NULL
    } else {
      data.frame(nodal2_cols, row.names = NULL)
    },
    globals = data.frame(global_cols, row.names = NULL)
  )
  if (length(nodal_cols) == 0) {
    state$nodal <- data.frame(matrix(nrow = n1, ncol = 0))
  }
  if (!is_one_mode && length(nodal2_cols) == 0) {
    state$nodal2 <- data.frame(matrix(nrow = n2, ncol = 0))
  }
  if (length(global_cols) == 0) {
    state$globals <- data.frame(matrix(nrow = 1, ncol = 0))
  }

  attr(state, "object_keys") <- data.frame(
    name = objects_table$name,
    component = components,
    key = keys,
    stringsAsFactors = FALSE
  )
  state
}
