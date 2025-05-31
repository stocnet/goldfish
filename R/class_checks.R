################################# ###
#
# Goldfish package
# Christoph Stadtfeld and James Hollway
#
# Functions related to
# Check functions for
# 1. basic checks
# 2. data objects validity
# 3. compatibility between objects
#
################################# ###

### 0. find helpers
## Find composition changes events for a nodeset
find_presence <- function(nodes) {
  if (!inherits(nodes, "nodes.goldfish")) {
    return(NULL)
  }
  if (!is.null(attr(nodes, "dynamic_attributes")) &&
    "present" %in% attr(nodes, "dynamic_attributes")) {
    # Get the indices where "present" is found
    present_indices <- which(attr(nodes, "dynamic_attributes") == "present")

    # Assuming 'events' is a parallel structure to 'dynamic_attributes'
    composition_changes <- attr(nodes, "events")[present_indices]
    if (anyNA(composition_changes)) {
      cli::cli_abort("Composition changes were mispecified.")
    }
    return(composition_changes)
  } else {
    return(NULL)
  }
}

# force_presence <- function(composition_changes, events, nodes) {
#   for (r in seq_len(nrow(events))) {
#     time <- events[r, ]["time"]$time
#     if ("node" %in% names(events)) {
#       event_nodes <- events[r, ]$node
#     } else {
#       event_nodes <- c(events[r, ]$sender, events[r, ]$receiver)
#     }
# 
#     # find index of the node(s)
#     if (all(is.character(event_nodes))) {
#       event_nodes <- which(nodes$label %in% event_nodes)
#     }
# 
#     # check presence
#     for (node in event_nodes) {
#       presence <- find_last_presence(node, time, nodes, composition_changes)
#       if (presence == -1) presence <- nodes$present[node]
#       if (!presence) {
#         composition_changes <- rbind(
#           data.frame(
#             time = as.POSIXct(as.Date(time) - 1),
#             node = nodes$label[node], replace = TRUE, stringsAsFactors = FALSE
#           ),
#           composition_changes
#         )
#       }
#     }
#   }
#   composition_changes <- composition_changes[order(composition_changes$time), ]
#   return(composition_changes)
# }

# force_until_present <- function(events, composition_changes, nodes) {
#   for (r in seq_len(nrow(events))) {
#     time <- events[r, ]["time"]$time
#     if ("node" %in% names(events)) {
#       event_nodes <- events[r, ]$node
#     } else {
#       event_nodes <- c(events[r, ]$sender, events[r, ]$receiver)
#     }
# 
#     # find index of the node(s)
#     if (all(is.character(event_nodes))) {
#       event_nodes <- which(nodes$label %in% event_nodes)
#     }
# 
#     # check presence
#     for (node in event_nodes) {
#       presence <- find_last_presence(node, time, nodes, composition_changes)
#       if (presence == -1) presence <- nodes$present[node]
#       if (!presence) {
#         if (all(is.character(composition_changes$node))) {
#           next_pres <- composition_changes[
#             composition_changes$node == nodes[node, "label"] &
#               composition_changes$replace == TRUE,
#             "time"
#           ]
#           next_pres <- next_pres[next_pres > time]
#           events[r, ]["time"]$time <- next_pres[1]
#         } else {
#           next_pres <- composition_changes[
#             composition_changes$node == node &
#               composition_changes$replace == TRUE,
#             "time"
#           ]
#           next_pres <- next_pres[next_pres > time]
#           events[r, ]["time"]$time <- next_pres[1]
#         }
#       }
#     }
#   }
#   events <- events[order(events$time), ]
#   return(events)
# }

## find later specification of a node presence
# node should be the index
find_last_presence <- function(node, time, nodes, composition_changes) {
  if (all(is.character(composition_changes$node))) {
    node <- nodes[node, ]$label
  }
  if (!is.numeric(time)) {
    time <- as.numeric(time, unit = "seconds")
  }
  node_events <- which(composition_changes$node == node)
  times <- composition_changes$time[node_events]
  if (!all(is.numeric(times))) {
    times <- as.numeric(times, unit = "seconds")
  }
  presences <- composition_changes[composition_changes$node == node, "replace"]
  times <- times - time
  times <- which(times <= 0)
  if (length(times) == 0) {
    return(-1)
  } else {
    return(presences[length(times)])
  }
}

### 1. BASIC CHECKS


#' check object is from a specific class
#'
#' @param object to check class
#' @param classes character vector with the set of classes to check agains
#'
#' @return logical vector, \code{TRUE} if object is from the specific class
#' @noRd
#'
#' @examples check_classes(c(1L, 2L), c("numeric", "integer", "character"))
check_classes <- function(object, classes) {
  vapply(classes, function(x) methods::is(object, x), logical(1))
}


#' assign object to a category given the class
#'
#' @param object a list of objects to assign categories
#' @param classes character vector with the classes to check against
#' @param category  character vector with the categories to assign
#'
#' @return character vector with the assigned category to each object
#' @noRd
#'
#' @examples
#' assign_category_object(
#'   list(
#'     logical(2), numeric(4), integer(3),
#'     matrix(FALSE, 2, 2), matrix(0L, 1, 1), matrix(0, 2, 2)
#'   )
#' )
assign_category_object <- function(
    object,
    classes = c("matrix", "Matrix", "numeric", "character", "logical"),
    category = c("network", "network", "attribute", "attribute", "attribute")) {
  stopifnot(length(classes) == length(category))
  object_classes <- vapply(
    object,
    FUN = check_classes,
    FUN.VALUE = logical(length(classes)),
    classes = classes
  )
  many_classes <- colSums(object_classes)
  object_cat <- apply(object_classes, 2, function(x) category[x])
  attributes(object_cat) <- list(
    none_class = any(many_classes == 0),
    many_classes = many_classes
  )
  return(object_cat)
}

#' Check columns
#' Check columns names and types of a data frame
#'
#' @param in_data_frame data.frame where the columns are checked
#' @param mandatory_names character vector. Names of columns that must exist.
#' @param incompatible_names character vector. One of those should exist
#'  but not both at the same time.
#' @param optional_names character vector. Names of columns that are optional.
#' @param classes names list of character vectors.
#'  For mandatory/incompatible/optional names of columns
#' specify the allowed classes of the column. \code{".allow"} tagged slot
#'  give the classes allowed for other columns.
#'
#' @return raise an error when either missing columns, incompatible columns
#'  or type/class is not supported.
#' @noRd
#'
#' @examples
#' check_columns(data.frame(sender = "1", receiver = "2", time = 2),
#'   mandatory_names = c("sender", "receiver", "time"),
#'   classes = list(
#'     sender = c("character", "numeric"),
#'     receiver = c("character", "numeric"),
#'     time = c("POSIXct", "numeric"),
#'     .allow = c("character", "numeric", "logical")
#'   )
#' )
check_columns <- function(
    in_data_frame,
    mandatory_names = NULL,
    incompatible_names = NULL,
    optional_names = NULL,
    classes = NULL) {
  column_names <- colnames(in_data_frame)
  if (!is.null(mandatory_names) && !all(mandatory_names %in% column_names)) {
    stop("Missing columns ",
      paste(
        mandatory_names[which(!(mandatory_names %in% column_names))],
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  if (!is.null(incompatible_names) && !any(column_names %in% incompatible_names)) {
    stop("Missing column that should be either ",
      paste(incompatible_names, collapse = " or "),
      call. = FALSE
    )
  }
  if (!is.null(incompatible_names) &&
    sum(colnames(in_data_frame) %in% incompatible_names) > 1) {
    stop("Incompatible columns ",
      paste(
        incompatible_names[
          which((incompatible_names %in% colnames(in_data_frame)))
        ],
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  # # vector helper to define types
  col_type <- column_names
  col_type[
    !column_names %in% c(mandatory_names, incompatible_names, optional_names)
  ] <- ".allow"
  checked <- Map(
    function(column, ct, name) {
      if (!any(check_classes(column, classes[[ct]]))) {
        stop("The column ", dQuote(name), " expects values of type ",
          paste(classes[[ct]], collapse = ", "), ".",
          call. = FALSE
        )
      } else {
        TRUE
      }
    },
    in_data_frame, col_type, column_names
  ) |>
    vapply(identity, logical(1))

  return(all(checked))
}


### 2. DATA FORMATS

## Nodesets


#' check nodes object requirements
#'
#' A nodeset should be a \code{data.frame} object that contains:
#' \itemize{
#'   \item a column "label" a character/numeric vector
#'    (no NAs and no duplicates)
#'   \item (optional) a column "present" of logical values
#'   \item any other column of attributes containing characters or numerics
#'    or logical values
#'   \item And should have: one attribute "events" linked to a valid
#'    and compatible events list
#' }
#'
#' @param nodes a data frame to check
#'
#' @return TRUE if the data frame is correctly specified
#' @noRd
#'
#' @examples check_nodes(data.frame(label = "1", present = TRUE, age = 10))
check_nodes <- function(nodes) {
  # dataframe type (note: having the class node.goldfish is not mandatory,
  # a simple dataframe can be enough for certain models)
  if (!is.data.frame(nodes)) stop("A nodeset should be a data frame.")
  # columns names and types
  tryCatch(
    check_columns(
      in_data_frame = nodes,
      mandatory_names = "label",
      optional_names = "present",
      classes = list(
        label = "character", present = "logical",
        .allow = c("numeric", "character", "logical")
      )
    )
  )
  # special case of labels
  if (anyNA(as.vector(nodes$label))) {
    stop("Labels column cannot have missing values.", call. = FALSE)
  }
  if (anyDuplicated(as.vector(nodes$label))) {
    stop("Labels should not be redundant (duplicate values).", .call = FALSE)
  }
  # events attribute
  if (!is.null(attr(nodes, "events")) && !is.character(attr(nodes, "events"))) {
    stop(
      "The nodeset attribute ", dQuote("events"),
      " should be a character vector.",
      call. = FALSE
    )
  }
  return(TRUE)
}


## Networks
#

#' check network object requirements
#'
#' A network should be a matrix. And should have:
#' \itemize{
#'   \item one attribute "events" linked to a valid and compatible events list
#'   \item one attribute "nodes" linked to one valid and compatible
#'    \code{nodeset}
#'   \item one attribute "directed" linked to a logical compatible with
#'    the matrix form
#'   \item one optional attribute "nodes2" linked to one valid and compatible
#'    \code{nodeset}
#' }
#'
#' @param matrix object with the network as a adjacency matrix
#' @param nodes data frame with nodes information
#' @param nodes_name character vector with the names of the nodes data frames
#' @param nodes2 data frame with the second mode nodes information
#'
#' @return TRUE if the object fulfill requirements for network
#' @noRd
#'
#' @examples
#' check_network(
#'   matrix = structure(
#'     matrix(0, 2, 3),
#'     dimnames = list(sprintf("A%d", 1:2), sprintf("B%d", 1:3)),
#'     directed = TRUE,
#'     class = c("network.goldfish", "matrix", "array"),
#'     nodes = c("n1", "n2")
#'   ),
#'   nodes = data.frame(label = sprintf("A%d", 1:2)), nodesName = c("n1", "n2"),
#'   nodes2 = data.frame(label = sprintf("B%d", 1:3))
#' )
check_network <- function(matrix, nodes, nodes_name, nodes2 = NULL) {
  # matrix type: It's done in make_network
  # if (!any(checkClasses(matrix, c("matrix", "Matrix"))))
  #   stop("A network should be a matrix.", call. = FALSE)
  # network class (here this class is mandatory)
  if (!inherits(matrix, "network.goldfish")) {
    stop(
      "A network should be of the class network.goldfish.",
      " Please use the function \"make_network\"."
    )
  }
  # events, nodes, directed attributes
  if (!is.null(attr(matrix, "events")) &&
        !is.character(attr(matrix, "events"))) { # styler: off
    stop("The network attribute \"events\" should be a character vector.")
  }
  if (is.null(attr(matrix, "nodes"))) {
    stop(
      "The network attribute \"nodes\" should contain",
      " the name of one or two nodesets."
    )
  }
  if (!is.character(attr(matrix, "nodes")) &&
        !length(attr(matrix, "nodes")) %in% c(1, 2)) { # styler: off
    stop(
      "The network attribute \"nodes\" should contain",
      "the name of one or two nodesets."
    )
  }
  if (!is.logical(attr(matrix, "directed"))) {
    stop("The network attribute \"directed\" should be a boolean.")
  }
  if (any(attr(matrix, "nodes") != nodes_name)) {
    stop("The nodesets associated to this network were mispecified.")
  }
  # validity of nodes
  is_two_mode <- !is.null(nodes2)
  if (!(inherits(nodes, "nodes.goldfish") &&
    is_two_mode && !inherits(nodes2, "nodes.goldfish"))) {
    tryCatch(
      {
        check_nodes(nodes)
        if (is_two_mode) check_nodes(nodes2)
      },
      error = function(e) {
        e$message <- paste("Invalid nodeset(s): ", e$message)
        stop(e)
      }
    )
  }

  # compatibility between nodes and matrix
  if (!is_two_mode && !all(dim(matrix) == nrow(nodes))) {
    stop("The matrix dimensions are not coherent with the nodeset size.")
  }
  if (is_two_mode && any(dim(matrix)[1] != nrow(nodes) &&
    dim(matrix)[2] != nrow(nodes2))) {
    stop("The matrix dimensions are not coherent with the nodesets sizes.")
  }

  # labels when present agree
  if (!is.null(dimnames(matrix))) {
    dim_names <- dimnames(matrix)
    row_in <- dim_names[[1]] %in% nodes$label
    if (!all(row_in)) {
      stop(
        "Some row node labels are not in nodes data frame: ",
        paste(dim_names[[1]][!row_in], collapse = ", ")
      )
    }
    col_in <- dim_names[[2]] %in% if (!is_two_mode) nodes$label else nodes2$label
    if (!all(col_in)) {
      stop(
        "Some column node labels are not in nodes",
        ifelse(is_two_mode, "2", ""), " data frame: ",
        paste(dim_names[[2]][!col_in], collapse = ", ")
      )
    }
    if (!all(dim_names[[1]] == nodes$label) ||
      !all(dim_names[[2]] == if (!is_two_mode) nodes$label else nodes2$label)) {
      stop(
        "The order of nodes in either row or columns is",
        "not the same as in \"nodes\"",
        ifelse(is_two_mode, "and \"nodes2\"", ""), " data frame",
        ifelse(is_two_mode, "s", "")
      )
    }
  } else {
    warning(
      dQuote("matrix"), " object doesn't have a \"dimnames\" attribute. ",
      "The order of rows and columns is assumed to be the same as in \"nodes\"",
      ifelse(is_two_mode, "and \"nodes2\"", ""), " data frame",
      ifelse(is_two_mode, "s", ""),
      call. = FALSE
    )
  }

  return(TRUE)
}

## Dependent events lists
# Dependent events should be a valid eventlist for either a nodeset
#   or a network with
# with one or two associated nodesets
# And should have:
# - one attribute "nodes" linked to one or two valid and compatible nodeset(s)

check_dependent_events <- function(events, events_name, nodes, nodes2,
                                 default_network, environment) {
  # check whether there's a column increment/replace or not (optional)
  update_column <- any(c("increment", "replace") %in% names(events))
  if ("node" %in% names(events)) {
    tryCatch(
      {
        check_events(
          object = nodes, events = events, events_name = events_name,
          update_column = update_column,
          environment = environment
        )
      },
      error = function(e) {
        stop("These events were assumed to be monadic events.", e$message)
      }
    )
  } else if (all(c("sender", "receiver") %in% names(events))) {
    tryCatch(
      {
        check_events(
          object = default_network, events = events, events_name = events_name,
          update_column = update_column, environment = environment,
          nodes = nodes, nodes2 = nodes2
        )
      },
      error = function(e) {
        stop("These events were assumed to be dyadic events.", e$message)
      }
    )
  } else {
    stop(
      "Invalid event list: missing one column node or two columns",
      "sender and receiver."
    )
  }

  return(TRUE)
}

## Global attributes
# A gobal attribute should be a data.frame that contains:
# - a column "time" with numerics or POSIX times
# - a column "replace" of numerics or characters or booleans

check_global_attribute <- function(global) {
  if (!is.data.frame(global)) stop("A global attribute should be a data frame.")

  # check content
  tryCatch(
    check_columns(global,
      mandatory_names = c("time", "replace"),
      classes = list(
        time = c("POSIXlt", "POSIXct", "POSIXt", "numeric"),
        replace = c("logical", "numeric", "character")
      )
    )
  )

  return(TRUE)
}

### 3. CHECK COMPATIBILITY BETWEEN DATA FORMATS
## Events lists
# An events list should be a dataframe that contains:
# - a column "time" of numerics or POSIX times
# - a column "node" with labels (characters) or ids (numerics)
#   IF it's associated to a nodeset
# - 2 columns "sender" and "receiver" with labels (characters) or ids (numerics)
#   IF it's associated to a network
# - a column "replace" OR "increment" of characters or numerics or booleans

check_events <- function(
    object, events, events_name,
    update_column = TRUE, environment = environment(), ...) {
  UseMethod("check_events", object)
}

## Nodesets and Events
# When adding an event to a nodeset:
# - events should be a valid events list for a nodeset
# - attribute should exist and be compatible
# - nodes labels/indexes should be correct
# if no attribute is specified, check for the one indicated in the
# nodeset attribute "dynamic_attributes". If there's nothing, it means
# that these events are not related to attributes.

#' @export
check_events.nodes.goldfish <- function(
    object, events, events_name,
    update_column = TRUE, environment = environment(),
    attribute = NULL, ...) {
  # check attributes
  if (!is.data.frame(events)) {
    stop("The events object should be a data frame.")
  }
  dynamic_attributes <- attr(object, "dynamic_attributes")
  events_linked <- attr(object, "events")
  if (is.null(dynamic_attributes) || is.null(events_linked)) {
    stop("The provide node set has no linked events.")
  }
  if (anyNA(dynamic_attributes) || anyNA(events_linked)) {
    stop(
      "The linked events for the provide nose set were mispecified,\n",
      "there are missing values in the attributes or events names."
    )
  }
  if (!is.null(attribute)) {
    if (!(is.character(attribute) && length(attribute) == 1)) {
      stop("The 'attribute' argument must be a character vector of length one.")
    }
    idx_attr <- which(dynamic_attributes == attribute)
    if (length(idx_attr) == 0) {
      stop("The attribute '", attribute, "'  has no linked events.")
    }
    if (!events_name %in% events_linked[idx_attr]) {
      stop(
        "The events object '", events_name,
        "' is not linked to the attribute '", attribute, "'."
      )
    }
  } else if (!events_name %in% events_linked) {
    stop(
      "The events object '", events_name,
      "' is not linked to any attribute of the nodeset."
    ) 
  } else{ 
    idx_events <- which(events_linked == events_name)
    if (length(idx_events) > 1) {
      stop(
        "The events object '", events_name,
        "' is linked to several attributes of the nodeset."
      )
    }
    attribute <- dynamic_attributes[idx_events]
  }

  classes_to_check <- list(
    time = c("POSIXlt", "POSIXct", "POSIXt", "numeric"),
    node = c("integer", "character"),
    increment = "numeric",
    replace = c("logical", "numeric", "character")
  )
  error_message <- function(e) {
    e$message <- paste("Invalid events list: ", e$message)
    stop(e)
  }
  if (!(attribute == "present")) {
    if (update_column) {
      tryCatch(
        check_columns(events,
          mandatory_names = c("time", "node"),
          incompatible_names = c("increment", "replace"),
          classes = classes_to_check
        ),
        error = error_message
      )
    } else {
      tryCatch(
        check_columns(events,
          mandatory_names = c("time", "node"),
          classes = classes_to_check
        ),
        error = error_message
      )
    }
  } else if (attribute == "present") {
    classes_to_check["replace"] <- "logical"
    tryCatch(
      check_columns(events,
        mandatory_names = c("time", "node", "replace"),
        classes = classes_to_check
      ),
      error = error_message
    )
  }

  if (is.unsorted(events$time)) {
    stop("Invalid events list: Events should be ordered by time.")
  }
  composition_changes <- find_presence(object)
  if (!is.null(composition_changes) && attribute != "present") {
    tryCatch(
      check_presence(
        events,
        object,
        get(composition_changes, envir = environment)
      )
    )
  }
  if (!is.null(attribute)) {
    if (is.null(object[[attribute]])) {
      stop(
        "The attribute ", dQuote(attribute),
        " doesn't exist in the nodeset."
      )
    }
    class_attr <- class(object[[attribute]])
    event_update <- if (!is.null(events$replace)) {
      events$replace
    } else {
      events$increment
    }
    class_even <- class(event_update)
    if (!all(check_classes(object[[attribute]], class_even)) &&
      !all(check_classes(event_update, class_attr))) {
      stop(
        "The type of the attribute ", dQuote(attribute),
        " is incompatible with the associated event list.",
        "\n\tattribute class: ", paste(class_attr, collapse = ", "),
        "\n\tevent (increment/replace) class: ",
        paste(class_even, collapse = ", ")
      )
    }
  }
  if (!all(events$node %in% object$label)) {
    stop(
      "Nodes labels for the attribute ", dQuote(attribute), " are incorrect."
    )
  }
  return(TRUE)
}

## Networks and Events
# When adding event to a network:
# - events should be valid event lists for a network
# - nodes labels/indexes should be correct
# If the network is not specified, less checks are possible !!!

#' @export
check_events.network.goldfish <- function(
    object, events, events_name,
    update_column = TRUE, environment = environment(),
    nodes, nodes2 = NULL, ...) {
  is_two_mode <- !is.null(nodes2)
  nodes_name <- c(
    as.character(substitute(nodes, environment)),
    as.character(substitute(nodes2, environment))
  )
  composition_changes <- find_presence(nodes)
  if (!is.null(composition_changes)) {
    composition_changes <- get(composition_changes, envir = environment)
  }
  if (is_two_mode) {
    composition_changes2 <- find_presence(nodes2)
    if (!is.null(composition_changes2)) {
      composition_changes2 <- get(composition_changes2, envir = environment)
    }
  }

  if (!is.data.frame(events)) stop("An event list should be a data frame.")
  # check nodeset type
  if (!inherits(nodes, "nodes.goldfish") ||
    (is_two_mode && !inherits(nodes2, "nodes.goldfish"))) {
    tryCatch(
      {
        check_nodes(nodes)
        if (is_two_mode) check_nodes(nodes2)
      },
      error = function(e) {
        e$message <- paste("Invalid nodeset(s): ", e$message)
        stop(e)
      }
    )
  }
  classes_to_check <- list(
    time = c("POSIXlt", "POSIXct", "POSIXt", "numeric"),
    sender = c("integer", "character"),
    receiver = c("integer", "character"),
    increment = "numeric",
    replace = c("logical", "numeric", "character")
  )
  if (update_column) {
    tryCatch(
      check_columns(events,
        mandatory_names = c("time", "sender", "receiver"),
        incompatible_names = c("increment", "replace"),
        classes = classes_to_check
      )
    )
  } else {
    tryCatch(
      check_columns(events,
        mandatory_names = c("time", "sender", "receiver"),
        classes = classes_to_check
      )
    )
  }

  if (is.unsorted(events$time)) stop("Events should be ordered by time.")

  # self-directed event
  if (any(events[, "sender"] == events[, "receiver"])) {
    warning("At least one self-directed event in data.")
  }
  if (is.null(attr(object, "nodes")) ||
    !all(nodes_name %in% attr(object, "nodes"))) {
    stop("The nodeset(s) associated to this network were mispecified.")
  }
  if (!is.null(composition_changes)) {
    tryCatch(
      check_presence(events, nodes, composition_changes, onlyReceiver = FALSE)
    )
  }
  if (is_two_mode && !is.null(composition_changes2)) {
    tryCatch(
      check_presence(events, nodes2, composition_changes2, onlyReceiver = TRUE)
    )
  }
  if (!is_two_mode) nodes2 <- nodes
  if (!all(events$sender %in% nodes$label)) {
    stop("Nodes labels for the sender column are incorrect.")
  }
  if (!all(events$receiver %in% nodes2$label)) {
    stop("Nodes labels for the receiver column are incorrect.")
  }
  event_update <- if (!is.null(events$replace)) {
    events$replace
  } else {
    events$increment
  }
  if (!all(check_classes(event_update, mode(object)))) {
    stop(
      "The class of the associated event list is incompatible",
      " with the mode of the 'network.goldfish' object.",
      "\n\tevent (increment/replace) class: ",
      paste(class(event_update), collapse = ", "),
      "\n\tmode network: ", paste(mode(object), collapse = ", ")
    )
  }

  return(TRUE)
}

## Presence of nodes in events
# Once we have all the data objects, we can check whether events times are
# coherent
# with the nodes presence specified in the nodeset(s)
# this function doesn't check anything else than presence coherence!

check_presence <- function(
    events, nodes, composition_changes, onlyReceiver = FALSE) {
  for (r in seq_len(nrow(events))) {
    # find time and nodes for this event
    time <- events[r, ]["time"]$time
    if ("node" %in% names(events)) {
      event_nodes <- events[r, ]$node
    } else {
      event_nodes <- c(events[r, ]$sender, events[r, ]$receiver)
    }
    if (all(is.character(event_nodes))) {
      event_nodes <- which(nodes$label %in% event_nodes)
    }
    if (length(event_nodes) == 1) {
      node <- event_nodes
      presence <- find_last_presence(node, time, nodes, composition_changes)
      if (presence == -1) presence <- nodes$present[node]
      if (!presence) {
        stop(
          "Error in the events timestamps: the node ",
          nodes$label[node], " is not present at time ", time
        )
      }
    }
    if (length(event_nodes) == 2) {
      if (!onlyReceiver) {
        node <- event_nodes[1]
        presence <- find_last_presence(node, time, nodes, composition_changes)
        if (presence == -1) presence <- nodes$present[node]
        if (!presence) {
          stop(
            "Error in the events timestamps: the node ", nodes$label[node],
            " is not present at time ", time
          )
        }
      }
      node <- event_nodes[2]
      presence <- find_last_presence(node, time, nodes, composition_changes)
      if (presence == -1) presence <- nodes$present[node]
      if (!presence) {
        stop(
          "Error in the events timestamps: the node ", nodes$label[node],
          " is not present at time ", time
        )
      }
    }
  }
}

#' check if model and subModel parameters are conformable
#'
#' @param model character string defining the model type
#' @param sub_model character string defining the subModel type
#' @param model_list character string vector defining allowed options
#' @param sub_model_list list with character string vectors defining allowed
#'    sub_model options by each model
#'
#' @return invisible TRUE if model and subModel check conditions
#'
#' @examples
#' checkModelPar(
#'   c("DyNAM", "REM"), "Rate", c("DyNAM", "REM"),
#'   list(
#'     DyNAM = c("choice", "rate", "choice_coordination"),
#'     REM = c("choice")
#'   )
#' )
#' @noRd

check_model_par <- function(model, sub_model, model_list, sub_model_list) {
  stopifnot(
    inherits(model, "character"), length(model) == 1,
    inherits(sub_model, "character"), length(sub_model) == 1
  )
  if (!model %in% model_list) {
    stop("model: '", model, "' is not between the available options")
  }
  if (!sub_model %in% sub_model_list[[model]]) {
    stop(
      "model: '", model, "' doesn't allow subModel: '",
      sub_model, "' available options '",
      paste(sub_model_list[[model]], collapse = ", "), "'"
    )
  }

  invisible(TRUE)
}
