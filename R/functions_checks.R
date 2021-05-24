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
## Find composition changes events for one nodeset
findPresence <- function(nodes) {
  if (!is.null(attr(nodes, "dynamicAttributes")) && "present" %in% attr(nodes, "dynamicAttributes")) {
    compositionChanges <- attr(nodes, "events")[which(attr(nodes, "dynamicAttributes") == "present")]
    if (is.na(compositionChanges)) stop("Composition changes were mispecified.")
    return(compositionChanges)
  } else {
    return(NULL)
  }
}

forcePresence <- function(compositionChanges, events, nodes) {
  for (r in seq_len(nrow(events))) {
    time <- events[r, ]["time"]$time
    if ("node" %in% names(events)) {
      eventNodes <- events[r, ]$node
    } else {
      eventNodes <- c(events[r, ]$sender, events[r, ]$receiver)
    }

    # find index of the node(s)
    if (all(is.character(eventNodes))) {
      eventNodes <- which(nodes$label %in% eventNodes)
    }

    # check presence
    for (node in eventNodes) {
      presence <- findLastPresence(node, time, nodes, compositionChanges)
      if (presence == -1) presence <- nodes$present[node]
      if (!presence) {
        compositionChanges <- rbind(
          data.frame(
            time = as.POSIXct(as.Date(time) - 1),
            node = nodes$label[node], replace = TRUE, stringsAsFactors = F
          ),
          compositionChanges
        )
      }
    }
  }
  compositionChanges <- compositionChanges[order(compositionChanges$time), ]

  return(compositionChanges)
}

forceUntilPresent <- function(events, compositionChanges, nodes) {
  for (r in seq_len(nrow(events))) {
    time <- events[r, ]["time"]$time
    if ("node" %in% names(events)) {
      eventNodes <- events[r, ]$node
    } else {
      eventNodes <- c(events[r, ]$sender, events[r, ]$receiver)
    }

    # find index of the node(s)
    if (all(is.character(eventNodes))) {
      eventNodes <- which(nodes$label %in% eventNodes)
    }

    # check presence
    for (node in eventNodes) {
      presence <- findLastPresence(node, time, nodes, compositionChanges)
      if (presence == -1) presence <- nodes$present[node]
      if (!presence) {
        if (all(is.character(compositionChanges$node))) {
          nextPres <- compositionChanges[compositionChanges$node == nodes[node, "label"] &
                                           compositionChanges$replace == TRUE, "time"]
          nextPres <- nextPres[nextPres > time]
          events[r, ]["time"]$time <- nextPres[1]
        } else {
          nextPres <- compositionChanges[compositionChanges$node == node & compositionChanges$replace == TRUE, "time"]
          nextPres <- nextPres[nextPres > time]
          events[r, ]["time"]$time <- nextPres[1]
        }
      }
    }
  }
  events <- events[order(events$time), ]
  return(events)
}

## find later specification of a node presence
# node should be the index
findLastPresence <- function(node, time, nodes, compositionChanges) {
  if (all(is.character(compositionChanges$node))) {
    node <- nodes[node, ]$label
  }
  if (!is.numeric(time)) {
    time <- as.numeric(time, unit = "seconds")
  }

  nodeEvents <- which(compositionChanges$node == node)
  times <- compositionChanges$time[nodeEvents]
  if (!all(is.numeric(times))) {
    times <- as.numeric(times, unit = "seconds")
  }
  presences <- compositionChanges[compositionChanges$node == node, "replace"]

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
#' @importFrom methods is
#' @noRd
#'
#' @examples checkClasses(c(1L, 2L), c("numeric", "integer", "character"))
checkClasses <- function(object, classes) {
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
#' @examples assignCatToObject(list(logical(2), numeric(4), integer(3),
#'                                  matrix(FALSE, 2, 2), matrix(0L, 1, 1), matrix(0, 2, 2)))
assignCatToObject <- function(object,
                        classes  = c("matrix",  "Matrix",  "numeric",   "character", "logical"),
                        category = c("network", "network", "attribute", "attribute", "attribute")) {
  stopifnot(length(classes) == length(category))
  objectClasses <- vapply(object, FUN = checkClasses, FUN.VALUE = logical(length(classes)), classes = classes)
  manyClasses <- colSums(objectClasses)
  objectCat <- apply(objectClasses, 2, function(x) category[x])
  attributes(objectCat) <- list(noneClass = any(manyClasses == 0), manyClasses = manyClasses)
  return(objectCat)
}

#' Check columns
#' Check columns names and types of a data frame
#'
#' @param inDataFrame data.frame where the columns are checked
#' @param mandatoryNames character vector. Names of columns that must exist.
#' @param incompatibleNames character vector. One of those should exist but not both at the same time.
#' @param optionalNames character vector. Names of columns that are optional.
#' @param classes names list of character vectors. For mandatory/incompatible/optional names of columns
#' specify the allowed classes of the column. \code{".allow"} tagged slot give the classes allowed for other columns.
#'
#' @return raise an error when either missing columns, incompatible columns or type/class is not supported.
#' @noRd
#'
#' @examples
#' checkColumns(data.frame(sender = "1", receiver = "2", time = 2),
#'   mandatoryNames = c("sender", "receiver", "time"),
#'   classes = list(
#'     sender = c("character", "numeric"), receiver = c("character", "numeric"),
#'     time = c("POSIXct", "numeric"), .allow = c("character", "numeric", "logical")
#'   ))
checkColumns <- function(
  inDataFrame,
  mandatoryNames = NULL,
  incompatibleNames = NULL,
  optionalNames = NULL,
  classes = NULL
) {
  columnNames <- colnames(inDataFrame)
  if (!is.null(mandatoryNames) && !all(mandatoryNames %in% columnNames)) {
    stop("Missing columns ",
         paste(mandatoryNames[which(!(mandatoryNames %in% columnNames))], collapse = ", "),
         call. = FALSE)
  }
  if (!is.null(incompatibleNames) && !any(columnNames %in% incompatibleNames)) {
    stop("Missing column that should be either ",
         paste(incompatibleNames, collapse = " or "),
         call. = FALSE)
  }
  if (!is.null(incompatibleNames) && sum(colnames(inDataFrame) %in% incompatibleNames) > 1) {
    stop("Incompatible columns",
         paste(incompatibleNames[which((incompatibleNames %in% colnames(inDataFrame)))], collapse = ", "),
         call. = FALSE)
  }

  # # vector helper to define types
  colType <- columnNames
  colType[!columnNames %in% c(mandatoryNames, incompatibleNames, optionalNames)] <- ".allow"

  checked <- mapply(
    function(column, ct, name) {
      if (!any(checkClasses(column, classes[[ct]]))) {
        stop("The column ", dQuote(name), " expects values of type ",
             paste(classes[[ct]], collapse = ", "), ".",
             call. = FALSE
        )
      } else TRUE
    },
    inDataFrame, colType, columnNames
  )

  return(all(checked))
}


### 2. DATA FORMATS

## Nodesets


#' check nodes object requirements
#'
#' A nodeset should be a \code{data.frame} object that contains:
#' \itemize{
#'   \item a column "label" a character/numeric vector (no NAs and no duplicates)
#'   \item (optional) a column "present" of logical values
#'   \item any other column of attributes containing characters or numerics or logical values
#'   \item And should have: one attribute "events" linked to a valid and compatible events list
#' }
#'
#' @param nodes a data frame to check
#'
#' @return TRUE if the data frame is correctly specified
#' @noRd
#'
#' @examples checkNodes(data.frame(label = "1", present = TRUE, age = 10))
checkNodes <- function(nodes) {
  # dataframe type (note: having the class node.goldfish is not mandatory,
  # a simple dataframe can be enough for certain models)
  if (!is.data.frame(nodes)) stop("A nodeset should be a data frame.")
  # columns names and types
  tryCatch(
    checkColumns(
      inDataFrame =  nodes,
      mandatoryNames = "label",
      optionalNames = "present",
      classes = list(
        label = "character", present = "logical",
        .allow = c("numeric", "character", "logical")
      )
    )
  )
  # special case of labels
  if (anyNA(as.vector(nodes$label))) stop("Labels column cannot have missing values.", call. = FALSE)
  if (anyDuplicated(as.vector(nodes$label))) stop("Labels should not be redundant (duplicate values).", .call = FALSE)
  # events attribute
  if (!is.null(attr(nodes, "events")) && !is.character(attr(nodes, "events")))
    stop("The nodeset attribute ", dQuote("events"), " should be a character vector.", call. = FALSE)
  return(TRUE)
}


## Networks
#

#' check network object requirements
#'
#' A network should be a matrix. And should have:
#' \itemize{
#'   \item one attribute "events" linked to a valid and compatible events list
#'   \item one attribute "nodes" linked to one valid and compatible \code{nodeset}
#'   \item one attribute "directed" linked to a logical compatible with the matrix form
#'   \item one optional attribute "nodes2" linked to one valid and compatible \code{nodeset}
#' }
#'
#' @param matrix object with the network as a adjacency matrix
#' @param nodes data frame with nodes information
#' @param nodesName character vector with the names of the nodes data frames
#' @param nodes2 data frame with the second mode nodes information
#'
#' @return TRUE if the object fulfill requirements for network
#' @noRd
#'
#' @examples
#' checkNetwork(
#'   matrix = structure(matrix(0, 2, 3),
#'                      dimnames = list(sprintf("A%d", 1:2), sprintf("B%d", 1:3)),
#'                      directed = TRUE, class = c("network.goldfish", "matrix", "array"), nodes = c("n1", "n2")),
#'   nodes = data.frame(label = sprintf("A%d", 1:2)), nodesName = c("n1", "n2"),
#'   nodes2 = data.frame(label = sprintf("B%d", 1:3))
#' )
checkNetwork <- function(matrix, nodes, nodesName, nodes2 = NULL) {
  # matrix type: It's done in defineNetwork
  # if (!any(checkClasses(matrix, c("matrix", "Matrix"))))
  #   stop("A network should be a matrix.", call. = FALSE)
  # network class (here this class is mandatory)
  if (!inherits(matrix, "network.goldfish")) {
    stop("A network should be of the class network.goldfish. Please use the function \"defineNetwork\".")
  }
  # events, nodes, directed attributes
  if (!is.null(attr(matrix, "events")) && !is.character(attr(matrix, "events")))
    stop("The network attribute \"events\" should be a character vector.")
  if (is.null(attr(matrix, "nodes")))
    stop("The network attribute \"nodes\" should contain the name of one or two nodesets.")
  if (!is.character(attr(matrix, "nodes")) && !length(attr(matrix, "nodes")) %in% c(1, 2))
    stop("The network attribute \"nodes\" should contain the name of one or two nodesets.")
  if (!is.logical(attr(matrix, "directed")))
    stop("The network attribute \"directed\" should be a boolean.")
  if (any(attr(matrix, "nodes") != nodesName))
    stop("The nodesets associated to this network were mispecified.")
  # validity of nodes
  isTwoMode <- !is.null(nodes2)
  if (!(inherits(nodes, "nodes.goldfish") && isTwoMode && !inherits(nodes2, "nodes.goldfish")))
    tryCatch({
      checkNodes(nodes)
      if (isTwoMode) checkNodes(nodes2)
    }, error = function(e) {
      e$message <- paste("Invalid nodeset(s): ", e$message)
      stop(e)
    })

  # compatibility between nodes and matrix
  if (!isTwoMode && !all(dim(matrix) == nrow(nodes))) {
    stop("The matrix dimensions are not coherent with the nodeset size.")
  }
  if (isTwoMode && any(dim(matrix)[1] != nrow(nodes) && dim(matrix)[2] != nrow(nodes2))) {
    stop("The matrix dimensions are not coherent with the nodesets sizes.")
  }

  # labels when present agree
  if (!is.null(dimnames(matrix))) {
    dimNames <- dimnames(matrix)
    rowIn <- dimNames[[1]] %in% nodes$label
    if (!all(rowIn))
      stop("Some row node labels are not in nodes data frame: ",
           paste(dimNames[[1]][!rowIn], collapse = ", "))

    colIn <- dimNames[[2]] %in% if (!isTwoMode) nodes$label else nodes2$label
    if (!all(colIn))
      stop("Some column node labels are not in nodes", ifelse(isTwoMode, "2", ""), " data frame: ",
           paste(dimNames[[2]][!colIn], collapse = ", "))

    if (!all(dimNames[[1]] == nodes$label) ||
        !all(dimNames[[2]] == if (!isTwoMode) nodes$label else nodes2$label))
      stop("The order of nodes in either row or columns is not the same as in \"nodes\"",
           ifelse(isTwoMode, "and \"nodes2\"", ""), " data frame", ifelse(isTwoMode, "s", ""))

  } else
    warning(dQuote("matrix"), " object doesn't have a \"dimnames\" attribute. ",
            "The order of rows and columns is assumed to be the same as in \"nodes\"",
            ifelse(isTwoMode, "and \"nodes2\"", ""), " data frame", ifelse(isTwoMode, "s", ""),
            call. = FALSE)

  return(TRUE)
}


## Dependent events lists
# Dependent events should be a valid eventlist for either a nodeset or a network with
# with one or two associated nodesets
# And should have:
# - one attribute "nodes" linked to one or two valid and compatible nodeset(s)

checkDependentEvents <- function(events, eventsName, nodes, nodes2,
                                 defaultNetwork, environment) {

  # check whether there's a column increment/replace or not (optional)
  updateColumn <- any(c("increment", "replace") %in% names(events))

  # check content
  if ("node" %in% names(events)) {
    tryCatch({
      checkEvents(nodes, events, eventsName, updateColumn = updateColumn, environment = environment)
    }, error = function(e) {
      stop("These events were assumed to be monadic events.", e$message)
    })
  } else if (all(c("sender", "receiver") %in% names(events))) {
    tryCatch({
      checkEvents(defaultNetwork, events, eventsName, nodes, nodes2,
        updateColumn = updateColumn, environment = environment)
    }, error = function(e) {
      stop("These events were assumed to be dyadic events.", e$message)
    })
  } else {
    stop("Invalid event list: missing one column node or two columns sender and receiver.")
  }

  return(TRUE)
}


## Global attributes
# A gobal attribute should be a data.frame that contains:
# - a column "time" with numerics or POSIX times
# - a column "replace" of numerics or characters or booleans

checkGlobalAttribute <- function(global) {
  # check type
  if (!is.data.frame(global)) stop("A global attribute should be a data frame.")

  # check content
  tryCatch(
    checkColumns(global,
      mandatoryNames = c("time", "replace"),
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
# - a column "node" with labels (characters) or ids (numerics) IF it's associated to a nodeset
# - 2 columns "sender" and "receiver" with labels (characters) or ids (numerics) IF it's associated to a network
# - a column "replace" OR "increment" of characters or numerics or booleans

checkEvents <- function(object, ...)
  UseMethod("checkEvents", object)

## Nodesets and Events
# When adding an event to a nodeset:
# - events should be a valid events list for a nodeset
# - attribute should exist and be compatible
# - nodes labels/indexes should be correct
# if no attribute is specified, check for the one indicated in the
# nodeset attribute "dynamicAttributes". If there's nothing, it means
# that these events are not related to attributes.

checkEvents.nodes.goldfish <- function(
  object, events, eventsName,
  attribute = NULL, updateColumn = TRUE, environment = environment()) {
  # check attributes
  if (!is.data.frame(events)) stop("An event list should be a data frame.")
  if (!is.null(attribute)) {
    if (!(is.character(attribute) && length(attribute) == 1)) stop("An attribute should be a character object.")
    if (is.null(attr(object, "dynamicAttributes")) || !(attribute %in% attr(object, "dynamicAttributes")))
      stop("The dynamic attributes for this nodeset were mispecified.")
    if (!eventsName %in% attr(object, "events")[which(attr(object, "dynamicAttributes") == attribute)])
      stop("The events related to the dynamic attributes of this nodeset were mispecified.")
  } else if (!is.null(attr(object, "events")) && eventsName %in% attr(object, "events")) {
      if (is.null(attr(object, "dynamicAttributes")) ||
        is.na(attr(object, "dynamicAttributes")[which(attr(object, "events") == eventsName)]))
        stop("The events related to the dynamic attributes of this nodeset were mispecified.")
      attribute <- attr(object, "dynamicAttributes")[which(attr(object, "events") == eventsName)]
  }

  # check classes and names of columns in events data.frame
  classesToCheck <- list(
    time = c("POSIXlt", "POSIXct", "POSIXt", "numeric"),
    node = c("integer", "character"),
    increment = "numeric",
    replace = c("logical", "numeric", "character")
  )
  errorMessage <- function(e) {
    e$message <- paste("Invalid events list: ", e$message)
    stop(e)
  }
  if (!(attribute == "present")) {
    if (updateColumn) {
      tryCatch(
        checkColumns(events,
                     mandatoryNames = c("time", "node"),
                     incompatibleNames = c("increment", "replace"),
                     classes = classesToCheck),
        error = errorMessage)
    } else tryCatch(
      checkColumns(events,
                   mandatoryNames = c("time", "node"),
                   classes = classesToCheck),
      error = errorMessage)
  } else if (attribute == "present") {
    classesToCheck["replace"] <- "logical"
    tryCatch(
      checkColumns(events,
                   mandatoryNames = c("time", "node", "replace"),
                   classes = classesToCheck),
      error = errorMessage)
  }

  if (is.unsorted(events$time)) stop("Invalid events list: Events should be ordered by time.")

  # check presence of nodes
  compositionChanges <- findPresence(object)
  if (!is.null(compositionChanges) && attribute != "present") {
    tryCatch(checkPresence(events, object, get(compositionChanges, envir = environment)))
  }

  # check attributes compatibility
  if (!is.null(attribute)) {
    if (is.null(object[[attribute]])) stop("The attribute ", sQuote(attribute), " doesn't exist in the nodeset.")

    classAttr <- class(object[[attribute]])
    eventUpdate <- if (!is.null(events$replace)) events$replace else events$increment
    classEven <- class(eventUpdate)
    if (!all(checkClasses(object[[attribute]], classEven)) && !all(checkClasses(eventUpdate, classAttr)))
      stop("The type of the attribute ", sQuote(attribute), " is incompatible with the associated event list.",
           "\n\tattribute class: ", paste(classAttr, collapse = ", "),
           "\n\tevent (increment/replace) class: ", paste(classEven, collapse = ", "))
  }
  # if (all(events$node %in% object$label) && is.integer(events$node) &&
  #     (min(events$node) < 1 || max(events$node) > dim(object)[1]))
  #   stop("Nodes indexes for the attribute ", sQuote(attribute), " are incorrect.")
  if (!all(events$node %in% object$label)) stop("Nodes labels for the attribute ", sQuote(attribute), " are incorrect.")

  return(TRUE)
}


## Networks and Events
# When adding event to a network:
# - events should be valid event lists for a network
# - nodes labels/indexes should be correct
# If the network is not specified, less checks are possible !!!

checkEvents.network.goldfish <- function(
  object, events, eventsName, nodes, nodes2 = NULL,
  updateColumn = TRUE, environment = environment()) {
  # get data frames of presence events over the nodes sets
  isTwoMode <- !is.null(nodes2)
  nodesName <- c(as.character(substitute(nodes, environment)), as.character(substitute(nodes2, environment)))
  compositionChanges <- findPresence(nodes)
  if (!is.null(compositionChanges)) compositionChanges <- get(compositionChanges, envir = environment)
  if (isTwoMode) {
    compositionChanges2 <- findPresence(nodes2)
    if (!is.null(compositionChanges2)) compositionChanges2 <- get(compositionChanges2, envir = environment)
  }

  if (!is.data.frame(events)) stop("An event list should be a data frame.")
  # check nodeset type
  if (!inherits(nodes, "nodes.goldfish") || (isTwoMode && !inherits(nodes2, "nodes.goldfish")))
    tryCatch({
      checkNodes(nodes)
      if (isTwoMode) checkNodes(nodes2)
    }, error = function(e) {
      e$message <- paste("Invalid nodeset(s): ", e$message)
      stop(e)
    })

  classesToCheck <- list(
    time = c("POSIXlt", "POSIXct", "POSIXt", "numeric"),
    sender = c("integer", "character"),
    receiver = c("integer", "character"),
    increment = "numeric",
    replace = c("logical", "numeric", "character")
  )
  if (updateColumn) {
    tryCatch(
      checkColumns(events,
                   mandatoryNames = c("time", "sender", "receiver"),
                   incompatibleNames = c("increment", "replace"),
                   classes = classesToCheck
      )
    )
  } else {
    tryCatch(
      checkColumns(events,
                   mandatoryNames = c("time", "sender", "receiver"),
                   classes = classesToCheck
      )
    )
  }

  if (is.unsorted(events$time)) stop("Events should be ordered by time.")

  # self-directed event
  if (any(events[, "sender"] == events[, "receiver"])) warning("At least one self-directed event in data.")

  # other checks
  # if (is.null(attr(object, "events")) || !any(eventsName %in% attr(object, "events")))
  #   warning("The events associated to this network were mispecified.",
  #           "\nNetwork events attached: ", paste(attr(object, "events"), collapse = ", "),
  #           "\nevents being checked: ", paste(eventsName, collapse = ""))
  if (is.null(attr(object, "nodes")) || !all(nodesName %in% attr(object, "nodes")))
    stop("The nodeset(s) associated to this network were mispecified.")

  # check presence of nodes
  if (!is.null(compositionChanges))
    tryCatch(checkPresence(events, nodes, compositionChanges, onlyReceiver = FALSE))
  if (isTwoMode && !is.null(compositionChanges2))
    tryCatch(checkPresence(events, nodes2, compositionChanges2, onlyReceiver = TRUE))

  # check attributes compatibility
  if (!isTwoMode) nodes2 <- nodes
  # if (!all(events$sender %in% nodes$label) && is.integer(events$sender)
  #     && (min(events$sender) < 1 || max(events$sender) > dim(nodes)[1]))
  #   stop("Nodes indexes in the sender column are incorrect.")
  # if (!all(events$receiver %in% nodes2$label) && is.integer(events$receiver)
  #     && (min(events$receiver) < 1 || max(events$receiver) > dim(nodes2)[1]))
  #   stop("Nodes indexes in the receiver column are incorrect.")
  if (!all(events$sender %in% nodes$label)) stop("Nodes labels for the sender column are incorrect.")
  if (!all(events$receiver %in% nodes2$label)) stop("Nodes labels for the receiver column are incorrect.")

  eventUpdate <- if (!is.null(events$replace)) events$replace else events$increment
  if (!all(checkClasses(eventUpdate, mode(object))))
    stop("The class of the associated event list is incompatible with the mode of the 'network.goldfish' object.",
         "\n\tevent (increment/replace) class: ", paste(class(eventUpdate), collapse = ", "),
         "\n\tmode network: ", paste(mode(object), collapse = ", "))

  return(TRUE)
}


## Presence of nodes in events
# Once we have all the data objects, we can check whether events times are coherent
# with the nodes presence specified in the nodeset(s)
# this function doesn't check anything else than presence coherence!
checkPresence <- function(events, nodes, compositionChanges, onlyReceiver = FALSE) {
  for (r in seq_len(nrow(events))) {

    # find time and nodes for this event
    time <- events[r, ]["time"]$time
    if ("node" %in% names(events)) {
      eventNodes <- events[r, ]$node
    } else {
      eventNodes <- c(events[r, ]$sender, events[r, ]$receiver)
    }

    # find index of the node(s)
    if (all(is.character(eventNodes))) {
      eventNodes <- which(nodes$label %in% eventNodes)
    }

    # check presence if it's a node event
    if (length(eventNodes) == 1) {
      node <- eventNodes
      presence <- findLastPresence(node, time, nodes, compositionChanges)
      if (presence == -1) presence <- nodes$present[node]
      if (!presence)
        stop("Error in the events timestamps: the node ", nodes$label[node], " is not present at time ", time)
    }

    # check presence if it's a network event
    if (length(eventNodes) == 2) {
      if (!onlyReceiver) {
        node <- eventNodes[1]
        presence <- findLastPresence(node, time, nodes, compositionChanges)
        if (presence == -1) presence <- nodes$present[node]
        if (!presence)
          stop("Error in the events timestamps: the node ", nodes$label[node], " is not present at time ", time)
      }
      node <- eventNodes[2]
      presence <- findLastPresence(node, time, nodes, compositionChanges)
      if (presence == -1) presence <- nodes$present[node]
      if (!presence)
        stop("Error in the events timestamps: the node ", nodes$label[node], " is not present at time ", time)
    }
  }
}

#' check if model and subModel parameters are conformable
#'
#' @param model character string defining the model type
#' @param subModel character string defining the subModel type
#' @param modelList character string vector defining allowed options
#' @param subModelList list with character string vectors defining allowed subModel options by each model
#'
#' @return invisible TRUE if model and subModel check conditions
#'
#' @examples
#' checkModelPar(c("DyNAM", "REM"), "Rate", c("DyNAM", "REM"),
#'               list(DyNAM = c("choice", "rate", "choice_coordination"), REM = c("choice")))
#' @noRd

checkModelPar <- function(model, subModel, modelList, subModelList) {
  stopifnot(
    inherits(model, "character"), length(model) == 1,
    inherits(subModel, "character"), length(subModel) == 1
  )

  if (!model %in% modelList) {
    stop("model: '", model, "' is not between the available options")
  }

  if (!subModel %in% subModelList[[model]]) {
    stop(
      "model: '", model, "' doesn't allow subModel: '", subModel, "' available options '",
      paste(subModelList[[model]], collapse = ", "), "'"
    )
  }

  invisible(TRUE)
}
