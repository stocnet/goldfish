#####################################
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
#####################################


### 1. BASIC CHECKS

## checkColumns
# Check columns names and types of a data frame
checkColumns <- function(inDataFrame,
                         mandatoryNames = NULL, incompatibleNames = NULL,
                         mandatoryTypes = NULL, incompatibleTypes = NULL, optionalTypes = NULL) {
  if (!is.null(mandatoryNames) && !min(mandatoryNames %in% colnames(inDataFrame))) {
    missings <- mandatoryNames[which(!(mandatoryNames %in% colnames(inDataFrame)))]
    stop(paste("Missing columns", paste(missings, collapse = ", "), sep = " "))
  }

  if (!is.null(incompatibleNames) && sum(colnames(inDataFrame) %in% incompatibleNames) == 0) {
    stop(paste("Missing column that should be either", paste(incompatibleNames, collapse = " or "), sep = " "))
  }

  if (!is.null(incompatibleNames) && sum(colnames(inDataFrame) %in% incompatibleNames) > 1) {
    incompatibles <- incompatibleNames[which( (incompatibleNames %in% colnames(inDataFrame)))]
    stop(paste("Incompatible columns", paste(incompatibles, collapse = ", "), sep = " "))
  }

  for (coll in colnames(inDataFrame)) {
    if (coll %in% mandatoryNames) {
      tryCatch(checkColumnContent(inDataFrame, coll, mandatoryTypes[[coll]]))
    } else if (coll %in% incompatibleNames) {
      tryCatch(checkColumnContent(inDataFrame, coll, incompatibleTypes[[coll]]))
    } else {
      tryCatch(checkColumnContent(inDataFrame, coll, optionalTypes))
    }
  }

  return(TRUE)
}

## checkColumnContent
# Check whether contains the right type
checkColumnContent <- function(inDataFrame, colname, types) {
  isValid <- FALSE
  for (type in types) {
    if (type == "numeric" && all(is.numeric(as.vector(inDataFrame[[colname]])))) {
      isValid <- TRUE
      break
    } else if (type == "integer" && all(is.numeric(as.vector(inDataFrame[[colname]]))) &&
               all( (as.vector(inDataFrame[[colname]])) %% 1 == 0)) {
      isValid <- TRUE
      break
    } else if (type == "character" && all(is.character(as.vector(inDataFrame[[colname]])))) {
      isValid <- TRUE
      break
    } else if (type == "logical" && all(is.logical(as.vector(inDataFrame[[colname]])) ||
                                        min(as.vector(inDataFrame[[colname]]) %in% c(0, 1)))) {
      isValid <- TRUE
      break
    } else {
      if (all(class(as.vector(inDataFrame[[colname]])) == type)) {
        isValid <- TRUE
        break
      }
    }
  }

  if (isValid) {
    TRUE
  } else {
    stop(paste("The column ", colname, " expects values of type ", paste(types, collapse = ", "), "."), sep = "")
  }
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


### 2. DATA FORMATS

## Nodesets
# A nodeset should be a dataframe object that contains:
# - a column "label" of characters (no NAs and no duplicates)
# - (optional) a column "present" of boolean or 0/1
# - any other column of attributes containing characters or numerics or booleans
# And should have:
# - one attribute "events" linked to a valid and compatible events list

checkNodes <- function(nodes) {

  # dataframe type (note: having the class node.goldfish is not mandatory,
  # a simple dataframe can be enough for certain models)
  if (!is.data.frame(nodes)) stop(paste("A nodeset should be a data frame."))

  # columns names and types
  if ("present" %in% colnames(nodes)) {
    tryCatch(
      checkColumns(nodes,
        mandatoryNames = c("label", "present"),
        mandatoryTypes = list(label = "character", present = "logical"),
        optionalTypes = c("numeric", "character", "logical")
      )
    )
  } else {
    tryCatch(
      checkColumns(nodes,
        mandatoryNames = c("label"),
        mandatoryTypes = list(label = "character"),
        optionalTypes = c("numeric", "character", "logical")
      )
    )
  }

  # special case of labels
  if (max(is.na(as.vector(nodes$label)))) stop(paste("Labels cannot be NAs."))
  if (max(duplicated(as.vector(nodes$label)))) stop(paste("Labels should not be redundant."))

  # events attribute
  if (length(attr(nodes, "events")) > 0 && !is.character(attr(nodes, "events"))) {
    stop(paste("The nodeset attribute \"events\" should be a character vector."))
  }

  return(TRUE)
}


## Networks
# A network should be a matrix
# And should have:
# - one attribute "events" linked to a valid and compatible events list
# - one attribute "nodes" linked to one valid and compatible nodeset
# - one attribute "directed" linked to a boolean compatible with the matrix form
# - one optional attribute "nodes2" linked to one valid and compatible nodeset

checkNetwork <- function(matrix, nodes, nodesName, nodes2 = NULL) {

  # matrix type
  if (!is.matrix(matrix)) stop(paste("A network should be a matrix."))

  # network class (here this class is mandatory)
  if (!"network.goldfish" %in% class(matrix)) {
    stop(paste("A network should be of the class network.goldfish. Please use the function \"defineNetwork\"."))
  }

  # events, nodes, directed attributes
  if (!is.null(attr(matrix, "events")) && !is.character(attr(matrix, "events"))) {
    stop(paste("The network attribute \"events\" should be a character vector."))
  }
  if (is.null(attr(matrix, "nodes"))) {
    stop(paste("The network attribute \"nodes\" should contain the name of one or two nodesets."))
  }
  if (!is.character(attr(matrix, "nodes")) && !length(attr(matrix, "nodes")) %in% c(1, 2)) {
    stop(paste("The network attribute \"nodes\" should contain the name of one or two nodesets."))
  }
  if (!is.logical(attr(matrix, "directed"))) {
    stop(paste("The network attribute \"directed\" should be a boolean."))
  }
  if (any(attr(matrix, "nodes") != nodesName)) {
    stop(paste("The nodesets associated to this network were mispecified."))
  }

  # validity of nodes
  isTwoMode <- !is.null(nodes2)
  tryCatch({
    checkNodes(nodes)
    if (isTwoMode) checkNodes(nodes2)
  }, error = function(e) {
    e$message <- paste("Invalid nodeset(s): ", e$message)
    stop(e)
  })

  # compatibility between nodes and matrix
  if (!isTwoMode && !all(dim(matrix) == nrow(nodes))) {
    stop(paste("The matrix dimensions are not coherent with the nodeset size."))
  }
  if (isTwoMode && any(dim(matrix)[1] != nrow(nodes) && dim(matrix)[2] != nrow(nodes2))) {
    stop(paste("The matrix dimensions are not coherent with the nodesets sizes."))
  }

  return(TRUE)
}


## Events lists
# An events list should be a dataframe that contains:
# - a column "time" of numerics or POSIX times
# - a column "node" with labels (characters) or ids (numerics) IF it's associated to a nodeset
# - 2 columns "sender" and "receiver" with labels (characters) or ids (numerics) IF it's associated to a network
# - a column "replace" OR "increment" of characters or numerics or booleans

checkEvents <- function(events, type = NULL, presence = FALSE, updateColumn = TRUE) {

  # check type
  if (!is.data.frame(events)) {
    stop(paste("An event list should be a data frame."))
  }

  # check content
  if (type == "nodes" && !presence) {
    if (updateColumn) {
      tryCatch(
        checkColumns(events,
          mandatoryNames = c("time", "node"),
          incompatibleNames = c("increment", "replace"),
          mandatoryTypes = list(
            time = c("POSIXlt", "POSIXct", "POSIXt", "numeric"),
            node = c("integer", "character")
          ),
          incompatibleTypes = list(
            increment = "numeric",
            replace = c("logical", "numeric", "character")
          )
        )
      )
    } else {
      tryCatch(
        checkColumns(events,
          mandatoryNames = c("time", "node"),
          mandatoryTypes = list(
            time = c("POSIXlt", "POSIXct", "POSIXt", "numeric"),
            node = c("integer", "character")
          )
        )
      )
    }
  } else if (type == "nodes" && presence) {
    tryCatch(
      checkColumns(events,
        mandatoryNames = c("time", "node", "replace"),
        mandatoryTypes = list(
          time = c("POSIXlt", "POSIXct", "POSIXt", "numeric"),
          node = c("integer", "character"),
          replace = c("logical", "numeric", "character")
        )
      )
    )
  } else if (type == "network") {
    if (updateColumn) {
      tryCatch(
        checkColumns(events,
          mandatoryNames = c("time", "sender", "receiver"),
          incompatibleNames = c("increment", "replace"),
          mandatoryTypes = list(
            time = c("POSIXlt", "POSIXct", "POSIXt", "numeric"),
            sender = c("integer", "character"),
            receiver = c("integer", "character")
          ),
          incompatibleTypes = list(
            increment = "numeric",
            replace = c("logical", "numeric", "character")
          )
        )
      )
    } else {
      tryCatch(
        checkColumns(events,
          mandatoryNames = c("time", "sender", "receiver"),
          mandatoryTypes = list(
            time = c("POSIXlt", "POSIXct", "POSIXt", "numeric"),
            sender = c("integer", "character"),
            receiver = c("integer", "character")
          )
        )
      )
    }
  }

  # order of events
  if (is.unsorted(events$time)) {
    stop(paste("Events should be ordered by time."))
  }

  # self-directed event
  if (!is.null(events$sender) && !is.null(events$sender)) {
    if (any(events[, "sender"] == events[, "receiver"])) {
      warning(paste("At least one self-directed event in data."))
    }
  }


  return(TRUE)
}


## Dependent events lists
# Dependent events should be a valid eventlist for either a nodeset or a network with
# with one or two associated nodesets
# And should have:
# - one attribute "nodes" linked to one or two valid and compatible nodeset(s)

checkDependentEvents <- function(events, nodes, eventsName, nodesName,
                                 defaultNetwork, compositionChanges, nodes2 = NULL, compositionChanges2 = NULL) {

  # check whether there's a column increment/replace or not (optional)
  updateColumn <- "increment" %in% names(events) || "replace" %in% names(events)

  # check content
  if ("node" %in% names(events)) {
    tryCatch({
      checkEventsNodes(events, nodes, eventsName,
        compositionChanges = compositionChanges, updateColumn = updateColumn
      )
    }, error = function(e) {
      stop(paste("These events were assumed to be monadic events.", e$message))
    })
  } else if (all(c("sender", "receiver") %in% names(events))) {
    tryCatch({
      checkEventsNetwork(events, nodes, eventsName, nodesName,
        network = defaultNetwork, compositionChanges = compositionChanges,
        updateColumn = updateColumn, nodes2 = nodes2, compositionChanges2 = compositionChanges2
      )
    }, error = function(e) {
      stop(paste("These events were assumed to be dyadic events.", e$message))
    })
  } else {
    stop(paste("Invalid event list: missing one column node or two columns sender and receiver."))
  }

  return(TRUE)
}


## Global attributes
# A gobal attribute should be a data.frame that contains:
# - a column "time" with numerics or POSIX times
# - a column "replace" of numerics or characters or booleans

checkGlobalAttribute <- function(global) {

  # check type
  if (!is.data.frame(global)) {
    stop(paste("A global attribute should be a data frame."))
  }

  # check content
  tryCatch(
    checkColumns(global,
      mandatoryNames = c("time", "replace"),
      mandatoryTypes = list(
        time = c("POSIXlt", "POSIXct", "POSIXt", "numeric"),
        replace = c("logical", "numeric", "character")
      )
    )
  )

  return(TRUE)
}



### 3. CHECK COMPATIBILITY BETWEEN DATA FORMATS

## Nodesets and Events
# When adding an event to a nodeset:
# - events should be a valid events list for a nodeset
# - attribute should exist and be compatible
# - nodes labels/indexes should be correct
# if no attribute is specified, check for the one indicated in the
# nodeset attribute "dynamicAttributes". If there's nothing, it means
# that these events are not related to attributes.

checkEventsNodes <- function(events, nodes, eventsName,
                             attribute = NULL, compositionChanges = NULL, updateColumn = TRUE) {

  # check nodeset type
  tryCatch({
    checkNodes(nodes)
  }, error = function(e) {
    e$message <- paste("Invalid nodeset: ", e$message)
    stop(e)
  })

  # check attributes
  if (!is.null(attribute)) {
    if (!(is.character(attribute) && length(attribute) == 1)) {
      stop(paste("An attribute should be a character object."))
    }
    if (is.null(attr(nodes, "dynamicAttributes")) || !(attribute %in% attr(nodes, "dynamicAttributes"))) {
      stop(paste("The dynamic attributes for this nodeset were mispecified."))
    }
    if (!eventsName %in% attr(nodes, "events")[which(attr(nodes, "dynamicAttributes") == attribute)]) {
      stop(paste("The events related to the dynamic attributes of this nodeset were mispecified."))
    }
  } else {
    if (!is.null(attr(nodes, "events")) && eventsName %in% attr(nodes, "events")) {
      if (is.null(attr(nodes, "dynamicAttributes")) ||
        is.na(attr(nodes, "dynamicAttributes")[which(attr(nodes, "events") == eventsName)])) {
        stop(paste("The events related to the dynamic attributes of this nodeset were mispecified."))
      }
      attribute <- attr(nodes, "dynamicAttributes")[which(attr(nodes, "events") == eventsName)]
    }
  }

  # check events
  tryCatch({
    if (!is.null(attribute) && attribute == "present") {
      checkEvents(events, "nodes", presence = TRUE, updateColumn = updateColumn)
    } else {
      checkEvents(events, "nodes", updateColumn = updateColumn)
    }
  }, error = function(e) {
    e$message <- paste("Invalid events list: ", e$message)
    stop(e)
  })

  # check presence of nodes
  if (!is.null(compositionChanges) && attribute != "present") {
    tryCatch(checkPresence(events, nodes, compositionChanges))
  }

  # check attributes compatibility
  if (!is.null(attribute)) {
    if (is.null(nodes[[attribute]])) stop(paste("The attribute ", attribute, " doesn't exist in the nodeset."))
    if (is.character(events$node) && !(min(events$node) %in% nodes$label) && !(min(events$node) %in% nodes$label)) {
      stop(paste("Nodes labels for the attribute ", attribute, " are incorrect."))
    }
    if (is.integer(events$node) && (min(events$node) < 1 || max(events$node) > dim(nodes)[1])) {
      stop(paste("Nodes indexes for the attribute ", attribute, " are incorrect."))
    }
    if (!(class(nodes[[attribute]]) == class(events$increment)) &&
        !(class(nodes[[attribute]]) == class(events$replace))) {
      stop(paste("The type of the attribute ", attribute, " is incompatible with the associated event list."))
    }
  }

  return(TRUE)
}


## Networks and Events
# When adding event to a network:
# - events should be valid event lists for a network
# - nodes labels/indexes should be correct
# If the network is not specified, less checks are possible !!!

checkEventsNetwork <- function(events, nodes, eventsName, nodesName,
                               network = NULL, compositionChanges = NULL, updateColumn = TRUE,
                               nodes2 = NULL, compositionChanges2 = NULL) {

  # check nodeset type
  tryCatch({
    checkNodes(nodes)
    if (!is.null(nodes2)) checkNodes(nodes2)
  }, error = function(e) {
    e$message <- paste("Invalid nodeset(s): ", e$message)
    stop(e)
  })

  # check events
  tryCatch({
    checkEvents(events, "network", updateColumn = updateColumn)
  }, error = function(e) {
    e$message <- paste("Invalid event list: ", e$message)
    stop(e)
  })

  # check network
  if (!is.null(network)) {
    tryCatch(checkNetwork(network, nodes, nodesName, nodes2 = nodes2), error = function(e) {
      e$message <- paste("Invalid network: ", e$message)
      stop(e)
    })
    if (is.null(attr(network, "events")) || !(eventsName %in% attr(network, "events"))) {
      stop(paste("The events associated to this network were mispecified."))
    }
    if (is.null(attr(network, "nodes")) || !all(nodesName %in% attr(network, "nodes"))) {
      stop(paste("The nodeset(s) associated to this network were mispecified."))
    }
  }

  # check presence of nodes
  if (!is.null(compositionChanges))
    tryCatch(checkPresence(events, nodes, compositionChanges, onlyReceiver = FALSE))
  if (!is.null(nodes2) && !is.null(compositionChanges2))
    tryCatch(checkPresence(events, nodes2, compositionChanges2, onlyReceiver = TRUE))

  # check attributes compatibility
  if (is.null(nodes2)) nodes2 <- nodes
  if (is.character(events$sender) && !(min(events$sender) %in% nodes$label) &&
      !(min(events$sender) %in% nodes$label)) {
    stop(paste("Nodes labels for the sender column are incorrect."))
  }
  if (is.character(events$receiver) && !(min(events$receiver) %in% nodes2$label) &&
      !(min(events$receiver) %in% nodes2$label)) {
    stop(paste("Nodes labels for the receiver column are incorrect."))
  }
  if (is.integer(events$node) && (min(events$node) < 1 || max(events$node) > dim(nodes)[1])) {
    stop(paste("Nodes indexes in the sender column are incorrect."))
  }
  if (is.integer(events$node) && (min(events$node) < 1 || max(events$node) > dim(nodes2)[1])) {
    stop(paste("Nodes indexes in the receiver column are incorrect."))
  }

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
      if (!presence) {
        stop(paste(
          "Error in the events timestamps: the node", nodes$label[node],
          "is not present at time", time
        ))
      }
    }

    # check presence if it's a network event
    if (length(eventNodes) == 2) {
      if (!onlyReceiver) {
        node <- eventNodes[1]
        presence <- findLastPresence(node, time, nodes, compositionChanges)
        if (presence == -1) presence <- nodes$present[node]
        if (!presence) {
          stop(paste("Error in the events timestamps: the node", nodes$label[node], "is not present at time", time))
        }
      }
      node <- eventNodes[2]
      presence <- findLastPresence(node, time, nodes, compositionChanges)
      if (presence == -1) presence <- nodes$present[node]
      if (!presence) {
        stop(paste("Error in the events timestamps: the node", nodes$label[node], "is not present at time", time))
      }
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
#' checkModelPar(c("DyNAM", "REM"), "Rate", c("DyNAM", "REM"), list(DyNAM = c("choice", "rate", "choice_coordination"), REM = c("choice")))
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
