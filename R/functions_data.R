#####################################
#
# Goldfish package
#
# Functions related to the creation of data objects
#
#####################################

# TODO: (data team)
# - It is much prettier do define two functions
#   1) linkEvents.goldfish.nodes and
#   2) linkEvents.goldfish.network
#   Checks that relate to both can be put somewhere else. This one is really hard to debug...

## INTERFACE objects

#' Create a data frame from a dynamic nodes object
#' @param x a goldfish nodes object
#' @param time a numeric or time format to define the state of the nodes object at time - epsiolon
#' @param startTime a numeric or time format; prior events are disregarded
#' @param ... additional arguments to be passed to or from methods
#' @export
#' @return a data frame
as.data.frame.nodes.goldfish <- function(x, time = -Inf, startTime = -Inf, ...) {
  df <- x
  dynamicAttributes <- attr(df, "dynamicAttribute")
  eventNames <- attr(df, "events")
  if (is.character(time)) time <- as.POSIXct(time)
  time <- as.numeric(time)
  startTime <- as.numeric(startTime)
  if (length(eventNames) == 0) {
    return(df)
  }
  for (i in seq_along(eventNames)) {
    events <- get(eventNames[i])
    events <- sanitizeEvents(events, df)
    events <- events[events$time >= startTime & events$time < time, ]

    if (nrow(events) > 0 && !is.null(events$replace)) {
      df[[dynamicAttributes[i]]][events$node] <- events$replace
    }
    if (nrow(events) > 0 && !is.null(events$increment)) {
      for (k in seq_len(nrow(events))) {
        oldValue <- df[[dynamicAttributes[i]]][events[k, ]$node]
        df[[dynamicAttributes[i]]][events[k, ]$node] <- oldValue + events[k, ]$increment
      }
    }
  }
  df
}

#' Create a Matrix from a dynamic nodes object
#' @param x a dynamic goldfish network object
#' @param time a numeric or time format to define the state of the nodes object at time - epsiolon
#' @param startTime a numeric or time format; prior events are disregarded
#' @param ... additional arguments to be passed to or from methods
#' @export
#' @return a matrix
as.matrix.network.goldfish <- function(x, time = -Inf, startTime = -Inf, ...) {
  net <- x
  if (is.character(time)) time <- as.POSIXct(time)
  time <- as.numeric(time)
  startTime <- as.numeric(startTime)
  dim <- dim(net)
  useLoop <- F
  isDirected <- attr(net, "directed")
  eventNames <- attr(net, "events")
  nodeNames <- attr(net, "nodes")
  nodes <- nodeNames[1]
  nodes2 <- nodes
  if (length(nodeNames) == 2) {
    nodes2 <- nodeNames[2]
  }
  if (is.null(eventNames)) {
    return(x[1:dim[1], 1:dim[2]])
  }
  events <- lapply(lapply(eventNames, get),
    sanitizeEvents,
    nodes = nodes, nodes2 = nodes2
  )
  # quick update for single event lists with replace
  if (length(events) == 1) {
    df <- events[[1]][events[[1]]$time < time & events[[1]]$time >= startTime, ]
    if (nrow(df) > 0) {
      if (!is.null(df$increment)) {
        useLoop <- T
      }
      if (!is.null(df$replace)) {
        net[cbind(df$sender, df$receiver)] <- df$replace
        if (!isDirected) {
          net[cbind(df$receiver, df$sender)] <- df$replace
        }
      }
    }
  }
  if (length(events) > 1 || useLoop) {
    times <- sort(unique(unlist(lapply(events, getElement, "time"))))
    times <- times[times < time & times >= startTime]
    # update loop
    for (t in times) {
      for (i in seq_len(length(events))) {
        df <- events[[i]][events[[i]]$time == t, ]
        if (nrow(df) > 0) {
          if (!is.null(df$replace)) {
            net[cbind(df$sender, df$receiver)] <- df$replace
          }
          if (!is.null(df$increment)) {
            net[cbind(df$sender, df$receiver)] <-
              df$increment + net[cbind(df$sender, df$receiver)]
            if (!isDirected) {
              net[cbind(df$receiver, df$sender)] <-
                df$increment + net[cbind(df$receiver, df$sender)]
            }
          }
        }
      }
    }
  }

  return(net[1:dim[1], 1:dim[2]])
}

#' Return details about any goldfish objects in a given list
#' @param y a list of objects. Leave blank to capture the global environment.
#' @return classes, dimensions, and any related nodesets or events
#' for any goldfish objects in a given list.
#' @export
#' @examples
#' goldfishObjects()
goldfishObjects <- function(y = ls(envir = .GlobalEnv), envir = .GlobalEnv) {
  tryCatch({
    # identify goldfish objects
    ClassFilter <- function(x) inherits(get(x), "nodes.goldfish") |
        inherits(get(x), "network.goldfish") |
        inherits(get(x), "dependent.goldfish") |
        inherits(get(x), "global.goldfish")
    object <- Filter(ClassFilter, y)
    # if(is.null(object)) stop("No goldfish objects defined.")

    # identify classes of these objects
    classes <- t(sapply(object, function(x) class(get(x))))
    classes[, 2] <- rownames(classes)

    if ("nodes.goldfish" %in% classes[, 1]) {
      cat("Goldfish Nodes\n")
      names <- classes[classes[, 1] == "nodes.goldfish", 2]
      n <- t(sapply(names, function(x) dim(get(x))))[, 1]
      attributes <- sapply(names, function(x) paste(names(get(x)), collapse = ", "))
      events <- sapply(names, function(x) paste(attr(get(x), "dynamicAttributes"), collapse = ", "))
      print(data.frame(row.names = names, n, attributes, events))
      cat("\n")
    }

    if ("network.goldfish" %in% classes[, 1]) {
      cat("Goldfish Networks\n")
      names <- classes[classes[, 1] == "network.goldfish", 2]
      dimensions <- t(sapply(names, function(x) dim(get(x))))
      dimensions <- paste(dimensions[, 1], dimensions[, 2], sep = " x ")
      nodesets <- sapply(names, function(x) paste(attr(get(x), "nodes"), collapse = ", "))
      events <- sapply(names, function(x) paste(attr(get(x), "events"), collapse = ", "))
      print(data.frame(row.names = names, dimensions, nodesets, events))
      cat("\n")
    }

    if ("dependent.goldfish" %in% classes[, 1]) {
      cat("Goldfish Dependent Events\n")
      names <- classes[classes[, 1] == "dependent.goldfish", 2]
      n <- t(sapply(names, function(x) dim(get(x))))[, 1]
      network <- sapply(names, function(x) attr(get(x), "defaultNetwork"))
      if (any(sapply(network, function(x) is.null(x)))) {
        network[sapply(network, is.null)] <- ""
        network <- unlist(network)
      }
      print(data.frame(row.names = names, n, network))
      cat("\n")
    }

    if ("global.goldfish" %in% classes[, 1]) {
      cat("Goldfish Global Attributes\n")
      names <- classes[classes[, 1] == "global.goldfish", 2]
      out <- t(sapply(names, function(x) dim(get(x))))
      dimensions <- paste(out[, 1], out[, 2], sep = " x ")
      print(data.frame(row.names = names, dimensions))
      cat("\n")
    }
  }, error = function(e) return(NULL))
}

## DEFINE objects

#' Defining a node set with (dynamic) node attributes.
#'
#' The defineNodes function processes and checks the data.frame passed to nodes argument. This is a necessary step before the definition of the network.
#' @param nodes Data frame with a character column 'label' containing the nodes labels (mandatory). It can also contains a column of
#' type boolean or binary 'present', indicating if the respective node is present at the first timepoint, and columns containing the
#' initial values of nodes attributes (of types 'numeric', 'character' or 'boolean').
#' @return an object of class nodes.goldfish
#' @export
#' @seealso \link{defineNetwork}
#' @examples
#' actors <- data.frame(
#'   actor = 1:5, label = paste("Actor", 1:5),
#'   present = TRUE, gender = sample.int(2, 5, replace = TRUE)
#' )
#' actors <- defineNodes(nodes = actors)
defineNodes <- function(nodes) {

  # check input types
  if (!is.data.frame(nodes)) {
    stop(paste("Invalid argument: this function expects a data frame."))
  }

  # define class
  class(nodes) <- unique(c("nodes.goldfish", class(nodes)))

  # # name nodeset
  # attr(nodes, "nodes") <- as.character(nodes)

  # create events attribute
  attr(nodes, "events") <- vector("character")
  attr(nodes, "dynamicAttributes") <- vector("character")

  # check format
  tryCatch(checkNodes(nodes), error = function(e) {
    scalls <- sys.calls()
    e$call <- scalls[[1]]
    nodes <- NA
    e$message <- paste("The nodeset couldn't be constructed: ", e$message)
    stop(e, call. = FALSE)
  })

  return(nodes)
}

#' Defining a network with dynamic events
#'
#' Once the nodeset is defined, the defineNetwork function defines a network object either from a nodeset or from a sociomatrix. If a sociomatrix or adjacency matrix is used as input, defineNetwork returns a static Network. If the nodeset only is used as input, defineNetwork returns an empty network. From there, a dynamic network can be constructed by linking dynamic events to the network object.

#' @param matrix An initial matrix (optional)
#' @param nodes A nodeset (nodes.goldfish object)
#' @param nodes2 a second optional nodeset for the definition of two-mode networks
#' @param directed Boolean indicating whether the network is directed
#' @export
#' @return an object of class network.goldfish
#' @details If a sociomatrix is used as input, defineNetwork returns a static Network. This matrix must contain the same nodeset as defined with the defineNodes function and the order of the rows must correspond. The matrix must be binary (if unweighted?) and can be directed or undirected (as specified with the directed argument).
#' If this network is updated over time (e.g., a new wave of friendship data is collected), these changes can be added with the \link{linkEvents} function - similar to link changing attribute events to a nodeset. This time, the user needs to provide the network and the associated nodeset.
#' If no matrix is provided, goldfish only considers the nodeset and assumes the initial state to be empty (i.e., a matrix containing only 0s). For the network to become dynamic,  the adjacency matrix or the nodeset can be linked to a dynamic event-list data.frame in the initial state or empty network object by using the function \link{linkEvents}.
#'
#' @seealso \link{defineNodes} \link{linkEvents} \link{one-mode} \link{two-mode}
#' @examples
#' # If no matrix is provided
#' callNetwork <- defineNetwork(nodes = actors)
#'
#' # If a sociomatrix is provided
#' friendship.mat <- matrix(c(0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0), ncol = 5)
#' friendshipNetwork <- defineNetwork(matrix = friendship.mat, nodes = actors, directed = TRUE)
defineNetwork <- function(matrix = NULL, nodes, nodes2 = NULL, directed = TRUE) {

  # check input types
  twomode <- !is.null(nodes2)
  if (!is.null(matrix) && !is.matrix(matrix)) {
    stop(paste("Invalid argument matrix: this function expects a matrix."))
  }
  if (!is.data.frame(nodes)) {
    stop(paste("Invalid argument nodes: this function expects a dataframe or a nodes.goldfish object."))
  }
  if (!is.null(nodes2) && !is.data.frame(nodes2)) {
    stop(paste("Invalid argument nodes2: this function expects a dataframe or a nodes.goldfish object."))
  }
  if (!is.logical(directed)) {
    stop(paste("Invalid argument directed: this function expects a boolean."))
  }

  # Create empty matrix if needed
  # TODO: Consider a sparse representation
  if (is.null(matrix)) {
    if (twomode) {
      matrix <- matrix(0, nrow(nodes), nrow(nodes2))
    } else {
      matrix <- matrix(0, nrow(nodes), nrow(nodes))
    }
  }

  # define class
  class(matrix) <- unique(c("network.goldfish", class(matrix)))

  # create attributes
  attr(matrix, "events") <- vector("character")
  if (twomode) {
    nodesName <- c(as.character(substitute(nodes)), as.character(substitute(nodes2)))
  } else {
    nodesName <- as.character(substitute(nodes))
  }
  attr(matrix, "nodes") <- nodesName
  attr(matrix, "directed") <- directed

  # check format
  tryCatch(checkNetwork(matrix, nodes, nodesName, nodes2 = nodes2),
    error = function(e) {
      scalls <- sys.calls()
      e$call <- scalls[[1]]
      e$message <- paste("The network couldn't be constructed: ", e$message)
      matrix <- NA
      stop(e, call. = FALSE)
    }
  )

  return(matrix)
}

#' Define dependent events for a model
#'
#' The final step in defining the data objects is to identify the dependent events.
#'
#' @param events a data frame containing the event list that should be considered as a dependent variable in models.
#' @param nodes a data frame or a nodes.goldfish object containing the nodes used in the event list
#' @param nodes2 a second nodeset in the case of two mode events
#' @param defaultNetwork the name of a goldfish network object
#' @return an object of class dependent.goldfish
#' @export
#' @details Before this step is performed, we have to define: 1. the nodeset (defineNodes), the network (defineNetwork) and the eventlist of the network (linkEvents).
#' @seealso \link{defineNodes} \link{defineNetwork} \link{linkEvents}
#' @examples
#' actors <- data.frame(
#'   actor = 1:5, label = paste("Actor", 1:5),
#'   present = TRUE, gender = sample.int(2, 5, replace = TRUE)
#' )
#' actors <- defineNodes(nodes = actors)
#' calls <- data.frame(
#'   time = c(12, 27, 45, 56, 66, 68, 87), sender = paste("Actor", c(1, 3, 5, 2, 3, 4, 2)),
#'   receiver = paste("Actor", c(4, 2, 3, 5, 1, 2, 5)), increment = rep(1, 7)
#' )
#' callNetwork <- defineNetwork(nodes = actors)
#' callNetwork <- linkEvents(x = callNetwork, changeEvent = calls, nodes = actors)
#'
#' # Defining the dependent events:
#' callDependent <- defineDependentEvents(events = calls, nodes = actors, defaultNetwork = callNetwork)
defineDependentEvents <- function(events, nodes, nodes2 = NULL, defaultNetwork = NULL) {

  # check input types
  twomode <- !is.null(nodes2)
  if (!is.data.frame(events)) {
    stop(paste("Invalid argument events: this function expects a data frame."))
  }
  if (!is.data.frame(nodes)) {
    stop(paste("Invalid argument nodes: this function expects a data frame or a nodes.goldfish object."))
  }
  if (!is.null(nodes2) && !is.data.frame(nodes2)) {
    stop(paste("Invalid argument nodes2: this function expects a data frame or a nodes.goldfish object."))
  }
  if (!is.null(defaultNetwork) && !("network.goldfish" %in% class(defaultNetwork))) {
    stop(paste("Invalid argument defaultNetwork: this function expects a network.goldfish object."))
  }

  # link objects
  if (twomode) {
    nodesName <- c(as.character(substitute(nodes)), as.character(substitute(nodes2)))
  } else {
    nodesName <- as.character(substitute(nodes))
  }
  attr(events, "nodes") <- nodesName

  # define class
  class(events) <- unique(c("dependent.goldfish", class(events)))

  # link events if defaultNetwork
  if (!is.null(defaultNetwork)) {
    if (!all(attr(defaultNetwork, "nodes") == nodesName)) {
      stop("Node sets of default networks differ from node sets of dependent variable")
    }
    attr(events, "defaultNetwork") <- as.character(substitute(defaultNetwork))
  }

  # check format
  # TODO: removed defaultNetwork from check
  tryCatch({
    compositionChanges <- findPresence(nodes)
    if (!is.null(compositionChanges)) compositionChanges <- get(compositionChanges)
    if (!is.null(nodes2)) compositionChanges2 <- findPresence(nodes2)
    if (!is.null(nodes2) && !is.null(compositionChanges2)) compositionChanges2 <- get(compositionChanges2)
    checkDependentEvents(events, nodes, as.character(substitute(events)), nodesName,
      defaultNetwork = NULL, compositionChanges,
      nodes2 = nodes2, compositionChanges2 = compositionChanges2
    )
  }, error = function(e) {
    scalls <- sys.calls()
    e$call <- scalls[[1]]
    e$message <- paste("The dependent events couldn't be constructed: ", e$message)
    events <- NA
    stop(e, call. = FALSE)
  })

  return(events)
}


#' Define a global attribute
#'
#' This function allows to define a global attribute of the nodeset (i.e a variable that is identical for each node but changes over time).
#'
#' @param global a data frame containing all the values this global attribute takes along time
#' @return an object of class global.goldfish
#' @export
#' @details  For instance, seasonal climate changes could be defined as a changing global attribute. Then, this global attribute can be linked to the nodeset by using \link{linkEvents}
#' @examples
#' seasons <- defineGlobalAttribute(data.frame(time = 1:12, replace = 1:12))
defineGlobalAttribute <- function(global) {

  # check input types
  if (!is.data.frame(global)) {
    stop(paste("Invalid argument: this function expects a data frame."))
  }

  # define class
  class(global) <- unique(c("global.goldfish", class(global)))

  # check format
  tryCatch(checkGlobalAttribute(global), error = function(e) {
    scalls <- sys.calls()
    e$call <- scalls[[1]]
    e$message <- paste("The global attribute couldn't be constructed: ", e$message)
    global <- NA
    stop(e, call. = FALSE)
  })

  return(global)
}


#' Attach dynamic events to a nodeset or a network
#' @param x Either a nodeset (nodes.goldfish object) or a network
#'   (network.goldfish object)
#' @param changeEvents The name of a dataframe that represents a valid events
#'   list
#' @param attribute a character vector indicating the names of the attributes
#'   that should be updated by the specified events (ONLY if the object x is a
#'   nodeset)
#' @param nodes a nodeset (dataframe or nodes.goldfish object) related to the
#'   network (ONLY if x is a network)
#' @param nodes2 an optional nodest (dataframe or nodes.goldfish object) related
#'   to the network (ONLY if x is a network)
#' @return an object of class nodes.goldfish or network.goldfish
#' @export
#' @seealso \link{defineNodes} \link{defineNetwork} \link{one-mode} \link{two-mode}
#' @examples
#' actors <- data.frame(
#'   actor = 1:5, label = paste("Actor", 1:5),
#'   present = TRUE, gender = sample.int(2, 5, replace = TRUE)
#' )
#' actors <- defineNodes(nodes = actors)
#' callNetwork <- defineNetwork(nodes = actors)
#'
#' # Link events to a Nodeset
#' compositionChangeEvents <- data.frame(time = c(14, 60), node = 4, replace = c(FALSE, TRUE))
#' actorsnew <- linkEvents(x = actors, attribute = "present", changeEvents = compositionChangeEvents)
#'
#' # Link events to a Network
#' calls <- data.frame(
#'   time = c(12, 27, 45, 56, 66, 68, 87), sender = paste("Actor", c(1, 3, 5, 2, 3, 4, 2)),
#'   receiver = paste("Actor", c(4, 2, 3, 5, 1, 2, 5)), increment = rep(1, 7)
#' )
#' callNetwork <- linkEvents(x = callNetwork, changeEvent = calls, nodes = actors)
linkEvents <- function(x, changeEvents, attribute = NULL, nodes = NULL, nodes2 = NULL) {

  # check input types
  if (!("nodes.goldfish" %in% class(x)) && !("network.goldfish" %in% class(x))) {
    stop(paste("Invalid argument x: this function expects either a nodes.goldfish or a network.goldfish object."))
  }
  if ("nodes.goldfish" %in% class(x) && is.null(attribute)) {
    stop(paste("Invalid argument attribute: a nodeset is specified, this function expects an argument attribute."))
  }
  if ("nodes.goldfish" %in% class(x) && !is.null(nodes)) {
    stop(paste("Invalid argument nodes: a nodeset is specified, this function doesn't expect an argument nodes."))
  }
  if ("network.goldfish" %in% class(x) && is.null(nodes)) {
    stop(paste("Invalid argument nodes: a network is specified, this function expects an argument nodes."))
  }
  if ("network.goldfish" %in% class(x) && !is.null(attribute)) {
    stop(paste("Invalid argument attribute: a network is specified,",
               "this function doesn't expect an argument attribute."))
  }
  if (!is.data.frame(changeEvents)) {
    stop(paste("Invalid argument changeEvents: this function expects a data frame."))
  }
  if (!is.null(attribute) && !is.character(attribute)) {
    stop(paste("Invalid argument attributes: this function expects a character vector."))
  }
  twomode <- !is.null(nodes2)
  if ("network.goldfish" %in% class(x) && !is.data.frame(nodes)) {
    stop(paste("Invalid argument nodes: this function expects a nodeset (data frame or nodes.goldfish object)."))
  }
  if ("network.goldfish" %in% class(x) && twomode && !is.data.frame(nodes2)) {
    stop(paste("Invalid argument nodes2: this function expects a nodeset (data frame or nodes.goldfish object)."))
  }
  # data frame has to be passed as a variable name
  if (!is.name(substitute(changeEvents))) {
    stop("Parameter change events has to be the name of a data frame (rather than a data frame)")
  }

  # link data
  initial <- x
  if (length(attr(x, "events")) > 0) {
    if (as.character(substitute(changeEvents)) %in% attr(x, "events")) {
      warning(paste("The event ", substitute(changeEvents), " were already linked to this object."))
      return(x)
    }
  }
  attr(x, "events") <- c(attr(x, "events"), as.character(substitute(changeEvents)))

  if ("nodes.goldfish" %in% class(x)) {
    attr(x, "dynamicAttributes") <- c(attr(x, "dynamicAttributes"), attribute)
  }

  # check format
  tryCatch({
    if ("nodes.goldfish" %in% class(x)) {
      compositionChanges <- findPresence(x)
      if (!is.null(compositionChanges) && attribute != "present") compositionChanges <- get(compositionChanges)
      checkEventsNodes(changeEvents, x, as.character(substitute(changeEvents)),
        attribute = attribute, compositionChanges = compositionChanges
      )
    } else if ("network.goldfish" %in% class(x)) {
      if (twomode) {
        nodesName <- c(as.character(substitute(nodes)), as.character(substitute(nodes2)))
      } else {
        nodesName <- as.character(substitute(nodes))
      }
      compositionChanges <- findPresence(nodes)
      if (!is.null(compositionChanges)) compositionChanges <- get(compositionChanges)
      if (!is.null(nodes2)) compositionChanges2 <- findPresence(nodes2)
      if (!is.null(nodes2) && !is.null(compositionChanges2)) compositionChanges2 <- get(compositionChanges2)
      checkEventsNetwork(changeEvents, nodes, as.character(substitute(changeEvents)), nodesName,
        nodes2 = nodes2, network = x, compositionChanges = compositionChanges, compositionChanges2 = compositionChanges2
      )
    }
  }, error = function(e) {
    scalls <- sys.calls()
    e$call <- scalls[[1]]
    e$message <- paste("The events couldn't be added: ", e$message)
    x <- initial
    stop(e, call. = FALSE)
  })

  return(x)
}


## Find composition changes events for one nodeset
findPresence <- function(nodes) {
  if (!is.null(attr(nodes, "dynamicAttributes")) && "present" %in% attr(nodes, "dynamicAttributes")) {
    compositionChanges <- attr(nodes, "events")[which(attr(nodes, "dynamicAttributes") == "present")]
    if (is.na(compositionChanges)) {
      stop(paste("Composition changes were mispecified."))
    }
  } else {
    return(NULL)
  }

  return(compositionChanges)
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

  # TODO: trim down to a TRUE FALSE sequence
  # for (node in compositionChanges$node){
  #   trim <- rle(compositionChanges[compositionChanges$node==node,"replace"])
  #   compositionChanges <- compositionChanges[cumsum(c(1, trim$lengths[-length(trim$lengths)])),]
  # }

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

createDist2events <- function(network, nodes, nodes2, attribute, FUN) {
  times <- vector("character")
  if (attribute %in% attr(nodes, "dynamicAttributes")) {
    att.events <- attr(nodes, "events")[which(attr(nodes, "dynamicAttributes") == attribute)]
    att.events <- get(att.events)
    times <- c(times, as.character(unique(att.events$time)))
  }
  if (length(attr(network, "events")) > 0) {
    net.events <- attr(network, "events")
    net.events <- get(net.events)
    times <- c(times, as.character(unique(net.events$time)))
  }
  times <- as.POSIXct(times)

  mat <- vector()
  for (t in times) {
    net <- as.matrix(network, time = t)
    raw <- as.data.frame(nodes, time = t)[, attribute]
    val <- apply(net, 2, function(x) FUN(raw[which(x == 1)], na.rm = TRUE))
    val[is.nan(val)] <- NA
    val[val == -Inf] <- NA
    mat <- rbind(mat, val)
  }

  row.names(mat) <- as.character(times)

  out <- vector()
  for (o in as.character(times)[2:length(times)]) {
    if (!all(mapply(identical, mat[match(o, row.names(mat)), ], mat[match(o, row.names(mat)) - 1, ]))) {
      out <- rbind(
        out,
        data.frame(
          time = o,
          node = nodes2$label[which(!mapply(
            identical,
            mat[match(o, row.names(mat)), ],
            mat[match(o, row.names(mat)) - 1, ]
          ))                                                ],
          replace = mat[o, which(!mapply(
            identical,
            mat[match(o, row.names(mat)), ],
            mat[match(o, row.names(mat)) - 1, ]
          ))                                          ]
        )
      )
    }
  }
  row.names(out) <- NULL
  out$time <- as.POSIXct(as.character(out$time))
  out$node <- as.character(out$node)
  return(out)
}

# create an initial network for an event list (and node list) and a specific date
createStart <- function(eventlist, nodelist, d){
  nodes <- nodelist$label
  pre <- eventlist %>% filter(sender %in% nodes & receiver %in% nodes) %>%
    filter(time <= d)
  extras <- nodes[(!nodes %in% pre$sender) | (!nodes %in% pre$receiver)]
  pre <- rbind(pre, data.frame(sender = extras, receiver = extras,
                               time = d, increment = 0))
  pre <- as.matrix(table(pre$sender, pre$receiver))
  pre <- pre + t(pre)
  diag(pre) <- 0
  pre
}
