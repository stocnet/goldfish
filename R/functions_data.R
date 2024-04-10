################################## ###
#
# Goldfish package
#
# Functions related to the creation of data objects
#
################################## ###

#' Methods to update a nodes or network object
#'
#' Methods to create a data frame from an object of class `nodes.goldfish`
#' (see [defineNodes()]) or a matrix from an object of class
#' `network.goldfish` (see [defineNetwork()]) with the attributes
#' or the network ties updated according with the events linked to the object
#' using the [linkEvents()]) function.
#' @param x an object of class `nodes.goldfish` for `as.data.frame()`
#' method or `network.goldfish` for `as.matrix()` method.
#' @param time a numeric value or a calendar date value (see [as.Date()])
#' to update the state of the object `x` until this time value
#' (event time < time).
#' @param startTime a numeric `as.Date` format value; prior events are
#' disregarded.
#' @param ... Not further arguments are required.
#' @param envir an `environment` where the nodes and linked events
#'   objects are available.
#' @return The respective object updated accordingly to the events link to it.
#' For `nodes.goldfish` object the attributes are updated according to the
#' events linked to them.
#' For `network.goldfish` object the network ties are updated according to the
#' events linked to it.
#' @seealso [defineNetwork()], [defineNodes()], [linkEvents()]
#' @examples
#' \donttest{
#' data("Fisheries_Treaties_6070")
#' states <- defineNodes(states)
#' states <- linkEvents(states, sovchanges, attribute = "present")
#' states <- linkEvents(states, regchanges, attribute = "regime")
#' states <- linkEvents(states, gdpchanges, attribute = "gdp")
#'
#' bilatnet <- defineNetwork(bilatnet, nodes = states, directed = FALSE)
#' bilatnet <- linkEvents(bilatnet, bilatchanges, nodes = states)
#'
#' updateStates <- as.data.frame(
#'   states,
#'   time = as.numeric(as.POSIXct("1965-12-31"))
#' )
#'
#'
#' updateNet <- as.matrix(bilatnet, time = as.numeric(as.POSIXct("1965-12-31")))
#' }
#'
#' @name update-method
NULL

# Create a data frame from a dynamic nodes object
#' @export
#' @rdname update-method
as.data.frame.nodes.goldfish <- function(x, ..., time = -Inf,
    startTime = -Inf, envir = new.env()) {
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
    events <- get(eventNames[i], envir = envir)
    events <- sanitizeEvents(events, df, envir = envir)
    events <- events[events$time >= startTime & events$time < time, ]

    if (nrow(events) > 0 && !is.null(events$replace)) {
      df[[dynamicAttributes[i]]][events$node] <- events$replace
    }
    if (nrow(events) > 0 && !is.null(events$increment)) {
      for (k in seq_len(nrow(events))) {
        oldValue <- df[[dynamicAttributes[i]]][events[k, ]$node]
        df[[dynamicAttributes[i]]][events[k, ]$node] <-
          oldValue + events[k, ]$increment
      }
    }
  }
  df
}

#' Create a Matrix from a dynamic nodes object
#' @export
#' @rdname update-method
as.matrix.network.goldfish <- function(x, ..., time = -Inf, startTime = -Inf) {
  net <- x
  if (is.character(time)) time <- as.POSIXct(time)
  time <- as.numeric(time)
  startTime <- as.numeric(startTime)
  dim <- dim(net)
  useLoop <- FALSE
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
        useLoop <- TRUE
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

# A report of goldfish objects in an environment
#
# Return details about any goldfish objects in a given environment.
# @param envir an `R` [environment-class] where the goldfish objects are
# searched. The default value is [.GlobalEnv].
# @return Not value, called for printing side effect.
#
# Print detail information of the goldfish object in the environment where the
# function is call.
# For `nodes.goldfish` objects prints the total number of nodes, the attributes
# and the linked events to the attributes in the object.
# For `network.goldfish` objects prints the dimensions of the network, the
# nodes sets and events linked to them.
# For `dependent.goldfish` objects prints the number of events and the default
# network linked to them.
# @export
# @examples
# data("Fisheries_Treaties_6070")
# states <- defineNodes(states)
# states <- linkEvents(states, sovchanges, attribute = "present")
# states <- linkEvents(states, regchanges, attribute = "regime")
# states <- linkEvents(states, gdpchanges, attribute = "gdp")
#
# bilatnet <- defineNetwork(bilatnet, nodes = states, directed = FALSE)
# bilatnet <- linkEvents(bilatnet, bilatchanges, nodes = states)
#
# contignet <- defineNetwork(contignet, nodes = states, directed = FALSE)
# contignet <- linkEvents(contignet, contigchanges, nodes = states)
#
# createBilat <- defineDependentEvents(
#   events = bilatchanges[bilatchanges$increment == 1, ],
#   nodes = states, defaultNetwork = bilatnet
# )
#
# goldfishObjects()
# goldfishObjects <- function(envir = .GlobalEnv) {
#   y = ls(envir = envir)
#   tryCatch({
#     # identify goldfish objects
#     classesToKeep <- c(
#       "nodes.goldfish", "network.goldfish", "dependent.goldfish",
#       "global.goldfish"
#     )
#     ClassFilter <- function(x)
#       any(checkClasses(get(x, envir = envir), classes = classesToKeep))
#
#     object <- Filter(ClassFilter, y)
#     # if(is.null(object)) stop("No goldfish objects defined.")
#
#     # identify classes of these objects
#     classes <- vapply(
#       object,
#       FUN = function(x)
#         checkClasses(get(x, envir = envir), classes = classesToKeep),
#       FUN.VALUE = logical(length(classesToKeep))
#     )
#
#     if (any(classes["nodes.goldfish", ])) {
#       cat("Goldfish Nodes\n")
#       names <- object[classes["nodes.goldfish", ]]
#       n <- vapply(names, function(x) nrow(get(x, envir = envir)), integer(1))
#       attributes <- vapply(
#         names,
#         function(x) paste(names(get(x, envir = envir)), collapse = ", "),
#         character(1)
#       )
#       events <- vapply(
#         names,
#         function(x) paste(attr(get(x, envir = envir), "dynamicAttributes"),
#                           collapse = ", "),
#         character(1)
#       )
#       print(data.frame(row.names = names, n, attributes, events))
#       cat("\n")
#     }
#
#     if (any(classes["network.goldfish", ])) {
#       cat("Goldfish Networks\n")
#       names <- object[classes["network.goldfish", ]]
#       dimensions <- vapply(
#         names,
#         function(x) paste(dim(get(x, envir = envir)), collapse = " x "),
#         character(1)
#       )
#       nodesets <- vapply(
#         names,
#         function(x) paste(attr(get(x, envir = envir), "nodes"),
#                           collapse = ", "),
#         character(1)
#       )
#       events <- vapply(
#         names,
#         function(x) paste(attr(get(x, envir = envir), "events"),
#                           collapse = ", "),
#         character(1)
#       )
#       print(data.frame(row.names = names, dimensions, nodesets, events))
#       cat("\n")
#     }
#
#     if (any(classes["dependent.goldfish", ])) {
#       cat("Goldfish Dependent Events\n")
#       names <- object[classes["dependent.goldfish", ]]
#       n <- vapply(names, function(x) nrow(get(x, envir = envir)), integer(1))
#       network <- vapply(names,
#                         function(x) {
#                           net <- attr(get(x, envir = envir), "defaultNetwork")
#                           ifelse(is.null(net), "", net)
#                         },
#                         character(1))
#       print(data.frame(row.names = names, n, network))
#       cat("\n")
#     }
#
#     if (any(classes["global.goldfish", ])) {
#       cat("Goldfish Global Attributes\n")
#       names <- object[classes["global.goldfish", ]]
#       dimensions <- vapply(
#         names,
#         function(x) nrow(get(x, envir = envir)),
#         integer(1)
#       )
#       print(data.frame(row.names = names, dimensions))
#       cat("\n")
#     }
#   }, error = function(e) return(NULL))
# }

#' Defining a node set with (dynamic) node attributes.
#'
#' The `defineNodes()` function processes and checks the `data.frame` passed to
#' the `nodes` argument.
#' This is a recommended step before the definition of the network.
#'
#' Additional variables in the `nodes` data frame object are considered as the
#' initial values of the nodes attributes.
#' Those variables must be of class `numeric`, `character`, `logical`.
#'
#' It is important that the initial definition of the node set contain all the
#' nodes that could be potential senders or receivers of events.
#' In case that all the nodes are not available at all times, the `present`
#' variable can be used to define compositional changes.
#' Therefore, the initial node set would contain all the potential senders and
#' receivers nodes and the variable `present` will indicate all the nodes
#' present at the beginning as senders or receivers.
#' Using [linkEvents()] is possible to link events where the composition of
#' available nodes changes over time, see `vignette("teaching2")`.
#'
#' For the attributes in the nodeset to become dynamic, them
#' can be linked to a dynamic event-list data frames in the initial state
#' object by using the [linkEvents()].
#' A new call of `linkEvents()` is required for each attribute that is dynamic.
#'
#' Objects of class [tbl_df] from the tibble package frequently use in the
#' tidyverse ecosystem and objects of class `data.table` will produce
#' errors in later steps for goldfish.
#' Current implementation of goldfish relies on the subsetting behavior of
#' data frames objects. The previous mentioned objects classes change this
#' behavior producing errors.
#'
#' @param nodes a `data.frame` object with the nodes attributes with the
#' following reserved names
#' \describe{
#'  \item{label}{`character` variable containing the nodes labels
#'    (mandatory)}
#'  \item{present}{`logical` variable indicating if the respective node
#'  is present at the first time-point (optional)}
#' }
#'
#' @return an object with an additional class `nodes.goldfish` with attributes:
#' \item{events}{An empty character vector. [linkEvents()] is used to
#' link event data frames.}
#' \item{dynamicAttributes}{An empty character vector. [linkEvents()] is used to
#' link event data frames and their related attribute.}
#'
#' The object can be modified using methods for data frames.
#' @export
#' @seealso [defineNetwork()], [linkEvents()]
#' @examples
#' nodesAttr <- data.frame(
#'   label = paste("Actor", 1:5),
#'   present = c(TRUE, FALSE, TRUE, TRUE, FALSE),
#'   gender = c(1, 2, 1, 1, 2)
#' )
#' nodesAttr <- defineNodes(nodes = nodesAttr)
#'
#' # Social evolution nodes definition
#' data("Social_Evolution")
#' actors <- defineNodes(actors)
#'
#' # Fisheries treaties nodes definition
#' data("Fisheries_Treaties_6070")
#' states <- defineNodes(states)
defineNodes <- function(nodes) {
  # check input types
  if (!is.data.frame(nodes)) {
    stop(
      "Invalid argument ", dQuote("nodes"), ": ",
      "this function expects objects of class ",
      dQuote("data.frame"), "."
    )
  }

  # define class
  class(nodes) <- unique(c("nodes.goldfish", class(nodes)))
  # create events attribute
  attr(nodes, "events") <- vector("character")
  attr(nodes, "dynamicAttributes") <- vector("character")
  # check format
  tryCatch(checkNodes(nodes), error = function(e) {
    scalls <- sys.calls()
    e$call <- scalls[[1]]
    nodes <- NA
    e$message <- paste("The nodeset couldn't be constructed: ", e$message)
    stop(e)
  })
  return(nodes)
}

#' Defining a network with dynamic events
#'
#' The function defines a network object either from a nodeset or
#' from a matrix (sociomatrix or adjacency matrix). If a matrix is used as
#' input, `defineNetwork()` returns a network filled with the same values
#' as the ones present in the provided network.
#' If the nodeset is the only argument, `defineNetwork()` returns an
#' empty network with the number of columns and rows corresponding to the
#' size of the nodeset.
#' These networks are static, but they can be turned into dynamic networks
#' by linking dynamic events to the network objectw using [linkEvents()].
#'
#' @param matrix An initial matrix (optional), and object of class `matrix`.
#' @param nodes A node-set (see [defineNodes()]).
#' @param nodes2 A second optional node-set for the definition of
#'   two-mode networks.
#' @param directed A logical value indicating whether the network is directed.
#' @param envir An [environment-class] object where the nodes-set objects are
#' defined. The default value is [environment()].
#' @export
#' @return an object with additional class `network.goldfish` with attributes:
#' \item{nodes}{a character vector with the names of the nodes set objects used
#' during the definition. `nodes` and `nodes2` arguments.}
#' \item{directed}{Logical value indicating whether the network is directed.
#' `directed` argument}
#' \item{events}{An empty character vector. [linkEvents()] is used to
#' link event data frames.}
#'
#' The object can be modified using methods for matrix.
#' @details
#' If a matrix is used as input, its dimension names must be a subset of the
#' nodes in the nodeset as defined with the [defineNodes()] and the order of
#' the labels in rows and columns must correspond to the order of node labels
#' in the nodeset.
#' The matrix can be directed or undirected (as specified with the
#' `directed` argument).
#'
#' If the network is updated over time (e.g., a new wave of friendship data is
#' collected), these changes can be added with the [linkEvents()] - similar to
#' link changing attribute events to a nodeset.
#' This time, the user needs to provide the network and the associated nodeset.
#' If no matrix is provided, goldfish only considers the nodeset and assumes
#' the initial state to be empty (i.e., a matrix containing only 0s).
#'
#' @seealso [defineNodes()], [linkEvents()]
#' @examples
#' # If no intial matrix is provided
#' data("Social_Evolution")
#' callNetwork <- defineNetwork(nodes = actors)
#'
#' # If a initial matrix is provided
#' data("Fisheries_Treaties_6070")
#' bilatnet <- defineNetwork(bilatnet, nodes = states, directed = FALSE)
defineNetwork <- function(
    matrix = NULL, nodes, nodes2 = NULL, directed = TRUE,
    envir = environment()) {
  # check input types
  isTwoMode <- !is.null(nodes2)
  nRow <- nrow(nodes)
  nCol <- ifelse(isTwoMode, nrow(nodes2), nrow(nodes))

  if (!any(checkClasses(nodes, c("data.frame", "nodes.goldfish")))) {
    stop(
      "Invalid argument ", dQuote("nodes"), ": ",
      "this function expects objects of class ",
      dQuote("data.frame"), " or ", dQuote("nodes.goldfish"), "."
    )
  }

  if (!is.null(nodes2) &&
    !any(checkClasses(nodes2, c("data.frame", "nodes.goldfish")))) {
    stop(
      "Invalid argument ", dQuote("nodes2"), ": ",
      "this function expects objects of class ",
      dQuote("data.frame"), " or ", dQuote("nodes.goldfish"), "."
    )
  }

  if (!is.logical(directed)) {
    stop(
      "Invalid argument ", dQuote("directed"), ": ",
      "this function expects a logical value."
    )
  }

  # Create empty matrix if needed
  if (is.null(matrix)) {
    matrix <- matrix(
      0, nRow, nCol,
      dimnames = list(
        sender = nodes$label,
        receiver = if (isTwoMode) nodes2$label else nodes$label
      )
    )
  } else if (is.table(matrix)) {
    if (length(dim(matrix)) != 2) {
      stop(dQuote("matrix"), ' object has an incorrect number of dimensions.",
           "Expected 2 dimensions')
    }
    matrix <- structure(matrix, class = NULL, call = NULL)
  } else if (!any(checkClasses(matrix, c("matrix", "Matrix")))) {
    stop(
      "Invalid argument ", dQuote("matrix"), ": ",
      "this function expects an objects of class ",
      dQuote("matrix"), " or ", dQuote("Matrix"), "."
    )
  }

  # define class
  class(matrix) <- unique(c("network.goldfish", class(matrix)))

  # create attributes
  attr(matrix, "events") <- vector("character")

  nodesName <- c(
    as.character(substitute(nodes, env = envir)),
    as.character(substitute(nodes2, env = envir))
  )

  attr(matrix, "nodes") <- nodesName
  attr(matrix, "directed") <- directed

  # check format
  tryCatch(checkNetwork(matrix, nodes, nodesName, nodes2 = nodes2),
    error = function(e) {
      scalls <- sys.calls()
      e$call <- scalls[[1]]
      e$message <- paste("The network couldn't be constructed: ", e$message)
      matrix <- NA
      stop(e)
    }
  )

  return(matrix)
}

#' Define dependent events for a model
#'
#' The final step in defining the data objects is to identify
#' the dependent events.
#'
#' @param events a data frame containing the event list that should be
#' considered as a dependent variable in models.
#' @param nodes a data frame or a `nodes.goldfish` object containing the nodes
#' used in the event list.
#' @param nodes2 a second nodeset in the case that the events occurs in a
#' two-mode network.
#' @param defaultNetwork the name of a `network.goldfish` object.
#' @param envir An [environment-class] object where the nodes-set
#' and default network objects are defined. The default value is
#' [environment()].
#' @return an object with additional class `dependent.goldfish` with attributes:
#' \item{nodes}{a character vector with the names of the nodes set that define
#' the dimensions of the `defaultNetwork`. `nodes` and `nodes2` arguments.}
#' \item{defaultNetwork}{A character value with the name of the network object
#' when this is present. `defaultNetwork` argument.}
#' \item{type}{A character value that can take values monadic or dyadic
#' depending on the arguments used during the definition.}
#'
#' The object can be modified using methods for data frames.
#' @export
#' @details Before this step is performed, we have to define:
#' the nodeset ([defineNodes()]), the network ([defineNetwork()])
#'  and the link the event list to the network ([linkEvents()]).
#'
#' During the definition as a dependent event, some checks are done to ensure
#' consistency with the default network and the nodeset.
#' In particular, consistency of the labels of nodes in the events with the
#' nodes' labels in the network and the nodeset is done.
#'
#' It is possible to define as a dependent event a different set of events to
#' the ones link to the default network.
#' This is useful to model different type of events where the event dynamic is
#' driven by different effects or its weight differs.
#' [Fisheries_Treaties_6070] has an example of it, the relational event modeled
#' are fisheries treaties between countries. The `bilatchanges` data frame
#' contains information of creation and dissolution of treaties.
#' `vignette(teaching2)` shows how to model just the creation of treaties
#' conditional on creation and dissolution.
#' @seealso [defineNodes()], [defineNetwork()], [linkEvents()]
#' @examples
#' actors <- data.frame(
#'   actor = 1:5, label = paste("Actor", 1:5),
#'   present = TRUE, gender = sample.int(2, 5, replace = TRUE)
#' )
#' actors <- defineNodes(nodes = actors)
#' calls <- data.frame(
#'   time = c(12, 27, 45, 56, 66, 68, 87),
#'   sender = paste("Actor", c(1, 3, 5, 2, 3, 4, 2)),
#'   receiver = paste("Actor", c(4, 2, 3, 5, 1, 2, 5)), increment = rep(1, 7)
#' )
#' callNetwork <- defineNetwork(nodes = actors)
#' callNetwork <- linkEvents(
#'   x = callNetwork, changeEvent = calls, nodes = actors
#' )
#'
#' # Defining the dependent events:
#' callDependent <- defineDependentEvents(
#'   events = calls, nodes = actors, defaultNetwork = callNetwork
#' )
defineDependentEvents <- function(events, nodes, nodes2 = NULL,
                                  defaultNetwork = NULL,
                                  envir = environment()) {
  # check input types
  isTwoMode <- !is.null(nodes2)
  if (!is.data.frame(events)) {
    stop(
      "Invalid argument ", dQuote("events"), ": ",
      "this function expects objects of class ",
      dQuote("data.frame"), "."
    )
  }

  if (!any(checkClasses(nodes, c("data.frame", "nodes.goldfish")))) {
    stop(
      "Invalid argument ", dQuote("nodes"), ": ",
      "this function expects objects of class ",
      dQuote("data.frame"), " or ", dQuote("nodes.goldfish"), "."
    )
  }

  if (isTwoMode &&
    !any(checkClasses(nodes2, c("data.frame", "nodes.goldfish")))) {
    stop(
      "Invalid argument ", dQuote("nodes2"), ": ",
      "this function expects objects of class ",
      dQuote("data.frame"), " or ", dQuote("nodes.goldfish"), "."
    )
  }

  if (!is.null(defaultNetwork) &&
      !inherits(defaultNetwork, "network.goldfish")) { # styler: off
    stop(
      "Invalid argument ", dQuote("defaultNetwork"), ": ",
      "this function expects objects of class ",
      dQuote("network.goldfish"), "."
    )
  }

  # link objects
  nodesName <- c(
    as.character(substitute(nodes, envir)),
    as.character(substitute(nodes2, envir))
  )
  objEvents <- as.character(substitute(events, envir))
  objDefNet <- as.character(substitute(defaultNetwork, envir))

  attr(events, "nodes") <- nodesName

  # define class
  class(events) <- unique(c("dependent.goldfish", class(events)))

  # link events if defaultNetwork
  if (!is.null(defaultNetwork)) {
    if (!all(attr(defaultNetwork, "nodes") == nodesName)) {
      stop(
        "Node sets of default networks differ from",
        " node sets of dependent event data frame."
      )
    }

    attr(events, "defaultNetwork") <- objDefNet
    attr(events, "type") <- "dyadic"

    # check defaultNetwork is defined with the same events
    if (!any(objEvents %in% attr(defaultNetwork, "events"))) {
      warning(
        "The events data frame is not linked to the defaultNetwork.",
        "\nEvents attached to the ", dQuote("defaultNetwork"), ": ",
        paste(attr(defaultNetwork, "events"), collapse = ", "),
        "\nDependent events: ", paste(objEvents, collapse = ""),
        "\n"
      )
    }
  } else {
    attr(events, "type") <- "monadic"
  }

  # check format
  tryCatch(
    checkDependentEvents(
      events = events, eventsName = objEvents,
      nodes = nodes, nodes2 = nodes2,
      defaultNetwork = defaultNetwork, environment = envir
    ),
    error = function(e) {
      scalls <- sys.calls()
      e$call <- scalls[[1]]
      e$message <- paste(
        "The dependent events couldn't be constructed: ",
        e$message
      )
      # events <- NA
      stop(e)
    }
  )

  return(events)
}

#' Define a global time-varying attribute
#'
#' This function allows to define a global attribute of the nodeset
#' (i.e a variable that is identical for each node but changes over time).
#'
#' @param global a data frame containing all the values this global attribute
#' takes along time.
#' @return an object of class `global.goldfish`
#' @export
#' @details  For instance, seasonal climate changes could be defined as a
#' changing global attribute.
#' Then, this global attribute can be linked to the nodeset by using
#' [linkEvents()]
#' @examples
#' seasons <- defineGlobalAttribute(data.frame(time = 1:12, replace = 1:12))
defineGlobalAttribute <- function(global) {
  # check input types
  if (!is.data.frame(global)) {
    stop("Invalid argument: this function expects a data frame.")
  }

  # define class
  class(global) <- unique(c("global.goldfish", class(global)))

  # check format
  tryCatch(
    checkGlobalAttribute(global),
    error = function(e) {
      scalls <- sys.calls()
      e$call <- scalls[[1]]
      e$message <- paste(
        "The global attribute couldn't be constructed: ",
        e$message
      )
      # global <- NA
      stop(e)
    }
  )

  return(global)
}

#' Link dynamic events to a nodeset or a network
#'
#' Link events stored in data frames that modify attributes of the nodeset or
#' modify ties in a network.
#'
#' The data frame that contains the events must contain variables with specific
#' names depending if they refer to dynamic attributes or dynamic networks.
#'
#' For dynamic networks stored in object of class `network.goldfish` the
#' `changeEvents` data frame must contain the following variables:
#'
#' \describe{
#' \item{time}{`numeric` or `POSIXct` (see [base::as.Date()]) variable
#' containing the time-stamps when the event happen.}
#' \item{sender}{`character` variable indicating the label of the sender
#' of the event.}
#' \item{receiver}{`character` variable indicating the label of the receiver
#' of the event.}
#' }
#'
#' See the `bilatchanges` and `contigchanges` data frames in the
#' [Fisheries_Treaties_6070] datasets for examples of event data frames that
#' relate with dynamic networks.
#'
#' For dynamic attributes stored in object of class `nodes.goldfish` the
#' `changeEvents` data frame must contain the following variables:
#'
#' \describe{
#' \item{time}{`numeric` or `POSIXct` (see [base::as.Date()]) variable
#' containing the time-stamps when the event happen.}
#' \item{label}{`character` variable indicating the label of the node
#' for which the attribute changes.}
#' }
#'
#' See `sovchanges`, `regchanges` and `gdpchanges` data frames in the
#' [Fisheries_Treaties_6070] datasets for examples of event data frames that
#' relate with dynamic attributes
#'
#' For both cases an additional variable indicates the change in value of either
#' the ties or attributes.
#' The class of this variable must be the same as the tie value or attribute
#' value that will be updated, i.e., when the `present` variable is dynamic the
#' updating values must be `logical` (see [defineNodes()] for a description
#' of this variable.
#' There are two possibilities on how to specify those
#' changes but only one can be used at a time:
#' \describe{
#' \item{increment}{with a numerical value that represent the increment
#' (when it's positive value) or the decrement (when it's a negative value)
#' of the dynamic element from their past value (with respect to the `time`
#' value).
#'
#' In the [Social_Evolution] dataset the `calls` data frame contains
#' the calling events between students where the increment represent a new call.
#' With every new call the dyad (sender-receiver) increase the count of calls
#' that had happen in the past.}
#'
#' \item{replace}{contains the value that would replace at point-time `time`
#' the attribute or tie value. It is usually the way to represent changes in
#' node attributes.
#'
#' In the [Fisheries_Treaties_6070] dataset the `sovchanges`,
#' `regchanges` and `gdpchanges`data frames are examples where the `replace`
#' variable is used to specify attribute changes and their class match with the
#' variable in the node set.
#'
#' Dynamic network attributes can be also defined using the `replace` variable.
#' The `contigchanges` data frame in the [Fisheries_Treaties_6070] dataset, and
#' `friendship` data frame in the [Social_Evolution] are examples of this.
#' }
#' }
#'
#' @param x Either a nodeset (`nodes.goldfish` object) or a network
#'   (`network.goldfish` object)
#' @param ... additional arguments to be passed to the method.
#' @param changeEvents The name of a data frame that represents a
#' valid events list.
#' @param attribute a character vector indicating the names of the attributes
#'   that should be updated by the specified events (ONLY if the object is a
#'   nodeset).
#' @param nodes a nodeset (`data.frame` or `nodes.goldfish` object)
#'   related to the network (ONLY if `x` is a network)
#' @param nodes2 an optional nodeset (`data.frame` or `nodes.goldfish` object)
#'   related to the network (ONLY if `x` is a network)
#' @return an object with the same class as the object `x`.
#' For objects of class `network.goldfish` the attribute `events` with the name
#' of the event data frame passed through with the argument `changeEvents`.
#' For objects of class `nodes.goldfish` attibutes `events` and
#' `dynamicAttribute` are modified with name of the objects passed through with
#' the arguments `changeEvents` and `attribute` respectively.
#' @export linkEvents
#' @seealso [defineNodes()], [defineNetwork()]
#' @examples
#' actors <- data.frame(
#'   actor = 1:5, label = paste("Actor", 1:5),
#'   present = TRUE, gender = sample.int(2, 5, replace = TRUE)
#' )
#' actors <- defineNodes(nodes = actors)
#' callNetwork <- defineNetwork(nodes = actors)
#'
#' # Link events to a nodeset
#' compositionChangeEvents <- data.frame(
#'   time = c(14, 60),
#'   node = "Actor 4",
#'   replace = c(FALSE, TRUE)
#' )
#' actorsnew <- linkEvents(
#'   x = actors, attribute = "present", changeEvents = compositionChangeEvents
#' )
#'
#' # Link events to a Network
#' calls <- data.frame(
#'   time = c(12, 27, 45, 56, 66, 68, 87),
#'   sender = paste("Actor", c(1, 3, 5, 2, 3, 4, 2)),
#'   receiver = paste("Actor", c(4, 2, 3, 5, 1, 2, 5)),
#'   increment = rep(1, 7)
#' )
#' callNetwork <- linkEvents(
#'   x = callNetwork, changeEvent = calls, nodes = actors
#' )
linkEvents <- function(x, ...) {
  UseMethod("linkEvents", x)
}

#' @rdname linkEvents
#' @export
linkEvents.nodes.goldfish <- function(x, changeEvents, attribute, ...) {
  # check input types
  if (!(is.character(attribute) && length(attribute) == 1)) {
    stop(
      "Invalid argument attributes:",
      " this function expects a character attribute value."
    )
  }

  if (!is.data.frame(changeEvents)) {
    stop("Invalid argument changeEvents: this function expects a data frame.")
  }

  # data frame has to be passed as a variable name
  linkEnvir <- environment()
  if (!is.name(substitute(changeEvents, linkEnvir))) {
    stop(
      "Parameter change events has to be the name of a data frame",
      " (rather than a data frame)"
    )
  }

  # link data
  # initial <- object
  objEventsPrev <- attr(x, "events")
  objEventCurr <- as.character(substitute(changeEvents, linkEnvir))

  if (length(objEventsPrev) > 0 && objEventCurr %in% objEventsPrev) {
    warning(
      "The event ", dQuote(objEventCurr),
      " were already linked to this object."
    )
    return(x)
  }

  attr(x, "events") <- c(objEventsPrev, objEventCurr)
  attr(x, "dynamicAttributes") <- c(attr(x, "dynamicAttributes"), attribute)

  # check format
  tryCatch(
    {
      checkEvents(
        object = x, events = changeEvents, eventsName = objEventCurr,
        attribute = attribute, environment = linkEnvir
      )
    },
    error = function(e) {
      scalls <- sys.calls()
      e$call <- scalls[[1]]
      e$message <- paste("The events couldn't be added: ", e$message)
      # object <- initial
      stop(e)
    }
  )

  return(x)
}

#' @rdname linkEvents
#' @export
linkEvents.network.goldfish <- function(x, changeEvents,
                                        nodes = NULL, nodes2 = NULL, ...) {
  # check input types
  if (is.null(nodes)) {
    stop(
      "Invalid argument nodes: a network is specified,",
      "this function expects an argument nodes."
    )
  }
  if (!is.data.frame(changeEvents)) {
    stop("Invalid argument changeEvents: this function expects a data frame.")
  }

  isTwoMode <- !is.null(nodes2)
  if (!is.data.frame(nodes)) {
    stop(
      "Invalid argument nodes: this function expects a nodeset",
      " (data frame or nodes.goldfish object)."
    )
  }
  if (isTwoMode && !is.data.frame(nodes2)) {
    stop(
      "Invalid argument nodes2: this function expects a nodeset",
      " (data frame or nodes.goldfish object)."
    )
  }

  # data frame has to be passed as a variable name
  linkEnvir <- environment()
  if (!is.name(substitute(changeEvents, linkEnvir))) {
    stop(
      "Parameter change events has to be the name of a data frame",
      " (rather than a data frame)"
    )
  }

  # link data
  # initial <- x
  objEventsPrev <- attr(x, "events")
  objEventCurr <- as.character(substitute(changeEvents, linkEnvir))

  if (length(objEventsPrev) > 0 && objEventCurr %in% objEventsPrev) {
    warning(
      "The event ", dQuote(objEventCurr),
      " were already linked to this object."
    )
    return(x)
  }
  attr(x, "events") <- c(objEventsPrev, objEventCurr)

  # check format
  tryCatch(
    {
      checkEvents(
        object = x, events = changeEvents, eventsName = objEventCurr,
        nodes = nodes, nodes2 = nodes2, environment = linkEnvir
      )
    },
    error = function(e) {
      scalls <- sys.calls()
      e$call <- scalls[[1]]
      e$message <- paste("The events couldn't be added: ", e$message)
      # x <- initial
      stop(e)
    }
  )

  return(x)
}

#' @rdname linkEvents
#' @export
linkEvents.default <- function(x, ...) {
  if (!any(checkClasses(x, c("nodes.goldfish", "network.goldfish")))) {
    stop(
      "Invalid argument object: this function expects either a ",
      dQuote("nodes.goldfish"), " or a ", dQuote("network.goldfish"),
      " object."
    )
  }
}
