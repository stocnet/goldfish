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
#' (see [make_nodes()]) or a matrix from an object of class
#' `network.goldfish` (see [make_network()]) with the attributes
#' or the network ties updated according with the events linked to the object
#' using the [link_events()]) function.
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
#' @seealso [make_network()], [make_nodes()], [link_events()]
#' @examples
#' \donttest{
#' data("Fisheries_Treaties_6070")
#' states <- make_nodes(states)
#' states <- link_events(states, sovchanges, attribute = "present")
#' states <- link_events(states, regchanges, attribute = "regime")
#' states <- link_events(states, gdpchanges, attribute = "gdp")
#'
#' bilatnet <- make_network(bilatnet, nodes = states, directed = FALSE)
#' bilatnet <- link_events(bilatnet, bilatchanges, nodes = states)
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
  dynamic_attributes <- attr(df, "dynamic_attributes")
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
      df[[dynamic_attributes[i]]][events$node] <- events$replace
    }
    if (nrow(events) > 0 && !is.null(events$increment)) {
      for (k in seq_len(nrow(events))) {
        oldValue <- df[[dynamic_attributes[i]]][events[k, ]$node]
        df[[dynamic_attributes[i]]][events[k, ]$node] <-
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
# states <- make_nodes(states)
# states <- link_events(states, sovchanges, attribute = "present")
# states <- link_events(states, regchanges, attribute = "regime")
# states <- link_events(states, gdpchanges, attribute = "gdp")
#
# bilatnet <- make_network(bilatnet, nodes = states, directed = FALSE)
# bilatnet <- link_events(bilatnet, bilatchanges, nodes = states)
#
# contignet <- make_network(contignet, nodes = states, directed = FALSE)
# contignet <- link_events(contignet, contigchanges, nodes = states)
#
# createBilat <- make_dependent_events(
#   events = bilatchanges[bilatchanges$increment == 1, ],
#   nodes = states, default_network = bilatnet
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
#       any(check_classes(get(x, envir = envir), classes = classesToKeep))
#
#     object <- Filter(ClassFilter, y)
#     # if(is.null(object)) stop("No goldfish objects defined.")
#
#     # identify classes of these objects
#     classes <- vapply(
#       object,
#       FUN = function(x)
#         check_classes(get(x, envir = envir), classes = classesToKeep),
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
#         function(x) paste(attr(get(x, envir = envir), "dynamic_attributes"),
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
#                           net <- attr(get(x, envir = envir), "default_network")
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
#' The `make_nodes()` function processes and checks the `data.frame` passed to
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
#' Using [link_events()] is possible to link events where the composition of
#' available nodes changes over time, see `vignette("teaching2")`.
#'
#' For the attributes in the nodeset to become dynamic, them
#' can be linked to a dynamic event-list data frames in the initial state
#' object by using the [link_events()].
#' A new call of `link_events()` is required for each attribute that is dynamic.
#'
#' Objects of class [tibble::tbl_df] from the tibble package frequently use in the
#' tidyverse ecosystem and objects from the \pkg{data.table} package will produce
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
#' \item{events}{An empty character vector. [link_events()] is used to
#' link event data frames.}
#' \item{dynamic_attributes}{An empty character vector. [link_events()] is used to
#' link event data frames and their related attribute.}
#'
#' The object can be modified using methods for data frames.
#' @export
#' @seealso [make_network()], [link_events()]
#' @examples
#' nodesAttr <- data.frame(
#'   label = paste("Actor", 1:5),
#'   present = c(TRUE, FALSE, TRUE, TRUE, FALSE),
#'   gender = c(1, 2, 1, 1, 2)
#' )
#' nodesAttr <- make_nodes(nodes = nodesAttr)
#'
#' # Social evolution nodes definition
#' data("Social_Evolution")
#' actors <- make_nodes(actors)
#'
#' # Fisheries treaties nodes definition
#' data("Fisheries_Treaties_6070")
#' states <- make_nodes(states)
make_nodes <- function(nodes) {
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
  attr(nodes, "dynamic_attributes") <- vector("character")
  # check format
  tryCatch(check_nodes(nodes), error = function(e) {
    scalls <- sys.calls()
    e$call <- scalls[[1]]
    nodes <- NA
    e$message <- paste("The nodeset couldn't be constructed: ", e$message)
    stop(e)
  })
  return(nodes)
}

# alias
#' @rdname make_nodes
#' @export
make_nodes_goldfish <- make_nodes

#' Defining a network with dynamic events
#'
#' The function defines a network object either from a nodeset or
#' from a matrix (sociomatrix or adjacency matrix). If a matrix is used as
#' input, `make_network()` returns a network filled with the same values
#' as the ones present in the provided network.
#' If the nodeset is the only argument, `make_network()` returns an
#' empty network with the number of columns and rows corresponding to the
#' size of the nodeset.
#' These networks are static, but they can be turned into dynamic networks
#' by linking dynamic events to the network objectw using [link_events()].
#'
#' @param matrix An initial matrix (optional), and object of class `matrix`.
#' @param nodes A node-set (see [make_nodes()]).
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
#' \item{events}{An empty character vector. [link_events()] is used to
#' link event data frames.}
#'
#' The object can be modified using methods for matrix.
#' @details
#' If a matrix is used as input, its dimension names must be a subset of the
#' nodes in the nodeset as defined with the [make_nodes()] and the order of
#' the labels in rows and columns must correspond to the order of node labels
#' in the nodeset.
#' The matrix can be directed or undirected (as specified with the
#' `directed` argument).
#'
#' If the network is updated over time (e.g., a new wave of friendship data is
#' collected), these changes can be added with the [link_events()] - similar to
#' link changing attribute events to a nodeset.
#' This time, the user needs to provide the network and the associated nodeset.
#' If no matrix is provided, goldfish only considers the nodeset and assumes
#' the initial state to be empty (i.e., a matrix containing only 0s).
#'
#' @seealso [make_nodes()], [link_events()]
#' @examples
#' # If no intial matrix is provided
#' data("Social_Evolution")
#' callNetwork <- make_network(nodes = actors)
#'
#' # If a initial matrix is provided
#' data("Fisheries_Treaties_6070")
#' bilatnet <- make_network(bilatnet, nodes = states, directed = FALSE)
make_network <- function(
    matrix = NULL, nodes, nodes2 = NULL, directed = TRUE,
    envir = environment()) {
  # check input types
  is_two_mode <- !is.null(nodes2)
  n_row <- nrow(nodes)
  n_col <- ifelse(is_two_mode, nrow(nodes2), nrow(nodes))
  if (!any(check_classes(nodes, c("data.frame", "nodes.goldfish")))) {
    stop(
      "Invalid argument ", dQuote("nodes"), ": ",
      "this function expects objects of class ",
      dQuote("data.frame"), " or ", dQuote("nodes.goldfish"), "."
    )
  }
  if (!is.null(nodes2) &&
      !any(check_classes(nodes2, c("data.frame", "nodes.goldfish")))) {
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
      0, n_row, n_col,
      dimnames = list(
        sender = nodes$label,
        receiver = if (is_two_mode) nodes2$label else nodes$label
      )
    )
  } else if (is.table(matrix)) {
    if (length(dim(matrix)) != 2) {
      stop(dQuote("matrix"), ' object has an incorrect number of dimensions.",
           "Expected 2 dimensions')
    }
    matrix <- structure(matrix, class = NULL, call = NULL)
  } else if (!any(check_classes(matrix, c("matrix", "Matrix")))) {
    stop(
      "Invalid argument ", dQuote("matrix"), ": ",
      "this function expects an objects of class ",
      dQuote("matrix"), " or ", dQuote("Matrix"), "."
    )
  }

  # define class
  class(matrix) <- unique(c("network.goldfish", class(matrix)))
  
  # create attributes
  nodesName <- c(
    as.character(substitute(nodes, env = envir)),
    as.character(substitute(nodes2, env = envir))
  )
  
  attr(matrix, "nodes") <- nodesName
  attr(matrix, "directed") <- directed
  attr(matrix, "is_two_mode") <- is_two_mode
  attr(matrix, "events") <- vector("character")

  # check format
  tryCatch(check_network(matrix, nodes, nodesName, nodes2 = nodes2),
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

# Alias for make_network
#' @export
#' @rdname make_network
make_network_goldfish <- make_network

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
#' @param default_network the name of a `network.goldfish` object.
#' @param envir An [environment-class] object where the nodes-set
#' and default network objects are defined. The default value is
#' [environment()].
#' @return an object with additional class `dependent.goldfish` with attributes:
#' \item{nodes}{a character vector with the names of the nodes set that define
#' the dimensions of the `default_network`. `nodes` and `nodes2` arguments.}
#' \item{default_network}{A character value with the name of the network object
#' when this is present. `default_network` argument.}
#' \item{type}{A character value that can take values monadic or dyadic
#' depending on the arguments used during the definition.}
#'
#' The object can be modified using methods for data frames.
#' @export
#' @details Before this step is performed, we have to define:
#' the nodeset ([make_nodes()]), the network ([make_network()])
#'  and the link the event list to the network ([link_events()]).
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
#' @seealso [make_nodes()], [make_network()], [link_events()]
#' @examples
#' actors <- data.frame(
#'   actor = 1:5, label = paste("Actor", 1:5),
#'   present = TRUE, gender = sample.int(2, 5, replace = TRUE)
#' )
#' actors <- make_nodes(nodes = actors)
#' calls <- data.frame(
#'   time = c(12, 27, 45, 56, 66, 68, 87),
#'   sender = paste("Actor", c(1, 3, 5, 2, 3, 4, 2)),
#'   receiver = paste("Actor", c(4, 2, 3, 5, 1, 2, 5)), increment = rep(1, 7)
#' )
#' callNetwork <- make_network(nodes = actors)
#' callNetwork <- link_events(
#'   x = callNetwork, change_events = calls, nodes = actors
#' )
#'
#' # Defining the dependent events:
#' callDependent <- make_dependent_events(
#'   events = calls, nodes = actors, default_network = callNetwork
#' )
make_dependent_events <- function(events, nodes, nodes2 = NULL,
                                  default_network = NULL,
                                  envir = environment()) {
  # check input types
  is_two_mode <- !is.null(nodes2)
  if (!is.data.frame(events)) {
    stop(
      "Invalid argument ", dQuote("events"), ": ",
      "this function expects objects of class ",
      dQuote("data.frame"), "."
    )
  }

  if (!any(check_classes(nodes, c("data.frame", "nodes.goldfish")))) {
    stop(
      "Invalid argument ", dQuote("nodes"), ": ",
      "this function expects objects of class ",
      dQuote("data.frame"), " or ", dQuote("nodes.goldfish"), "."
    )
  }

  if (is_two_mode &&
    !any(check_classes(nodes2, c("data.frame", "nodes.goldfish")))) {
    stop(
      "Invalid argument ", dQuote("nodes2"), ": ",
      "this function expects objects of class ",
      dQuote("data.frame"), " or ", dQuote("nodes.goldfish"), "."
    )
  }

  if (!is.null(default_network) &&
    !inherits(default_network, "network.goldfish")) { # styler: off
    stop(
      "Invalid argument ", dQuote("default_network"), ": ",
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
  objDefNet <- as.character(substitute(default_network, envir))

  attr(events, "nodes") <- nodesName

  # define class
  class(events) <- unique(c("dependent.goldfish", class(events)))

  # link events if default_network
  if (!is.null(default_network)) {
    if (!all(attr(default_network, "nodes") == nodesName)) {
      stop(
        "Node sets of default networks differ from",
        " node sets of dependent event data frame."
      )
    }

    attr(events, "default_network") <- objDefNet
    attr(events, "type") <- "dyadic"

    # check default_network is defined with the same events
    events_network <- attr(default_network, "events")
    if (!any(objEvents %in% events_network)) {
      warning(
        "The events data frame is not linked to the default_network",
        "\nEvents attached to the ", dQuote("default_network"), ": ",
        if (length(events_network) > 0) 
          paste(events_network, collapse = ", ")  else "no events linked",
        "\nDependent events: ", paste(objEvents, collapse = ""),
        "\n"
      )
    }
  } else {
    attr(events, "type") <- "monadic"
  }

  # check format
  tryCatch(
    check_dependent_events(
      events = events, events_name = objEvents,
      nodes = nodes, nodes2 = nodes2,
      default_network = default_network, environment = envir
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

#' @rdname make_dependent_events
#' @export
make_dependent_events_goldfish <- make_dependent_events

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
#' [link_events()]
#' @examples
#' seasons <- make_global_attribute(data.frame(time = 1:12, replace = 1:12))
make_global_attribute <- function(global) {
  # check input types
  if (!is.data.frame(global)) {
    stop("Invalid argument: this function expects a data frame.")
  }

  # define class
  class(global) <- unique(c("global.goldfish", class(global)))

  # check format
  tryCatch(
    check_global_attribute(global),
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

#' Create a data object for goldfish models
#'
#' This function creates a new object of class `data.goldfish` and
#' populates it with the provided `R` objects and their linked objects,
#' as specified by attributes common in the 'goldfish' package.
#' This is useful for creating a self-contained data context for `estimate()`
#' and `gather_model_data()`.
#'
#' The function recursively searches for linked objects:
#' \itemize{
#'   \item{For a \code{nodes.goldfish} object:} Events that modify 
#'     its nodal attributes.
#'   \item{For a \code{network.goldfish} object:} Events that modify 
#'     its structure, and the \code{nodes.goldfish} object(s) that define
#'     its nodes.
#'   \item{For a \code{dependent.goldfish} object:} The \code{network.goldfish} 
#'     object and \code{nodes.goldfish} object(s) defining its events' scope.
#' }
#' Linked objects are searched for in the `parent_env`
#' (defaults to the calling environment) and the enclosing frames of the
#' `parent_env` environment (see [base::get()], [base::exists()]).
#'
#' @param ... Objects to be included in the data environment. These objects
#'   will be copied by their given argument names.
#' @param parent_env The parent environment for the new data environment.
#'   Also, the environment from which linked objects (not explicitly provided
#'   in `...`) will be searched. Defaults to `parent.frame()`.
#'
#' @return An environment of class `data.goldfish`
#'   containing the specified objects and their resolved dependencies.
#'
#' @export
#' @examples
#' data("Social_Evolution")
#' callNetwork <- make_network(nodes = actors, directed = TRUE)
#' callNetwork <- link_events(
#'   x = callNetwork, change_event = calls,
#'   nodes = actors
#' )
#' callsDependent <- make_dependent_events(
#'   events = calls, nodes = actors,
#'   default_network = callNetwork
#' )
#' socialEvolutionData <- make_data(
#'   callNetwork, callsDependent, actors
#' )
#' 
#' data("Fisheries_Treaties_6070")
#' states <- make_nodes(states)
#' states <- link_events(states, sovchanges, attribute = "present")
#' states <- link_events(states, regchanges, attribute = "regime")
#' states <- link_events(states, gdpchanges, attribute = "gdp")
#'
#' bilatnet <- make_network(bilatnet, nodes = states, directed = FALSE)
#' bilatnet <- link_events(bilatnet, bilatchanges, nodes = states)
#'
#' contignet <- make_network(contignet, nodes = states, directed = FALSE)
#' contignet <- link_events(contignet, contigchanges, nodes = states)
#'
#' createBilat <- make_dependent_events(
#'   events = bilatchanges[bilatchanges$increment == 1, ],
#'   nodes = states, default_network = bilatnet
#' )
#' 
#' fisheriesData <- make_data(
#'   bilatnet, createBilat, states,
#'   contignet, sovChanges, regChanges, gdpChanges
#' )
#' 
make_data <- function(..., parent_env = parent.frame()) {
  data_env <- new.env(parent = parent_env)

  initial_objects <- list(...)
  # Capture argument names as they were passed
  arg_names <- vapply(
    X = match.call(expand.dots = FALSE)$...,
    FUN = deparse,
    FUN.VALUE = character(1)
  )

  # Handle cases with no arguments or only named arguments carefully
  if (length(initial_objects) == 0 && length(arg_names) == 0) {
    stop("No arguments provided to make_data.", call. = FALSE)
  } else if (length(initial_objects) > 0 &&
             length(arg_names) != length(initial_objects)) {
    # This can happen if objects in ... are themselves named lists.
    # If `...` contains a mix of named and unnamed,
    # `arg_names` should still be correct.
    stop("make_data() received a mix of named and unnamed arguments.",
         call. = FALSE)
    # Potentially reconcile arg_names with names(initial_objects)
  }

  queue <- character(0) # Stores names of objects to process for links
  processed_names <- character(0) # Stores names of objects already processed
  all_nodes_names <- character(0)
  all_events_names <- character(0)

  # Initial population of data_env with explicitly provided objects
  for (i in seq_along(initial_objects)) {
    obj_name <- arg_names[i]
    obj <- initial_objects[[i]]
    assign(obj_name, obj, envir = data_env)
    # Add to queue for link checking if not already processed or queued
    if (!(obj_name %in% queue) && !(obj_name %in% processed_names)) {
      queue <- c(queue, obj_name)
    }
  }

  # Recursively find and add linked objects
  while (length(queue) > 0) {
    current_name <- queue[1]
    queue <- queue[-1] # Dequeue

    if (current_name %in% processed_names) {
      next # Already processed this object
    }

    if (!exists(current_name, where = data_env, inherits = FALSE)) {
      warning(sprintf(
        "Object '%s' was in queue but not found in data_env. Skipping.",
        current_name
      ))
      next
    }
    
    current_obj <- get(current_name, envir = data_env, inherits = FALSE)
    processed_names <- c(processed_names, current_name)
    
    linked_names_to_find <- character(0)

    # Check for linked objects and collect their names
    if (inherits(current_obj, "dependent.goldfish")) {
      nodes_attr <- attr(current_obj, "nodes")
      network_attr <- attr(current_obj, "default_network")
      linked_names_to_find <- c(linked_names_to_find, nodes_attr, network_attr)
      all_nodes_names <- c(all_nodes_names, nodes_attr)
    } else if (inherits(current_obj, "network.goldfish")) {
      nodes_attr <- attr(current_obj, "nodes")
      events_attr <- attr(current_obj, "events")
      linked_names_to_find <- c(linked_names_to_find, nodes_attr, events_attr)
      all_nodes_names <- c(all_nodes_names, nodes_attr)
      all_events_names <- c(all_events_names, events_attr)
    } else if (inherits(current_obj, "nodes.goldfish")) {
      events_attr <- attr(current_obj, "events") # Name of the events data frame
      if (length(events_attr) > 0) {
        linked_names_to_find <- c(linked_names_to_find, events_attr)
        all_events_names <- c(all_events_names, events_attr)
      }
    }

    # Clean up potential empty strings from attribute values
    linked_names_to_find <- linked_names_to_find[nzchar(linked_names_to_find)]
    linked_names_to_find <- unique(linked_names_to_find)

    for (linked_name in linked_names_to_find) {
      if (!exists(linked_name, where = data_env, inherits = FALSE)) {
        # Try to find the linked object in the parent_env
        if (exists(linked_name, where = parent_env, inherits = TRUE)) {
          linked_obj <- get(linked_name, envir = parent_env, inherits = TRUE)
          assign(linked_name, linked_obj, envir = data_env)
          # Add the newly found linked object to the queue
          if (!(linked_name %in% queue) &&
              !(linked_name %in% processed_names)) {
            queue <- c(queue, linked_name)
          }
        } else {
          warning(
            paste0(
              "Linked object '", linked_name, "' (required by '",
              current_name, "') was not found in the specified parent_env."
            )
          )
        }
      }
    }
  }

  assign(".nodeset_names",
         unique(all_nodes_names[nzchar(all_nodes_names)]),
         envir = data_env)
  assign(".events_names",
         unique(all_events_names[nzchar(all_events_names)]),
         envir = data_env)


  class(data_env) <- c("data.goldfish", "environment")
  return(data_env)
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
#' `change_events` data frame must contain the following variables:
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
#' `change_events` data frame must contain the following variables:
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
#' updating values must be `logical` (see [make_nodes()] for a description
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
#' @param change_events The name of a data frame that represents a
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
#' of the event data frame passed through with the argument `change_events`.
#' For objects of class `nodes.goldfish` attibutes `events` and
#' `dynamic_attribute` are modified with name of the objects passed through with
#' the arguments `change_events` and `attribute` respectively.
#' @export link_events
#' @seealso [make_nodes()], [make_network()]
#' @examples
#' actors <- data.frame(
#'   actor = 1:5, label = paste("Actor", 1:5),
#'   present = TRUE, gender = sample.int(2, 5, replace = TRUE)
#' )
#' actors <- make_nodes(nodes = actors)
#' callNetwork <- make_network(nodes = actors)
#'
#' # Link events to a nodeset
#' compositionChangeEvents <- data.frame(
#'   time = c(14, 60),
#'   node = "Actor 4",
#'   replace = c(FALSE, TRUE)
#' )
#' actorsnew <- link_events(
#'   x = actors, attribute = "present", change_events = compositionChangeEvents
#' )
#'
#' # Link events to a Network
#' calls <- data.frame(
#'   time = c(12, 27, 45, 56, 66, 68, 87),
#'   sender = paste("Actor", c(1, 3, 5, 2, 3, 4, 2)),
#'   receiver = paste("Actor", c(4, 2, 3, 5, 1, 2, 5)),
#'   increment = rep(1, 7)
#' )
#' callNetwork <- link_events(
#'   x = callNetwork, change_events = calls, nodes = actors
#' )
link_events <- function(x, ...) {
  UseMethod("link_events", x)
}

#' @rdname link_events
#' @export
link_events.nodes.goldfish <- function(x, change_events, attribute, ...) {
  # check input types
  if (!(is.character(attribute) && length(attribute) == 1)) {
    stop(
      "The 'attribute' argument must be a character vector of length one."
    )
  }

  if (!is.data.frame(change_events)) {
    stop("Invalid argument change_events: this function expects a data frame.")
  }

  # data frame has to be passed as a variable name
  linkEnvir <- environment()
  if (!is.name(substitute(change_events, linkEnvir))) {
    stop(
      "Parameter change events has to be the name of a data frame",
      " (rather than a data frame)"
    )
  }

  # link data
  # initial <- object
  objEventsPrev <- attr(x, "events")
  objEventCurr <- as.character(substitute(change_events, linkEnvir))

  if (length(objEventsPrev) > 0 && objEventCurr %in% objEventsPrev) {
    warning(
      "The event ", dQuote(objEventCurr),
      " were already linked to this object."
    )
    return(x)
  }

  attr(x, "events") <- c(objEventsPrev, objEventCurr)
  attr(x, "dynamic_attributes") <- c(attr(x, "dynamic_attributes"), attribute)

  # check format
  tryCatch(
    {
      check_events(
        object = x, events = change_events, events_name = objEventCurr,
        environment = linkEnvir,
        attribute = attribute
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

#' @rdname link_events
#' @export
link_events.network.goldfish <- function(x, change_events,
                                        nodes = NULL, nodes2 = NULL, ...) {
  # check input types
  if (is.null(nodes)) {
    stop(
      "Invalid argument nodes: a network is specified,",
      "this function expects an argument nodes."
    )
  }
  if (!is.data.frame(change_events)) {
    stop("Invalid argument change_events: this function expects a data frame.")
  }

  is_two_mode <- !is.null(nodes2)
  if (!is.data.frame(nodes)) {
    stop(
      "Invalid argument nodes: this function expects a nodeset",
      " (data frame or nodes.goldfish object)."
    )
  }
  if (is_two_mode && !is.data.frame(nodes2)) {
    stop(
      "Invalid argument nodes2: this function expects a nodeset",
      " (data frame or nodes.goldfish object)."
    )
  }

  # data frame has to be passed as a variable name
  linkEnvir <- environment()
  if (!is.name(substitute(change_events, linkEnvir))) {
    stop(
      "Parameter change events has to be the name of a data frame",
      " (rather than a data frame)"
    )
  }

  # link data
  # initial <- x
  objEventsPrev <- attr(x, "events")
  objEventCurr <- as.character(substitute(change_events, linkEnvir))

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
      check_events(
        object = x, events = change_events, events_name = objEventCurr,
        environment = linkEnvir,
        nodes = nodes, nodes2 = nodes2
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

#' @rdname link_events
#' @export
link_events.default <- function(x, ...) {
  if (!any(check_classes(x, c("nodes.goldfish", "network.goldfish")))) {
    stop(
      "Invalid argument object: this function expects either a ",
      dQuote("nodes.goldfish"), " or a ", dQuote("network.goldfish"),
      " object."
    )
  }
}
