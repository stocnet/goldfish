##################### ###
#
# Goldfish package
# Some utility functions useful in
# various parts of the code
#
##################### ###


#' get data objects
#'
#' @param namedList list
#' @param keepOrder logical.
#' @param removeFirst logical.
#'
#' @return a
#' @noRd
#'
#' @examples
#' \donttest{
#' data("Social_Evolution")
#' callNetwork <- make_network(nodes = actors, directed = TRUE)
#' callNetwork <- link_events(
#'   x = callNetwork, change_events = calls, nodes = actors
#' )
#' callsDependent <- defineDependentEvents(
#'   events = calls, nodes = actors, default_network = callNetwork
#' )
#' parsedformula <- parseFormula(callsDependent ~ ego(actors$floor))
#' objectsEffectsLink <- getObjectsEffectsLink(parsedformula$rhsNames, 1L)
#' getDataObjects(list(rownames(objectsEffectsLink)), removeFirst = FALSE)
#' }
getDataObjects <- function(namedList, keepOrder = FALSE, removeFirst = TRUE) {
  # strip function names
  objNames <- unlist(namedList)
  if (removeFirst) objNames <- unlist(lapply(namedList, "[", -1))

  # strip named parameters except for reserved ones
  if (!is.null(names(objNames))) {
    ids <- isReservedElementName(names(objNames)) | names(objNames) == ""
    objNames <- objNames[ids]
  }

  if (!keepOrder) objNames <- unique(objNames)

  # # case list(...)
  areList <- grepl("list\\(\\s*(.+)\\s*\\)", objNames)
  .split <- ifelse(areList,
    gsub("list\\(\\s*(.+)\\s*\\)", "\\1", objNames),
    objNames
  )
  .split <- unlist(strsplit(.split, split = "\\s*,\\s*"))

  if (!keepOrder) .split <- unique(.split)
  # # case attributes
  split <- strsplit(.split, split = "$", fixed = TRUE)

  objNameTable <- Reduce(
    rbind,
    lapply(
      split,
      \(v) {
        if (length(v) == 1) {
          data.frame(
            object = v,
            nodeset = NA,
            attribute = NA,
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            object = NA,
            nodeset = v[1],
            attribute = v[2],
            stringsAsFactors = FALSE
          )
        }
      }
    )
  )

  return(cbind(name = .split, objNameTable, stringsAsFactors = FALSE))
}


getElementFromDataObjectTable <- function(x, envir = environment()) {
  elements <- list()
  if (nrow(x) == 0) {
    return(elements)
  }
  for (i in seq_len(nrow(x))) {
    elements[[i]] <- NA
    row <- x[i, ]
    if (!is.na(row$object)) {
      elements[[i]] <- get(row$object, envir = envir)
    }
    if (!is.na(row$nodeset) && !is.na(row$attribute)) {
      elements[[i]] <- getElement(
        get(row$nodeset, envir = envir),
        row$attribute
      )
    }
  }
  return(elements)
}


# Check whether (date) input is of POSIXct format
# currently not needed in parseTimeWindows because
# POSIXct doesn't have duration names and
# we don't want to have any unnecessary package dependencies.
# We keep this utility function however,
# as it may be useful elsewhere...
# is.POSIXct <- function(x) inherits(x, "POSIXct")


isReservedElementName <- function(x) {
  x %in% c("network", "attribute", "network2", "attribute2")
}


#' Sanitize events
#'
#' replace labels with IDs and specific time formats with numeric
#'
#' @param events a dataframe that represents a valid events list
#' @inheritParams link_events
#'
#' @return a data frame with IDs instead of labels and time in numeric format
#' @noRd
#'
#' @examples
#' \donttest{
#' data("Social_Evolution")
#' afterSanitize <- sanitizeEvents(calls, "actors")
#' }
sanitizeEvents <- function(events, nodes, nodes2 = nodes, envir = new.env()) {
  if (is.character(nodes)) nodes <- get(nodes, envir = envir)
  if (is.character(nodes2)) nodes2 <- get(nodes2, envir = envir)
  if (is.character(events$node)) {
    events$node <- match(events$node, nodes$label)
  }
  if (is.character(events$sender)) {
    events$sender <- match(events$sender, nodes$label)
  }
  if (is.character(events$receiver)) {
    events$receiver <- match(events$receiver, nodes2$label)
  }
  events$time <- as.numeric(events$time)
  events
}

#' Reduce preprocess output
#'
#' It took a preprocess object and return a matrix with all the
#' change statistics together for each effect.
#' `effectPos` argument allows to reduce just for a subset of effects,
#' it won't reduce the time or memory space used.
#'
#' @param preproData a preprocess data object from preprocess.
#' @param type a character. `"withTime"` returns the dependent stats changes
#' with the time where they occur.
#' @param effectPos a vector of integers of the effects to keep.
#'
#' @return a list with a matrix for each effect.
#' @noRd
#'
#' @examples
#' \donttest{
#' data("Social_Evolution")
#' callNetwork <- make_network(nodes = actors, directed = T)
#' callNetwork <- link_events(
#'   x = callNetwork, change_events = calls, nodes = actors
#' )
#' callsDependent <- make_dependent_events(
#'   events = calls, nodes = actors, default_network = callNetwork
#' )
#' prep <- estimate_dynam(callsDependent ~ inertia + trans,
#'   sub_model = "choice",
#'   preprocessing_only = TRUE, silent = TRUE
#' )
#' v00 <- ReducePreprocess(prep, "withTime")
#' v01 <- ReducePreprocess(prep, "withTime", c(1L, 3L))
#' v03 <- ReducePreprocess(prep, "withoutTime")
#' }
ReducePreprocess <- function(
    preproData,
    type = c("withTime", "withoutTime"),
    effectPos = NULL) {
  stopifnot(
    is.null(effectPos) || !is.null(effectPos) && inherits(effectPos, "integer")
  )
  type <- match.arg(type)

  nEffects <- dim(preproData$initialStats)[3]

  stopifnot(
    is.null(effectPos) || !is.null(effectPos) && max(effectPos) <= nEffects
  )

  ReduceEffUpdates <- function(statsChange, eventTime) {
    reduce <- Map(
      \(x, y) {
        lapply(
          x,
          \(z) {
            if (is.null(z)) {
              return(NULL)
            } # no changes, no problem
            if (nrow(z) == 1) {
              return(
                if (type == "withTime") cbind(time = y, z) else z
              )
            } # just one update, no problem

            discard <- duplicated(z[, c("node1", "node2")], fromLast = TRUE)
            changes <- cbind(
              time = if (type == "withTime") rep(y, sum(!discard)) else NULL,
              z[!discard, , drop = FALSE]
            )
            if (nrow(changes) == 1) {
              return(changes)
            }
            # print(changes)
            changes <- changes[order(changes[, "node1"], changes[, "node2"]), ]
          }
        ) # multiple updates might be repeated, keep the last
      },
      statsChange, eventTime
    )

    return(lapply(
      seq_len(nEffects),
      function(i) {
        Reduce(rbind, lapply(reduce, "[[", i))
      }
    ))
  }

  outDependentStatChange <- ReduceEffUpdates(
    preproData$dependentStatsChange,
    preproData$eventTime[preproData$orderEvents == 1]
  )

  if ((preproData$subModel == "rate" || preproData$model == "REM") &&
    length(preproData$rightCensoredStatsChange) > 0) {
    rightCensoredStatChange <- ReduceEffUpdates(
      preproData$rightCensoredStatsChange,
      preproData$eventTime[preproData$orderEvents == 2]
    )

    # combine lists
    reducedPrepro <- list()
    for (ii in seq.int(length(outDependentStatChange))) {
      reducedPrepro[[ii]] <- list(
        dependent = outDependentStatChange[[ii]],
        rightCensored = rightCensoredStatChange[[ii]]
      )
    }

    if (!is.null(effectPos)) {
      return(reducedPrepro[effectPos])
    } else {
      return(reducedPrepro)
    }
  } else if (!is.null(effectPos)) {
    return(outDependentStatChange[effectPos])
  } else {
    return(outDependentStatChange)
  }
}

#' Expand a set of changes
#'
#' given a `node` and a `replace` value, set the change to all the nodes in
#' the nodes `set`. Add the `time` to the array if provided.
#'
#' @param nodes a numeric vector with the sanitize position of the nodes
#' @param replace a numeric vector with the replace value
#' @param time a numeric vector with the time-stamp when the changes happen
#' @param set a numeric vector with the index id of the node set
#' @param is_two_mode logical, whether self ties are allow or not
#'
#' @return an array with columns `node1`, `node2`, `replace` and `time`
#' @noRd
#'
#' @examples
#' fillChanges(c(1, 3), c(4, 8), NULL, 1:5)
fillChanges <- function(nodes, replace, time, set, is_two_mode = FALSE) {
  times <- ifelse(is_two_mode, length(set), length(set) - 1)

  cbind(
    time = if (!is.null(time)) rep(time, each = times) else NULL,
    node1 = rep(nodes, each = times),
    node2 = Reduce(c, lapply(nodes, \(x) set[!set %in% x])),
    replace = rep(replace, each = times)
  )
}

#' update a network (adjacency matrix)
#'
#' @param network a network (adjacency matrix) to update with an event list.
#' @inheritParams link_events
#'
#' @return a network (adjacency matrix) after update events from `change_events`
#' @noRd
#'
#' @examples
#' \donttest{
#' data("Social_Evolution")
#' callNetwork <- make_network(nodes = actors, directed = TRUE)
#' callNetwork <- link_events(
#'   x = callNetwork, change_events = calls, nodes = actors
#' )
#' callsDependent <- make_dependent_events(
#'   events = calls, nodes = actors, default_network = callNetwork
#' )
#' prep <- estimate_dynam(callsDependent ~ inertia + trans,
#'   sub_model = "choice",
#'   preprocessing_only = TRUE, silent = TRUE
#' )
#' finalNet <- UpdateNetwork(callNetwork, calls, nodes = "actors")
#' finalStat <- UpdateNetwork(
#'   prep$initialStats[, , 1], ReducePreprocess(prep, "withoutTime", 1L)[[1]]
#' )
#' }
UpdateNetwork <- function(network, changeEvents, nodes = NULL, nodes2 = nodes) {
  stopifnot(
    inherits(network, "matrix"),
    inherits(changeEvents, "data.frame") || inherits(changeEvents, "matrix")
  )

  if (!is.null(nodes)) {
    changeEvents <- sanitizeEvents(changeEvents, nodes, nodes2)
  }


  if (inherits(changeEvents, "matrix") &&
    all(c("node1", "node2", "replace") %in% colnames(changeEvents))) {
    changeEvents <- data.frame(changeEvents)
    names(changeEvents)[match(c("node1", "node2"), names(changeEvents))] <-
      c("sender", "receiver")
  } else if (!all(c("sender", "receiver") %in% names(changeEvents))) {
    stop("Expected changeEvents with either c('sender', 'receiver') or ")
  }

  if ("time" %in% names(changeEvents)) {
    changeEvents$time <- NULL
  }

  # include additional checks
  if ("increment" %in% names(changeEvents)) {
    if (any(network[!is.na(network)] != 0)) {
      posNE <- which(!is.na(network) & network != 0, arr.ind = TRUE)
      # print(str(posNE))
      changeEvents <- rbind(
        changeEvents,
        data.frame(
          sender = posNE[, 1],
          receiver = posNE[, 2],
          increment = network[posNE]
        )
      )
    }

    redEvents <- stats::aggregate(
      increment ~ sender + receiver, changeEvents, sum
    )
    chIncrement <- match("increment", names(redEvents))
    names(redEvents)[chIncrement] <- "replace"
  } else if ("replace" %in% names(changeEvents)) {
    discard <- duplicated(changeEvents[, c("sender", "receiver")],
      fromLast = TRUE
    )
    redEvents <- changeEvents[
      !discard,
      c("sender", "receiver", "replace"),
      drop = FALSE
    ]
  }

  network[cbind(redEvents$sender, redEvents$receiver)] <- redEvents$replace
  return(network)
}

GetDetailPrint <- function(
    objectsEffectsLink,
    parsedformula,
    fixedParameters = NULL) {
  # matrix with the effects in rows and objects in columns,
  # which net or actor att
  maxObjs <- max(objectsEffectsLink, na.rm = TRUE)
  effectDescription <- matrix(
    t(
      apply(
        objectsEffectsLink, 2,
        function(x) {
          notNA <- !is.na(x)
          objs <- x[notNA]
          objs <- names(objs[order(objs)])
          c(objs, rep("", maxObjs - length(objs)))
        }
      )
    ),
    nrow = ncol(objectsEffectsLink),
    ncol = maxObjs
  )
  # # handle degenerate case one effect one object
  dimnames(effectDescription) <- list(
    colnames(objectsEffectsLink),
    if (ncol(effectDescription) == 1) {
      "Object"
    } else {
      sprintf("Object %d", seq_len(ncol(effectDescription)))
    }
  )

  objectsName <- colnames(effectDescription)
  # adding other parameters: each effect refers to which network
  # or actor attribute

  # effectDescription <- cbind(
  #   effect = rownames(effectDescription),
  #   effectDescription
  # )

  if (any(unlist(parsedformula$ignore_rep_parameter))) {
    effectDescription <- cbind(effectDescription,
      ignore_repetitions = ifelse(parsedformula$ignore_rep_parameter, "B", "")
    )
  }
  if (any(unlist(parsedformula$weighted_parameter))) {
    effectDescription <- cbind(effectDescription,
      weighted = ifelse(parsedformula$weighted_parameter, "W", "")
    )
  }
  if (any(parsedformula$type_parameter != "")) {
    effectDescription <- cbind(effectDescription,
      type = parsedformula$type_parameter
    )
  }
  hasWindows <- FALSE
  if (!all(vapply(parsedformula$window_parameters, is.null, logical(1)))) {
    hasWindows <- TRUE
    effectDescription <- cbind(
      effectDescription,
      window = vapply(
        parsedformula$window_parameters,
        function(x) ifelse(is.null(x), "", gsub("['\"]", "", x)),
        character(1)
      )
    )
    # reduce object name
    effectDescription[, objectsName] <- t(apply(
      effectDescription,
      1,
      \(x) {
        gsub(
          paste0("^(.+)_", gsub(" ", "", x["window"]), "$"),
          "\\1",
          x[objectsName]
        )
      }
    ))
  }
  if (any(parsedformula$trans_parameter != "")) {
    effectDescription <- cbind(effectDescription,
      transformer_fn = parsedformula$trans_parameter
    )
  }
  if (any(parsedformula$summ_parameter != "")) {
    effectDescription <- cbind(effectDescription,
      summarizer_fn = parsedformula$summ_parameter
    )
  }
  # DyNAMi
  if (any(parsedformula$joining_parameter != "")) {
    effectDescription <- cbind(effectDescription,
      joining = parsedformula$joining_parameter
    )
  }
  if (any(parsedformula$sub_type_parameter != "")) {
    effectDescription <- cbind(effectDescription,
      subType = parsedformula$sub_type_parameter
    )
  }
  if (any(parsedformula$historyParameter != "")) {
    effectDescription <- cbind(effectDescription,
                               history = parsedformula$historyParameter
    )
  }
  # rownames(effectDescription) <- NULL
  if (parsedformula$has_intercept) {
    effectDescription <- rbind("", effectDescription)
    rownames(effectDescription)[1] <- "Intercept"
  }

  if (!is.null(fixedParameters)) {
    effectDescription <- cbind(effectDescription,
      fixed = !is.na(fixedParameters)
    )
  }

  attr(effectDescription, "hasWindows") <- hasWindows
  return(effectDescription)
}

GetFixed <- function(object) {
  if ("fixed" %in% colnames(object$names)) {
    vapply(
      object$names[, "fixed"],
      function(x) eval(parse(text = x)),
      logical(1)
    )
  } else {
    rep(FALSE, length(object$parameters))
  }
}

checkArgsEstimation <- function(variables) {

}
