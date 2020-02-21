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
#' \dontrun{
#' data("Social_Evolution")
#' callNetwork <- defineNetwork(nodes = actors, directed = TRUE)
#' callNetwork <- linkEvents(x = callNetwork, changeEvent = calls, nodes = actors)
#' callsDependent <- defineDependentEvents(events = calls, nodes = actors, defaultNetwork = callNetwork)
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
                  objNames)
  .split <- unlist(strsplit(.split, split = "\\s*,\\s*"))

  if (!keepOrder) .split <- unique(.split)
  # # case attributes
  split <- strsplit(.split, split = "$", fixed = TRUE)

  objNameTable <- Reduce(
    rbind,
    lapply(
      split,
      function(v) if (length(v) == 1) {
          data.frame(object = v, nodeset = NA, attribute = NA, stringsAsFactors = FALSE)
        } else {
          data.frame(object = NA, nodeset = v[1], attribute = v[2], stringsAsFactors = FALSE)
        }
    )
  )

  return(cbind(name = .split, objNameTable, stringsAsFactors = FALSE))
}


getElementFromDataObjectTable <- function(x, envir = as.environment(-1)) {
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
      elements[[i]] <- getElement(get(row$nodeset, envir = envir), row$attribute)
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
is.POSIXct <- function(x) inherits(x, "POSIXct")


isReservedElementName <- function(x) {
  x %in% c("network", "attribute", "network2", "attribute2")
}


#' Sanitize events
#'
#' replace labels with IDs and specific time formats with numeric
#'
#' @param events a dataframe that represents a valid events list
#' @inheritParams linkEvents
#'
#' @return a data frame with IDs instead of labels and time in numeric format
#' @noRd
#'
#' @examples
#' \dontrun{
#' data("Social_Evolution")
#' afterSanitize <- sanitizeEvents(calls, "actors")
#' }
sanitizeEvents <- function(events, nodes, nodes2 = nodes) {
  if (is.character(nodes)) nodes <- get(nodes)
  if (is.character(nodes2)) nodes2 <- get(nodes2)
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
#' It took a preprocess object and return a matrix with all the change statistics
#' together for each effect. A subset of effects can be return using the effectPos parameter,
#' it won't reduce the time or memory space used.
#'
#' @param preproData a preprocess data object from preprocess.
#' @param type a character. "withTime" returns the dependent stats changes with the time where they occur.
#' @param effectPos a vector of integers of the effects to keep.
#'
#' @return a list with a matrix for each effect.
#' @noRd
#'
#' @examples
#' \dontrun{
#' data("Social_Evolution")
#' callNetwork <- defineNetwork(nodes = actors, directed = T)
#' callNetwork <- linkEvents(x = callNetwork, changeEvent = calls, nodes = actors)
#' callsDependent <- defineDependentEvents(events = calls, nodes = actors, defaultNetwork = callNetwork)
#' prep <- estimate(callsDependent ~ inertia + trans,
#'   model = "DyNAM", subModel = "choice",
#'   preprocessingOnly = TRUE, silent = TRUE
#' )
#' v00 <- ReducePreprocess(prep, "withTime")
#' v01 <- ReducePreprocess(prep, "withTime", c(1L, 3L))
#' v03 <- ReducePreprocess(prep, "withoutTime")
#' }
ReducePreprocess <- function(preproData, type = c("withTime", "withoutTime"), effectPos = NULL) {
  stopifnot(
    is.null(effectPos) || !is.null(effectPos) && inherits(effectPos, "integer")
  )
  type <- match.arg(type)

  nEffects <- dim(preproData$initialStats)[3]

  stopifnot(is.null(effectPos) || !is.null(effectPos) && max(effectPos) <= nEffects)

  if (type == "withTime") {
    addTime <- mapply(
      function(x, y) {
        lapply(
          x,
          function(z) {
            if (is.null(z)) {
              return(NULL)
            } # no changes, no problem
            if (nrow(z) == 1) {
              return(cbind(time = y, z))
            } # just one update, no problem

            discard <- duplicated(z[, c("node1", "node2")], fromLast = TRUE)
            changes <- cbind(time = rep(y, sum(!discard)), z[!discard, , drop = FALSE])
            if (nrow(changes) == 1) {
              return(changes)
            }
            # print(changes)
            changes <- changes[order(changes[, "node1"], changes[, "node2"]), ]
          }
        ) # multiple updates might be repeated, keep the last
      },
      preproData$dependentStatsChange, preproData$eventTime,
      SIMPLIFY = FALSE
    )

    outDependetStatChange <- lapply(
      seq_len(nEffects),
      function(i) {
        Reduce(rbind, lapply(addTime, "[[", i))
      }
    )
  } else if (type == "withoutTime") {
    whTime <- lapply(
      preproData$dependentStatsChange,
      function(x) {
        lapply(
          x,
          function(z) {
            if (is.null(z)) {
              return(NULL)
            } # no changes, no problem
            if (nrow(z) == 1) {
              return(z)
            } # just one update, no problem

            discard <- duplicated(z[, c("node1", "node2")], fromLast = TRUE)
            changes <- z[!discard, , drop = FALSE]
            if (nrow(changes) == 1) {
              return(changes)
            }
            changes <- changes[order(changes[, "node1"], changes[, "node2"]), ]
          }
        )
      }
    ) # multiple updates might be repeated, keep the last


    outDependetStatChange <- lapply(
      seq_len(nEffects),
      function(i) {
        Reduce(rbind, lapply(whTime, "[[", i))
      }
    )
  }

  if (!is.null(effectPos)) {
    return(outDependetStatChange[effectPos])
  } else {
    return(outDependetStatChange)
  }
}




#' update a network (adjacency matrix)
#'
#' @param network a network (adjacency matrix) to update with an event list.
#' @inheritParams linkEvents
#'
#' @return a network (adjacency matrix) after update events from changeEvents
#' @noRd
#'
#' @examples
#' \dontrun{
#' data("Social_Evolution")
#' callNetwork <- defineNetwork(nodes = actors, directed = T)
#' callNetwork <- linkEvents(x = callNetwork, changeEvent = calls, nodes = actors)
#' callsDependent <- defineDependentEvents(events = calls, nodes = actors, defaultNetwork = callNetwork)
#' prep <- estimate(callsDependent ~ inertia + trans,
#'   model = "DyNAM", subModel = "choice",
#'   preprocessingOnly = TRUE, silent = TRUE
#' )
#' finalNet <- UpdateNetwork(callNetwork, calls, nodes = "actors")
#' finalStat <- UpdateNetwork(prep$initialStats[, , 1], ReducePreprocess(prep, "withoutTime", 1L)[[1]])
#' }
UpdateNetwork <- function(network, changeEvents, nodes = NULL, nodes2 = nodes) {
  stopifnot(
    inherits(network, "matrix"),
    inherits(changeEvents, "data.frame") || inherits(changeEvents, "matrix")
  )

  if (!is.null(nodes)) {
    changeEvents <- sanitizeEvents(changeEvents, nodes, nodes2)
  }


  if (inherits(changeEvents, "matrix") && all(c("node1", "node2", "replace") %in% colnames(changeEvents))) {
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

    redEvents <- aggregate(increment ~ sender + receiver, changeEvents, sum)
    chIncrement <- match("increment", names(redEvents))
    names(redEvents)[chIncrement] <- "replace"
  } else if ("replace" %in% names(changeEvents)) {
    discard <- duplicated(changeEvents[, c("sender", "receiver")], fromLast = TRUE)
    redEvents <- changeEvents[!discard, c("sender", "receiver", "replace"), drop = FALSE]
  }

  network[cbind(redEvents$sender, redEvents$receiver)] <- redEvents$replace
  return(network)
}


GetDetailPrint <- function(objectsEffectsLink, parsedFormula) {
    # effect description table
  orderedObjs <- apply(objectsEffectsLink, 2, function(x)
    names(which(!is.na(x))[order(x[!is.na(x)])]))
  # orderedObjs: each effect refers to which network or actor attribute
  if (ncol(orderedObjs) == 1) {
    maxParams <- nrow(orderedObjs)
    orderedObjs2 <- orderedObjs
    parameterOverview <- array(data = orderedObjs2, dim = c(1, maxParams))
  } else {
    maxParams <- max(vapply(orderedObjs, length, integer(1)))
    orderedObjs2 <- lapply(orderedObjs, function(x) c(x, rep("", maxParams - length(x))))
    parameterOverview <- Reduce(rbind, orderedObjs2)
  }
  
  colnames(parameterOverview) <- NULL
  effectDescription <- cbind(
    name = colnames(objectsEffectsLink),
    object = parameterOverview
  )
  if (any(unlist(parsedformula$ignoreRepParameter))) {
    effectDescription <- cbind(effectDescription,
      ignoreRep = unlist(ifelse(ignoreRepParameter, "B", ""))
    )
  }
  if (sum(unlist(parsedformula$weightedParameter)) > 0) {
    effectDescription <- cbind(effectDescription,
      weighted = unlist(ifelse(weightedParameter, "W", ""))
    )
  }
  if (sum(parsedformula$userSetParameter == "") < length(parsedformula$userSetParameter)) {
    effectDescription <- cbind(effectDescription,
      type = userSetParameter
    )
  }
  hasWindows <- FALSE
  if (!all(vapply(parsedformula$windowParameters, is.null, logical(1)))) {
    hasWindows <- TRUE
    effectDescription <- cbind(effectDescription,
      window = vapply(
        parsedformula$windowParameters,
        function(x) ifelse(is.null(x), "", x),
        character(1))
    )
  }
  rownames(effectDescription) <- NULL

}
