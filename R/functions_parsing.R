####################### #
#
# Goldfish package
# Parsings (and checking) formulae
#
####################### #

#' parse formula
#' A valid formula should have:
#' - on the left side a list of dependent events
#' - on the right side a list of names that correspond to effects we have in our pre-defined functions
#' - parameters for the effects that are coherent with the documentation
#' on top of this, we parse the formula to the right format for the rest of the estimation
#' @param formula a class \code{formula} object that defines the model
#'
#' @return a list with parsed values needed in the next steps
#' @noRd
#'
#' @examples
#' \dontrun{
#' parseFormula(calls ~ outdeg(call.Network, type="ego") + indeg(call.Network, type="alter"))
#' }
parseFormula <- function(formula, envir = globalenv()) {
  # check left side
  depName <- getDependentName(formula)
  if (!inherits(get(depName), "dependent.goldfish")) {
    stop("The left hand side of the formula should contain dependent events",
         " (check the function defineDependentEvents).", call. = FALSE)
  }
  # check right side
  rhsNames <- getRHSNames(formula)
  if (length(rhsNames) == 0) {
    stop("A model without effects cannot be estimated.", call. = FALSE)
  }
  # check right side: intercept
  int <- parseIntercept(rhsNames)
  rhsNames <- int[[1]]
  hasIntercept <- int[[2]]
  # check right side: default network
  defaultNetworkName <- attr(get(depName), "defaultNetwork")
  if (!is.null(defaultNetworkName)) {
    noObjectIds <- which(1 == vapply(rhsNames, length, integer(1)))
    for (i in noObjectIds) {
      rhsNames[[i]][[2]] <- defaultNetworkName
    }
  }
  # check right side: all parameters
  for (i in seq_along(rhsNames)) {
    if ("binary" %in% names(rhsNames[[i]])) {
      stop("The use of the binary parameter is no longer available,
                 please use ignoreRep.", call. = FALSE)
    }
    if ("isBipartite" %in% names(rhsNames[[i]])) {
      stop("The use of the isBipartite parameter is no longer available,
           please use isTwoMode", call. = FALSE)
    }
  }
  # check right side: windows
  windowParameters <- lapply(rhsNames, getElement, "window")
  rhsNames <- parseTimeWindows(rhsNames)
  # check right side: ignoreRep parameter
  mult <- parseMultipleEffects(rhsNames)
  rhsNames <- mult[[1]]
  ignoreRepParameter <- mult[[2]]
    # check mismatch with default parameter
  if (any(unlist(ignoreRepParameter)) && is.null(defaultNetworkName)) {
    stop("No default network defined, thus ", sQuote("ignoreRep = TRUE"), " effects cannot be used.", call. = FALSE)
  }
  # check right side: weighted parameter
  weightedParameter <- lapply(rhsNames, function(x) {
    v <- getElement(x, "weighted")
    ifelse(!is.null(v) && substr(v, 1, 1) == "T", TRUE, FALSE)
  })
  # check right side: type = c("ego", "alter")
  typeParameter <- lapply(rhsNames, function(x) {
    v <- getElement(x, "type")
    ifelse(!is.null(v), v, "")
  })
  # check right side: transformFun & aggregateFun
  getFunName <- function(x, which) {
    v <- getElement(x, which)
    v <- ifelse(!is.null(v), v, "")
    v <- gsub("['\" ]", "", v)  # replace quotation marks
    v <- ifelse(
      grepl("function.?\\(", v) || nchar(v) > 12,
      "userDefined", v) # if it is a function, it is replace by short text
  }
  transParameter <- lapply(rhsNames, getFunName, "transformFun")
  aggreParameter <- lapply(rhsNames, getFunName, "aggregateFun")

  # DyNAM-i ONLY: check right side: joining parameter
  joiningParameter <- lapply(rhsNames, function(x) {
    v <- getElement(x, "joining")
    ifelse(!is.null(v), v, "")
  })
  # DyNAM-i ONLY: check right side: subtype parameter
  subTypeParameter <- lapply(rhsNames, function(x) {
    v <- getElement(x, "subType")
    ifelse(!is.null(v), v, "")
  })

  # return all the results of the formula parsing
  res <- list(
    rhsNames = rhsNames,
    depName = depName,
    hasIntercept = hasIntercept,
    defaultNetworkName = defaultNetworkName,
    windowParameters = windowParameters,
    ignoreRepParameter = ignoreRepParameter,
    weightedParameter = weightedParameter,
    typeParameter = typeParameter,
    transParameter = transParameter,
    aggreParameter = aggreParameter,
    joiningParameter = joiningParameter,
    subTypeParameter = subTypeParameter
  )
  return(res)
}


# Comparison of two parsed formulas for preprocessingInit
# throws errors when: dependent events or default network are not the same, when there is righ-censoring
# for one and not the other
# returns: a list of the size of the new formula, with zeros when the effects are new, and with the
#          the index of the effect in the old formula if the effect was already there
compareFormulas <- function(oldparsedformula, newparsedformula, model, subModel) {

  # test dependent events and default network
  if (oldparsedformula$depName != newparsedformula$depName) {
    stop("The dependent events in the formula are not the ones used in",
         " the preprocessed object given in preprocessingInit.")
  }
  if (!identical(oldparsedformula$defaultNetworkName, newparsedformula$defaultNetworkName)) {
    stop("The default network in the formula is not the one used in",
         " the preprocessed object given in preprocessingInit.")
  }
  # test the right-censoring
  # for now it's easier to just reject inconsistent formulas, otherwise,
  # we would need go in the details of the RC intervals and updates
  oldhasIntercept <- oldparsedformula$hasIntercept
  newhasIntercept <- newparsedformula$hasIntercept
  if (model %in% c("DyNAM") && subModel %in% c("choice", "choice_coordination") && oldhasIntercept) {
    oldhasIntercept <- FALSE
    newhasIntercept <- FALSE
  }
  if (oldhasIntercept && !newhasIntercept) {
    stop("The preprocessing for the object in preprocessingInit was not done",
         " with the right-censored intervals that this formula requires.")
  }
  if (!oldhasIntercept && newhasIntercept) {
    stop("The preprocessing for the object in preprocessingInit was done",
         " with right-censored intervals and this formula does not include those.")
  }
  # counters for remembering which of the old effects are found in the new formula
  sizeold <- length(oldparsedformula$rhsNames)
  sizenew <- length(newparsedformula$rhsNames)
  effectsindexes <- rep(0, sizenew)
  # go through all new effects to check whether they already existed in the old formula
  for (i in seq.int(sizenew)) {
    effectname <- newparsedformula$rhsNames[[i]][[1]]
    effectobject <- newparsedformula$rhsNames[[i]][[2]]
    effectwindow <- newparsedformula$windowParameters[[i]]
    effectignorerep <- newparsedformula$ignoreRepParameter[[i]]
    effectweighted <- newparsedformula$weightedParameter[[i]]
    effectparameter <- newparsedformula$userSetParameter[[i]]
    for (j in seq.int(sizeold)) {
      # 1 check name of the effect
      if (!identical(oldparsedformula$rhsNames[[j]][[1]], effectname)) {
        next
      }
      # 2 check object of the effect
      if (!identical(oldparsedformula$rhsNames[[j]][[2]], effectobject)) {
        next
      }
      # 3 check windows
      if (!identical(oldparsedformula$windowParameters[[j]], effectwindow)) {
        next
      }
      # 4 check other parameters
      if (!identical(oldparsedformula$ignoreRepParameter[[j]], effectignorerep)) {
        next
      }
      if (!identical(oldparsedformula$weightedParameter[[j]], effectweighted)) {
        next
      }
      if (!identical(oldparsedformula$userSetParameter[[j]], effectparameter)) {
        next
      }
      # else it is the same effect
      effectsindexes[i] <- j
    }
  }
  return(effectsindexes)
}


# Creation of the different effects with the right parameters
# in which the first empty parameters are replaced with the ones found in effectInit
# ignores parameters that are not used in the updates computation (parmsIgnore)
createEffectsFunctions <- function(effectInit, model, subModel,
                                   envir = environment()) {
  # CHANGED ALVARO: adapted to model, subModel naming and additional functions
  .statMethod <- paste("init", model, subModel, sep = "_")

  effects <- lapply(
    effectInit, function(x, model, subModel) {
      funText <- paste("update", model, subModel, x[[1]], sep = "_")
      FUN <- tryCatch(eval(parse(text = funText)), error = function(e) NULL)
      # FUN <- NULL
      if (is.null(FUN)) {
        tryCatch({
          FUN <- eval(parse(text = x[[1]]))
        },
        error = function(e) stop("Unknown effect ", x[[1]]) # ,
        # finally = warning("Effect ")
        )
      }

      # collect update functions for stat
      .FUNStat <- utils::getS3method(.statMethod, x[[1]], optional = TRUE) # , envir = envir)
      if (is.null(.FUNStat)) {
        .FUNStat <- utils::getS3method(.statMethod, "default", optional = TRUE)
      } # , envir = envir)

      # Update signatures of the effects based on default parameters and above specified parameters
      .signature <- formals(FUN)
      .argsNames <- names(.signature)
      parmsToSet <- x[-1]

      if (is.null(names(parmsToSet))) {
        namedParams <- rep(FALSE, length(parmsToSet))
      } else {
        namedParams <- unlist(lapply(names(parmsToSet), function(v) is.character(v) && v != ""))
      }

      # change parameter type from character to an expression
      nameArg <- parmsToSet[[1]]
      parmsToSet <- lapply(parmsToSet, function(s) call("eval", parse(text = s)))

      # replace named and unnamed parameters
      .argsReplace <- pmatch(names(parmsToSet), .argsNames)

      names <- names(parmsToSet)[namedParams]
      # replace named args in formals
      .signature[na.omit(.argsReplace)] <- parmsToSet[!is.na(.argsReplace)]
      isCondition <- isReservedElementName(.argsNames) & !(.argsNames %in% names)
      .signature[isCondition] <- parmsToSet[!namedParams]

      # set isTwoMode parameter, checking if different
      if ("network" %in% .argsNames && "isTwoMode" %in% .argsNames) {
        # cat(x[[1]], is.null(parmsToSet[["isTwoMode"]]))
        isTwoMode <- length(attr(eval(.signature[["network"]], envir = envir), "nodes")) > 1
        if (!is.null(parmsToSet[["isTwoMode"]]) && eval(parmsToSet[["isTwoMode"]]) != isTwoMode) {
          warning("The \"isTwoMode\" parameter in effect ", x[[1]], " has a different value than",
            " the attributes on network argument '", x[[2]], "'", call. = FALSE, immediate. = TRUE)
        } else if (isTwoMode && is.null(parmsToSet[["isTwoMode"]])) {
          .signature[["isTwoMode"]] <- isTwoMode
          warning("Setting 'isTwoMode' parameter in effect ", x[[1]],
                  " to TRUE for network '", x[[2]], "'", call. = FALSE, immediate. = TRUE)
        }
      }
      # if ("network2" %in% .argsNames && "isTwoMode" %in% .argsNames) {
      #   isTwoMode <- length(attr(eval(.signature[["network2"]], envir = envir), "nodes")) > 2
      #   if (!is.null(parmsToSet[["isTwoMode"]]) && eval(parmsToSet[["isTwoMode"]]) != isTwoMode) {
      #     warning(
      #       "The 'isTwoMode' parameter in effect ", x[[1]], " has a diferent value than the network argument",
      #       .signature[["network2"]]
      #     )
      #   } else if (isTwoMode) .signature[["isTwoMode"]] <- isTwoMode
      # }

      # if(inherits(.signature, "matrix")) .signature <- apply(.signature, 2, invisible)

      # Assign signatures with default values to generic functions
      formals(FUN) <- .signature

      return(list(effect = FUN, initEffect = .FUNStat))
    },
    model, subModel
  )

  # class(effects) <- "goldfish.formulae"
  structure(effects, class = "goldfish.formulae")
}


# create new events lists when a window should be applied
createWindowedEvents <- function(objectEvents, window) {

  # create dissolution events
  # - for increment, add the opposite increment
  # - for replace, replace by 0
  dissolveEvents <- objectEvents
  dissolveEvents$time <- dissolveEvents$time + window # always seconds
  if ("increment" %in% names(objectEvents)) {
    dissolveEvents$increment <- -dissolveEvents$increment
  }
  if ("replace" %in% names(objectEvents)) {
    dissolveEvents$replace <- 0
  }

  # merge initial and dissolution events, and sort them by time
  newEvents <- rbind(objectEvents, dissolveEvents)
  sort.order <- order(newEvents$time)
  for (n in seq_along(names(newEvents))) {
    name <- names(newEvents)[n]
    newEvents[[name]] <- newEvents[[name]][sort.order]
  }

  return(newEvents)
}


# inspired by the ergm package function parser
extractFormulaTerms <- function(rhs) {
  # most inner term reached
  if (is.symbol(rhs)) {
    return(rhs)
  }
  if (!is.call(rhs[[1]]) &&
    rhs[[1]] != "+" &&
    rhs[[1]] != "*") {
    return(rhs)
    # return(list(rightHandSide[[1]], rightHandSide[[2]]))
  } else {
    return(c(extractFormulaTerms(rhs[[2]]), rhs[[3]]))
  }
}


getDependentName <- function(formula) {
  dep <- list(formula[[2]])
  depName <- unlist(lapply(dep, deparse))
}


getEventsAndObjectsLink <- function(depName, rhsNames, nodes = NULL, nodes2 = NULL,
                                    envir = environment()) {
  # Find objects (irrespective of where they occur)
  objectNames <- getDataObjects(rhsNames)

  # Find event lists of objects and link
  events <- list()
  events[[1]] <- get(depName, envir = envir)
  names(events) <- depName
  eventsObjectsLink <- data.frame(
    events = depName,
    name = NA,
    object = NA,
    nodeset = NA,
    attribute = NA, stringsAsFactors = FALSE
  )

  # replace dependent labels with ids
  events[[1]] <- sanitizeEvents(events[[1]], nodes, nodes2)
  # if(is.character(events[[1]]$sender) && is.character(events[[1]]$receiver)) {
  #   events[[1]]$sender <- match(events[[1]]$sender, get(nodes)$label)
  #   events[[1]]$receiver <- match(events[[1]]$receiver, get(nodes2)$label)
  # }

  isAttribute <- is.na(objectNames$object)
  # Link attributes to events
  for (i in which(isAttribute)) {
    nodeSet <- objectNames[i, ]$nodeset
    attributeName <- objectNames[i, ]$attribute
    dynamicAttributes <- attr(get(objectNames[i, ]$nodeset, envir = envir), "dynamicAttribute")
    eventListNames <- attr(get(objectNames[i, ]$nodeset, envir = envir), "events")
    evName <- eventListNames[which(dynamicAttributes == attributeName)]

    if (length(evName) > 0) {
      eventsObjectsLink <- rbind(
        eventsObjectsLink,
        cbind(events = evName, objectNames[i, ])
      )
      evs <- lapply(evName, function(x) sanitizeEvents(get(x), nodeSet))

      events <- append(events, evs)
      names(events)[(length(events) - length(evName) + 1):length(events)] <- evName
    }
  }
  # Link networks to events
  for (i in which(!isAttribute)) {
    evNames <- attr(get(objectNames[i, ]$object, envir = envir), "events")
    evs <- lapply(evNames, get, envir = envir)
    nodesObject <- attr(get(objectNames[i, ]$object, envir = envir), "nodes")

    if (length(nodesObject) > 1) {
      nodes <- nodesObject[1]
      nodes2 <- nodesObject[2]
    } else nodes <- nodes2 <- nodesObject

    # replace labels with ids
    if (length(evNames) > 0) {
      for (j in seq_along(evs)) {
        evs[[j]] <- sanitizeEvents(evs[[j]], nodes, nodes2)
      }
      eventsObjectsLink <- rbind(
        eventsObjectsLink,
        cbind(events = evNames, objectNames[i, ], row.names = NULL)
      )
      events <- append(events, evs)
      names(events)[(length(events) - length(evs) + 1):length(events)] <- evNames
    }
  }

  return(list(
    events,
    eventsObjectsLink
  ))
}


getEventsEffectsLink <- function(events, rhsNames, eventsObjectsLink) {
  eventsEffectsLink <- matrix(
    data = NA, nrow = length(events), ncol = length(rhsNames),
    dimnames = list(names(events),
                    vapply(rhsNames, FUN = "[[", FUN.VALUE = character(1), i = 1))
  )
  for (i in seq_along(rhsNames)) {
    # objects of effect
    obj <- getDataObjects(rhsNames[i])$name
    # get events that relate to any of the objects
    eventIds <- which(eventsObjectsLink$name %in% obj)
    eventsEffectsLink[eventIds, i] <- 1
  }

  eventsEffectsLink
}


getObjectsEffectsLink <- function(rhsNames) {
  objNames <- getDataObjects(rhsNames)$name
  effNames <- vapply(rhsNames, FUN = "[[", FUN.VALUE = character(1), i = 1)
  objectsEffectsLink <- matrix(
    data = NA, nrow = length(objNames), ncol = length(effNames),
    dimnames = list(objNames, effNames)
  )
  objAsParams <- lapply(rhsNames, function(x) getDataObjects(list(x)))
  for (i in seq_along(objAsParams)) {
    names <- objAsParams[[i]]$name
    objectsEffectsLink[names, i] <- seq_along(names)
  }
  objectsEffectsLink
}


getRHSNames <- function(formula) {
  rhs <- extractFormulaTerms(formula[[3]])
  # embed single parameter models in list
  if (!is.list(rhs)) rhs <- list(rhs)
  rhsNames <- lapply(rhs, function(term) lapply(term, deparse))
}


parseIntercept <- function(rhsNames) {
  intercept <- FALSE
  v <- NA
  tryCatch(
    v <- as.numeric(rhsNames[[1]][[1]]),
    warning = function(x) {
      }
  )
  if (!is.na(v) && v == 1) {
    intercept <- TRUE
    rhsNames <- rhsNames[-1]
  }

  return(list(rhsNames, intercept))
}


# Figures out which effect is a multiple effect
# then finds a network object from the other parameters that this is related to
# unless a network name is passed to the multiple attribute
parseMultipleEffects <- function(rhsNames, default = FALSE) {
  multiple <- list()
  multipleNames <- character(0)
  for (i in seq_along(rhsNames)) {
    name <- ""
    id <- which(names(rhsNames[[i]]) == "ignoreRep")
    multipleParam <- ifelse(length(id) == 1, rhsNames[[i]][[id]], default)

    if (multipleParam %in% c("T", "F", "TRUE", "FALSE")) multipleParam <- as.logical(multipleParam)
    if (!multipleParam) {
      table <- getDataObjects(rhsNames[i])
      netIds <- vapply(getElementFromDataObjectTable(table),
                       FUN = inherits,
                       FUN.VALUE = logical(1),
                       what = "network.goldfish")
      name <- table[netIds, "name"][1] # take first network
      # apply(getDataObjects(rhsNames)
      # netIds <- sapply(, function(x) "network.goldfish" %in% class(get(x)))
      # name <- getDataObjects(rhsNames)$object[netIds][[1]]  # get the first network item
    }
    if (is.character(multipleParam)) {
      name <- multipleParam
      multipleParam <- FALSE
    }

    if (!is.na(name) && name != "" && !exists(name))
      stop("Unknown object in 'ignoreRep' parameter: ", name, call. = FALSE)

    multiple <- append(multiple, multipleParam)
    multipleNames <- c(multipleNames, name)
    rhsNames[[i]] <- if (length(id) > 0) rhsNames[[i]][-id] else rhsNames[[i]]
  }
  names(multiple) <- multipleNames
  return(list(rhsNames, multiple))
}


# Identify time windows objects, create new events and objects, link updates and
# finally update rhs (and thus effectInit) accordingly
parseTimeWindows <- function(rhsNames, envir = globalenv()) {
  objectNames <- getDataObjects(rhsNames)

  hasWindows <- which(vapply(rhsNames, function(x) !is.null(getElement(x, "window")), logical(1)))

  for (i in hasWindows) {
    windowName <- rhsNames[[i]]$window
    window <- tryCatch(
      eval(parse(text = windowName), envir = envir),
      error = function(e) {
        e$message <- paste("Invalid window parameter for effect ", rhsNames[[i]][[1]], " ", rhsNames[[i]][[2]],
                           ":\n", e$message)
        stop(e)
      }
    )

    # # in the case windows is provided as an object name, it should start with alphabetic character
    isValidName <- grepl("^[[:alpha:]][[:alnum:]_.]+$", windowName)

    # support for lubridate object classes for date operations
    if (inherits(window, c("Period", "Duration")) & "lubridate" %in% attr(attr(window, "class"), "package")) {
      if (!isValidName) {
        windowName <- gsub("\\s", "", as.character(window))
        if (inherits(window, "Duration")) windowName <- gsub("^(\\d+s)\\s*(\\(.+\\))$", "\\1", as.character(window))
        }
    } else if (inherits(window, "character")) {
      if (!isValidName) windowName <- gsub(" ", "", window)

      if (!is.numeric(window) & !grepl("^\\d+ (sec|min|hour|day|week|month|year)", window)) {
        stop(
          "The window effect specified with the effect ", rhsNames[[i]][[1]],
          " ", rhsNames[[i]][[2]], " is not in the form 'number unit'\n",
          " or the number is not an integer number\n",
          " or the unit is not between the accepted options:\n\t",
          "seconds, minutes, hours, weeks, months, years"
        )
      }

      if (grepl("sec", window)) {
        window <- as.numeric(strsplit(window, " ")[[1]][1]) * 1
      }
      if (grepl("min", window)) {
        window <- as.numeric(strsplit(window, " ")[[1]][1]) * 60
      }
      if (grepl("hour", window)) {
        window <- as.numeric(strsplit(window, " ")[[1]][1]) * 3600
      }
      if (grepl("day", window)) {
        window <- as.numeric(strsplit(window, " ")[[1]][1]) * 86400
      }
      if (grepl("week", window)) {
        window <- as.numeric(strsplit(window, " ")[[1]][1]) * 604800
      }
      if (grepl("month", window)) {
        window <- as.numeric(strsplit(window, " ")[[1]][1]) * 2629800 # lubridate approximation
      }
      if (grepl("year", window)) {
        window <- as.numeric(strsplit(window, " ")[[1]][1]) * 31557600 # lubridate approximation
      }
    } else if (is.numeric(window)) { # check numeric type

      if (window < 0)
        stop("The window specified with the effect ", rhsNames[[i]][[1]], " ", rhsNames[[i]][[2]],
             " is not a positive numeric value")
    }



    # get initial object, check whether it's an attribute or a network
    name <- rhsNames[[i]][[2]]
    objects <- objectNames[objectNames$name == name, ]
    isAttribute <- !is.na(objects$attribute)

    # add new RHS term for the windowed element
    # newRhs <- list()
    # newRhs[[1]] <- rhsNames[[i]][[1]]
    # newRhs[[2]] <- paste(rhsNames[[i]][[2]], window, sep="_")
    # rhsNames[[length(rhsNames)+1]] <- newRhs
    rhsNames[[i]][[2]] <- paste(rhsNames[[i]][[2]],
                                windowName,
                                sep = "_"
    )


    if (isAttribute) {
      # get nodes & attribute, add new windowed attribute, get related events to be windowed later
      nameNodes <- objects$nodeset
      nodes <- get(nameNodes, envir = envir)
      attribute <- objects$attribute

      newAttribute <- paste(attribute, windowName, sep = "_")
      nodes[newAttribute] <- nodes[attribute]

      allEvents <- attr(nodes, "events")
      allDynamicAttributes <- attr(nodes, "dynamicAttributes")
      allEvents <- allEvents[allDynamicAttributes == attribute]
    } else {

      # get network, create windowed network, get related events to be windowed later
      network <- get(name, envir = envir)

      newNetwork <- matrix(0, nrow = nrow(network), ncol = ncol(network))
      newName <- paste(name, windowName, sep = "_")
      attr(newNetwork, "events") <- NULL

      # add attribute
      attr(newNetwork, "nodes") <- attr(network, "nodes")
      attr(newNetwork, "directed") <- attr(network, "directed")
      dimnames(newNetwork) <- dimnames(network)
      class(newNetwork) <- class(network)

      allEvents <- attr(network, "events")
    }

    # create new windowed events lists, link them, add them to the environment
    for (events in allEvents) {
      objectEvents <- get(events, envir = envir)
      newEvents <- createWindowedEvents(objectEvents, window)
      nameNewEvents <- paste(events, window, sep = "_")

      if (isAttribute) {
        attr(nodes, "events") <- c(attr(nodes, "events"), nameNewEvents)
        attr(nodes, "dynamicAttributes") <- c(attr(nodes, "dynamicAttributes"), newAttribute)
      } else {
        attr(newNetwork, "events") <- c(attr(newNetwork, "events"), nameNewEvents)
      }

      assign(nameNewEvents, newEvents, envir = envir)
    }

    # Put nodes/networks elements back in the environments
    if (isAttribute) {
      assign(nameNodes, nodes, envir = envir)
    } else {
      assign(newName, newNetwork, envir = envir)
    }

    # not sure about this: should we remove the window parts from the rhs names?
    # rhsNames[[i]]$window <- NULL
  }
  return(rhsNames)
}
