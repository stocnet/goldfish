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

# defineChoiceOptionObserv<-function(Actors, Events, model=c("DyNAM-rate","DyNAM-choice")) {
#
#   if (missing(model)) {
#     warning("The model is not specified. DyNAM-rate is assumed by default.")
#     model="DyNAM-rate"
#   } else if(model %in% c("DyNAM-rate", "DyNAM-choice")==FALSE) {
#     warning(paste(model, "is not a valid model option, DyNAM-rate is assumed instead."))
#     model="DyNAM-rate"
#   }
#   ### Choice
#   choice<-c()
#   for (i in 1:nrow(Events)) {
#     # Define sender and receiver's position
#     receiver.temp<-as.numeric(gsub("Actor ", "",Events$receiver[i]))
#     sender.temp<-as.numeric(gsub("Actor ", "",Events$sender[i]))
#
#     temp<-rep(0,each=length(Actors$label))
#
#
#
#
#     if (model=="DyNAM-choice") {
#       #Identify the receiver and give him a value of 1. He is the choice of the sender.
#       temp[receiver.temp]<-1
#       #now remove the sender from the choice vector. create a boolean which allows to
#       #remove the corresponding position of the sender
#       #       temp2<-1:length(Actors$label)
#       temp3<-temp2==sender.temp
#       choice.temp<-temp[!temp3]
#     }else if (model=="DyNAM-rate") {
#       #Identify the receiver and give him a value of 1. He is the choice of the sender.
#       temp[sender.temp]<-1
#       choice.temp<-temp
#     }
#
#     choice<-c(choice, choice.temp)
#   }
#
#
#   ### Option
#   option<-c()
#   for (i in 1:nrow(Events)) {
#     # Define sender and receiver's position
#     receiver.temp<-as.numeric(gsub("Actor ", "",Events$receiver[i]))
#     sender.temp<-as.numeric(gsub("Actor ", "",Events$sender[i]))
#
#     option.temp<-Actors$label
#
#     #now remove the sender from the choice vector. create a boolean which allows to
#     #remove the corresponding position of the sender
#     #
#
#
#     if (model=="DyNAM-choice") {
#       temp2<-1:length(Actors$label)
#       temp3<-temp2==sender.temp
#       option.temp<-option.temp[!temp3]
#     } else if (model=="DyNAM-rate") {
#       option.temp<-1:length(Actors$label)
#     }
#
#     option<-c(option, option.temp)
#
#   }
#
#
#   ### Observation number
#   if (model=="DyNAM-choice") {
#     observ<-rep(c(1:nrow(Events)), each=length(Actors$label)-1)
#   } else if (model=="DyNAM-rate") {
#     observ<-rep(c(1:nrow(Events)), each=length(Actors$label))
#   }
#
#   out<-list()
#   out$observ<-observ
#   out$choice<-choice
#   out$option<-option
#
#   return(out)
# }



# Define the statistics effects as a vector
# Recover a vectors defining the effects statistics from the preprocessed objects.
# The vector can then be used for an mlogit estimation.
# (useful e.g. for mlogit estimation on a choice model)
#
# @param e The position in the preprocessing formula of the desired effect to vectorise. e.g if the
# formula is as follows: out~trans+recip+inertia, e=1 for transitivity (first argument of the additive formula),
#  e=2 for reciprocity and e=3 for inertia
# @param preprocessingStat The preprocessing objects of a first goldfish estimation.
# @param Actors  a data frame that represents the list of actors.
# @param Events a dataframe that represents a valid events list
# @param model a bi-optional variable in which the model used for estimation must be indicated as:
#  "DyNAM-rate" or "DyNAM-choice". The default option is "DyNAM-rate".
#
# @return a statistic in the form of an effect vector
# @noRd
#
# @examples
# \dontrun{
# DynamEstimData_prep<-estimate(outCalls ~ trans+recip+inertia
# , model = "DyNAM", subModel = "choice", silent=T, preprocessingOnly = T)
# trans0<-transstatisticEffectCalculation(1,DynamEstimData_prep, actors, calls, "DyNAM-choice")
# recip0<-statisticEffectCalculation(2,DynamEstimData_prep, actors, calls, "DyNAM-choice")
# ine0<-statisticEffectCalculation(3,DynamEstimData_prep, actors, calls, "DyNAM-choice")
# dt<-data.frame(choice,observ,option, trans0, recip0, inertia0) #add effects vectors to the data set
# mlogitData<-mlogit.data(dt, shape="long", choice = "choice", alt.var = "option", id.var="observ", chid.var ="observ")
# mlogitEstim<-mlogit(choice ~ -1+trans0+recip0+inertia0, data=mlogitData)
# }


# statisticEffectCalculation<-function(e, preprocessingStat, Actors, Events, model=c("DyNAM-rate","DyNAM-choice")) {
#
#   if (missing(model)) {
#     warning("The model is not specified. DyNAM-rate is assumed by default.")
#     model="DyNAM-rate"
#   } else if(model %in% c("DyNAM-rate", "DyNAM-choice")==FALSE) {
#     warning(paste(model, "is not a valid model option, DyNAM-rate is assumed instead."))
#     model="DyNAM-rate"
#   }
#
#
#
#   nUpdates<-length(preprocessingStat$eventTime)
#   updateMatrix<-initialMatrix<-preprocessingStat$initialStats[,,e] #84x84 matrix
#
#   initialMatrix[is.nan(initialMatrix)]<-NA # We want nan values on diagonal, hence replace all possible nan values
#   by na values so that the function  does not mix up diagonal elements with off diagonal elements.
#   diag(initialMatrix)<-NaN
#
#
#   updatingEvents<-as.data.frame(goldfish:::ReducePreprocess(preprocessingStat, type="withTime")[[e]] )
#
#   effectObjects<-list()
#   effectObjects[[1]]<-list()
#   effectObjects[[1]]$updatedMatrix<-updateMatrix
#   effectObjects[[1]]$timeUpdate<-0
#
#   effect<-c()
#
#
#   for (i in 1:(nUpdates)) {  # For each event
#
#     if (nUpdates!=0) {
#       nInnerUpdates<-nrow(as.data.frame(preprocessingStat$dependentStatsChange[[i]][[e]]))
#       subsetUpdatingEvents<-as.data.frame(preprocessingStat$dependentStatsChange[[i]][[e]])
#
#       if (!is.null(nInnerUpdates)&&nInnerUpdates!=0) {
#         for (j in 1:nInnerUpdates) {
#           # For each new opportunity, define the sender and receiver and determine which matrix position
#           # should be replaced
#           sender<-subsetUpdatingEvents[j,"node1"]
#           receiver<-subsetUpdatingEvents[j,"node2"]
#           replacing<-subsetUpdatingEvents[j,"replace"]
#           updateMatrix[sender,receiver]<-replacing      # replace the opportunity
#         }
#       }
#
#       if (i==1) {
#         timeEv<-max(1,preprocessingStat$eventTime[[i]])
#         #if it is the first event, we take the event time happening before the first dependent stats change
#       } else {
#         timeEv<-preprocessingStat$eventTime[[i]]
#       }
#
#
#       if (model=="DyNAM-rate") {
#         updateMatrix[is.nan(updateMatrix)]<-NA # We want nan values on diagonal, hence replace all possible nan
#         #values by na values so that the function  does not mix up diagonal elements with off diagonal elements.
#         diag(updateMatrix)<-NaN
#         diag(updateMatrix)<-cbind(updateMatrix,updateMatrix[,1])[which(updateMatrix%in%diag(updateMatrix)) +
#           nrow(updateMatrix)] # (at each iteration, search same guy next column and use his value)
#       }
#
#       effectObjects[[i+1]]<-list()
#       effectObjects[[i+1]]$updatedMatrix<-matrix_to_use<-updateMatrix  # Update the matrix of opportunities
#       effectObjects[[i+1]]$timeUpdate<-timeEv
#
#       colnames(matrix_to_use)<-Actors$label
#       rownames(matrix_to_use)<-Actors$label
#       effect0sender<-Events$sender[i]
#
#       # if the effect is not an attribute
#       if (model=="DyNAM-choice") {
#         col.names.remove<-paste0(effect0sender)
#         matrix_to_use<-matrix_to_use[,!(row.names(matrix_to_use) %in% col.names.remove)]
#         effect0<-as.vector(matrix_to_use[paste0(effect0sender),])
#
#       } else if (model=="DyNAM-rate") {
#         effect0<-as.vector(matrix_to_use[,paste0(effect0sender)])
#       }
#
#
#
#     } else { #if n updates =0 -> unchanging effect (attribute)
#       if (model=="DyNAM-rate") {
#         # If we are using a rate model, fill the diagonal with attribute values
#         diag(initialMatrix)<-cbind(initialMatrix,initialMatrix[,1])[which(initialMatrix%in%diag(initialMatrix)) +
#         nrow(initialMatrix)] #(search same guy next column and use his value)
#         effect0sender<-Events$sender[i]
#           # Because we want to evaluate the attribute of all the senders so we work with the columns
#         colnames(initialMatrix)<-Actors$label
#         rownames(initialMatrix)<-Actors$label
#         effect0<-as.vector(initialMatrix[,paste0(effect0sender)])
#
#       } else if (model=="DyNAM-choice")
#         effect0sender<-Events$sender[i]
#       colnames(initialMatrix)<-Actors$label
#       rownames(initialMatrix)<-Actors$label
#       effect_vect<-as.vector(initialMatrix[paste0(effect0sender),])
#       effect0<-effect_vect[!is.nan(effect_vect)]
#     }
#     effect<-c(effect, effect0)
#   }
#
#   return(effect)
# }


GetDetailPrint <- function(objectsEffectsLink, parsedformula, fixedParameters = NULL) {
  # matrix with the effects in rows and objects in columns, which net or actor att
  maxObjs <- max(objectsEffectsLink, na.rm = TRUE)
  effectDescription <- matrix(t(
    apply(
      objectsEffectsLink, 2,
      function(x) {
        notNA <- !is.na(x)
        objs <- x[notNA]
        objs <- names(objs[order(objs)])
        c(objs, rep("", maxObjs - length(objs)))
      })
  ),
  nrow = ncol(objectsEffectsLink),
  ncol = maxObjs
  )
  # # handle degenerate case one effect one object
  dimnames(effectDescription) <-  list(
    colnames(objectsEffectsLink),
    if (ncol(effectDescription) == 1) {
      "Object"
    } else sprintf("Object %d", seq(ncol(effectDescription)))
  )

  objectsName <- colnames(effectDescription)
  # adding other parameters: each effect refers to which network or actor attribute

  # effectDescription <- cbind(
  #   effect = rownames(effectDescription),
  #   effectDescription
  # )
  if (!is.null(fixedParameters)) {
    effectDescription <- cbind(effectDescription,
      fixed = !is.na(fixedParameters)
    )
  }


  if (any(unlist(parsedformula$ignoreRepParameter))) {
    effectDescription <- cbind(effectDescription,
      ignoreRep = ifelse(parsedformula$ignoreRepParameter, "B", "")
    )
  }
  if (any(unlist(parsedformula$weightedParameter))) {
    effectDescription <- cbind(effectDescription,
      weighted = ifelse(parsedformula$weightedParameter, "W", "")
    )
  }
  if (any(parsedformula$typeParameter != "")) {
    effectDescription <- cbind(effectDescription,
      type = parsedformula$typeParameter
    )
  }
  hasWindows <- FALSE
  if (!all(vapply(parsedformula$windowParameters, is.null, logical(1)))) {
    hasWindows <- TRUE
    effectDescription <- cbind(
      effectDescription,
      window = vapply(
        parsedformula$windowParameters,
        function(x) ifelse(is.null(x), "", gsub("['\"]", "", x)),
        character(1))
    )
    # reduce object name
    effectDescription[, objectsName] <- t(apply(
      effectDescription,
      1,
      function(x) gsub(paste0("^(.+)_", gsub(" ", "", x["window"]), "$"), "\\1", x[objectsName])
    ))
  }
  if (any(parsedformula$transParameter != "")) {
    effectDescription <- cbind(effectDescription,
      transformFun = parsedformula$transParameter
    )
  }
  if (any(parsedformula$aggreParameter != "")) {
    effectDescription <- cbind(effectDescription,
      aggregateFun = parsedformula$aggreParameter
    )
  }
  # rownames(effectDescription) <- NULL
  if (parsedformula$hasIntercept) {
    effectDescription <- rbind("", effectDescription)
    rownames(effectDescription)[1] <- "Intercept"
  }

  attr(effectDescription, "hasWindows") <- hasWindows
  return(effectDescription)
}

GetFixed <- function(object) {
  if ("fixed" %in% colnames(object$names)) {
    fixed <- vapply(object$names[, "fixed"], function(x) eval(parse(text = x)), logical(1))

  } else  fixed <- rep(FALSE, length(object$parameters))
  fixed
}

OldNames <- function(object) {
  if (inherits(object, "result.goldfish")) {
    change <- c("standard.errors" = "standardErrors", "log.likelihood" = "logLikelihood",
                "final.score" = "finalScore",
                "final.informationMatrix" = "finalInformationMatrix",
                "n.iterations" = "nIterations", "n.events" = "nEvents",
                "right.censored" = "rightCensored")
    objName <- names(object)
    posChange <- match(newName, change)
    if (anyNA(posChange)) stop("Sorry, smth wrong! Maybe old elements are missing")
    names(object)[posChange] <- names(change)
    model <- object$model
    subModel <- object$subModel
    hasIntercept <- object$right.censored
    # maybe outdated if new models available
    if (model == "REM") {
      if (!hasIntercept) {
        modelTypeCall <- "REM-ordered"
      } else {
        modelTypeCall <- "REM"
      }
    } else if (model %in% c("DyNAM", "TriNAM")) {
      if (subModel == "rate" && !hasIntercept) {
        modelTypeCall <- "DyNAM-M-Rate-ordered"
      } else if (subModel == "rate") {
        modelTypeCall <- "DyNAM-M-Rate"
      } else if (subModel == "choice_coordination") {
        modelTypeCall <- "DyNAM-MM"
      } else {
        modelTypeCall <- "DyNAM-M"
      }
    }
    object$model.type <- modelTypeCall
  } else stop("not ", dQuote("result.goldfish"), " object", call. = FALSE)
}
