#' internal function to perform simulation based on preprocessing
#'
#' Create a preprocess.goldfish class objectRate with the necessary information
#' for simulation.
#'
#' @inheritParams estimate
#' @inheritParams simulate
#' @inheritParams preprocess
#'
#' @return an array with simulated events
#'
#' @keywords internal
#' @noRd
simulate_engine <- function(
    model,
    subModel,
    parametersRate,
    parsedformulaRate,
    parametersChoice,
    parsedformulaChoice,
    nEvents,
    # multipleParameter,
    nodes,
    nodes2 = nodes,
    isTwoMode,
    # add more parameters
    startTime = 0,
    endTime = NULL,
    rightCensored = FALSE,
    verbose = TRUE,
    silent = FALSE) {
  
  prepEnvir <- environment()




  # get node sets of dependent variable
  nodes <- attr(get(parsedformulaRate$depName), "nodes")
  isTwoMode <- FALSE
  
  # two-mode networks(2 kinds of nodes)
  if (length(nodes) == 2) {
    nodes2 <- nodes[2]
    nodes <- nodes[1]
    isTwoMode <- TRUE
  } else {
    nodes2 <- nodes
  }
  
  ## 2.1 INITIALIZE OBJECTS for all cases: preprocessingInit or not
  
  # enviroment from which get the objects
  envir <- environment()
  
  # effect and objectsEffectsLink for sender-deciding process
  effectsRate <- createEffectsFunctions(parsedformulaRate$rhsNames,
                                        model, subModel, envir = envir)
  objectsEffectsLinkRate <- getObjectsEffectsLink(
    parsedformulaRate$rhsNames)
  
  # effect and objectsEffectsLink for receiver-deciding process
  effectsChoice <- NULL
  objectsEffectsLinkChoice <- NULL
  if (!is.null(parametersChoice)) {
    effectsChoice <- createEffectsFunctions(parsedformulaChoice$rhsNames,
                                            model, subModel, envir = envir)
    objectsEffectsLinkChoice <- getObjectsEffectsLink(
      parsedformulaChoice$rhsNames)
  }
  
  # 
  n1 <- nrow(get(nodes))
  n2 <- nrow(get(nodes2))
  nEffectsRate <- length(effectsRate)
  nEffectsChoice <- length(effectsChoice)
  
  if (!silent) cat("Initializing cache objects and statistical matrices.\n")
  # ToDo: Impute misssing data
  #   startTime and endTime handling
  
  # Initialize stat matrix for rate model
  statCacheRate <- initializeCacheStat(
    objectsEffectsLink = objectsEffectsLinkRate,
    effects = effectsRate,
    groupsNetwork = NULL, 
    windowParameters = parsedformulaRate$windowParameters,
    n1 = n1, n2 = n2,
    model = model, subModel = "rate",
    envir = prepEnvir
  )

  # Initialize stat matrix for the choice model
  if (!is.null(parametersChoice)) {
    statCacheChoice <- initializeCacheStat(
      objectsEffectsLink = objectsEffectsLinkChoice,
      effects = effectsChoice,
      groupsNetwork = NULL,
      windowParameters = parsedformulaChoice$windowParameters,
      n1 = n1, n2 = n2,
      model, "choice",
      envir = prepEnvir
    )
    # the variable subModel is for the sender-deciding process.  ToDo: check
    subModel <- "rate"
  }

  # We put the initial stats to the previous format of 3 dimensional array
  initialStatsRate <- array(
    unlist(lapply(statCacheRate, "[[", "stat")),
    dim = c(n1, n2, nEffectsRate)
  )
  statMatRate <- initialStatsRate
  statCacheRate <- lapply(statCacheRate, "[[", "cache")
  # for receiver-deciding process if it's necessary
  if (!is.null(parametersChoice)) {
    initialStatsChoice <- array(
      unlist(lapply(statCacheChoice, "[[", "stat")),
      dim = c(n1, n2, nEffectsChoice)
    )
    statMatChoice <- initialStatsChoice
    statCacheChoice <- lapply(statCacheChoice, "[[", "cache")
  }

  # ToDo: change to startTime
  currentTime <- 0
  events <- matrix(0, nEvents, 4)
  
  # initialize progressbar output
  showProgressBar <- FALSE
  # progressEndReached <- FALSE
  
  if (!silent) {
    cat("Simulating events.\n")
    showProgressBar <- TRUE
    # # how often print, max 50 prints
    pb <- utils::txtProgressBar(max = nEvents, char = "*", style = 3)
    dotEvents <- ifelse(nEvents > 50, ceiling(nEvents / 50), 1)
  }
  
  # Simulation each event
  for (i in 1:nEvents) {
    # # progress bar
    if (showProgressBar && i %% dotEvents == 0) {
      utils::setTxtProgressBar(pb, i)
    } else if (showProgressBar && i == nEvents) {
      utils::setTxtProgressBar(pb, i)
      close(pb)
    }

    # nextEvent <- 1
    effIdsRate <- seq.int(length(objectsEffectsLinkRate))
    effIdsChoice <- seq.int(length(objectsEffectsLinkChoice))
    objTableRate <- getDataObjects(
      list(rownames(objectsEffectsLinkRate)),
      removeFirst = FALSE)
    objectNameRate <- objTableRate$name
    objectRate <- getElementFromDataObjectTable(
      objTableRate, envir = prepEnvir)[[1]]

    #### GENERATING EVENT
    # We consider only two types of model, REM and DyNAM, and don't consider DyNAM-MM
    if (model == "REM") {
      simulatedEvent <- generationREM(
        statMatRate, parametersRate, n1, n2, isTwoMode)
      waitingTime <- simulatedEvent$waitingTime
      simulatedSender <- simulatedEvent$simulatedSender
      simulatedReceiver <- simulatedEvent$simulatedReceiver
    } else if (model == "DyNAM" && subModel == "rate") {
      simulatedSenderEvent <- generationDyNAMRate(
        statMatRate, parametersRate, n1, n2, isTwoMode)
      waitingTime <- simulatedSenderEvent$waitingTime
      simulatedSender <- simulatedSenderEvent$simulatedSender
      simulatedReceiverEvent <- generationDyNAMChoice(
        statMatChoice, parametersChoice, simulatedSender, n1, n2, isTwoMode)
      simulatedReceiver <- simulatedReceiverEvent$simulatedReceiver
    }

    # event <- c(simulatedSender,simulatedReceiver,objectRate[simulatedSender, simulatedReceiver])
    event <- data.frame(
      sender = as.integer(simulatedSender),
      receiver = as.integer(simulatedReceiver),
      replace = objectRate[simulatedSender, simulatedReceiver] + 1)
    # RECORD EVENT
    events[i, ] <- c(
      currentTime + waitingTime,
      simulatedSender,
      simulatedReceiver,
      1)

    ### CALCULATE UPDATES
    isUndirectedNet <- FALSE
    updatesList <- getUpdates(
      event, effectsRate, effIdsRate,
      objectsEffectsLinkRate, isUndirectedNet, n1, n2,
      isTwoMode, prepEnvir, "statCacheRate")
    ### APPLYING UPDATES TO statMatRate
    # For sender
    for (id in effIdsRate) {
      if (id <= length(updatesList) && !is.null(updatesList[[id]])) {
        updates <- updatesList[[id]]
        # ToDo: check
        statMatRate[cbind(updates[, "node1"], updates[, "node2"], id)] <-
          updates[, "replace"]
      }
    }
    # For receiver
    if (!is.null(parametersChoice)) {
      updatesList <- getUpdates(
        event, effectsChoice, effIdsChoice,
        objectsEffectsLinkChoice, isUndirectedNet, n1, n2,
        isTwoMode, prepEnvir, "statCacheChoice")
      ### APPLYING UPDATES TO statMatRate
      # For receiver
      for (id in effIdsChoice) {
        if (id <= length(updatesList) && !is.null(updatesList[[id]])) {
          updates <- updatesList[[id]]
          statMatChoice[cbind(updates[, "node1"], updates[, "node2"], id)] <-
            updates[, "replace"]
        }
      }
    }

    ### update other information
    currentTime <- currentTime + waitingTime
    objectRate[simulatedSender, simulatedReceiver] <-
      objectRate[simulatedSender, simulatedReceiver] + 1
    eval(parse(text = paste(objectNameRate, "<- objectRate")),
         envir = prepEnvir)
  }

  return(events)
}


generationREM <- function(statMatRate, parametersRate, n1, n2, isTwoMode) {
  n_parameters <- dim(statMatRate)[3]
  # +1 for intercept
  stat_mat <- matrix(0, n1 * n2, n_parameters + 1)
  stat_mat[, 1] <- 1
  for (i in 1:n_parameters) {
    stat_mat[, i + 1] <- t(statMatRate[, , i])
  }
  expValue <- exp(stat_mat %*% parametersRate)
  if (!isTwoMode) {
    for (i in 1:n1) expValue[i + (i - 1) * n2] <- 0
  }

  # expected time
  tauSum <- sum(expValue)
  expectedWaitingtime <- 1 / tauSum
  waitingTime <- rexp(1, tauSum)

  # Conditional on the waiting time,
  # the process to choose a sender-receiver pair is a multinomial process
  simulatedSenderReceiver <- sample(1:length(expValue), 1, prob = expValue)
  simulatedSender <- ceiling(simulatedSenderReceiver / n2)
  simulatedReceiver <- simulatedSenderReceiver - (simulatedSender - 1) * n2


  return(list(
    simulatedSenderReceiver = simulatedSenderReceiver,
    simulatedSender = simulatedSender,
    simulatedReceiver = simulatedReceiver,
    expectedWaitingtime = expectedWaitingtime,
    waitingTime = waitingTime
  ))
}

generationDyNAMRate <- function(statMatRate, parametersRate, n1, n2, isTwoMode) {
  # Copy from functions_estimation_engine.R for matrix reduction
  # In the end, we will get a n1 x nEffectsRate matrix stat_mat.
  if (isTwoMode == FALSE) {
    dims <- dim(statMatRate) # statsArrayComp:
    stat_mat <- apply(statMatRate, 3, function(stat) {
      diag(stat) <- 0
      m <- stat
      stat <- rowMeans(m, na.rm = T) * (dim(m)[1]) / (dim(m)[1] - 1)
      stat
    })
  } else {
    dims <- dim(statMatRate) 
    # statsArrayComp: n_nodes1*n_nodes2*num_statistics matrix
    stat_mat <- apply(statMatRate, 3, function(stat) {
      m <- stat
      stat <- rowMeans(m, na.rm = T)
      stat
    })
  }
  expValue <- exp(stat_mat %*% parametersRate[-1] + parametersRate[1])

  # expected time
  tauSum <- sum(expValue)
  expectedWaitingtime <- 1 / tauSum
  waitingTime <- rexp(1, tauSum)

  # Conditional on the waiting time, the process to choose a sender is
  # a multinomial process
  simulatedSender <- sample(1:length(expValue), 1, prob = expValue)


  return(list(
    simulatedSender = simulatedSender,
    expectedWaitingtime = expectedWaitingtime,
    waitingTime = waitingTime
  ))
}

generationDyNAMChoice <- function(
    statMatRate, parametersChoice, simulatedSender, n1, n2, isTwoMode) {
  stat_mat <- statMatRate[simulatedSender, , ]
  expValue <- exp(stat_mat %*% parametersChoice)
  if (!isTwoMode) expValue[simulatedSender] <- 0
  # In DyNAM, we use multinomial process for receiver selection
  simulatedReceiver <- sample(1:length(expValue), 1, prob = expValue)
  return(list(simulatedReceiver = simulatedReceiver))
}

getUpdates <- function(
    event, effects, effIds,
    objectsEffectsLink, isUndirectedNet, n1, n2,
    isTwoMode, prepEnvir, cacheName) {
  # get the statCache from the prepEnvir
  # We does it in this way because we have to update the statCache in
  # the parent enviroment later.
  statCache = get(cacheName, envir = prepEnvir)
  # define the return variable
  updatesList = list()
  
  for (id in effIds) {
    # create the ordered list for the objects
    objectsToPass <- objectsEffectsLink[, id][!is.na(objectsEffectsLink[, id])]
    names <- rownames(objectsEffectsLink)[!is.na(objectsEffectsLink[, id])]
    orderedNames <- names[order(objectsToPass)]
    orderedObjectTable <- getDataObjects(list(list("", orderedNames)))
    .objects <- getElementFromDataObjectTable(
      orderedObjectTable, envir = prepEnvir)
    # identify class to feed effects functions
    objClass <- vapply(.objects, FUN = inherits, FUN.VALUE = integer(2),
                       what = c("numeric", "matrix"), which = TRUE) > 0
    attIDs <- which(objClass[1, ])
    netIDs <- which(objClass[2, ])
    
    # call effects function with required arguments
    .argsFUN <- list(
      network = if (length(.objects[netIDs]) == 1) {
        .objects[netIDs][[1]]
      } else {
        .objects[netIDs]
      },
      attribute = if (length(.objects[attIDs]) == 1) {
        .objects[attIDs][[1]]
      } else {
        .objects[attIDs]
      },
      cache = statCache[[id]],
      n1 = n1,
      n2 = n2
    )
    
    effectUpdate <- callFUN(
      effects, id, "effect", c(.argsFUN, event), " cannot update \n",
      colnames(objectsEffectsLink)[id]
    )
    
    updates <- effectUpdate$changes
    # if cache and changes are not null update cache
    if (!is.null(effectUpdate$cache) & !is.null(effectUpdate$changes)) {
      statCache[[id]] <- effectUpdate$cache
    }
    
    if (isUndirectedNet) {
      event2 <- event
      event2$sender <- event$receiver
      event2$receiver <- event$sender
      if (!is.null(effectUpdate$cache) & !is.null(effectUpdate$changes))
        .argsFUN$cache <- statCache[[id]]
      effectUpdate2 <- callFUN(
        effects, id, "effect", c(.argsFUN, event2), " cannot update \n",
        colnames(objectsEffectsLink)[id]
      )
      
      if (!is.null(effectUpdate2$cache) & !is.null(effectUpdate2$changes))
        statCache[[id]] <- effectUpdate2$cache
      updates2 <- effectUpdate2$changes
      updates <- rbind(updates, updates2)
    }
    
    updatesList[[id]] = updates
  }
  
  #update the statCache
  assign(cacheName, statCache, envir = prepEnvir)
  #return updatesList
  return(updatesList)
  
}
