########################
#
# Goldfish package
# Old estimation routine
# (also recomputes the previous format
# for the preprocessed object)
#
########################


# function taking in argument the preprocessed object with the current version that
# - modify this object to the previous preprocessing format (not memory-efficient at all!)
# - ude the previous estimation code (a bit more time-efficient)
estimate_old <- function(prep,
                         events,
                         nodes,
                         nodes2,
                         isTwoMode,
                         rightCensored,
                         addInterceptEffect,
                         modelType,
                         hasWindows,
                         ignoreRepParameter,
                         defaultNetworkName,
                         colOnly,
                         estimationInit,
                         initialDamping = ifelse(hasWindows, 30, 10),
                         parallelize = ifelse(cpus > 1, TRUE, FALSE),
                         cpus,
                         effectDescription,
                         verbose,
                         silent) {

  # TRANSFORM to old data format
  prepOld <- list()
  prepOld$dep <- list()
  stats <- prep$initialStats
  n1 <- nrow(get(nodes))
  n2 <- nrow(get(nodes2))

  # 4a. Update statististics WITH right-censored intervals (FIX)


  # with prep$orderEvents we know the number and sequence of dependent and exogenous events
  nEvents <- length(prep$orderEvents)
  nParams <- dim(prep$initialStats)[3]
  statsArray <- prep$initialStats
  time <- min(sapply(events, function(x) min(x$time)))
  previoustime <- time
  idep <- 1
  irc <- 1

  # utility functions for the stats update
  # stat: original initialStats in prep
  updFun <- function(stat, change) {
    if (!is.null(change)) stat[cbind(change[, "node1"], change[, "node2"])] <- change[, "replace"]
    return(stat)
  }

  # IMPUTE missing statistics with current mean; Now in preprocessing
  # imputeFun <- function(m) {
  #   m[is.na(m)] <- mean(m, na.rm = TRUE)
  #   m
  # }

  # we iterate among all events (dependent AND exogenous)
  for (i in 1:nEvents) {
    previoustime <- time

    # check whether it's a dependent or right-censored event
    isDependent <- (prep$orderEvents[[i]] == 1)
    current_time <- prep$eventTime[[i]]
    # get current event stats
    if (isDependent) {
      # dependent event   nParams: n_effects
      for (j in 1:nParams) {
        statsArray[, , j] <- updFun(statsArray[, , j], prep$dependentStatsChange[[idep]][[j]])
      }
      time <- time + prep$intervals[[idep]]
      timespan <- prep$intervals[[idep]]
      activeDyad <- c(events[[1]][idep, "sender"], events[[1]][idep, "receiver"])
      idep <- idep + 1
    } else {
      # right-censored
      for (j in 1:nParams) {
        statsArray[, , j] <- updFun(statsArray[, , j], prep$rightCensoredStatsChange[[irc]][[j]])
      }
      time <- time + prep$rightCensoredIntervals[[irc]]
      timespan <- prep$rightCensoredIntervals[[irc]]
      activeDyad <- NULL
      irc <- irc + 1
    }

    # IMPUTE missing statistics with current mean
    # for (j in 1:nParams) {
    #   statsArray[, , j] <- imputeFun(statsArray[, , j])
    # }

    # fill in the old preprocessed object
    prepOld$dep[[i]] <- list(
      change.contributions = statsArray,
      actors = activeDyad, # sender receiver
      right.censored = !isDependent,
      timespan = timespan,
      eventTime = current_time
    )
  }



  # 4b. Handle the ignoreRep option
  if (any(unlist(ignoreRepParameter))) {
    if (!silent) cat("Setting events with ignoring repetitions.\n")
    net <- get(defaultNetworkName)
    ignoreRepIds <- which(unlist(ignoreRepParameter))
    startTime <- -Inf
    for (i in 1:nEvents) {
      mat <- as.matrix(net, time = prepOld$dep[[i]]$eventTime, startTime = startTime)
      ones <- which(mat > 0, arr.ind = TRUE)
      prepOld$dep[[i]]$change.contributions[cbind(
        ones[, 1],
        ones[, 2],
        rep(ignoreRepIds, each = length(ignoreRepIds) * nrow(ones))
      )] <- 0
      startTime <- prepOld$dep[[i]]$eventTime
      net[1:dim(net)[1], 1:dim(net)[2]] <- mat
    }
  }


  # 4c. Handle composition change: remove nonpresent actors
  dependentEvents <- events[[1]]
  compChangeName1 <- attr(get(nodes), "events")["present" == attr(
    get(nodes),
    "dynamicAttribute"
  )]
  compChangeName2 <- attr(get(nodes2), "events")["present" == attr(
    get(nodes2),
    "dynamicAttribute"
  )]
  # sovchanges indicates composition change, i.e., replace = TRUE indicates this node becomes present
  prepOld$dep <- removeNonPresent(
    prep = prepOld$dep,
    nodes = nodes,
    nEvents = nEvents,
    compChangeName = compChangeName1,
    dim = 1
  )
  prepOld$dep <- removeNonPresent(prepOld$dep,
    nodes2,
    nEvents,
    compChangeName2,
    dim = 2
  )

  # 4d. Apply colOnly ex post (to be done in preprocessing and used in a smart way in the estimation)
  # in rate model, colOnly=true, compute mean value of statistics in each row
  # CHANGED SIWEI: treat one-mode and two-mode seperately
  if (colOnly) {
    if (!silent) cat("Calculating mean statistics per actor.\n")
    if (isTwoMode == FALSE) {
      prepOld$dep <- lapply(prepOld$dep, function(stat) {
        dims <- dim(stat$change.contributions)
        arr <- apply(stat$change.contributions, 3, function(mat) {
          diag(mat) <- 0
          matrix(rowMeans(mat, na.rm = TRUE), dim(mat)[1], dim(mat)[2], byrow = FALSE)
        })
        dim(arr) <- dims
        stat$change.contributions <- arr * (dims[1]) / (dims[1] - 1)
        stat
      })
    }
    else {
      prepOld$dep <- lapply(prepOld$dep, function(stat) {
        dims <- dim(stat$change.contributions)
        arr <- apply(stat$change.contributions, 3, function(mat) {
          matrix(rowMeans(mat, na.rm = TRUE), dim(mat)[1], dim(mat)[2], byrow = FALSE)
        })
        dim(arr) <- dims
        stat$change.contributions <- arr
        stat
      })
    }
  }

  modelTypeCall <- modelType
  hasIntercept <- addInterceptEffect
  if (modelType == "DyNAM-M-Rate" && !hasIntercept) {
    modelTypeCall <- "DyNAM-M-Rate-ordered"
  }
  if (modelType == "REM" && !hasIntercept) {
    modelTypeCall <- "REM-ordered"
  }

  #############  Old estimation ######################
  additionalArgs <- list(
    statsList = prepOld$dep,
    addInterceptEffect = hasIntercept,
    modelType = modelTypeCall,
    initialDamping = ifelse(hasWindows, 30, 10),
    parallelize = ifelse(cpus > 1, TRUE, FALSE),
    cpus = cpus,
    isTwoMode = isTwoMode,
    verbose = verbose,
    silent = silent
  )
  # prefer user-defined arguments
  argsEstimation <- append(
    estimationInit,
    additionalArgs[!(names(additionalArgs) %in% names(estimationInit))]
  )

  tryCatch(
    result <- do.call("estimate_int_old", args = argsEstimation),
    error = function(e) stop("Error in ", modelType, " estimation: ", e)
  )

  # old estimation
  # result$names <- if (!hasIntercept) {
  #   effectDescription
  # } else {
  #   rbind(c("Intercept", rep("", ncol(effectDescription) - 1)), effectDescription)
  # }  # # utility GetDetailPrint does
  result$names <- effectDescription
  result$formula <- formula
  result$model.type <- modelType
  result$right.censored <- hasIntercept

  return(result)
}


# Generic estimation function
estimate_int_old <- function(statsList,
                             modelType = c("DyNAM-MM", "DyNAM-M", "REM-ordered",
                                           "DyNAM-M-Rate", "REM", "DyNAM-M-Rate-ordered"),
                             initialParameters = NULL,
                             excludeParameters = NULL,
                             initialDamping = 1,
                             maxIterations = 20,
                             dampingIncreaseFactor = 2,
                             dampingDecreaseFactor = 3,
                             maxScoreStopCriterion = 0.001,
                             # additional return objects
                             returnEventProbabilities = FALSE,
                             # additional parameter for DyNAM-MM
                             allowRecursive = FALSE, # DEPRECATED
                             allowReflexive = allowRecursive,
                             isTwoMode = FALSE,
                             # additional parameter for DyNAM-M-Rate
                             addInterceptEffect = FALSE,
                             returnIntervalLogL = FALSE,
                             parallelize = FALSE,
                             cpus = 6,
                             verbose = FALSE,
                             silent = FALSE) {

  ## SET VARIABLES

  minDampingFactor <- initialDamping
  nParams <- dim(statsList[[1]]$change.contributions)[3] - length(excludeParameters) + addInterceptEffect
  parameters <- initialParameters
  if (is.null(initialParameters)) parameters <- rep(0, nParams)

  modelType <- match.arg(modelType)

  ## PARAMETER CHECKS

  if (length(parameters) != nParams)
    stop("Error in estimation. Wrong number of initial parameters passed to function: ", length(parameters))
  if (!(length(minDampingFactor) %in% c(1, nParams)))
    stop("Error in estimation. minDampingFactor has wrong length: ", length(minDampingFactor))
  if (dampingIncreaseFactor < 1 || dampingDecreaseFactor < 1)
    stop("Error in estimation. Damping increase / decrease factors cannot be smaller than one.")

  ## REDUCE STATISTICS LIST
  if (verbose) cat("Reducing data\n")
  # exclude effects
  statsList <- modifyStatisticsList_old(statsList, excludeParameters = excludeParameters)

  # reduce for dynam-m RATE model
  if (modelType == "DyNAM-M-Rate") {
    statsList <- modifyStatisticsList_old(statsList,
      # dropZeroTimespans = TRUE,
      reduceMatrixToVector = TRUE,
      addInterceptEffect = addInterceptEffect
    )
    # replace first parameter with an initial estimate of the intercept
    if (addInterceptEffect) {
      totalTime <- sum(unlist(lapply(statsList, getElement, "timespan")), na.rm = TRUE)
      nEvents <- sum(!unlist(lapply(statsList, getElement, "right.censored")), na.rm = TRUE)
      nAvgActors <- nAct <- mean(Reduce(cbind, lapply(lapply(statsList, getElement, "change.contributions"), dim))[1, ])
      if (is.null(initialParameters)) {
        initialInterceptEstimate <- log(nEvents / totalTime / nAvgActors)
        parameters[1] <- initialInterceptEstimate
      }
    }
  }

  if (modelType == "DyNAM-M-Rate-ordered") {
    statsList <- modifyStatisticsList_old(statsList,
      reduceMatrixToVector = TRUE,
      dropRightCensored = TRUE
    )
  }


  # reduce for REM (continuous-time)
  if (modelType == "REM") {
    statsList <- modifyStatisticsList_old(statsList,
      addInterceptEffect = addInterceptEffect
    )

    if (addInterceptEffect) {
      totalTime <- sum(unlist(lapply(statsList, getElement, "timespan")), na.rm = TRUE)
      nEvents <- sum(!unlist(lapply(statsList, getElement, "right.censored")), na.rm = TRUE)
      nAvgActors <- nAct <- mean(Reduce(cbind, lapply(lapply(statsList, getElement, "change.contributions"), dim))[1, ])
      if (is.null(initialParameters)) {
        initialInterceptEstimate <- log(nEvents / totalTime / (nAvgActors * (nAvgActors - 1)))
        parameters[1] <- initialInterceptEstimate
      }
    }
  }

  # reduce for dynam-m CHOICE model
  if (modelType == "DyNAM-M") {
    # drop the diagonal elements??
    statsList <- modifyStatisticsList_old(statsList,
      reduceArrayToMatrix = TRUE,
      dropRightCensored = TRUE
    )
  }

  # reduce for ORDERED REM

  if (modelType == "REM-ordered") {
    statsList <- modifyStatisticsList_old(statsList,
      dropRightCensored = TRUE
    )
  }

  if (length(statsList) == 0) stop("Statistics list reduced to length zero. Does the data fit to the model type?")

  ## SET VARIABLES BASED ON STATSLIST

  nEvents <- length(statsList)

  ## ESTIMATION

  if (verbose) cat("Estimating model type ", modelType, "\n")

  iIteration <- 1
  informationMatrix <- matrix(0, nParams, nParams)
  score <- rep(0, nParams)
  logLikelihood <- 0
  isConverged <- FALSE
  isInitialEstimation <- T
  logLikelihood.old <- -Inf

  # if (parallelize && require("snowfall", quietly = TRUE)) {
  #   snowfall::sfStop()
  #   snowfall::sfInit(parallel = TRUE, cpus = cpus)
  #   snowfall::sfExport("getMultinomialProbabilities_old", "getLikelihoodMM",
  #                      "getFirstDerivativeMM", "getMultinomialInformationMatrix", namespace = "goldfish")
  #   # sfLibrary("goldfish")
  # }

  while (TRUE) {

    # calculate logL, score and information; pass parallel computing parameters
    res <- getIterationStepState_old(statsList, parameters,
      parallelize = parallelize,
      cpus = cpus,
      modelType = modelType, # model type case discinction
      returnIntervalLogL = returnIntervalLogL,
      returnEventProbabilities = returnEventProbabilities,
      allowReflexive = allowReflexive,
      isTwoMode = isTwoMode,
      verbose = verbose
    )

    logLikelihood <- res[[1]]
    score <- res[[2]]
    informationMatrix <- res[[3]]
    if (returnIntervalLogL) intervalLogL <- res[[4]]
    #  add a possibility to return the whole probability matrix
    if (returnEventProbabilities) {
      eventProbabilities <- if (is.null(res$pMatrix)) {
        paste("not implemented for model type", modelType)
      } else {
        res$pMatrix
      }
    }

    if (isInitialEstimation && any(is.na(unlist(res))) && !all(parameters[-1] == 0)) {
      stop("Estimation not possible with initial parameters. Try using zeros instead.")
    }

    if (!verbose && !silent) {
      cat("\rMax score: ",
          round(max(abs(score)), round(-logb(maxScoreStopCriterion / 1, 10)) + 1),
          " (", iIteration, ").        ", sep = "")
    }
    if (verbose) {
      cat("\n\nLikelihood:", logLikelihood, "in iteration", iIteration,
          "\nParameters:", toString(parameters),
          "\nScore:", toString(score))
      # print(informationMatrix)
    }

    if (logLikelihood <= logLikelihood.old || any(is.na(unlist(res)))) {
      if (verbose) message("\nNo improvement in estimation. Resetting values and adjusting damping.")
      # reset values
      logLikelihood <- logLikelihood.old
      parameters <- parameters.old
      score <- score.old
      informationMatrix <- informationMatrix.old
      minDampingFactor <- minDampingFactor * dampingIncreaseFactor
    } else {
      logLikelihood.old <- logLikelihood
      parameters.old <- parameters
      score.old <- score
      informationMatrix.old <- informationMatrix
      minDampingFactor <- max(1, minDampingFactor / ifelse(isInitialEstimation, 1, dampingDecreaseFactor))
    }

    # end of initial estimation
    isInitialEstimation <- FALSE

    # Calculate the UPDATE distance taking into account the DAMPING
    dampingFactor <- minDampingFactor

    # INVERT information matrix
    inverseInformation <- try(solve(informationMatrix), silent = TRUE)
    if (inherits(inverseInformation, "try-error")) {
      stop("Matrix cannot be inverted; probably due to collinearity between parameters.")
    }
    update <- (inverseInformation %*% score) / dampingFactor

    if (verbose) {
      cat("\nUpdate: ", toString(update), sep = "")
      cat("\nDamping factor:", toString(dampingFactor), sep = "")
    }

    # check for stop criteria
    if (max(abs(score)) <= maxScoreStopCriterion) {
      isConverged <- TRUE
      cat("\nStopping as maximum absolute score is below ", maxScoreStopCriterion, ".\n", sep = "")
      # (", max(abs(score)), ")"))
      break
    }
    if (iIteration > maxIterations) {
      message("\nStopping as maximum of ", maxIterations, " iterations have been reached. No convergence.")
      break
    }

    parameters <- parameters + update

    iIteration <- iIteration + 1
  } # end of while

  # if (parallelize && require("snowfall", quietly = TRUE)) {
  #   snowfall::sfStop()
  # }


  # calculate standard errors
  stdErrors <- sqrt(diag(inverseInformation))

  # define, type and return result
  estimationResult <- list(
    parameters = parameters,
    standardErrors = stdErrors,
    logLikelihood = logLikelihood,
    finalScore = score,
    finalInformationMatrix = informationMatrix,
    convergence = list(isConverged = isConverged, maxAbsScore = max(abs(score))),
    nIterations = iIteration,
    nEvents = nEvents
  )
  if (returnIntervalLogL) estimationResult$intervalLogL <- intervalLogL
  if (returnEventProbabilities) estimationResult$eventProbabilities <- eventProbabilities
  attr(estimationResult, "class") <- "result.goldfish"
  estimationResult
}


# Function to return log likelihood, score and information matrix for a given set of parameters
# for the MM, M, REM, and M-Rate function, and REM-ordered
getIterationStepState_old <- function(statsList, parameters,
                                      parallelize = FALSE, cpus = 4,
                                      modelType,
                                      returnIntervalLogL = FALSE,
                                      returnEventProbabilities = FALSE,
                                      allowReflexive = TRUE,
                                      isTwoMode = FALSE,
                                      verbose = FALSE) {
  nDimensions <- length(dim(statsList[[1]]$change.contributions))
  nEvents <- length(statsList)
  nParams <- dim(statsList[[1]]$change.contributions)[nDimensions]

  # iterate over all events
  informationMatrix <- matrix(0, nParams, nParams)
  score <- rep(0, nParams)
  logLikelihood <- 0

  # Refactor this nasty case statement that overwrites function getEventValues

  if (modelType == "DyNAM-MM") {
    getEventValues <- function(eventStats, parameters) {
      currentStats <- eventStats
      statsArray <- currentStats[[1]]
      activeDyad <- currentStats[[2]]

      multinomialProbabilities <-
        getMultinomialProbabilities_old(currentStats, parameters, allowReflexive = allowReflexive)
      eventLikelihoods <- getLikelihoodMM(multinomialProbabilities)
      firstDerivatives <- getFirstDerivativeMM(statsArray, eventLikelihoods, multinomialProbabilities)
      eventScore <- firstDerivatives[activeDyad[1], activeDyad[2], ]
      eventInformationMatrix <- getMultinomialInformationMatrix(eventLikelihoods, firstDerivatives)

      return(list(
        logLikelihood = log(eventLikelihoods[activeDyad[1], activeDyad[2]]),
        score = eventScore,
        informationMatrix = eventInformationMatrix,
        pMatrix = eventLikelihoods
      ))
    }
  }

  if (modelType == "DyNAM-M") {
    getEventValues <- function(eventStats, parameters) {
      currentStats <- eventStats
      statsArray <- currentStats$change.contributions
      activeDyad <- currentStats$actors

      eventProbabilities <-
        getMultinomialProbabilities_old(currentStats, parameters, actorNested = TRUE,
                                        allowReflexive = FALSE, isTwoMode = isTwoMode)
      firstDerivatives <- getFirstDerivativeM(statsArray, eventProbabilities)
      eventScore <- firstDerivatives[activeDyad[2], ]
      eventInformationMatrix <- getMultinomialInformationMatrixM(eventProbabilities, firstDerivatives)

      return(list(
        logLikelihood = log(eventProbabilities[activeDyad[2]]),
        score = eventScore,
        informationMatrix = eventInformationMatrix,
        pMatrix = eventProbabilities
      ))
    }
  }

  if (modelType == "REM-ordered") {
    getEventValues <- function(eventStats, parameters) {
      currentStats <- eventStats
      statsArray <- currentStats$change.contributions
      activeDyad <- currentStats$actors

      eventProbabilities <-
        getMultinomialProbabilities_old(currentStats, parameters, actorNested = FALSE, allowReflexive = FALSE)
      firstDerivatives <- getFirstDerivativeREM(statsArray, eventProbabilities)
      eventScore <- firstDerivatives[activeDyad[1], activeDyad[2], ]
      eventInformationMatrix <- getInformationMatrixREM(eventProbabilities, firstDerivatives)

      return(list(
        logLikelihood = log(eventProbabilities[activeDyad[1], activeDyad[2]]),
        score = eventScore,
        informationMatrix = eventInformationMatrix,
        pMatrix = eventProbabilities
      ))
    }
  }

  if (modelType == "DyNAM-M-Rate-ordered") {
    getEventValues <- function(eventStats, parameters) {
      currentStats <- eventStats
      statsMatrix <- currentStats$change.contributions # reduced to matrix
      activeActor <- currentStats$actors[1]
      parameters <- c(parameters)

      rates <- exp(rowSums(t(t(statsMatrix) * parameters)))
      eventProbabilities <- rates / sum(rates)
      expectedStatistics <- colSums(statsMatrix * eventProbabilities)
      logLikelihood <- sum(statsMatrix[activeActor, ] * parameters) - log(sum(rates))
      # deviation from actual statistics
      deviations <- t(t(statsMatrix) - expectedStatistics)
      score <- deviations[activeActor, ]
      # Fisher information matrix
      eventInformationMatrix <- matrix(
        rowSums(t(t(apply(deviations, 1, function(x) outer(x, x)))
        * eventProbabilities)),
        length(parameters), length(parameters)
      )

      return(list(
        logLikelihood = logLikelihood,
        score = score,
        informationMatrix = eventInformationMatrix,
        pMatrix = eventProbabilities
      ))
    }
  }

  if (modelType %in% c("DyNAM-M-Rate", "REM")) {
    getEventValues <- function(eventStats, parameters) {
      currentStats <- eventStats

      activeDyad <- currentStats$actors
      activeActor <- activeDyad[1]
      statsArray <- currentStats$change.contributions
      dimMatrix <- dim(statsArray)
      if (modelType == "REM") {
        activeActor <- activeDyad[1] + (activeDyad[2] - 1) * dimMatrix[1]
        statsArray <- apply(statsArray, 3, c)
      }

      isRightCensored <- currentStats$right.censored
      timespan <- currentStats$timespan
      parameters <- as.numeric(parameters)

      # test if time interval is NA
      if (is.na(timespan)) timespan <- 0

      # vector of rates
      objectiveFunctions <- rowSums(t(t(statsArray) * parameters)) # a vector
      objectiveFunctionOfSender <- objectiveFunctions[activeActor]
      statsOfSender <- statsArray[activeActor, ]
      rates <- exp(objectiveFunctions)
      ratesSum <- sum(rates)
      # k vector with all rho * s_k summed over all actors i
      ratesStats <- rates * statsArray
      ratesStatsSum <- colSums(rates * statsArray)

      ratesStatsStatsSum <- colSums(t(apply(statsArray, 1, function(x) outer(x, x))) * rates)
      if (length(parameters) == 1 && modelType == "DyNAM-M-Rate") {
        v <- as.vector(statsArray)
        sum <- 0
        for (i in seq_along(v)) {
          sum <- sum + v[i] * v[i] * rates[i]
        }
        ratesStatsStatsSum <- sum
      }
      dim(ratesStatsStatsSum) <- rep(length(parameters), 2)

      logL <- -timespan * ratesSum + if (!isRightCensored) objectiveFunctionOfSender else 0
      score <- -timespan * ratesStatsSum + if (!isRightCensored) statsOfSender else 0
      hessian <- -timespan * ratesStatsStatsSum
      pVector <- objectiveFunctions + (-timespan * ratesSum)
      if (modelType == "REM") dim(pVector) <- c(dimMatrix[1], dimMatrix[2])

      return(list(
        logLikelihood = logL,
        score = score,
        informationMatrix = -hessian,
        pMatrix = pVector
      ))
    }
  }


  # check for parallelization
  # if (!parallelize || !require("snowfall", quietly = TRUE)) {
  #   # no parallelization
    eventValues <- lapply(statsList, getEventValues, parameters = parameters)
  # } else {
  #   eventValues <- snowfall::sfLapply(statsList, getEventValues, parameters = parameters)
  # }

  logLikelihood <- Reduce("+", lapply(eventValues, function(v) v$logLikelihood))
  score <- Reduce("+", lapply(eventValues, function(v) v$score))
  informationMatrix <- Reduce("+", lapply(eventValues, function(v) v$informationMatrix))

  returnList <- list(
    logLikelihood = logLikelihood,
    score = score,
    informationMatrix = informationMatrix
  )
  if (returnIntervalLogL) returnList$eventLogL <- sapply(eventValues, function(v) v$logLikelihood, simplify = TRUE)
  if (returnEventProbabilities) returnList$pMatrix <- lapply(eventValues, getElement, "pMatrix")

  return(returnList)
}


# Function to calculate a matrix of i->j multinomial choice probabilities (non-logged)
# for one term of the
getMultinomialProbabilities_old <- function(currentStats, parameters,
                                            actorNested = TRUE, allowReflexive = TRUE, isTwoMode = FALSE) {
  statsArray <- currentStats[[1]]

  # allow this for a two- OR a three-dimensional array provided as input
  nDimensions <- length(dim(statsArray))
  if (!(nDimensions %in% 2:3))
    stop("StatsArray in getMultinomialProbabilities_old has to be two- or three-dimensional.")

  nParams <- dim(statsArray)[nDimensions]
  nActors <- dim(statsArray)[1]
  if (nDimensions == 3) {
    matrixSize <- nActors * nActors
    # multiply parameters with the statistics; slice by slice
    # the cube has to be transposed for third-dimension-wise multyplication
    weightedStatsArray <- statsArray * rep(parameters, each = matrixSize)
    # get utility = exp( value of objective function )
    utility <- exp(apply(weightedStatsArray, 1:2, sum))
    if (!allowReflexive & !isTwoMode) diag(utility) <- 0
    if (actorNested) {
      denominators <- rowSums(utility)
    } else {
      # for REM
      denominators <- sum(utility)
    }
  }
  if (nDimensions == 2) {
    weightedStatsArray <- sweep(statsArray, MARGIN = 2, parameters, "*")
    utility <- exp(rowSums(weightedStatsArray))
    # allow reflexive?
    if (!allowReflexive & !isTwoMode) utility[currentStats$actors[1]] <- 0
    denominators <- sum(utility)
  }
  utility / denominators
}
# for testing the above
# statsArray <- aperm(array(floor(runif(18)*5), dim = c(3,3,2)), c(2,1,3))
# parameters <- c(1, 2)
# multinomialProbabilities <- utility / denominators


# Utility function to modify the statistics list
modifyStatisticsList_old <- function(statsList,
                                     excludeParameters = NULL,
                                     reduceMatrixToVector = FALSE,
                                     reduceArrayToMatrix = FALSE,
                                     dropRightCensored = FALSE,
                                     dropZeroTimespans = FALSE,
                                     addInterceptEffect = FALSE) {
  if (reduceMatrixToVector && reduceArrayToMatrix) {
    stop("Cannot reduce matrix to vector and array to matrix at the same time")
  }

  if (dropRightCensored) {
    rightCensoredIds <- which(unlist(lapply(statsList, getElement, "right.censored")))
    if (length(rightCensoredIds)) {
      statsList <- statsList[-rightCensoredIds]
    }
  }

  # reduce statistics matrix to a vector
  if (reduceMatrixToVector) {
    statsList <- lapply(statsList, function(v) {
      # check it there is row variation. Then: stop
      apply(v$change.contributions, c(3, 1), function(row) {
        if (min(row, na.rm = TRUE) != max(row, na.rm = TRUE)) {
          stop("Rate variable varies within event senders.")
        }
      })
      # generates a matrix from an array
      v$change.contributions <- apply(v$change.contributions, 3, rowMeans, na.rm = TRUE)
      if (is.null(dim(v$change.contributions)) && is.numeric(v$change.contributions)) {
        v$change.contributions <- matrix(v$change.contributions, 1, 1) # if only one actor
      }
      v
    })
  }

  # reduce array to matrices
  if (reduceArrayToMatrix) {
    statsList <- lapply(statsList, function(v) {
      oldDim <- dim(v$change.contributions)
      v$change.contributions <- matrix(v$change.contributions[v$actors[1], , ], oldDim[2], oldDim[3])
      v
    })
  }

  # add a rate intercept that is 1 for everyone (dummy for \theta_0)
  # The format may be a vector or a matrix (see above)
  if (addInterceptEffect) {
    statsList <- lapply(statsList, function(v) {
      # data is in matrix format
      dimensions <- dim(v$change.contributions)
      if (length(dimensions) == 3) {
        oldValues <- v$change.contribution
        newValues <- matrix(1, dimensions[1], dimensions[2])
        v$change.contributions <- array(c(newValues, oldValues), dim = dimensions + c(0, 0, 1))
      }
      # data is in vector format
      if (length(dimensions) == 2) {
        v$change.contributions <- cbind(1, v$change.contributions)
      }
      # data is in vector format but there is only one actor...
      if (is.null(dimensions) && is.numeric(v$change.contributions)) {
        v$change.contributions <- cbind(1, v$change.contributions)
      }
      v
    })
  }


  # drop statistics with a time span of zero
  if (dropZeroTimespans) {
    hasZeroTime <- which(0 == unlist(lapply(statsList, getElement, "timespan")))
    statsList <- statsList[-hasZeroTime]
  }


  # exclude effect statistics
  if (!is.null(excludeParameters)) {
    unknownIndexes <- setdiff(excludeParameters, 1:dim(statsList[[1]]$change.contributions)[3])
    if (length(unknownIndexes) > 0) {
      stop("Unknown parameter indexes in 'excludeIndexes': ", paste(unknownIndexes, collapse = " "))
    }

    statsList <- lapply(statsList, function(v) {
      v.new <- v
      v.new$change.contributions <- v.new$change.contributions[, , -excludeParameters]
      v.new
    })
  }

  statsList
}


# CHANGED SIWEI: iterate over all dependent and right-cencored events for composition change
removeNonPresent <- function(prep, nodes, nEvents, compChangeName, dim = 1) {
  # check format agains
  if (length(compChangeName) == 0) {
    return(prep)
  }

  compChange <- get(compChangeName)
  compChange <- sanitizeEvents(compChange, nodes)
  presence <- get(nodes)$present
  oldTime <- -Inf

  for (iEvent in 1:nEvents) {
    time <- prep[[iEvent]]$eventTime
    update <- compChange[compChange$time <= time & compChange$time > oldTime, ]
    presence[update$node] <- update$replace
    oldTime <- time
    dims <- dim(prep[[iEvent]]$change.contributions)
    prep[[iEvent]]$change.contributions <-
      prep[[iEvent]]$change.contributions[
        if (dim == 1) presence else TRUE,
        if (dim == 2) presence else TRUE,
      ]
    dim(prep[[iEvent]]$change.contributions) <- c(
      if (dim == 1) sum(presence) else dims[1],
      if (dim == 2) sum(presence) else dims[2],
      dims[3]
    )
    if (prep[[iEvent]]$right.censored == FALSE) {
      position <- which(prep[[iEvent]]$actors[dim] == which(presence))
      if (length(position) == 0) {
        stop("Active node ", prep[[iEvent]]$actors[dim], " not present in event ", iEvent)
      }
      prep[[iEvent]]$actors[dim] <- which(prep[[iEvent]]$actors[dim] == which(presence))
    }
  }
  return(prep)
}
