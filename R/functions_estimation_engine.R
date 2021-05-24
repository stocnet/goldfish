##################### ##
#
# Goldfish package
# Internal estimation routine
#
#################### ###

# Estimation
estimate_int <- function(
  statsList,
  nodes, nodes2,
  defaultNetworkName,
  modelType = c("DyNAM-MM", "DyNAM-M", "REM-ordered",
                "DyNAM-M-Rate", "REM", "DyNAM-M-Rate-ordered"),
  initialParameters = NULL,
  fixedParameters = NULL,
  excludeParameters = NULL,
  initialDamping = 1,
  maxIterations = 20,
  dampingIncreaseFactor = 2,
  dampingDecreaseFactor = 3,
  maxScoreStopCriterion = 0.001,
  # additional return objects
  returnEventProbabilities = FALSE,
  # additional parameter for DyNAM-MM
  allowReflexive = FALSE,
  isTwoMode = FALSE,
  # additional parameter for DyNAM-M-Rate
  addInterceptEffect = FALSE,
  returnIntervalLogL = FALSE,
  parallelize = FALSE,
  cpus = 6,
  verbose = FALSE,
  silent = FALSE,
  impute = TRUE,
  ignoreRepParameter,
  # restrictions of opportunity sets
  opportunitiesList = NULL) {

  ## SET VARIABLES

  minDampingFactor <- initialDamping
  # CHANGED MARION
  # nParams: number of effects + 1 (if has intercept)
  nParams <- dim(statsList$initialStats)[3] - length(excludeParameters) + addInterceptEffect
  #
  parameters <- initialParameters
  if (is.null(initialParameters)) parameters <- rep(0, nParams)
  # deal with fixedParameters
  idUnfixedCompnents <- seq_len(nParams)
  idFixedCompnents <- NULL
  likelihoodOnly <- FALSE
  if (!is.null(fixedParameters)) {
    if (length(fixedParameters) != nParams) {
      stop("The length of fixedParameters is inconsistent with the length of the parameters, which is ",
                 nParams, ".", call. = FALSE)
    }
    if (all(!is.na(fixedParameters))) likelihoodOnly <- TRUE
    parameters[!is.na(fixedParameters)] <- fixedParameters[!is.na(fixedParameters)]
    idUnfixedCompnents <- which(is.na(fixedParameters))
    idFixedCompnents <- which(!is.na(fixedParameters))
  }

  modelType <- match.arg(modelType)
  # modelType <- modelTypeCall

  ## PARAMETER CHECKS

  if (length(parameters) != nParams)
    stop("Error in estimation. Wrong number of initial parameters passed to function: ", length(parameters))
  if (!(length(minDampingFactor) %in% c(1, nParams)))
    stop("Error in estimation. minDampingFactor has wrong length: ", length(minDampingFactor))
  if (dampingIncreaseFactor < 1 || dampingDecreaseFactor < 1)
    stop("Error in estimation. Damping increase / decrease factors cannot be smaller than one.")

  ## REDUCE STATISTICS LIST

  if (verbose) cat("Reducing data\n")

  # CHANGED MARION: add colOnly and rowOnly in a smart way for the estimation
  reduceMatrixToVector <- F
  reduceArrayToMatrix <- F
  if (modelType %in% c("DyNAM-M-Rate", "DyNAM-M-Rate-ordered")) {
    reduceMatrixToVector <- T
  } else if (modelType == "DyNAM-M") {
    reduceArrayToMatrix <- T
  }

  # CHANGED MARION: updated function
  # for rate model with intercept, add a table of all 1 to the statsList$initStats
  statsList <- modifyStatisticsList(statsList, modelType,
    reduceMatrixToVector = reduceMatrixToVector,
    reduceArrayToMatrix = reduceArrayToMatrix,
    excludeParameters = excludeParameters,
    addInterceptEffect = addInterceptEffect
  )

  # CHANGED MARION: handle composition changes for counting average number of actors
  # and remove absent actors for each estimation step

  ## GET COMPOSITION CHANGES
  compChangeName1 <- attr(nodes, "events")["present" == attr(nodes, "dynamicAttribute")]
  compChangeName2 <- attr(nodes2, "events")["present" == attr(nodes2, "dynamicAttribute")]
  compChange1 <- NULL
  compChange2 <- NULL
  if (!is.null(compChangeName1) && length(compChangeName1) > 0) compChange1 <- get(compChangeName1)
  if (!is.null(compChangeName2) && length(compChangeName2) > 0) compChange2 <- get(compChangeName2)

  ## ADD INTERCEPT
  # CHANGED MARION
  # replace first parameter with an initial estimate of the intercept
  if (modelType %in% c("REM", "DyNAM-M-Rate") && addInterceptEffect) {
    totalTime <- sum(unlist(statsList$intervals), na.rm = TRUE) +
      sum(unlist(statsList$rightCensoredIntervals), na.rm = TRUE)
    nEvents <- length(statsList$orderEvents)
    # CHANGED MARION: remove the use of the events object
    time <- statsList$eventTime[[1]]
    previoustime <- time
    currentInterval <- 1
    currentRCInterval <- 1
    nAvgActors <- 0
    if (!is.null(nodes$present)) {
      nActors <- length(which(nodes$present == TRUE))
    } else {
      nActors <- dim(nodes)[1]
    }
    for (i in seq.int(nEvents)) {
      previoustime <- time
      if (statsList$orderEvents[[i]] == 1) {
        time <- time + statsList$intervals[[currentInterval]]
        currentInterval <- currentInterval + 1
      } else {
        time <- time + statsList$rightCensoredIntervals[[currentRCInterval]]
        currentRCInterval <- currentRCInterval + 1
      }
      nplus <- intersect(
        intersect(which(compChange1$time > previoustime), which(compChange1$time <= time)),
        which(compChange1$replace == TRUE)
      )
      nminus <- intersect(
        intersect(which(compChange1$time > previoustime), which(compChange1$time <= time)),
        which(compChange1$replace == FALSE)
      )
      nActors <- nActors + length(nplus) - length(nminus)
      nAvgActors <- nAvgActors + nActors
    }
    nAvgActors <- nAvgActors / length(statsList$orderEvents)
    if (is.null(initialParameters) && (is.null(fixedParameters) || is.na(fixedParameters[1]))) {
      initialInterceptEstimate <- log(nEvents / totalTime / nAvgActors)
      parameters[1] <- initialInterceptEstimate
    }
  }
  #

  ## SET VARIABLES BASED ON STATSLIST

  # CHANGED MARION
  nEvents <- length(statsList$orderEvents) # number of events

  ## ESTIMATION: INITIALIZATION

  if (verbose) cat("Estimating model type with the super efficient new method", modelType, "\n")

  iIteration <- 1
  informationMatrix <- matrix(0, nParams, nParams)
  score <- rep(0, nParams)
  logLikelihood <- 0
  isConverged <- FALSE
  isInitialEstimation <- T
  logLikelihood.old <- -Inf
  parameters.old <- initialParameters
  score.old <- NULL
  informationMatrix.old <- NULL

  # if (parallelize && require("snowfall", quietly = TRUE)) {
  #   snowfall::sfStop()
  #   snowfall::sfInit(parallel = TRUE, cpus = cpus)
  #   snowfall::sfExport("getMultinomialProbabilities", "getLikelihoodMM", "getFirstDerivativeMM",
  #                      "getMultinomialInformationMatrix", namespace = "goldfish")
  # }

  ## ESTIMATION: ITERATIONS
  while (TRUE) {
    # MARION to be make: update this function for the new stat list
    # calculate logL, score and information; pass parallel computing parameters
    res <- getIterationStepState(statsList,
      nodes,
      nodes2,
      defaultNetworkName,
      compChange1,
      compChange2,
      parameters,
      parallelize = parallelize,
      cpus = cpus,
      modelType = modelType, # model type case discinction
      returnIntervalLogL = returnIntervalLogL,
      returnEventProbabilities = returnEventProbabilities,
      allowReflexive = allowReflexive,
      isTwoMode = isTwoMode,
      reduceMatrixToVector = reduceMatrixToVector,
      reduceArrayToMatrix = reduceArrayToMatrix,
      silent = silent,
      verbose = verbose,
      ignoreRepParameter = ignoreRepParameter,
      impute = impute,
      opportunitiesList = opportunitiesList
    )

    logLikelihood <- res[[1]]
    score <- res[[2]]
    informationMatrix <- res[[3]]
    if (returnIntervalLogL) intervalLogL <- res[[4]]
    # add a possibility to return the whole probability matrix: to be make
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

    # If we only want the likelihood break here
    if (likelihoodOnly) {
      inverseInformationUnfixed <- matrix(0, nParams, nParams)
      score <- rep(0, nParams)
      isConverged <- TRUE
      break
    }

    # we don't consider the fixed components of the score. It's for the fixing parameter feature. \
    score[idFixedCompnents] <- 0

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
    isInitialEstimation <- F

    # Calculate the UPDATE distance taking into account the DAMPING
    dampingFactor <- minDampingFactor

    # INVERT information matrix
    # We only invert the unfixed part of the parameter. The fixed components of the score have already be set to be 0.
    # It's for the fixing parameter feature.
    informationMatrixUnfixed <- informationMatrix[idUnfixedCompnents, idUnfixedCompnents]
    inverseInformationUnfixed <- try(solve(informationMatrixUnfixed), silent = TRUE)
    if (inherits(inverseInformationUnfixed, "try-error")) {
      stop("Matrix cannot be inverted; probably due to collinearity between parameters.")
    }
    update <- rep(0, nParams)
    update[idUnfixedCompnents] <- (inverseInformationUnfixed %*% score[idUnfixedCompnents]) / dampingFactor

    if (verbose) {
      cat("\nUpdate: ", toString(update), sep = "")
      cat("\nDamping factor: ", toString(dampingFactor), sep = "")
    }

    # check for stop criteria
    if (max(abs(score)) <= maxScoreStopCriterion) {
      isConverged <- TRUE
      if (!silent) cat("\nStopping as maximum absolute score is below ", maxScoreStopCriterion, ".\n", sep = "")
      break
    }
    if (iIteration > maxIterations) {
      if (!silent)
        message("\nStopping as maximum of ", maxIterations, " iterations have been reached. No convergence.\n")
      break
    }

    parameters <- parameters + update

    iIteration <- iIteration + 1
  } # end of while

  ## ESTIMATION: END

  # if (parallelize && require("snowfall", quietly = TRUE)) {
  #   snowfall::sfStop()
  # }

  # calculate standard errors
  # the variance for the fixed compenents should be 0
  stdErrors <- rep(0, nParams)
  stdErrors[idUnfixedCompnents] <- sqrt(diag(inverseInformationUnfixed))

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


# Function to return log likelihood, score, information matrix, and p vector for each event
# CHANGED SIWEI: add three parameters: isRightCensored, timespan and allowReflexive
getEventValues <- function(statsArray, activeDyad, parameters, modelType, isRightCensored,
                           timespan, allowReflexive, isTwoMode) {
  if (modelType == "DyNAM-MM") {
    multinomialProbabilities <-
      getMultinomialProbabilities(statsArray, activeDyad, parameters, allowReflexive = allowReflexive)
    eventLikelihoods <- getLikelihoodMM(multinomialProbabilities)
    logLikelihood <- log(eventLikelihoods[activeDyad[1], activeDyad[2]])
    firstDerivatives <- getFirstDerivativeMM(statsArray, eventLikelihoods, multinomialProbabilities)
    score <- firstDerivatives[activeDyad[1], activeDyad[2], ]
    informationMatrix <- getMultinomialInformationMatrix(eventLikelihoods, firstDerivatives)
    pMatrix <- eventLikelihoods
  }

  if (modelType == "DyNAM-M") {
    eventProbabilities <-
      getMultinomialProbabilities(statsArray, activeDyad, parameters,
                                  actorNested = TRUE, allowReflexive = FALSE, isTwoMode = isTwoMode)
    logLikelihood <- log(eventProbabilities[activeDyad[2]])
    firstDerivatives <- getFirstDerivativeM(statsArray, eventProbabilities)
    score <- firstDerivatives[activeDyad[2], ]
    informationMatrix <- getMultinomialInformationMatrixM(eventProbabilities, firstDerivatives)
    pMatrix <- eventProbabilities
  }

  if (modelType == "REM-ordered") {
    eventProbabilities <-
      getMultinomialProbabilities(statsArray, activeDyad, parameters,
                                  actorNested = FALSE, allowReflexive = FALSE)
    logLikelihood <- log(eventProbabilities[activeDyad[1], activeDyad[2]])
    firstDerivatives <- getFirstDerivativeREM(statsArray, eventProbabilities)
    score <- firstDerivatives[activeDyad[1], activeDyad[2], ]
    informationMatrix <- getInformationMatrixREM(eventProbabilities, firstDerivatives)
    pMatrix <- eventProbabilities
  }

  if (modelType == "DyNAM-M-Rate-ordered") {
    # statsMatrix <- reduceArrayToMatrix(statsArray)
    statsMatrix <- statsArray
    activeActor <- activeDyad[1]
    parameters <- c(parameters)

    rates <- exp(rowSums(t(t(statsMatrix) * parameters)))
    eventProbabilities <- rates / sum(rates)
    expectedStatistics <- colSums(statsMatrix * eventProbabilities)
    # statsMatrix[activeActor, ] * parameters: rate for actor i (i=activeActor)
    logLikelihood <- sum(statsMatrix[activeActor, ] * parameters) - log(sum(rates))
    # deviation from actual statistics
    deviations <- t(t(statsMatrix) - expectedStatistics)
    score <- deviations[activeActor, ]
    # Fisher information matrix
    informationMatrix <- matrix(
      rowSums(t(t(matrix(apply(deviations, 1, function(x) outer(x, x)), ncol = length(eventProbabilities)))
      * eventProbabilities)),
      length(parameters), length(parameters)
    )
    pMatrix <- eventProbabilities
  }

  if (modelType %in% c("DyNAM-M-Rate", "REM")) {
    activeActor <- activeDyad[1]
    dimMatrix <- dim(statsArray)
    if (modelType == "REM") {
      activeActor <- activeDyad[1] + (activeDyad[2] - 1) * dimMatrix[1]
      statsArray <- apply(statsArray, 3, c)
    }

    parameters <- as.numeric(parameters)

    # test if time interval is NA, to be make
    if (is.na(timespan)) timespan <- 0

    # Don't consider self-connecting edge when both allowReflexive and  isTwoMode are false
    dontConsiderSelfConnecting <- (modelType == "REM") && !allowReflexive && !isTwoMode
    if (dontConsiderSelfConnecting) {
      idEdgeNotConsidered <- (seq.int(dimMatrix[1]) - 1) * dimMatrix[1] + seq.int(dimMatrix[1])
    } else {
      idEdgeNotConsidered <- numeric(0)
    }
    # vector of rates
    objectiveFunctions <- rowSums(t(t(statsArray) * parameters)) # a vector
    objectiveFunctionOfSender <- objectiveFunctions[activeActor]
    statsOfSender <- statsArray[activeActor, ]
    rates <- exp(objectiveFunctions)
    rates[idEdgeNotConsidered] <- 0
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

  return(list(
    logLikelihood = logLikelihood,
    score = score,
    informationMatrix = informationMatrix,
    pMatrix = pMatrix
  ))
}


# calculate the score contribution of one event of the M model
# to the log(!) likelihood
# The scores are the differences between expected and observed statistics
getFirstDerivativeM <- function(statsArray, eventProbabilities) {
  nParams <- dim(statsArray)[2]
  nActors <- dim(statsArray)[1]

  expectedStatistics <- colSums(statsArray * eventProbabilities)
  firstDerivatives <- sweep(statsArray, MARGIN = 2, expectedStatistics, "-")

  firstDerivatives
}


# Function to calculate the array of first derivatives of the log likelihood
# 1) get the likelihoods for each i-j combination
# 2) calculate the ratios of multP_{ij}' / multP_{ij} = s_{ij1} - \sum_{h} multP_{ih} s_{ih1}
#     for the derivative of the first parameter (deviation from expectation) in the multinomial part
# 3) calculate the constant that is substracted from each first derivative
# 4) calculate the derivative based on the values above (see paper)
getFirstDerivativeMM <- function(statsArray, likelihoods, multinomialProbabilities) {
  nParams <- dim(statsArray)[3]
  nActors <- dim(statsArray)[1]
  matrixSize <- nActors * nActors
  # 2)
  # multiply each "parameter slice" (third dimension) with the multinomial probability matrix
  weightedValues <- statsArray * rep(multinomialProbabilities, nParams)
  expectedStatistics <- apply(weightedValues, 3, rowSums)
  # deviation from expected statistic
  deviationFromExpectation <- statsArray - c(apply(expectedStatistics, 2, rep, nActors))

  # 3)
  # symmetrize the deviations from expectations
  symDeviations <- deviationFromExpectation + aperm(deviationFromExpectation, c(2, 1, 3))
  # the likelihoods have to divided by 2 as the matrix sum is 2 (it includes all likelihoods twice)
  constantWeightedDeviations <- apply(symDeviations * c(likelihoods / 2), 3, sum)

  # 4)
  derivatives <- symDeviations - rep(constantWeightedDeviations, each = matrixSize)

  derivatives
}


# calculate the score contribution of one event of the M model
# to the log(!) likelihood
# The scores are the differences between expected and observed statistics
getFirstDerivativeREM <- function(statsArray, eventProbabilities) {
  nActors <- dim(statsArray)[1]
  nParams <- dim(statsArray)[3]

  expectedStatistics <- apply(apply(statsArray, 3, function(m) m * eventProbabilities), 2, sum)
  firstDerivatives <- sweep(statsArray, MARGIN = 3, expectedStatistics, "-")

  firstDerivatives
}


getInformationMatrixREM <- function(eventProbabilities, firstDerivatives) {
  nParams <- dim(firstDerivatives)[3]
  nActors <- dim(firstDerivatives)[1]

  # all indexes: 1-1, 1-2, ..., nParams-nParams
  indexes <- expand.grid(seq.int(nParams), seq.int(nParams))
  # indexes <- indexes[indexes[, 1] <= indexes[, 2], ]

  values <- colSums(apply(
    indexes, 1,
    function(ind) firstDerivatives[, , ind[1]] * firstDerivatives[, , ind[2]] * eventProbabilities))
  information <- matrix(values, nParams, nParams)
  # symmetrize
  # information[lower.tri(information)] <- information[upper.tri(information)]

  information
}


# Function to return log likelihood, score and information matrix for all events, given a set of parameters
# for the MM, M, REM, and M-Rate function, and REM-ordered
getIterationStepState <- function(statsList,
                                  nodes,
                                  nodes2,
                                  defaultNetworkName,
                                  compChange1,
                                  compChange2,
                                  parameters,
                                  parallelize = FALSE, cpus = 4,
                                  modelType,
                                  returnIntervalLogL = FALSE,
                                  returnEventProbabilities = FALSE,
                                  allowReflexive = TRUE,
                                  isTwoMode = FALSE,
                                  reduceMatrixToVector = FALSE,
                                  reduceArrayToMatrix = FALSE,
                                  silent = TRUE,
                                  verbose = FALSE,
                                  ignoreRepParameter,
                                  impute = TRUE,
                                  opportunitiesList = opportunitiesList) {

  # CHANGED MARION: changed dims
  nEvents <- length(statsList$orderEvents)
  nParams <- dim(statsList$initialStats)[3]

  # iterate over all events
  informationMatrix <- matrix(0, nParams, nParams) # n_effects * n_effects
  score <- rep(0, nParams)
  logLikelihood <- 0

  # check for parallelization
  # if (parallelize && require("snowfall", quietly = TRUE)) {
  #   snowfall::sfStop()
  #   snowfall::sfInit(parallel = TRUE, cpus = cpus)
  #   snowfall::sfExport("getEventValues", namespace = "goldfish")
  # }

  # initialize progressbar output
  showProgressBar <- F
  progressEndReached <- F
  # if(!silent) {
  #  showProgressBar <- T
  #  dotEvents <- seq(1, nEvents, round(nEvents/min(nEvents, 50)))
  # }

  # CHANGED Marion: fill in the loop! stats array needs to be computed
  # also changes for dependent and rc events!
  statsArray <- statsList$initialStats # 84*84*6
  # CHANGED Marion: remove the use of events object
  time <- statsList$eventTime[[1]]
  previoustime <- time
  idep <- 1
  irc <- 1

  if (any(unlist(ignoreRepParameter))) {
    net <- get(defaultNetworkName)
    ignoreRepIds <- which(unlist(ignoreRepParameter))
    if (modelType %in% c("DyNAM-M-Rate", "REM")) {
      ignoreRepIds <- ignoreRepIds + 1 # with intercept, the first effect is the intercept without ignoreRep option
    }
    startTime <- -Inf
  }

  if (!is.null(nodes$present)) {
    presence <- nodes$present
  } else {
    presence <- rep(TRUE, length(nodes))
  }
  if (!is.null(nodes2$present)) {
    presence2 <- nodes2$present
  } else {
    presence2 <- rep(TRUE, length(nodes2))
  }

  # utility function for the statistics update
  updFun <- function(stat, change, reduceArrayToMatrix, reduceMatrixToVector) {
    # stat: current statistics (for one effect only)
    # change: statsList$dependentStatsChange[[current event]][[current effect]]
    if (!is.null(change)) stat[cbind(change[, "node1"], change[, "node2"])] <- change[, "replace"]
    return(stat)
  }

  # IMPUTE missing statistics with current mean
  imputeFun <- function(m) {
    m[is.na(m)] <- mean(m, na.rm = TRUE)
    m
  }
  oldTime <- -Inf
  EventProbabilities <- list()
  for (i in seq_len(nEvents)) {
    previoustime <- time

    # get current event stats from preprosessing results based on time order
    # CHANGED SIWEI: update statistics, timespan, isDependent and activeDyad in diff cases with or without intercept
    # CHANGED SIWEI: treat cases with / without intercept seperately
    if (modelType %in% c("DyNAM-M-Rate", "REM")) {
      # with intercept in the model
      if (statsList$orderEvents[[i]] == 1) {
        # dependent event
        isDependent <- T
        for (j in seq.int(nParams - 1)) {
          statsArray[, , j + 1] <-
            updFun(statsArray[, , j + 1], statsList$dependentStatsChange[[idep]][[j]],
                   reduceArrayToMatrix, reduceMatrixToVector)
        }
        time <- time + statsList$intervals[[idep]]
        timespan <- statsList$intervals[[idep]]
        # CHANGED Marion: remove the use of events object
        activeDyad <- c(statsList$eventSender[[i]], statsList$eventReceiver[[i]])
        idep <- idep + 1
      } else {
        # right-censored
        isDependent <- F
        for (j in seq.int(nParams - 1)) {
          statsArray[, , j + 1] <-
            updFun(statsArray[, , j + 1], statsList$rightCensoredStatsChange[[irc]][[j]],
                   reduceArrayToMatrix, reduceMatrixToVector)
        }
        time <- time + statsList$rightCensoredIntervals[[irc]]
        timespan <- statsList$rightCensoredIntervals[[irc]]
        activeDyad <- NULL
        irc <- irc + 1
      }
    } else {
      # without intercept in the model
      if (statsList$orderEvents[[i]] == 1) {
        # dependent event
        isDependent <- T
        for (j in seq.int(nParams)) {
          statsArray[, , j] <-
            updFun(statsArray[, , j], statsList$dependentStatsChange[[idep]][[j]],
                   reduceArrayToMatrix, reduceMatrixToVector)
        }
        time <- time + statsList$intervals[[idep]]
        timespan <- statsList$intervals[[idep]]
        # CHANGED Marion: remove the use of events object
        activeDyad <- c(statsList$eventSender[[i]], statsList$eventReceiver[[i]])
        idep <- idep + 1
      } else {
        # right-censored
        isDependent <- F
        for (j in seq.int(nParams)) {
          statsArray[, , j] <-
            updFun(statsArray[, , j], statsList$rightCensoredStatsChange[[irc]][[j]],
                   reduceArrayToMatrix, reduceMatrixToVector)
        }
        time <- time + statsList$rightCensoredIntervals[[irc]]
        timespan <- statsList$rightCensoredIntervals[[irc]]
        activeDyad <- NULL
        irc <- irc + 1
      }
    }

    # IMPUTE missing statistics with current mean
    if (impute) {
      for (j in seq.int(nParams)) {
        statsArray[, , j] <- imputeFun(statsArray[, , j])
      }
    }


    statsArrayComp <- statsArray
    # Handle the ignoreRep option
    if (any(unlist(ignoreRepParameter))) {
      mat <- as.matrix(net, time = statsList$eventTime[[i]], startTime = startTime)
      ones <- which(mat > 0, arr.ind = TRUE)
      statsArrayComp[cbind(
        ones[, 1],
        ones[, 2],
        rep(ignoreRepIds, each = length(ignoreRepIds) * nrow(ones))
        )] <- 0
      # CHANGED SIWEI
      startTime <- statsList$eventTime[[i]]
      net[seq.int(dim(net)[1]), seq.int(dim(net)[2])] <- mat
    }

    # update opportunity set
    opportunities <- rep(TRUE, nrow(nodes2))
    updateopportunities <- !is.null(opportunitiesList)
    if (updateopportunities)
      opportunities <- seq.int(nrow(nodes2)) %in% opportunitiesList[[i]]

    # update composition
    # CHANGED SIWEI: fixed errors for composition change update
    presence <- rep(TRUE, nrow(nodes))
    presence2 <- rep(TRUE, nrow(nodes2))
    updatepresence <- !is.null(compChange1)
    updatepresence2 <- !is.null(compChange2)

    current_time <- statsList$eventTime[[i]]
    if (updatepresence) {
      compChange1 <- sanitizeEvents(compChange1, nodes)
      dims <- dim(statsArrayComp)

      update <- compChange1[compChange1$time <= current_time & compChange1$time > oldTime, ]
      presence[update$node] <- update$replace
#
#       # add the opportunity sets restrictions
#       if (!is.null(opportunitiesList) & !isTwoMode)
#         presence <- presence & opportunities
#
#
#      statsArrayComp <- statsArrayComp[presence, , ]
#      dim(statsArrayComp) <- c(sum(presence), dims[2], dims[3])
#      if (statsList$orderEvents[[i]] == 1) {
#        position <- which(activeDyad[1] == which(presence))
#        if (length(position) == 0) {
#          stop("Active node ", activeDyad[1], " not present in event ", i)
#        }
#        activeDyad[1] <- which(activeDyad[1] == which(presence))
#      }
    }
    if (updatepresence2) {
      compChange2 <- sanitizeEvents(compChange2, nodes2)
      dims <- dim(statsArrayComp)

      update2 <- compChange2[compChange2$time <= current_time & compChange2$time > oldTime, ]
      presence2[update2$node] <- update2$replace
      #
      # # add the opportunity sets restrictions
      # if (!is.null(opportunitiesList))
      #   presence2 <- presence2 & opportunities
      #
      # statsArrayComp <- statsArrayComp[, presence2, ]
      # dim(statsArrayComp) <- c(dims[1], sum(presence2), dims[3])
      # if (statsList$orderEvents[[i]] == 1) {
      #   position <- which(activeDyad[2] == which(presence2))
      #   if (length(position) == 0) {
      #     stop("Active node ", activeDyad[2], " not present in event ", i)
      #   }
      #   activeDyad[2] <- which(activeDyad[2] == which(presence2))
      # }
    }
    oldTime <- current_time

    # remove potential absent lines and columns from the stats array
    if (updatepresence || (updateopportunities && !isTwoMode)) {
      subset <- presence
      if (updateopportunities && !isTwoMode) subset <- presence & opportunities
      dims <- dim(statsArrayComp)
      statsArrayComp <- statsArrayComp[subset, , ]
      dim(statsArrayComp) <- c(sum(subset), dims[2], dims[3])
      if (statsList$orderEvents[[i]] == 1) {
        position <- which(activeDyad[1] == which(subset))
        if (length(position) == 0) {
          stop(paste("Active node", activeDyad[1], "not present in event", i))
        }
        activeDyad[1] <- which(activeDyad[1] == which(subset))
      }
    }
    if (updatepresence2 || updateopportunities) {
      subset <- presence2 & opportunities
      dims <- dim(statsArrayComp)
      statsArrayComp <- statsArrayComp[, subset, ]
      dim(statsArrayComp) <- c(dims[1], sum(subset), dims[3])
      if (statsList$orderEvents[[i]] == 1) {
        position <- which(activeDyad[2] == which(subset))
        if (length(position) == 0) {
          stop(paste("Active node", activeDyad[2], "not available in event", i))
        }
        activeDyad[2] <- which(activeDyad[2] == which(subset))
      }
    }

    # TEMPORARY: handle the reductions here for now
    # CHANGED SIWEI: reduce the matrix to vector for rate model here in each step seperately
    # CHANGED SIWEI: treat one-mode and two-mode cases seperately
    # handle the reductions in one step outside the iteration loop, to be make
    if (reduceMatrixToVector) {
      if (isTwoMode == FALSE) {
        dims <- dim(statsArrayComp) # statsArrayComp: n_nodes1*n_nodes2*num_statistics matrix
        arr <- apply(statsArrayComp, 3, function(stat) {
          diag(stat) <- 0
          if (verbose && i == 1) cat("Replacing effects statistics by row means\n")
          m <- stat
          stat <- rowMeans(m, na.rm = TRUE) * (dim(m)[1]) / (dim(m)[1] - 1)
          stat
        })
      }
      else {
        dims <- dim(statsArrayComp) # statsArrayComp: n_nodes1*n_nodes2*num_statistics matrix
        arr <- apply(statsArrayComp, 3, function(stat) {
          if (verbose && i == 1) cat("Replacing effects statistics by row means\n")
          m <- stat
          stat <- rowMeans(m, na.rm = TRUE)
          stat
        })
      }
      statsArrayComp <- arr # statsArrayComp: n_nodes*n_effects matrix
    }

    # reduce array to matrices
    if (reduceArrayToMatrix) {
      oldDim <- dim(statsArrayComp)
      statsArrayComp <- matrix(statsArrayComp[activeDyad[1], , ], oldDim[2], oldDim[3])
    }

    # compute loglikelihood, score, information matrix and pmatrix for the current event
    # CHANGED SIWEI: add three arguments (isRightCensored, timespan and allowReflexive) to eventValues function
    isRightCensored <- !isDependent
    eventValues <-
      getEventValues(statsArrayComp, activeDyad, parameters, modelType, isRightCensored,
                     timespan, allowReflexive, isTwoMode)

    # update return list
    if (i == 1) {
      eventLogL <- eventValues$logLikelihood
    } else {
      eventLogL <- c(eventLogL, eventValues$logLikelihood)
    }
    if (returnEventProbabilities) EventProbabilities[[i]] <- eventValues$pMatrix

    logLikelihood <- logLikelihood + eventValues$logLikelihood
    score <- score + eventValues$score
    informationMatrix <- informationMatrix + eventValues$informationMatrix

    # update progress bar
    # if (showProgressBar && !progressEndReached) {
    #   if (i %in% dotEvents) {
    #     pos <- which(i == dotEvents)
    #     n <- length(dotEvents)
    #     cat("\r[", rep(".", pos), rep(" ", n - pos), "]", sep = "")
    #   }
    #   if (i == nEvents) {
    #     cat("\n")
    #     progressEndReached <- T
    #   }
    # }
  }

  returnList <- list(
    logLikelihood = logLikelihood,
    score = score,
    informationMatrix = informationMatrix
  )


  # if (parallelize && require("snowfall", quietly = TRUE)) {
  #   snowfall::sfStop()
  # }

  # if(returnIntervalLogL) returnList$eventLogL <- sapply(eventValues, function(v) v$logLikelihood, simplify = TRUE)
  if (returnIntervalLogL) returnList$eventLogL <- eventLogL
  # if(returnEventProbabilities) returnList$pMatrix <- lapply(eventValues, getElement, "pMatrix")
  if (returnEventProbabilities) returnList$pMatrix <- EventProbabilities

  return(returnList)
}


# Function to calculate the log likelihoods for each tie i<->j
getLikelihoodMM <- function(multinomialProbabilities) {
  symP <- multinomialProbabilities * t(multinomialProbabilities)
  diag(symP) <- 0
  denomiator <- sum(symP) / 2
  return(symP / denomiator)
}
# likelihoods <- symP / denominator


# Calculate the information matrix for multinomial models based on the schema in Cramer (2003), page 111 and others
# The derivatives are of the log likelihood and need to be transformed by multiplying with P
getMultinomialInformationMatrix <- function(likelihoods, derivatives) {
  nParams <- dim(derivatives)[3]
  nActors <- dim(derivatives)[1]
  matrixSize <- nActors * nActors

  # take upper triangle of the likelihoods matrix so that we do not count probabilities twice
  likelihoodsTriangle <- likelihoods * upper.tri(likelihoods)

  # In the Hessian formula H_{ijh} (7.11), p.111, we include the log likelihood derivatives and
  #  multiply each of the two with P_{is}

  # Multiply each pair of slices of the log likelihood with each other times the likelihood
  indexes <- cbind(seq.int(nParams), rep(seq.int(nParams), each = nParams))

  values <- apply(
    indexes, 1,
    function(ind) sum(derivatives[, , ind[1]] * derivatives[, , ind[2]] * likelihoodsTriangle)
  )
  informationMatrix <- matrix(values, nParams, nParams, byrow = FALSE)

  informationMatrix
}


getMultinomialInformationMatrixM <- function(eventProbabilities, firstDerivatives) {
  nParams <- dim(firstDerivatives)[2]
  nActors <- dim(firstDerivatives)[1]

  # all indexes: 1-1, 1-2, ..., nParams-nParams
  indexes <- expand.grid(seq.int(nParams), seq.int(nParams))

  temp <- apply(indexes, 1, function(ind) firstDerivatives[, ind[1]] * firstDerivatives[, ind[2]] * eventProbabilities)
  if (!is.null(dim(temp))) {
    values <- colSums(temp)
  } else {
    # in case that temp is a scalar
    values <- temp
  }
  information <- matrix(values, nParams, nParams)
}


# Function to calculate a matrix of i->j multinomial choice probabilities (non-logged)
# for one term of the
getMultinomialProbabilities <- function(statsArray, activeDyad, parameters,
                                        actorNested = TRUE, allowReflexive = TRUE, isTwoMode = FALSE) {

  # allow this for a two- OR a three-dimensional array provided as input, to be make
  nDimensions <- length(dim(statsArray))
  if (!(nDimensions %in% c(2, 3)))
    stop("StatsArray in getMultinomialProbabilities has to be two- or three-dimensional.")

  nParams <- dim(statsArray)[nDimensions]
  nActors1 <- dim(statsArray)[1]
  nActors2 <- dim(statsArray)[2]
  if (nDimensions == 3) {
    matrixSize <- nActors1 * nActors2
    # multiply parameters with the statistics; slice by slice
    # the cube has to be transposed for third-dimension-wise multyplication
    weightedStatsArray <- statsArray * rep(parameters, each = matrixSize)
    # get utility = exp( value of objective function )
    utility <- exp(apply(weightedStatsArray, c(1, 2), sum))
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
    if (!allowReflexive & !isTwoMode) utility[activeDyad[1]] <- 0
    denominators <- sum(utility)
  }
  utility / denominators
}


# Utility function to modify the statistics list
modifyStatisticsList <- function(statsList, modelType,
                                 reduceMatrixToVector = FALSE,
                                 reduceArrayToMatrix = FALSE,
                                 excludeParameters = NULL,
                                 addInterceptEffect = FALSE) {

  # exclude effect statistics
  if (!is.null(excludeParameters)) {
    unknownIndexes <- setdiff(excludeParameters, seq.int(dim(statsList$initialStats)[3]))
    if (length(unknownIndexes) > 0) {
      stop("Unknown parameter indexes in 'excludeIndexes': ", paste(unknownIndexes, collapse = " "))
    }
    statsList$initialStats <- statsList$initialStats[, , -excludeParameters]
  }

  # reduce for dynam-m RATE model
  if (modelType == "DyNAM-M-Rate") {
    statsList <- reduceStatisticsList(statsList,
      # dropZeroTimespans = TRUE,
      reduceMatrixToVector = TRUE,
      addInterceptEffect = addInterceptEffect
    )
  }

  if (modelType == "DyNAM-M-Rate-ordered") {
    statsList <- reduceStatisticsList(statsList,
      reduceMatrixToVector = TRUE,
      dropRightCensored = TRUE
    )
  }


  # reduce for REM (continuous-time)
  if (modelType == "REM") {
    statsList <- reduceStatisticsList(statsList,
      addInterceptEffect = addInterceptEffect
    )
  }

  # reduce for dynam-m CHOICE model
  if (modelType == "DyNAM-M") {
    # drop the diagonal elements??
    statsList <- reduceStatisticsList(statsList,
      reduceArrayToMatrix = TRUE,
      dropRightCensored = TRUE
    )
  }

  # reduce for ORDERED REM
  if (modelType == "REM-ordered") {
    statsList <- reduceStatisticsList(statsList,
      dropRightCensored = TRUE
    )
  }

  return(statsList)
}


reduceStatisticsList <- function(statsList,
                                 addInterceptEffect = FALSE,
                                 dropRightCensored = FALSE,
                                 reduceMatrixToVector = FALSE,
                                 reduceArrayToMatrix = FALSE,
                                 dropZeroTimespans = FALSE) {
  if (dropRightCensored) {
    # reorder the intervals with only dependent events
    idep <- 1
    irc <- 1
    newintervals <- list()
    for (i in seq_along(statsList$orderEvents)) {
      if (statsList$orderEvents[[i]] == 1) {
        newintervals <- append(newintervals, statsList$intervals[[idep]])
        idep <- idep + 1
      }
      if (statsList$orderEvents[[i]] > 1) {
        newintervals[[length(newintervals)]] <- newintervals[[length(newintervals)]] +
          statsList$rightCensoredIntervals[[irc]]
        irc <- irc + 1
      }
    }
    statsList$orderEvents <- rep(1, length(statsList$intervals))

    # information on right censoring and intervals is no longer needed
    statsList$rightCensoredStatsChange <- list()
    statsList$rightCensoredIntervals <- list()
  }

  # This part should be uncommented once we are sure about how to use
  # these reductions through the whole estimation. For now we reduce at each step

  # # reduce statistics matrix to a vector
  # if(reduceMatrixToVector) {
  #   apply(statsList$initialStats, c(3, 1), function(row) {
  #     if(min(row, na.rm = TRUE) != max(row, na.rm = TRUE))
  #       stop("Rate variable varies within event senders.")
  #   })
  # generates a matrix from an array
  #   statsList$initialStats <- apply(statsList$initialStats, 3, rowMeans, na.rm = TRUE)
  # }
  #
  # # reduce array to matrices
  # if(reduceArrayToMatrix) {
  #   oldDim <- dim(statsList$initialStats)
  #   statsList$initialStats <- matrix(statsList$initialStats[1, , ], oldDim[2], oldDim[3])
  # }

  # add a rate intercept that is 1 for everyone (dummy for \theta_0)
  # The format may be a vector or a matrix (see above)
  if (addInterceptEffect) {
    dimensions <- dim(statsList$initialStats)
    # data is in matrix format
    if (length(dimensions) == 3) {
      oldValues <- statsList$initialStats
      newValues <- matrix(1, dimensions[1], dimensions[2])
      statsList$initialStats <- array(c(newValues, oldValues), dim = dimensions + c(0, 0, 1))
    }
    # data is in vector format
    if (length(dimensions) == 2) {
      statsList$initialStats <- cbind(1, statsList$initialStats)
    }
  }

  # drop statistics with a time span of zero
  if (dropZeroTimespans) {
    hasZeroTime <- which(statsList$intervals == 0)
    statsList$dependentStatsChange <- statsList$dependentStatsChange[-hasZeroTime]
    statsList$intervals <- statsList$intervals[-hasZeroTime]
    hasZeroTime <- which(statsList$rightCensoredIntervals == 0)
    statsList$rightCensoredStatsChange <- statsList$rightCensoredStatsChange[-hasZeroTime]
    statsList$rightCensoredIntervals <- statsList$rightCensoredIntervals[-hasZeroTime]
  }

  return(statsList)
}
