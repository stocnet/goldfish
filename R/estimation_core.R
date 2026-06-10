##################### ##
#
# Goldfish package
# Internal estimation routine
#
#################### ###

# Estimation
estimate_int <- function(
  statsList,
  nodes,
  nodes2,
  defaultNetworkName,
  modelType = c(
    "DyNAM-MM",
    "DyNAM-M",
    "REM-ordered",
    "DyNAM-M-Rate",
    "REM",
    "DyNAM-M-Rate-ordered"
  ),
  initialParameters = NULL,
  fixedParameters = NULL,
  excludeParameters = NULL,
  initialDamping = 1,
  maxIterations = 20,
  dampingIncreaseFactor = 2,
  dampingDecreaseFactor = 3,
  score_tol = 1e-6,
  step_tol = 1e-8,
  # additional return objects
  returnEventProbabilities = FALSE,
  # additional parameter for DyNAM-MM
  allowReflexive = FALSE,
  is_two_mode = FALSE,
  # additional parameter for DyNAM-M-Rate
  hasIntercept = FALSE,
  returnIntervalLogL = FALSE,
  parallelize = FALSE,
  cpus = 6,
  verbose = FALSE,
  progress = FALSE,
  impute = TRUE,
  ignoreRepParameter,
  # restrictions of opportunity sets
  opportunitiesList = NULL,
  prepEnvir = new.env()
) {
  ## SET VARIABLES

  minDampingFactor <- initialDamping
  is_rate_model <- modelType %in% c("DyNAM-M-Rate", "DyNAM-M-Rate-ordered")
  nParams <- (if (is_rate_model) {
    ncol(statsList$initialStats)
  } else {
    dim(statsList$initialStats)[3]
  }) -
    length(excludeParameters) +
    hasIntercept
  parameters <- initialParameters
  if (is.null(initialParameters)) {
    parameters <- numeric(nParams)
  }
  # deal with fixedParameters
  idUnfixedCompnents <- seq_len(nParams)
  idFixedCompnents <- NULL
  likelihoodOnly <- FALSE
  if (!is.null(fixedParameters)) {
    if (length(fixedParameters) != nParams) {
      stop(
        "The length of fixedParameters is inconsistent with",
        "the number of the parameters.",
        "\n\tLength ",
        dQuote("fixedParameters"),
        " vector:",
        length(fixedParameters),
        "\n\tNumber of parameters:",
        nParams,
        call. = FALSE
      )
    }

    if (all(!is.na(fixedParameters))) {
      likelihoodOnly <- TRUE
    }
    parameters[!is.na(fixedParameters)] <-
      fixedParameters[!is.na(fixedParameters)]
    idUnfixedCompnents <- which(is.na(fixedParameters))
    idFixedCompnents <- which(!is.na(fixedParameters))
  }

  modelType <- match.arg(modelType)

  ## PARAMETER CHECKS

  if (length(parameters) != nParams) {
    stop(
      " Wrong number of initial parameters passed to function.",
      "\n\tLength ",
      dQuote("parameters"),
      " vector:",
      length(parameters),
      "\n\tNumber of parameters:",
      nParams,
      call. = FALSE
    )
  }

  if (!(length(minDampingFactor) %in% c(1, nParams))) {
    stop(
      "minDampingFactor has wrong length:",
      "\n\tLength ",
      dQuote("minDampingFactor"),
      " vector:",
      length(minDampingFactor),
      "\n\tNumber of parameters:",
      nParams,
      "\nIt should be length 1 or same as number of parameters.",
      call. = FALSE
    )
  }

  if (dampingIncreaseFactor < 1 || dampingDecreaseFactor < 1) {
    stop(
      "Damping increase / decrease factors cannot be smaller than one.",
      call. = FALSE
    )
  }

  ## REDUCE STATISTICS LIST

  if (verbose) {
    cat("Reducing data\n")
  }

  reduceArrayToMatrix <- modelType == "DyNAM-M"

  statsList <- modifyStatisticsList(
    statsList = statsList,
    modelType = modelType,
    reduceArrayToMatrix = reduceArrayToMatrix,
    excludeParameters = excludeParameters,
    addInterceptEffect = hasIntercept
  )

  ## GET COMPOSITION CHANGES
  hasCompChange1 <- length(statsList$active_mode1_changes) > 0
  hasCompChange2 <- length(statsList$active_mode2_changes) > 0 &&
    !is_rate_model

  compChange1 <- if (hasCompChange1) {
    data.frame(
      time = vapply(statsList$active_mode1_changes, `[[`, double(1), "time"),
      node = vapply(statsList$active_mode1_changes, `[[`, integer(1), "node"),
      replace = vapply(
        statsList$active_mode1_changes,
        `[[`,
        logical(1),
        "replace"
      )
    )
  } else {
    NULL
  }
  compChange2 <- if (hasCompChange2) {
    data.frame(
      time = vapply(statsList$active_mode2_changes, `[[`, double(1), "time"),
      node = vapply(statsList$active_mode2_changes, `[[`, integer(1), "node"),
      replace = vapply(
        statsList$active_mode2_changes,
        `[[`,
        logical(1),
        "replace"
      )
    )
  } else {
    NULL
  }

  presence <- statsList$active_mode1_init
  presence2 <- statsList$active_mode2_init

  nEvents <- length(statsList$is_dependent)

  ## ADD INTERCEPT
  # CHANGED MARION
  # replace first parameter with an initial estimate of the intercept
  if (
    modelType %in%
      c("REM", "DyNAM-M-Rate") &&
      hasIntercept &&
      is.null(initialParameters) &&
      (is.null(fixedParameters) || is.na(fixedParameters[1]))
  ) {
    totalTime <- sum(statsList$intervals, na.rm = TRUE)

    nActors <- sum(presence)

    if (hasCompChange1) {
      time <- statsList$startTime
      previoustime <- -Inf
      nAvgActors <- 0

      for (i in seq_len(nEvents)) {
        time <- time + statsList$intervals[[i]]

        changesAtTime <- compChange1$replace[
          intersect(
            which(compChange1$time > previoustime),
            which(compChange1$time <= time)
          )
        ]

        # add new present actors and substract non-present
        nActors <- nActors + sum(changesAtTime) - sum(!changesAtTime)
        nAvgActors <- nAvgActors + nActors
        previoustime <- time
      }
      nAvgActors <- nAvgActors / nEvents
    } else {
      nAvgActors <- nActors
    }

    # log crude rate event, estimate when not covariates
    initialInterceptEstimate <- log(nEvents / totalTime / nAvgActors)
    parameters[1] <- initialInterceptEstimate
  }
  ## SET VARIABLES BASED ON STATSLIST

  ## ESTIMATION: INITIALIZATION

  if (verbose) {
    cat("Estimating model type: ", modelType)
  }

  iIteration <- 1
  informationMatrix <- matrix(0, nParams, nParams)
  score <- rep(0, nParams)
  logLikelihood <- 0
  isConverged <- FALSE
  returnCode <- 0L
  update <- rep(0, nParams)
  isInitialEstimation <- TRUE
  logLikelihood.old <- -Inf
  parameters.old <- initialParameters
  score.old <- NULL
  informationMatrix.old <- NULL

  # if (parallelize && require("snowfall", quietly = TRUE)) {
  #   snowfall::sfStop()
  #   snowfall::sfInit(parallel = TRUE, cpus = cpus)
  #   snowfall::sfExport(
  #   "getMultinomialProbabilities", "getLikelihoodMM", "getFirstDerivativeMM",
  #   "getMultinomialInformationMatrix", namespace = "goldfish")
  # }

  ## ESTIMATION: ITERATIONS
  while (TRUE) {
    # MARION to be make: update this function for the new stat list
    # calculate logL, score and information; pass parallel computing parameters
    res <- getIterationStepState(
      statsList = statsList,
      nodes = nodes,
      nodes2 = nodes2,
      defaultNetworkName = defaultNetworkName,
      updatepresence = hasCompChange1,
      presence = presence,
      compChange1 = compChange1,
      updatepresence2 = hasCompChange2,
      presence2 = presence2,
      compChange2 = compChange2,
      hasIntercept = hasIntercept,
      parameters = parameters,
      parallelize = parallelize,
      cpus = cpus,
      modelType = modelType, # model type case distinction
      returnIntervalLogL = returnIntervalLogL,
      returnEventProbabilities = returnEventProbabilities,
      allowReflexive = allowReflexive,
      is_two_mode = is_two_mode,
      reduceArrayToMatrix = reduceArrayToMatrix,
      ignoreRepParameter = ignoreRepParameter,
      impute = impute,
      verbose = verbose,
      opportunitiesList = opportunitiesList,
      prepEnvir = prepEnvir
    )

    logLikelihood <- res[[1]]
    score <- res[[2]]
    informationMatrix <- res[[3]]
    if (returnIntervalLogL) {
      intervalLogL <- res[[4]]
    }
    # add a possibility to return the whole probability matrix: to be make
    if (returnEventProbabilities) {
      eventProbabilities <- if (is.null(res$pMatrix)) {
        paste("not implemented for model type", modelType)
      } else {
        res$pMatrix
      }
    }

    if (
      isInitialEstimation &&
        any(is.na(unlist(res))) &&
        !all(parameters[-1] == 0)
    ) {
      # # Check
      stop(
        "Estimation not possible with initial parameters.",
        " Try using zeros instead.",
        call. = FALSE
      )
    }

    # If we only want the likelihood break here
    if (likelihoodOnly) {
      inverseInformationUnfixed <- matrix(0, nParams, nParams)
      score <- rep(0, nParams)
      isConverged <- TRUE
      returnCode <- 1L
      break
    }

    # we don't consider the fixed components of the score.
    # It's for the fixing parameter feature. \
    score[idFixedCompnents] <- 0

    if (verbose) {
      cat(
        "\n\nLikelihood:",
        logLikelihood,
        "in iteration",
        iIteration,
        "\nParameters:",
        toString(parameters),
        "\nScore:",
        toString(score)
      )
      # print(informationMatrix)
    }

    stepAccepted <- !any(is.na(unlist(res))) &&
      is.finite(logLikelihood) &&
      logLikelihood > logLikelihood.old
    if (!stepAccepted) {
      if (verbose) {
        cat(
          "\nNo improvement in estimation.",
          " Resetting values and adjusting damping."
        )
      }
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
      minDampingFactor <- max(
        1,
        minDampingFactor / ifelse(isInitialEstimation, 1, dampingDecreaseFactor)
      )
    }

    # end of initial estimation
    isInitialEstimation <- FALSE

    # Calculate the UPDATE distance taking into account the DAMPING
    dampingFactor <- minDampingFactor

    # INVERT information matrix
    # We only invert the unfixed part of the parameter.
    # The fixed components of the score have already be set to be 0.
    # It's for the fixing parameter feature.
    informationMatrixUnfixed <-
      informationMatrix[idUnfixedCompnents, idUnfixedCompnents]
    inverseInformationUnfixed <- try(
      solve(informationMatrixUnfixed),
      silent = TRUE
    )
    if (inherits(inverseInformationUnfixed, "try-error")) {
      stop(
        "Matrix cannot be inverted;",
        " probably due to collinearity between parameters."
      )
    }

    update <- rep(0, nParams)
    update[idUnfixedCompnents] <-
      (inverseInformationUnfixed %*% score[idUnfixedCompnents]) / dampingFactor

    if (verbose) {
      cat(
        "\nUpdate: ",
        toString(update),
        "\nDamping factor: ",
        toString(dampingFactor)
      )
    }

    # check for stop criteria
    convergence <- check_convergence(
      score = score,
      log_likelihood = logLikelihood,
      update = update,
      step_accepted = stepAccepted,
      score_tol = score_tol,
      step_tol = step_tol
    )
    if (!verbose && progress) {
      cat(
        "\rMax score: ",
        round(convergence$score_rel_norm, round(-logb(score_tol, 10)) + 1),
        " Max step: ",
        round(max(abs(update)), round(-logb(step_tol, 10)) + 1),
        " (",
        iIteration,
        ").        "
      )
    }
    if (convergence$converged) {
      isConverged <- TRUE
      returnCode <- convergence$return_code
      if (progress) {
        if (returnCode == 1L) {
          cat("\nReturn code 1: gradient close to zero.\n")
        } else {
          cat("\nReturn code 2: step size close to zero (damped).\n")
        }
      }
      break
    }
    if (iIteration > maxIterations) {
      if (progress) {
        cat(
          "\nStopping as maximum of ",
          maxIterations,
          " iterations have been reached. No convergence.\n"
        )
      }
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
    convergence = list(
      isConverged = isConverged,
      returnCode = returnCode,
      maxAbsScore = max(abs(score)),
      maxAbsUpdate = max(abs(update))
    ),
    nIterations = iIteration,
    nEvents = nEvents
  )
  if (returnIntervalLogL) {
    estimationResult$intervalLogL <- intervalLogL
  }
  if (returnEventProbabilities) {
    estimationResult$eventProbabilities <- eventProbabilities
  }
  attr(estimationResult, "class") <- "result.goldfish"
  estimationResult
}

#' Check Newton-Raphson stopping criteria
#'
#' Evaluates the two stopping criteria on the state of the current iteration:
#' the likelihood-scaled relative score criterion and the damped step size
#' criterion. The score criterion is only evaluated when the iteration step
#' was accepted (`step_accepted = TRUE`), i.e., the trial step improved the
#' log-likelihood and produced finite values. Rejected steps (including
#' overshoots where the trial log-likelihood is `-Inf`) cannot trigger score
#' convergence; the step size criterion remains active as the stalled exit
#' when repeated damping drives the update towards zero.
#'
#' @param score numeric vector, score of the accepted state (post reset when
#'   the trial step was rejected).
#' @param log_likelihood numeric, log-likelihood of the accepted state.
#' @param update numeric vector, damped Newton update for the next iteration.
#' @param step_accepted logical, whether the trial step of this iteration
#'   improved the log-likelihood with finite values.
#' @param score_tol numeric, tolerance for the relative score criterion.
#' @param step_tol numeric, tolerance for the step size criterion.
#'
#' @return a list with components `converged` (logical), `return_code`
#'   (0L not converged, 1L gradient close to zero, 2L step size close to
#'   zero), and `score_rel_norm` (numeric, the likelihood-scaled relative
#'   score).
#' @noRd
check_convergence <- function(
  score,
  log_likelihood,
  update,
  step_accepted,
  score_tol,
  step_tol
) {
  score_rel_norm <- max(abs(score)) / max(1, abs(log_likelihood))
  score_converged <- isTRUE(
    step_accepted &&
      is.finite(log_likelihood) &&
      score_rel_norm <= score_tol
  )
  step_converged <- isTRUE(max(abs(update)) <= step_tol)
  list(
    converged = score_converged || step_converged,
    return_code = if (score_converged) 1L else if (step_converged) 2L else 0L,
    score_rel_norm = score_rel_norm
  )
}


# Function to return log likelihood, score, information matrix,
# and p vector for each event
# CHANGED SIWEI: add three parameters: isRightCensored, timespan and
#  allowReflexive
getEventValues <- function(
  statsArray,
  activeDyad,
  parameters,
  modelType,
  isRightCensored,
  timespan,
  allowReflexive,
  is_two_mode
) {
  if (modelType == "DyNAM-MM") {
    multinomialProbabilities <-
      getMultinomialProbabilities(
        statsArray,
        activeDyad,
        parameters,
        allowReflexive = allowReflexive
      )
    eventLikelihoods <- getLikelihoodMM(multinomialProbabilities)
    logLikelihood <- log(eventLikelihoods[activeDyad[1], activeDyad[2]])
    firstDerivatives <- getFirstDerivativeMM(
      statsArray,
      eventLikelihoods,
      multinomialProbabilities
    )
    score <- firstDerivatives[activeDyad[1], activeDyad[2], ]
    informationMatrix <- getMultinomialInformationMatrix(
      eventLikelihoods,
      firstDerivatives
    )
    pMatrix <- eventLikelihoods
  }

  if (modelType == "DyNAM-M") {
    eventProbabilities <-
      getMultinomialProbabilities(
        statsArray,
        activeDyad,
        parameters,
        actorNested = TRUE,
        allowReflexive = allowReflexive,
        is_two_mode = is_two_mode
      )
    logLikelihood <- log(eventProbabilities[activeDyad[2]])
    firstDerivatives <- getFirstDerivativeM(statsArray, eventProbabilities)
    score <- firstDerivatives[activeDyad[2], ]
    informationMatrix <- getMultinomialInformationMatrixM(
      eventProbabilities,
      firstDerivatives
    )
    pMatrix <- eventProbabilities
  }

  if (modelType == "REM-ordered") {
    eventProbabilities <-
      getMultinomialProbabilities(
        statsArray,
        activeDyad,
        parameters,
        actorNested = FALSE,
        allowReflexive = FALSE
      )
    logLikelihood <- log(eventProbabilities[activeDyad[1], activeDyad[2]])
    firstDerivatives <- getFirstDerivativeREM(statsArray, eventProbabilities)
    score <- firstDerivatives[activeDyad[1], activeDyad[2], ]
    informationMatrix <- getInformationMatrixREM(
      eventProbabilities,
      firstDerivatives
    )
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
    logLikelihood <- sum(statsMatrix[activeActor, ] * parameters) -
      log(sum(rates))
    # deviation from actual statistics
    deviations <- t(t(statsMatrix) - expectedStatistics)
    score <- deviations[activeActor, ]
    # Fisher information matrix
    informationMatrix <- matrix(
      rowSums(t(
        t(matrix(
          apply(deviations, 1, function(x) outer(x, x)),
          ncol = length(eventProbabilities)
        )) *
          eventProbabilities
      )),
      length(parameters),
      length(parameters)
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
    if (is.na(timespan)) {
      timespan <- 0
    }

    # Don't consider self-connecting edge when both allowReflexive
    # and  is_two_mode are false
    dontConsiderSelfConnecting <- (modelType == "REM") &&
      !allowReflexive &&
      !is_two_mode
    if (dontConsiderSelfConnecting) {
      idEdgeNotConsidered <- (seq_len(dimMatrix[1]) - 1) *
        dimMatrix[1] +
        seq_len(dimMatrix[1])
    } else {
      idEdgeNotConsidered <- numeric(0)
    }
    # vector of rates
    objectiveFunctions <- (statsArray %*% parameters)[, 1] # a vector
    objectiveFunctionOfSender <- objectiveFunctions[activeActor]
    statsOfSender <- statsArray[activeActor, ]
    rates <- exp(objectiveFunctions)
    rates[idEdgeNotConsidered] <- 0
    ratesSum <- sum(rates)
    # k vector with all rho * s_k summed over all actors i
    ratesStats <- rates * statsArray
    ratesStatsSum <- colSums(rates * statsArray)

    ratesStatsStatsSum <- colSums(
      t(
        apply(statsArray, 1, function(x) outer(x, x))
      ) *
        rates
    )
    if (length(parameters) == 1 && modelType == "DyNAM-M-Rate") {
      v <- as.vector(statsArray)
      sum <- 0
      for (i in seq_along(v)) {
        sum <- sum + v[i] * v[i] * rates[i]
      }
      ratesStatsStatsSum <- sum
    }
    dim(ratesStatsStatsSum) <- rep(length(parameters), 2)

    logL <- -timespan *
      ratesSum +
      if (!isRightCensored) objectiveFunctionOfSender else 0

    score <- -timespan *
      ratesStatsSum +
      if (!isRightCensored) statsOfSender else 0

    hessian <- -timespan * ratesStatsStatsSum
    pVector <- objectiveFunctions + (-timespan * ratesSum)
    if (modelType == "REM") {
      dim(pVector) <- c(dimMatrix[1], dimMatrix[2])
    }

    # cat("\ntimespan:", timespan)
    # cat("\nderivative:")
    # print(ratesStatsSum)
    # cat("\nfisher:")
    # print(ratesStatsStatsSum)
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
# 2) calculate the ratios of
#     multP_{ij}' / multP_{ij} = s_{ij1} - \sum_{h} multP_{ih} s_{ih1}
#     for the derivative of the first parameter (deviation from expectation)
#     in the multinomial part
# 3) calculate the constant that is substracted from each first derivative
# 4) calculate the derivative based on the values above (see paper)
getFirstDerivativeMM <- function(
  statsArray,
  likelihoods,
  multinomialProbabilities
) {
  nParams <- dim(statsArray)[3]
  nActors <- dim(statsArray)[1]
  matrixSize <- nActors * nActors
  # 2)
  # multiply each "parameter slice" (third dimension)
  # with the multinomial probability matrix
  weightedValues <- statsArray * rep(multinomialProbabilities, nParams)
  expectedStatistics <- apply(weightedValues, 3, rowSums)
  # deviation from expected statistic
  deviationFromExpectation <- statsArray -
    c(apply(expectedStatistics, 2, rep, nActors))

  # 3)
  # symmetrize the deviations from expectations
  symDeviations <- deviationFromExpectation +
    aperm(deviationFromExpectation, c(2, 1, 3))
  # the likelihoods have to divided by 2
  # as the matrix sum is 2 (it includes all likelihoods twice)
  constantWeightedDeviations <-
    apply(symDeviations * c(likelihoods / 2), 3, sum)

  # 4)
  derivatives <- symDeviations -
    rep(constantWeightedDeviations, each = matrixSize)

  derivatives
}

# calculate the score contribution of one event of the M model
# to the log(!) likelihood
# The scores are the differences between expected and observed statistics
getFirstDerivativeREM <- function(statsArray, eventProbabilities) {
  # nActors <- dim(statsArray)[1]
  # nParams <- dim(statsArray)[3]

  expectedStatistics <- apply(
    apply(statsArray, 3, function(m) m * eventProbabilities),
    2,
    sum
  )
  firstDerivatives <- sweep(statsArray, MARGIN = 3, expectedStatistics, "-")

  firstDerivatives
}


getInformationMatrixREM <- function(eventProbabilities, firstDerivatives) {
  nParams <- dim(firstDerivatives)[3]
  # nActors <- dim(firstDerivatives)[1]

  # all indexes: 1-1, 1-2, ..., nParams-nParams
  indexes <- expand.grid(seq_len(nParams), seq_len(nParams))
  # indexes <- indexes[indexes[, 1] <= indexes[, 2], ]

  values <- colSums(apply(
    indexes,
    1,
    \(ind) {
      firstDerivatives[,, ind[1]] *
        firstDerivatives[,, ind[2]] *
        eventProbabilities
    }
  ))
  information <- matrix(values, nParams, nParams)
  # symmetrize
  # information[lower.tri(information)] <- information[upper.tri(information)]

  information
}


# Function to return log likelihood, score and information matrix
# for all events, given a set of parameters
# for the MM, M, REM, and M-Rate function, and REM-ordered
getIterationStepState <- function(
  statsList,
  nodes,
  nodes2,
  defaultNetworkName,
  updatepresence,
  presence,
  compChange1,
  updatepresence2,
  presence2,
  compChange2,
  hasIntercept,
  parameters,
  modelType,
  parallelize = FALSE,
  cpus = 4,
  returnIntervalLogL = FALSE,
  returnEventProbabilities = FALSE,
  allowReflexive = TRUE,
  is_two_mode = FALSE,
  reduceArrayToMatrix = FALSE,
  ignoreRepParameter = NULL,
  impute = TRUE,
  verbose = FALSE,
  opportunitiesList = NULL,
  prepEnvir = new.env()
) {
  nEvents <- length(statsList$is_dependent)
  is_rate <- length(dim(statsList$initialStats)) == 2L
  nParams <- if (is_rate) {
    ncol(statsList$initialStats)
  } else {
    dim(statsList$initialStats)[3]
  }

  # iterate over all events
  informationMatrix <- matrix(0, nParams, nParams) # n_effects * n_effects
  score <- rep(0, nParams)
  logLikelihood <- 0

  if (returnEventProbabilities) {
    EventProbabilities <- vector(mode = "list", length = nEvents)
  }
  if (returnIntervalLogL) {
    eventLogL <- numeric(nEvents)
  }

  # check for parallelization
  # if (parallelize && require("snowfall", quietly = TRUE)) {
  #   snowfall::sfStop()
  #   snowfall::sfInit(parallel = TRUE, cpus = cpus)
  #   snowfall::sfExport("getEventValues", namespace = "goldfish")
  # }

  # initialize progressbar output
  # showProgressBar <- FALSE
  # progressEndReached <- FALSE
  # if(!silent) {
  #  showProgressBar <- T
  #  dotEvents <- seq(1, nEvents, round(nEvents/min(nEvents, 50)))
  # }

  # CHANGED Marion: fill in the loop! stats array needs to be computed
  # also changes for dependent and rc events!
  statsArray <- statsList$initialStats
  time <- statsList$startTime

  hasIgnoreRep <- any(ignoreRepParameter)
  if (hasIgnoreRep) {
    net <- get(defaultNetworkName, envir = prepEnvir) # add prepEnvir
    ignoreRepIds <- which(ignoreRepParameter) + hasIntercept
    # with intercept, the first effect is the intercept without
    # ignoreRep option
    startTime <- -Inf
  }

  # opportunities list initialization
  opportunities <- rep(TRUE, nrow(nodes2))
  updateopportunities <- !is.null(opportunitiesList) &&
    !modelType %in% c("DyNAM-M-Rate", "DyNAM-M-Rate-ordered")

  updFun <- function(stat, change, is_rate_stat = FALSE) {
    if (!is.null(change)) {
      if (is_rate_stat) {
        stat[change[, "node1"]] <- change[, "replace"]
      } else {
        stat[cbind(change[, "node1"], change[, "node2"])] <- change[, "replace"]
      }
    }
    return(stat)
  }

  # IMPUTE missing statistics with current mean
  imputeFun <- function(m) {
    m[is.na(m)] <- mean(m, na.rm = TRUE)
    m
  }
  oldTime <- -Inf

  for (i in seq_len(nEvents)) {
    isDependent <- statsList$is_dependent[[i]] == 1L
    pars2update <- !vapply(statsList$stats_change[[i]], is.null, logical(1))
    for (j in which(pars2update)) {
      if (is_rate) {
        statsArray[, j + hasIntercept] <-
          updFun(
            statsArray[, j + hasIntercept],
            statsList$stats_change[[i]][[j]],
            is_rate_stat = TRUE
          )
      } else {
        statsArray[,, j + hasIntercept] <-
          updFun(
            statsArray[,, j + hasIntercept],
            statsList$stats_change[[i]][[j]]
          )
      }
    }
    if (hasIntercept) {
      time <- time + statsList$intervals[[i]]
      timespan <- statsList$intervals[[i]]
    }
    if (isDependent) {
      activeDyad <- c(
        statsList$event_sender[[i]],
        statsList$event_receiver[[i]]
      )
    } else {
      activeDyad <- NULL
    }

    # IMPUTE missing statistics with current mean
    if (impute && anyNA(statsArray)) {
      if (is_rate) {
        for (j in which(apply(statsArray, 2, anyNA))) {
          statsArray[, j] <- imputeFun(statsArray[, j])
        }
      } else {
        for (j in which(apply(statsArray, 3, anyNA))) {
          statsArray[,, j] <- imputeFun(statsArray[,, j])
        }
      }
    }

    statsArrayComp <- statsArray
    # Handle the ignoreRep option
    if (hasIgnoreRep) {
      mat <- as.matrix(
        net,
        time = statsList$event_time[[i]],
        startTime = startTime
      )
      ones <- which(mat > 0, arr.ind = TRUE)
      statsArrayComp[cbind(
        ones[, 1],
        ones[, 2],
        rep(ignoreRepIds, each = length(ignoreRepIds) * nrow(ones))
      )] <- 0
      # CHANGED SIWEI
      startTime <- statsList$event_time[[i]]
      net[seq_len(dim(net)[1]), seq_len(dim(net)[2])] <- mat
    }

    # update opportunity set
    if (updateopportunities) {
      opportunities <- seq_len(nrow(nodes2)) %in% opportunitiesList[[i]]
    }

    # update composition
    # CHANGED SIWEI: fixed errors for composition change update
    # CHANGED MARION: fixed wrong initialization of compositions,
    #   removed next lines

    current_time <- statsList$event_time[[i]]
    if (updatepresence) {
      update <-
        compChange1[
          compChange1$time <= current_time &
            compChange1$time > oldTime,
        ]
      presence[update$node] <- update$replace
    }

    if (updatepresence2) {
      update2 <-
        compChange2[
          compChange2$time <= current_time &
            compChange2$time > oldTime,
        ]
      presence2[update2$node] <- update2$replace
    }
    oldTime <- current_time

    # patch to avoid collision with dropping absent people
    if (!is_two_mode && !is_rate) {
      for (parmPos in seq_len(dim(statsArrayComp)[3])) {
        diag(statsArrayComp[,, parmPos]) <- 0
      }
    }

    # remove potential absent lines and columns from the stats array
    if (updatepresence) {
      # || (updateopportunities && !is_two_mode)
      keepIn <- presence
      # if (updateopportunities && !is_two_mode)
      #   keepIn <- presence & opportunities
      statsArrayComp <- if (is_rate) {
        statsArrayComp[keepIn, , drop = FALSE]
      } else {
        statsArrayComp[keepIn, , , drop = FALSE]
      }
      if (isDependent) {
        position <- which(activeDyad[1] == which(keepIn))
        if (length(position) == 0) {
          stop(
            "Active node ",
            activeDyad[1],
            " not present in event ",
            i,
            call. = FALSE
          )
        }

        posSender <- activeDyad[1]
        activeDyad[1] <- position
      }
    } else {
      posSender <- activeDyad[1]
    }
    if ((updatepresence2 || updateopportunities)) {
      keepIn <- presence2 & opportunities
      # reducing stats array alters the correspondence between row/col
      # it needs to consider the reflexive case to avoid wrong calculation
      # excludes REM and DyNAM-MM
      if (!allowReflexive && grepl("DyNAM-M(-|$)?", modelType)) {
        if (!is_two_mode) {
          keepIn[posSender] <- FALSE
        }
        allowReflexiveCorrected <- TRUE
      } else {
        allowReflexiveCorrected <- FALSE
      }
      statsArrayComp <- statsArrayComp[, keepIn, , drop = FALSE]
      if (isDependent) {
        position <- which(activeDyad[2] == which(keepIn))
        if (length(position) == 0) {
          stop(
            "Active node ",
            activeDyad[2],
            " not available in event ",
            i,
            call. = FALSE
          )
        }

        activeDyad[2] <- position
      }
    } else {
      allowReflexiveCorrected <- allowReflexive
    }

    # reduce array to matrices
    if (reduceArrayToMatrix) {
      oldDim <- dim(statsArrayComp)
      statsArrayComp <- matrix(
        statsArrayComp[activeDyad[1], , ],
        oldDim[2],
        oldDim[3]
      )
    }
    # compute loglikelihood, score, information matrix and
    # pmatrix for the current event
    # CHANGED SIWEI: add three arguments
    #  (isRightCensored, timespan and allowReflexive) to eventValues function
    isRightCensored <- !isDependent
    eventValues <- getEventValues(
      statsArray = statsArrayComp,
      activeDyad = activeDyad,
      parameters = parameters,
      modelType = modelType,
      isRightCensored = isRightCensored,
      timespan = timespan,
      allowReflexive = allowReflexiveCorrected,
      is_two_mode = is_two_mode
    )

    # update return list
    if (returnIntervalLogL) {
      eventLogL[i] <- eventValues$logLikelihood
    }
    if (returnEventProbabilities) {
      EventProbabilities[[i]] <- eventValues$pMatrix
    }

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

  # if(returnIntervalLogL)
  #   returnList$eventLogL <- sapply(
  #     eventValues, function(v) v$logLikelihood, simplify = TRUE)
  if (returnIntervalLogL) {
    returnList$eventLogL <- eventLogL
  }
  # if(returnEventProbabilities)
  #   returnList$pMatrix <- lapply(eventValues, getElement, "pMatrix")
  if (returnEventProbabilities) {
    returnList$pMatrix <- EventProbabilities
  }

  return(returnList)
}

# Function to calculate the log likelihoods for each tie i<->j
getLikelihoodMM <- function(multinomialProbabilities) {
  symP <- multinomialProbabilities * t(multinomialProbabilities)
  diag(symP) <- 0
  denominator <- sum(symP) / 2
  return(symP / denominator)
}
# likelihoods <- symP / denominator

# Calculate the information matrix for multinomial models based on
# the schema in Cramer (2003), page 111 and others
# The derivatives are of the log likelihood and need to be transformed by
#  multiplying with P
getMultinomialInformationMatrix <- function(likelihoods, derivatives) {
  nParams <- dim(derivatives)[3]
  nActors <- dim(derivatives)[1]
  matrixSize <- nActors * nActors

  # take upper triangle of the likelihoods matrix so
  # that we do not count probabilities twice
  likelihoodsTriangle <- likelihoods * upper.tri(likelihoods)

  # In the Hessian formula H_{ijh} (7.11), p.111,
  # we include the log likelihood derivatives and
  #  multiply each of the two with P_{is}

  # Multiply each pair of slices of the log likelihood with
  # each other times the likelihood
  indexes <- cbind(seq_len(nParams), rep(seq_len(nParams), each = nParams))

  values <- apply(
    indexes,
    1,
    \(ind) {
      sum(derivatives[,, ind[1]] * derivatives[,, ind[2]] * likelihoodsTriangle)
    }
  )
  informationMatrix <- matrix(values, nParams, nParams, byrow = FALSE)

  informationMatrix
}


getMultinomialInformationMatrixM <- function(
  eventProbabilities,
  firstDerivatives
) {
  nParams <- dim(firstDerivatives)[2]
  # nActors <- dim(firstDerivatives)[1]

  # all indexes: 1-1, 1-2, ..., nParams-nParams
  indexes <- expand.grid(seq_len(nParams), seq_len(nParams))

  temp <- apply(
    indexes,
    1,
    \(ind) {
      firstDerivatives[, ind[1]] *
        firstDerivatives[, ind[2]] *
        eventProbabilities
    }
  )
  if (!is.null(dim(temp))) {
    values <- colSums(temp)
  } else {
    # in case that temp is a scalar
    values <- temp
  }
  information <- matrix(values, nParams, nParams)
}


# Function to calculate a matrix of i->j multinomial choice probabilities
# (non-logged)
# for one term of the
getMultinomialProbabilities <- function(
  statsArray,
  activeDyad,
  parameters,
  actorNested = TRUE,
  allowReflexive = TRUE,
  is_two_mode = FALSE
) {
  # allow this for a two- OR a three-dimensional array provided as input,
  # to be make
  nDimensions <- length(dim(statsArray))
  if (!(nDimensions %in% c(2, 3))) {
    stop(
      "StatsArray in getMultinomialProbabilities has to be",
      " two- or three-dimensional.",
      call. = FALSE
    )
  }

  # nParams <- dim(statsArray)[nDimensions]
  nActors1 <- dim(statsArray)[1]
  nActors2 <- dim(statsArray)[2]
  if (nDimensions == 3) {
    matrixSize <- nActors1 * nActors2
    # multiply parameters with the statistics; slice by slice
    # the cube has to be transposed for third-dimension-wise multyplication
    weightedStatsArray <- statsArray * rep(parameters, each = matrixSize)
    # get utility = exp( value of objective function )
    utility <- exp(apply(weightedStatsArray, c(1, 2), sum))
    if (!allowReflexive && !is_two_mode) {
      diag(utility) <- 0
    }
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
    if (!allowReflexive && !is_two_mode) {
      utility[activeDyad[1]] <- 0
    }
    denominators <- sum(utility)
  }
  utility / denominators
}


# Utility function to modify the statistics list
modifyStatisticsList <- function(
  statsList,
  modelType,
  reduceMatrixToVector = FALSE,
  reduceArrayToMatrix = FALSE,
  excludeParameters = NULL,
  addInterceptEffect = FALSE
) {
  # exclude effect statistics
  if (!is.null(excludeParameters)) {
    is_rate_stats <- length(dim(statsList$initialStats)) == 2L
    unknownIndexes <- setdiff(
      excludeParameters,
      seq_len(
        if (is_rate_stats) {
          ncol(statsList$initialStats)
        } else {
          dim(statsList$initialStats)[3]
        }
      )
    )
    if (length(unknownIndexes) > 0) {
      stop(
        "Unknown parameter indexes in 'excludeIndexes': ",
        paste(unknownIndexes, collapse = " ")
      )
    }
    statsList$initialStats <- if (is_rate_stats) {
      statsList$initialStats[, -excludeParameters, drop = FALSE]
    } else {
      statsList$initialStats[,, -excludeParameters]
    }
  }

  if (modelType == "DyNAM-M-Rate") {
    statsList <- reduceStatisticsList(
      statsList,
      addInterceptEffect = addInterceptEffect
    )
  }

  if (modelType == "DyNAM-M-Rate-ordered") {
    statsList <- reduceStatisticsList(statsList, dropRightCensored = FALSE)
  }

  # reduce for REM (continuous-time)
  if (modelType == "REM") {
    statsList <- reduceStatisticsList(
      statsList,
      addInterceptEffect = addInterceptEffect
    )
  }

  # reduce for dynam-m CHOICE model
  if (modelType == "DyNAM-M") {
    # drop the diagonal elements??
    statsList <- reduceStatisticsList(
      statsList,
      reduceArrayToMatrix = TRUE,
      dropRightCensored = FALSE
    )
  }

  # reduce for ORDERED REM
  if (modelType == "REM-ordered") {
    statsList <- reduceStatisticsList(statsList, dropRightCensored = FALSE)
  }

  return(statsList)
}

reduceStatisticsList <- function(
  statsList,
  addInterceptEffect = FALSE,
  dropRightCensored = FALSE,
  reduceArrayToMatrix = FALSE,
  dropZeroTimespans = FALSE
) {
  if (dropRightCensored) {
    is_dep <- statsList$is_dependent == 1L
    dep_positions <- which(is_dep)
    new_intervals <- statsList$intervals[is_dep]
    for (rc_pos in which(!is_dep)) {
      prev_dep <- max(dep_positions[dep_positions < rc_pos], 0)
      if (prev_dep > 0) {
        dep_idx <- which(dep_positions == prev_dep)
        new_intervals[dep_idx] <- new_intervals[dep_idx] +
          statsList$intervals[[rc_pos]]
      }
    }
    statsList$stats_change <- statsList$stats_change[is_dep]
    statsList$intervals <- new_intervals
    statsList$is_dependent <- rep(1L, sum(is_dep))
  }

  # This part should be uncommented once we are sure about how to use
  # these reductions through the whole estimation.
  # For now we reduce at each step

  # # reduce statistics matrix to a vector
  # if(reduceMatrixToVector) {
  #   apply(statsList$initialStats, c(3, 1), function(row) {
  #     if(min(row, na.rm = TRUE) != max(row, na.rm = TRUE))
  #       stop("Rate variable varies within event senders.")
  #   })
  # generates a matrix from an array
  #   statsList$initialStats <-
  #   apply(statsList$initialStats, 3, rowMeans, na.rm = TRUE)
  # }
  #
  # # reduce array to matrices
  # if(reduceArrayToMatrix) {
  #   oldDim <- dim(statsList$initialStats)
  #   statsList$initialStats <-
  #   matrix(statsList$initialStats[1, , ], oldDim[2], oldDim[3])
  # }

  # add a rate intercept that is 1 for everyone (dummy for \theta_0)
  # The format may be a vector or a matrix (see above)
  if (addInterceptEffect) {
    dimensions <- dim(statsList$initialStats)
    # data is in matrix format
    if (length(dimensions) == 3) {
      oldValues <- statsList$initialStats
      newValues <- matrix(1, dimensions[1], dimensions[2])
      statsList$initialStats <-
        array(c(newValues, oldValues), dim = dimensions + c(0, 0, 1))
    }
    # data is in vector format
    if (length(dimensions) == 2) {
      statsList$initialStats <- cbind(1, statsList$initialStats)
    }
  }

  if (dropZeroTimespans) {
    hasZeroTime <- which(
      statsList$intervals == 0 & statsList$is_dependent == 1L
    )
    statsList$stats_change <- statsList$stats_change[-hasZeroTime]
    statsList$intervals <- statsList$intervals[-hasZeroTime]
    statsList$is_dependent <- statsList$is_dependent[-hasZeroTime]
  }

  return(statsList)
}
