# Frozen reference implementations of the per-event contribution helpers,
# captured verbatim from R/estimation_core.R at the start of the
# likelihood-computation refactor. The old-vs-new equivalence harness
# (test-likelihood_equivalence.R) replays real per-event inputs through both
# these frozen copies and the live code, asserting per-event agreement of
# logLikelihood / score / informationMatrix / pMatrix within 1e-10.
#
# These functions are the "old" arm and MUST NOT be edited as the live helpers
# are refactored (§2-4): drifting them would defeat the equivalence gate. They
# are deleted only once every live helper has landed and passed (task 8.1).
# Internal helper calls are repointed to the `_ref` copies so the reference is
# a self-contained frozen call tree, independent of the live namespace.

# --- leaf helpers ----

event_contribution_rate_ref <- function(
  statsArray,
  activeDyad,
  parameters,
  isRightCensored,
  timespan,
  allowReflexive,
  is_two_mode,
  isREM,
  riskMask = NULL
) {
  activeActor <- activeDyad[1]
  dimMatrix <- dim(statsArray)
  maskedOut <- if (isREM && !is.null(riskMask)) {
    which(!as.vector(riskMask))
  } else {
    integer(0)
  }
  if (isREM) {
    activeActor <- activeDyad[1] + (activeDyad[2] - 1) * dimMatrix[1]
    statsArray <- apply(statsArray, 3, c)
  }

  parameters <- as.numeric(parameters)

  if (is.na(timespan)) {
    timespan <- 0
  }

  dontConsiderSelfConnecting <- isREM &&
    !allowReflexive &&
    !is_two_mode
  if (dontConsiderSelfConnecting) {
    idEdgeNotConsidered <- (seq_len(dimMatrix[1]) - 1) *
      dimMatrix[1] +
      seq_len(dimMatrix[1])
  } else {
    idEdgeNotConsidered <- numeric(0)
  }
  objectiveFunctions <- (statsArray %*% parameters)[, 1]
  objectiveFunctionOfSender <- objectiveFunctions[activeActor]
  statsOfSender <- statsArray[activeActor, ]
  rates <- exp(objectiveFunctions)
  rates[idEdgeNotConsidered] <- 0
  rates[maskedOut] <- 0
  ratesSum <- sum(rates)
  ratesStats <- rates * statsArray
  ratesStatsSum <- colSums(rates * statsArray)

  ratesStatsStatsSum <- colSums(
    t(
      apply(statsArray, 1, function(x) outer(x, x))
    ) *
      rates
  )
  if (length(parameters) == 1 && !isREM) {
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
  if (isREM) {
    dim(pVector) <- c(dimMatrix[1], dimMatrix[2])
  }

  list(
    logLikelihood = logL,
    score = score,
    informationMatrix = -hessian,
    pMatrix = pVector
  )
}

compute_first_derivative_choice_ref <- function(
  statsArray,
  eventProbabilities
) {
  nParams <- dim(statsArray)[2]
  nActors <- dim(statsArray)[1]

  expectedStatistics <- colSums(statsArray * eventProbabilities)
  firstDerivatives <- sweep(statsArray, MARGIN = 2, expectedStatistics, "-")

  firstDerivatives
}

compute_first_derivative_choice_coord_ref <- function(
  statsArray,
  likelihoods,
  multinomialProbabilities
) {
  nParams <- dim(statsArray)[3]
  nActors <- dim(statsArray)[1]
  matrixSize <- nActors * nActors
  weightedValues <- statsArray * rep(multinomialProbabilities, nParams)
  expectedStatistics <- apply(weightedValues, 3, rowSums)
  deviationFromExpectation <- statsArray -
    c(apply(expectedStatistics, 2, rep, nActors))

  symDeviations <- deviationFromExpectation +
    aperm(deviationFromExpectation, c(2, 1, 3))
  constantWeightedDeviations <-
    apply(symDeviations * c(likelihoods / 2), 3, sum)

  derivatives <- symDeviations -
    rep(constantWeightedDeviations, each = matrixSize)

  derivatives
}

compute_first_derivative_rem_ref <- function(statsArray, eventProbabilities) {
  expectedStatistics <- apply(
    apply(statsArray, 3, function(m) m * eventProbabilities),
    2,
    sum
  )
  firstDerivatives <- sweep(statsArray, MARGIN = 3, expectedStatistics, "-")

  firstDerivatives
}

getInformationMatrixREM_ref <- function(eventProbabilities, firstDerivatives) {
  nParams <- dim(firstDerivatives)[3]

  indexes <- expand.grid(seq_len(nParams), seq_len(nParams))

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

  information
}

getLikelihoodMM_ref <- function(multinomialProbabilities) {
  symP <- multinomialProbabilities * t(multinomialProbabilities)
  diag(symP) <- 0
  denominator <- sum(symP) / 2
  return(symP / denominator)
}

getMultinomialInformationMatrix_ref <- function(likelihoods, derivatives) {
  nParams <- dim(derivatives)[3]
  nActors <- dim(derivatives)[1]
  matrixSize <- nActors * nActors

  likelihoodsTriangle <- likelihoods * upper.tri(likelihoods)

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

getMultinomialInformationMatrixM_ref <- function(
  eventProbabilities,
  firstDerivatives
) {
  nParams <- dim(firstDerivatives)[2]

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
    values <- temp
  }
  information <- matrix(values, nParams, nParams)
}

getMultinomialProbabilities_ref <- function(
  statsArray,
  activeDyad,
  parameters,
  actorNested = TRUE,
  allowReflexive = TRUE,
  is_two_mode = FALSE,
  riskMask = NULL
) {
  nDimensions <- length(dim(statsArray))
  if (!(nDimensions %in% c(2, 3))) {
    stop(
      "StatsArray in getMultinomialProbabilities has to be",
      " two- or three-dimensional.",
      call. = FALSE
    )
  }

  nActors1 <- dim(statsArray)[1]
  nActors2 <- dim(statsArray)[2]
  if (nDimensions == 3) {
    matrixSize <- nActors1 * nActors2
    weightedStatsArray <- statsArray * rep(parameters, each = matrixSize)
    utility <- exp(apply(weightedStatsArray, c(1, 2), sum))
    if (!allowReflexive && !is_two_mode) {
      diag(utility) <- 0
    }
    if (!is.null(riskMask)) {
      utility[!riskMask] <- 0
    }
    if (actorNested) {
      denominators <- rowSums(utility)
    } else {
      denominators <- sum(utility)
    }
  }
  if (nDimensions == 2) {
    weightedStatsArray <- sweep(statsArray, MARGIN = 2, parameters, "*")
    utility <- exp(rowSums(weightedStatsArray))
    if (!allowReflexive && !is_two_mode) {
      utility[activeDyad[1]] <- 0
    }
    denominators <- sum(utility)
  }
  probabilities <- utility / denominators
  if (nDimensions == 3 && actorNested && !is.null(riskMask)) {
    probabilities[denominators == 0, ] <- 0
  }
  probabilities
}

# --- per-event contribution dispatch (frozen mirror of the live methods) ----

contribution_reference <- function(spec, ...) {
  UseMethod("contribution_reference")
}

contribution_reference.default <- function(spec, ...) {
  stop(
    "No contribution_reference method for class ",
    class(spec)[1],
    call. = FALSE
  )
}

contribution_reference.dynam_rate_spec <- function(
  spec,
  statsArray,
  activeDyad,
  parameters,
  isRightCensored,
  timespan,
  allowReflexive,
  is_two_mode
) {
  event_contribution_rate_ref(
    statsArray,
    activeDyad,
    parameters,
    isRightCensored,
    timespan,
    allowReflexive,
    is_two_mode,
    isREM = FALSE
  )
}

contribution_reference.dynami_rate_spec <-
  contribution_reference.dynam_rate_spec

contribution_reference.rem_rate_spec <- function(
  spec,
  statsArray,
  activeDyad,
  parameters,
  isRightCensored,
  timespan,
  allowReflexive,
  is_two_mode,
  riskMask = NULL
) {
  event_contribution_rate_ref(
    statsArray,
    activeDyad,
    parameters,
    isRightCensored,
    timespan,
    allowReflexive,
    is_two_mode,
    isREM = TRUE,
    riskMask = riskMask
  )
}

contribution_reference.dynam_rate_ordered_spec <- function(
  spec,
  statsArray,
  activeDyad,
  parameters,
  isRightCensored,
  timespan,
  allowReflexive,
  is_two_mode
) {
  statsMatrix <- statsArray
  activeActor <- activeDyad[1]
  parameters <- c(parameters)

  rates <- exp(rowSums(t(t(statsMatrix) * parameters)))
  eventProbabilities <- rates / sum(rates)
  expectedStatistics <- colSums(statsMatrix * eventProbabilities)
  logLikelihood <- sum(statsMatrix[activeActor, ] * parameters) -
    log(sum(rates))
  deviations <- t(t(statsMatrix) - expectedStatistics)
  score <- deviations[activeActor, ]
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
  list(
    logLikelihood = logLikelihood,
    score = score,
    informationMatrix = informationMatrix,
    pMatrix = eventProbabilities
  )
}

contribution_reference.dynami_rate_ordered_spec <-
  contribution_reference.dynam_rate_ordered_spec

contribution_reference.dynam_choice_spec <- function(
  spec,
  statsArray,
  activeDyad,
  parameters,
  isRightCensored,
  timespan,
  allowReflexive,
  is_two_mode
) {
  eventProbabilities <-
    getMultinomialProbabilities_ref(
      statsArray,
      activeDyad,
      parameters,
      actorNested = TRUE,
      allowReflexive = allowReflexive,
      is_two_mode = is_two_mode
    )
  logLikelihood <- log(eventProbabilities[activeDyad[2]])
  firstDerivatives <- compute_first_derivative_choice_ref(
    statsArray,
    eventProbabilities
  )
  score <- firstDerivatives[activeDyad[2], ]
  informationMatrix <- getMultinomialInformationMatrixM_ref(
    eventProbabilities,
    firstDerivatives
  )
  list(
    logLikelihood = logLikelihood,
    score = score,
    informationMatrix = informationMatrix,
    pMatrix = eventProbabilities
  )
}

contribution_reference.dynami_choice_spec <-
  contribution_reference.dynam_choice_spec

contribution_reference.dynam_choice_coord_spec <- function(
  spec,
  statsArray,
  activeDyad,
  parameters,
  isRightCensored,
  timespan,
  allowReflexive,
  is_two_mode,
  riskMask = NULL
) {
  multinomialProbabilities <-
    getMultinomialProbabilities_ref(
      statsArray,
      activeDyad,
      parameters,
      allowReflexive = allowReflexive,
      riskMask = riskMask
    )
  eventLikelihoods <- getLikelihoodMM_ref(multinomialProbabilities)
  logLikelihood <- log(eventLikelihoods[activeDyad[1], activeDyad[2]])
  firstDerivatives <- compute_first_derivative_choice_coord_ref(
    statsArray,
    eventLikelihoods,
    multinomialProbabilities
  )
  score <- firstDerivatives[activeDyad[1], activeDyad[2], ]
  informationMatrix <- getMultinomialInformationMatrix_ref(
    eventLikelihoods,
    firstDerivatives
  )
  list(
    logLikelihood = logLikelihood,
    score = score,
    informationMatrix = informationMatrix,
    pMatrix = eventLikelihoods
  )
}

contribution_reference.rem_rate_ordered_spec <- function(
  spec,
  statsArray,
  activeDyad,
  parameters,
  isRightCensored,
  timespan,
  allowReflexive,
  is_two_mode,
  riskMask = NULL
) {
  eventProbabilities <-
    getMultinomialProbabilities_ref(
      statsArray,
      activeDyad,
      parameters,
      actorNested = FALSE,
      allowReflexive = FALSE,
      riskMask = riskMask
    )
  logLikelihood <- log(eventProbabilities[activeDyad[1], activeDyad[2]])
  firstDerivatives <- compute_first_derivative_rem_ref(
    statsArray,
    eventProbabilities
  )
  score <- firstDerivatives[activeDyad[1], activeDyad[2], ]
  informationMatrix <- getInformationMatrixREM_ref(
    eventProbabilities,
    firstDerivatives
  )
  list(
    logLikelihood = logLikelihood,
    score = score,
    informationMatrix = informationMatrix,
    pMatrix = eventProbabilities
  )
}
