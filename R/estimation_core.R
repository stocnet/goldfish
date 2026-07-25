##################### ##
#
# Goldfish package
# Internal estimation routine
#
#################### ###

# Estimation
#
# S3 generic dispatched on the model specification class. The family
# methods configure the statistic shape knobs once from the spec class.
estimate_int <- function(spec, ...) {
  UseMethod("estimate_int")
}

estimate_int.sender_spec <- function(spec, ...) {
  estimate_int_impl(
    spec = spec,
    is_rate_model = TRUE,
    reduceArrayToMatrix = FALSE,
    ...
  )
}

estimate_int.dyad_spec <- function(spec, ...) {
  estimate_int_impl(
    spec = spec,
    is_rate_model = FALSE,
    reduceArrayToMatrix = inherits(spec, "dynam_choice_spec") ||
      inherits(spec, "dynami_choice_spec"),
    ...
  )
}

estimate_int_impl <- function(
  spec,
  statsList,
  is_rate_model,
  reduceArrayToMatrix,
  nodes,
  nodes2,
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
  return_event_scores = FALSE,
  parallelize = FALSE,
  cpus = 6,
  verbose = FALSE,
  progress = FALSE,
  # restrictions of opportunity sets
  opportunitiesList = NULL
) {
  ## SET VARIABLES

  # preprocessing guarantees NA-free statistics
  stopifnot(!anyNA(statsList$initialStats))

  minDampingFactor <- initialDamping
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

  statsList <- prepare_statslist(
    statsList = statsList,
    excludeParameters = excludeParameters,
    addInterceptEffect = hasIntercept,
    is_sender = is_rate_model
  )

  ## GET COMPOSITION CHANGES
  hasCompChange1 <- length(statsList$active_sender_changes) > 0
  hasCompChange2 <- length(statsList$active_dyad_changes) > 0 &&
    !is_rate_model

  compChange1 <- if (hasCompChange1) {
    data.frame(
      time = vapply(statsList$active_sender_changes, `[[`, double(1), "time"),
      node = vapply(statsList$active_sender_changes, `[[`, integer(1), "node"),
      replace = vapply(
        statsList$active_sender_changes,
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
      time = vapply(statsList$active_dyad_changes, `[[`, double(1), "time"),
      node = vapply(statsList$active_dyad_changes, `[[`, integer(1), "node"),
      replace = vapply(
        statsList$active_dyad_changes,
        `[[`,
        logical(1),
        "replace"
      )
    )
  } else {
    NULL
  }

  presence <- statsList$active_sender_init
  presence2 <- statsList$active_dyad_init

  nEvents <- length(statsList$is_dependent)

  ## ADD INTERCEPT
  # CHANGED MARION
  # replace first parameter with an initial estimate of the intercept
  if (
    inherits(spec, c("dynam_rate_spec", "dynami_rate_spec", "rem_rate_spec")) &&
      hasIntercept &&
      is.null(initialParameters) &&
      (is.null(fixedParameters) || is.na(fixedParameters[1]))
  ) {
    parameters[1] <- log(
      statsList$n_dep_events /
        statsList$total_time /
        statsList$avg_active_entity
    )
  }
  ## SET VARIABLES BASED ON STATSLIST

  ## ESTIMATION: INITIALIZATION

  if (verbose) {
    cat("Estimating model: ", class(spec)[1])
  }

  nr <- run_nr_loop(
    spec = spec,
    parameters = parameters,
    initialParameters = initialParameters,
    nParams = nParams,
    idUnfixedCompnents = idUnfixedCompnents,
    idFixedCompnents = idFixedCompnents,
    likelihoodOnly = likelihoodOnly,
    minDampingFactor = minDampingFactor,
    maxIterations = maxIterations,
    dampingIncreaseFactor = dampingIncreaseFactor,
    dampingDecreaseFactor = dampingDecreaseFactor,
    score_tol = score_tol,
    step_tol = step_tol,
    returnIntervalLogL = returnIntervalLogL,
    returnEventProbabilities = returnEventProbabilities,
    return_event_scores = return_event_scores,
    verbose = verbose,
    progress = progress,
    step_args = list(
      statsList = statsList,
      nodes = nodes,
      nodes2 = nodes2,
      updatepresence = hasCompChange1,
      presence = presence,
      compChange1 = compChange1,
      updatepresence2 = hasCompChange2,
      presence2 = presence2,
      compChange2 = compChange2,
      hasIntercept = hasIntercept,
      spec = spec,
      parallelize = parallelize,
      cpus = cpus,
      returnIntervalLogL = returnIntervalLogL,
      returnEventProbabilities = returnEventProbabilities,
      return_event_scores = return_event_scores,
      allowReflexive = allowReflexive,
      is_two_mode = is_two_mode,
      reduceArrayToMatrix = reduceArrayToMatrix,
      verbose = verbose,
      opportunitiesList = opportunitiesList
    )
  )

  parameters <- nr$parameters
  logLikelihood <- nr$logLikelihood
  score <- nr$score
  informationMatrix <- nr$informationMatrix
  inverseInformationUnfixed <- nr$inverseInformationUnfixed
  isConverged <- nr$isConverged
  returnCode <- nr$returnCode
  update <- nr$update
  iIteration <- nr$nIterations
  if (returnIntervalLogL) {
    intervalLogL <- nr$intervalLogL
  }
  if (return_event_scores) {
    event_scores <- nr$event_scores
  }
  if (returnEventProbabilities) {
    eventProbabilities <- nr$eventProbabilities
  }

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
      maxAbsUpdate = max(abs(update)),
      score_rel_norm = max(abs(score)) / max(1, abs(logLikelihood))
    ),
    nIterations = iIteration,
    nEvents = nEvents
  )
  if (returnIntervalLogL) {
    estimationResult$intervalLogL <- intervalLogL
  }
  if (return_event_scores) {
    estimationResult$event_scores <- event_scores
  }
  if (returnEventProbabilities) {
    estimationResult$eventProbabilities <- eventProbabilities
  }
  attr(estimationResult, "class") <- "result.goldfish"
  estimationResult
}

#' Newton-Raphson outer loop
#'
#' Shared estimation kernel extracted from `estimate_int_impl()`. Iterates
#' `compute_iteration_step()` to accumulate the
#' log-likelihood, score, and information matrix, applies damped Newton
#' updates, and stops on the dual `score_tol` / `step_tol` criteria with the
#' documented return codes (1 = gradient close to zero, 2 = step size close
#' to zero). The convergence logic is unchanged from the in-lined version;
#' algorithm variants (e.g. the future incremental REM engine) reuse it by
#' overriding `compute_step()`.
#'
#' @param step_args list of arguments forwarded to `compute_iteration_step()`
#'   each iteration (everything except `parameters`).
#' @return a list with the converged `parameters`, `logLikelihood`, `score`,
#'   `informationMatrix`, `inverseInformationUnfixed`, `isConverged`,
#'   `returnCode`, `update`, `nIterations`, and optional `intervalLogL` /
#'   `event_scores` / `eventProbabilities`.
#' @noRd
run_nr_loop <- function(
  spec,
  parameters,
  initialParameters,
  nParams,
  idUnfixedCompnents,
  idFixedCompnents,
  likelihoodOnly,
  minDampingFactor,
  maxIterations,
  dampingIncreaseFactor,
  dampingDecreaseFactor,
  score_tol,
  step_tol,
  returnIntervalLogL,
  returnEventProbabilities,
  return_event_scores,
  verbose,
  progress,
  step_args
) {
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
  intervalLogL <- NULL
  event_scores <- NULL
  eventProbabilities <- NULL

  # if (parallelize && require("snowfall", quietly = TRUE)) {
  #   snowfall::sfStop()
  #   snowfall::sfInit(parallel = TRUE, cpus = cpus)
  #   snowfall::sfExport(
  #   "getMultinomialProbabilities", "getLikelihoodMM",
  #   "compute_first_derivative_choice_coord",
  #   "getMultinomialInformationMatrix", namespace = "goldfish")
  # }

  ## ESTIMATION: ITERATIONS
  while (TRUE) {
    # MARION to be make: update this function for the new stat list
    # calculate logL, score and information; pass parallel computing parameters
    res <- do.call(
      compute_iteration_step,
      c(step_args, list(parameters = parameters))
    )

    logLikelihood <- res[[1]]
    score <- res[[2]]
    informationMatrix <- res[[3]]
    if (returnIntervalLogL) {
      intervalLogL <- res[[4]]
    }
    if (return_event_scores) {
      event_scores <- res$event_scores
    }
    # add a possibility to return the whole probability matrix: to be make
    if (returnEventProbabilities) {
      eventProbabilities <- if (is.null(res$pMatrix)) {
        paste("not implemented for model class", class(spec)[1])
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

  list(
    parameters = parameters,
    logLikelihood = logLikelihood,
    score = score,
    informationMatrix = informationMatrix,
    inverseInformationUnfixed = inverseInformationUnfixed,
    isConverged = isConverged,
    returnCode = returnCode,
    update = update,
    nIterations = iIteration,
    intervalLogL = intervalLogL,
    event_scores = event_scores,
    eventProbabilities = eventProbabilities
  )
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
    return_code = if (score_converged) {
      1L
    } else if (step_converged) {
      2L
    } else {
      0L
    },
    score_rel_norm = score_rel_norm
  )
}


# Per-event contribution to the log-likelihood, score, information matrix,
# and p vector.
#
# S3 generic dispatched on the model specification class. Each method
# returns the contribution of a single event for its model variant. The
# concrete method is resolved once before the estimation
# event loop (via bind_event_contribution()) and bound to a local variable;
# no S3 dispatch happens per event.
# CHANGED SIWEI: add three parameters: isRightCensored, timespan and
#  allowReflexive
compute_event_contribution <- function(
  spec,
  statsArray,
  activeDyad,
  parameters,
  isRightCensored,
  timespan,
  allowReflexive,
  is_two_mode
) {
  UseMethod("compute_event_contribution")
}

compute_event_contribution.default <- function(spec, ...) {
  cli::cli_abort(
    "No {.fn compute_event_contribution} method for class
     {.cls {class(spec)[1]}}."
  )
}

# Resolve the concrete compute_event_contribution() method for a spec once,
# walking its class vector. Returns a plain function to be called inside the
# event loop without further dispatch.
bind_event_contribution <- function(spec) {
  for (cls in class(spec)) {
    fn <- get0(paste0("compute_event_contribution.", cls))
    if (is.function(fn)) {
      return(fn)
    }
  }
  compute_event_contribution.default
}

# Shared worker for the rate models (DyNAM-M-Rate and REM). `isREM` is baked
# in by the calling method so no per-event class check is needed.
event_contribution_rate <- function(
  statsArray,
  activeDyad,
  parameters,
  isRightCensored,
  timespan,
  allowReflexive,
  is_two_mode,
  isREM,
  active_dyad_mask = NULL
) {
  activeActor <- activeDyad[1]
  dimMatrix <- dim(statsArray)
  # A support_constraint (REM) removes disallowed dyads from the risk set exactly
  # as the reflexive-edge exclusion does: their rate is zeroed, so they leave the
  # denominator (and the score / information sums) but the observed dyad's own
  # term is untouched. `active_dyad_mask` is the per-event mask reduced to the presence-
  # kept dyads, flattened column-major to match the rate vector.
  maskedOut <- if (isREM && !is.null(active_dyad_mask)) {
    which(!as.vector(active_dyad_mask))
  } else {
    integer(0)
  }
  if (isREM) {
    activeActor <- activeDyad[1] + (activeDyad[2] - 1) * dimMatrix[1]
    # Merge the dyad axes (n1 x n2) into the rows without copying: R is
    # column-major, so reinterpreting the n1 x n2 x nParams cube as
    # (n1 * n2) x nParams is byte-identical to apply(statsArray, 3, c) but
    # avoids materialising every slice.
    dim(statsArray) <- c(dimMatrix[1] * dimMatrix[2], dimMatrix[3])
  }

  parameters <- as.numeric(parameters)

  # test if time interval is NA, to be make
  if (is.na(timespan)) {
    timespan <- 0
  }

  # Don't consider self-connecting edge when both allowReflexive
  # and  is_two_mode are false
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
  # vector of rates
  objectiveFunctions <- (statsArray %*% parameters)[, 1] # a vector
  objectiveFunctionOfSender <- objectiveFunctions[activeActor]
  statsOfSender <- statsArray[activeActor, ]
  rates <- exp(objectiveFunctions)
  rates[idEdgeNotConsidered] <- 0
  rates[maskedOut] <- 0
  ratesSum <- sum(rates)
  # k vector with all rho * s_k summed over all actors i
  ratesStats <- rates * statsArray
  ratesStatsSum <- colSums(rates * statsArray)

  # Rate-weighted Fisher information sum_i rho_i s_i s_i^T as a single weighted
  # cross-product: crossprod(S, S * rates) = S^T diag(rates) S. This preserves
  # the nParams x nParams shape for every parameter count, so the former
  # single-parameter special-case loop (which patched the degenerate outer())
  # is no longer needed.
  ratesStatsStatsSum <- crossprod(statsArray, statsArray * rates)

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

compute_event_contribution.dynam_rate_spec <- function(
  spec,
  statsArray,
  activeDyad,
  parameters,
  isRightCensored,
  timespan,
  allowReflexive,
  is_two_mode
) {
  event_contribution_rate(
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

compute_event_contribution.rem_rate_spec <- function(
  spec,
  statsArray,
  activeDyad,
  parameters,
  isRightCensored,
  timespan,
  allowReflexive,
  is_two_mode,
  active_dyad_mask = NULL
) {
  event_contribution_rate(
    statsArray,
    activeDyad,
    parameters,
    isRightCensored,
    timespan,
    allowReflexive,
    is_two_mode,
    isREM = TRUE,
    active_dyad_mask = active_dyad_mask
  )
}

compute_event_contribution.dynami_rate_spec <-
  compute_event_contribution.dynam_rate_spec

compute_event_contribution.dynam_rate_ordered_spec <- function(
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

  # Sender softmax through the single-pass max-shift helper: finite
  # probabilities under overflow, and the observed sender's log-likelihood
  # log(p_i) = x_i - logNormalizer stays finite even when p_i underflows.
  linearPredictor <- (statsMatrix %*% parameters)[, 1]
  softmax <- stable_softmax(linearPredictor)
  eventProbabilities <- softmax$probabilities
  expectedStatistics <- colSums(statsMatrix * eventProbabilities)
  logLikelihood <- softmax$logProbabilities[activeActor]
  # deviation from actual statistics
  deviations <- t(t(statsMatrix) - expectedStatistics)
  score <- deviations[activeActor, ]
  # Fisher information matrix: sum_i p_i d_i d_i^T = D^T diag(p) D, a weighted
  # cross-product replacing the per-row outer(x, x) closures.
  informationMatrix <- crossprod(deviations, deviations * eventProbabilities)
  list(
    logLikelihood = logLikelihood,
    score = score,
    informationMatrix = informationMatrix,
    pMatrix = eventProbabilities
  )
}

compute_event_contribution.dynami_rate_ordered_spec <-
  compute_event_contribution.dynam_rate_ordered_spec

compute_event_contribution.dynam_choice_spec <- function(
  spec,
  statsArray,
  activeDyad,
  parameters,
  isRightCensored,
  timespan,
  allowReflexive,
  is_two_mode
) {
  multinomial <-
    getMultinomialProbabilities(
      statsArray,
      activeDyad,
      parameters,
      actorNested = TRUE,
      allowReflexive = allowReflexive,
      is_two_mode = is_two_mode
    )
  eventProbabilities <- multinomial$probabilities
  # log-space observed logL (finite under underflow)
  logLikelihood <- multinomial$logProbabilities[activeDyad[2]]
  firstDerivatives <- compute_first_derivative_choice(
    statsArray,
    eventProbabilities
  )
  score <- firstDerivatives[activeDyad[2], ]
  informationMatrix <- getMultinomialInformationMatrixM(
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

compute_event_contribution.dynami_choice_spec <-
  compute_event_contribution.dynam_choice_spec

compute_event_contribution.dynam_choice_coord_spec <- function(
  spec,
  statsArray,
  activeDyad,
  parameters,
  isRightCensored,
  timespan,
  allowReflexive,
  is_two_mode,
  active_dyad_mask = NULL
) {
  multinomial <-
    getMultinomialProbabilities(
      statsArray,
      activeDyad,
      parameters,
      allowReflexive = allowReflexive,
      active_dyad_mask = active_dyad_mask
    )
  multinomialProbabilities <- multinomial$probabilities
  eventLikelihoods <- getLikelihoodMM(multinomialProbabilities)
  # Observed dyad log-likelihood in log-space (finite under underflow): the
  # coordination likelihood is a softmax over unordered dyads with
  # log-weight log w_{ij} = log P(i->j) + log P(j->i); logL = log w_obs -
  # logSumExp over dyads. The upper/lower symmetry double-counts each unordered
  # dyad, matching getLikelihoodMM's denominator / 2.
  logSymmetric <- multinomial$logProbabilities + t(multinomial$logProbabilities)
  diag(logSymmetric) <- -Inf
  finiteWeights <- logSymmetric[is.finite(logSymmetric)]
  shift <- if (length(finiteWeights)) max(finiteWeights) else 0
  logDenominator <- shift + log(sum(exp(logSymmetric - shift))) - log(2)
  logLikelihood <- logSymmetric[activeDyad[1], activeDyad[2]] - logDenominator
  firstDerivatives <- compute_first_derivative_choice_coord(
    statsArray,
    eventLikelihoods,
    multinomialProbabilities
  )
  score <- firstDerivatives[activeDyad[1], activeDyad[2], ]
  informationMatrix <- getMultinomialInformationMatrix(
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

compute_event_contribution.rem_rate_ordered_spec <- function(
  spec,
  statsArray,
  activeDyad,
  parameters,
  isRightCensored,
  timespan,
  allowReflexive,
  is_two_mode,
  active_dyad_mask = NULL
) {
  multinomial <-
    getMultinomialProbabilities(
      statsArray,
      activeDyad,
      parameters,
      actorNested = FALSE,
      allowReflexive = FALSE,
      active_dyad_mask = active_dyad_mask
    )
  eventProbabilities <- multinomial$probabilities
  # log-space observed logL (finite under underflow)
  logLikelihood <- multinomial$logProbabilities[activeDyad[1], activeDyad[2]]
  firstDerivatives <- compute_first_derivative_rem(
    statsArray,
    eventProbabilities
  )
  score <- firstDerivatives[activeDyad[1], activeDyad[2], ]
  informationMatrix <- getInformationMatrixREM(
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


# calculate the score contribution of one event of the M model
# to the log(!) likelihood
# The scores are the differences between expected and observed statistics
compute_first_derivative_choice <- function(statsArray, eventProbabilities) {
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
compute_first_derivative_choice_coord <- function(
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
  # symmetrize the deviations from expectations. The aperm transpose of the
  # first two dimensions is a C-level layout swap and stays as-is.
  symDeviations <- deviationFromExpectation +
    aperm(deviationFromExpectation, c(2, 1, 3))
  # the likelihoods have to divided by 2
  # as the matrix sum is 2 (it includes all likelihoods twice).
  # colSums(., dims = 2) reduces each parameter slice in one C pass, replacing
  # the per-slice apply(., 3, sum) closure.
  constantWeightedDeviations <-
    colSums(symDeviations * c(likelihoods / 2), dims = 2)

  # 4)
  derivatives <- symDeviations -
    rep(constantWeightedDeviations, each = matrixSize)

  derivatives
}

# calculate the score contribution of one event of the M model
# to the log(!) likelihood
# The scores are the differences between expected and observed statistics
compute_first_derivative_rem <- function(statsArray, eventProbabilities) {
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
  arrayDim <- dim(firstDerivatives)
  nParams <- arrayDim[3]

  # Flatten the n1 x n2 x p derivative cube to (n1*n2) x p (column-major, no
  # copy of the data layout) and form the weighted cross-product
  # sum_c p_c d_c d_c^T = D^T diag(w) D, replacing the expand.grid + p^2 apply
  # closures over n^2 slices.
  dim(firstDerivatives) <- c(arrayDim[1] * arrayDim[2], nParams)
  weights <- as.vector(eventProbabilities)
  crossprod(firstDerivatives, firstDerivatives * weights)
}


# Resolve the concrete compute_step() method for a spec once, walking its
# class vector. Returns a plain function called inside the event loop without
# further dispatch.
bind_compute_step <- function(spec) {
  for (cls in class(spec)) {
    fn <- get0(paste0("compute_step.", cls))
    if (is.function(fn)) {
      return(fn)
    }
  }
  compute_step.default
}

# Per-event update of the running estimation state.
#
# S3 generic dispatched on the model spec. The default method implements the
# full-recompute pattern: apply the flat update slice to the
# running statsArray via apply_flat_update(), filter the active presence /
# opportunity set, zero the reflexive diagonal where applicable, call the
# bound compute_event_contribution(), and accumulate logL / score /
# information into `state`. Algorithm variants (e.g. the future incremental
# REM engine) override this method. `ctx` carries the loop-invariant context
# and `state` the mutable running quantities; the updated `state` is
# returned.
compute_step <- function(spec, state, i, ctx) {
  UseMethod("compute_step")
}

compute_step.default <- function(spec, state, i, ctx) {
  statsList <- ctx$statsList
  is_rate <- ctx$is_rate
  hasIntercept <- ctx$hasIntercept

  isDependent <- statsList$is_dependent[[i]] == 1L
  flatEnd <- statsList$stat_mat_pointer[i]
  if (flatEnd > state$flatPointer) {
    updatesSlice <- statsList$stat_mat_update[,
      (state$flatPointer + 1L):flatEnd,
      drop = FALSE
    ]
    if (hasIntercept) {
      updatesSlice[3, ] <- updatesSlice[3, ] + 1L
    }
    state$statsArray <- apply_flat_update(
      state$statsArray,
      updatesSlice,
      is_sender = is_rate
    )
  }
  state$flatPointer <- flatEnd

  if (!is.null(statsList$stat_mat_broadcast_pointer)) {
    bcEnd <- statsList$stat_mat_broadcast_pointer[i]
    if (bcEnd > state$bcPointer) {
      bcSlice <- statsList$stat_mat_broadcast[,
        (state$bcPointer + 1L):bcEnd,
        drop = FALSE
      ]
      if (hasIntercept) {
        bcSlice[3, ] <- bcSlice[3, ] + 1L
      }
      dims <- dim(state$statsArray)
      state$statsArray <- apply_broadcast_update(
        state$statsArray,
        bcSlice,
        is_sender = is_rate,
        n1 = dims[1],
        n2 = if (is_rate) NA_integer_ else dims[2],
        # Keep the reflexive diagonal cell in broadcast fan-out on the same
        # condition the risk set does: a two-mode model (the two indices are
        # different node sets) or one that allows self-ties. Reading is_two_mode
        # alone would drop the diagonal a reflexive one-mode risk set keeps.
        twomode_or_reflexive = ctx$is_two_mode || ctx$allowReflexive
      )
    }
    state$bcPointer <- bcEnd
  }

  # `timespan` is only meaningful with a time intercept (rate / REM); the
  # choice contribution ignores it. Define it unconditionally so the contribution
  # arguments can be assembled eagerly (previously it was passed as an unforced
  # promise for the no-intercept case).
  timespan <- NA_real_
  if (hasIntercept) {
    state$time <- state$time + statsList$intervals[[i]]
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

  statsArrayComp <- state$statsArray

  # update opportunity set
  if (ctx$updateopportunities) {
    state$opportunities <- seq_len(nrow(ctx$nodes2)) %in%
      ctx$opportunitiesList[[i]]
  }

  # update composition
  # CHANGED SIWEI: fixed errors for composition change update
  # CHANGED MARION: fixed wrong initialization of compositions,
  #   removed next lines

  current_time <- statsList$event_time[[i]]
  if (ctx$active_sender_folded) {
    # Maintain the folded `active_sender` by walking its per-event crossings
    # slice: apply this event's flips before its likelihood.
    hi <- ctx$active_sender_update_pointer[i]
    lo <- if (i > 1L) ctx$active_sender_update_pointer[i - 1L] else 0L
    if (hi > lo) {
      cols <- (lo + 1L):hi
      state$presence[ctx$active_sender_update[1L, cols]] <-
        as.logical(ctx$active_sender_update[2L, cols])
    }
  } else if (ctx$updatepresence) {
    update <-
      ctx$compChange1[
        ctx$compChange1$time <= current_time &
          ctx$compChange1$time > state$oldTime,
      ]
    state$presence[update$node] <- update$replace
  }

  if (ctx$active_dyad_folded) {
    # Maintain the folded `active_dyad` by its per-event crossings slice
    # applied before this event's likelihood. At the point encoding
    # `state$presence2` is the dense n1 x n2 matrix and the buffer carries
    # (node1, node2, replace); the broadcast encodings maintain a length-n2
    # vector with a (node, replace) buffer.
    hi <- ctx$active_dyad_update_pointer[i]
    lo <- if (i > 1L) ctx$active_dyad_update_pointer[i - 1L] else 0L
    if (hi > lo) {
      cols <- (lo + 1L):hi
      if (identical(ctx$active_dyad_encoding, "point")) {
        state$presence2[cbind(
          ctx$active_dyad_update[1L, cols],
          ctx$active_dyad_update[2L, cols]
        )] <- as.logical(ctx$active_dyad_update[3L, cols])
      } else {
        state$presence2[ctx$active_dyad_update[1L, cols]] <-
          as.logical(ctx$active_dyad_update[2L, cols])
      }
    }
  } else if (ctx$updatepresence2) {
    update2 <-
      ctx$compChange2[
        ctx$compChange2$time <= current_time &
          ctx$compChange2$time > state$oldTime,
      ]
    state$presence2[update2$node] <- update2$replace
  }
  state$oldTime <- current_time

  # patch to avoid collision with dropping absent people
  if (!ctx$is_two_mode && !is_rate) {
    for (parmPos in seq_len(dim(statsArrayComp)[3])) {
      diag(statsArrayComp[,, parmPos]) <- 0
    }
  }

  # remove potential absent lines and columns from the stats array
  # Sender-axis filter: presence. A support_constraint on a rate model folds its
  # per-event sender gate (a sender is at risk only if it has at least one
  # allowed receiver) into `active_sender` during preprocessing, so
  # `state$presence` already carries presence AND the gate — no separate filter.
  # A folded REM or coordination constraint carries both presences ∩ support in
  # the maintained dense `active_dyad` (used whole as `active_dyad_mask` below), so neither
  # axis is reduced — an absent or disallowed dyad is zeroed by the mask, not
  # dropped. Coordination joins REM here because its two-sided likelihood needs the
  # full matrix, not a per-sender row.
  folded_full <- ctx$active_dyad_folded && (ctx$is_rem || ctx$is_coord)
  if ((ctx$updatepresence || ctx$active_sender_folded) && !folded_full) {
    # || (updateopportunities && !is_two_mode)
    # When folded, `state$presence` already carries presence AND the sender
    # gate, so it is the sender filter directly.
    keepIn <- state$presence
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
  if (
    (ctx$updatepresence2 ||
      ctx$updateopportunities ||
      ctx$active_dyad_folded) &&
      !folded_full
  ) {
    # When folded, the receiver filter is read through the encoding accessor
    # `state$presence2` is the folded receiver availability and
    # already includes the support, so `opportunities` is not conjoined.
    keepIn <- if (ctx$active_dyad_folded) {
      active_dyad_row(
        ctx$active_dyad_encoding,
        posSender,
        state$presence2,
        state$presence
      )
    } else {
      state$presence2 & state$opportunities
    }
    # reducing stats array alters the correspondence between row/col
    # it needs to consider the reflexive case to avoid wrong calculation
    # excludes REM and DyNAM-MM
    if (ctx$correctReflexive) {
      if (!ctx$is_two_mode) {
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
    allowReflexiveCorrected <- ctx$allowReflexive
  }

  # reduce array to matrices
  if (ctx$reduceArrayToMatrix) {
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
  contrib_args <- list(
    spec = spec,
    statsArray = statsArrayComp,
    activeDyad = activeDyad,
    parameters = ctx$parameters,
    isRightCensored = isRightCensored,
    timespan = timespan,
    allowReflexive = allowReflexiveCorrected,
    is_two_mode = ctx$is_two_mode
  )
  # REM / coordination support_constraint: the contribution zeroes disallowed
  # dyads from the risk set. Only the REM and coordination contributions accept
  # `active_dyad_mask`, and it is passed only when a folded mask is present, so other
  # paths are unaffected. The maintained dense `active_dyad` is the per-event
  # mask, kept whole because no axis reduction was applied above.
  if (folded_full) {
    contrib_args$active_dyad_mask <- state$presence2
  }
  eventValues <- do.call(ctx$contribution_fn, contrib_args)

  if (ctx$returnIntervalLogL) {
    state$eventLogL[i] <- eventValues$logLikelihood
  }
  if (ctx$return_event_scores) {
    state$event_scores[i, ] <- eventValues$score
  }
  if (ctx$returnEventProbabilities) {
    state$EventProbabilities[[i]] <- eventValues$pMatrix
  }

  state$logLikelihood <- state$logLikelihood + eventValues$logLikelihood
  state$score <- state$score + eventValues$score
  state$informationMatrix <- state$informationMatrix +
    eventValues$informationMatrix

  state
}

# Function to return log likelihood, score and information matrix
# for all events, given a set of parameters
# for the MM, M, REM, and M-Rate function, and REM-ordered.
# Builds the loop-invariant context and the mutable running state once,
# resolves the compute_step() method once, and iterates events.
compute_iteration_step <- function(
  statsList,
  nodes,
  nodes2,
  updatepresence,
  presence,
  compChange1,
  updatepresence2,
  presence2,
  compChange2,
  hasIntercept,
  parameters,
  spec,
  parallelize = FALSE,
  cpus = 4,
  returnIntervalLogL = FALSE,
  returnEventProbabilities = FALSE,
  return_event_scores = FALSE,
  allowReflexive = TRUE,
  is_two_mode = FALSE,
  reduceArrayToMatrix = FALSE,
  verbose = FALSE,
  opportunitiesList = NULL
) {
  nEvents <- length(statsList$is_dependent)
  # Rate models are sender-indexed; read the family from the spec descriptor,
  # never from the statistics array dimensionality.
  is_rate <- identical(risk_set_axis(spec), "sender")
  nParams <- if (is_rate) {
    ncol(statsList$initialStats)
  } else {
    dim(statsList$initialStats)[3]
  }

  updateopportunities <- !is.null(opportunitiesList) && !is_rate
  correctReflexive <- !allowReflexive &&
    inherits(spec, c("dynam_choice_spec", "dynami_choice_spec"))

  # A sender-loop support_constraint is folded into `active_sender` at
  # preprocessing: the availability object already carries
  # presence AND the per-event sender gate as net crossings, so the engine
  # maintains it by walking its flat buffer per event (by index) and uses it
  # directly as the sender filter — no separate estimation-time recombination.
  active_sender_folded <- isTRUE(statsList$active_sender_folded)
  # A DyNAM-choice support_constraint is folded into `active_dyad` at
  # preprocessing: the receiver-axis availability already
  # carries receiver presence AND the folded support, so the engine maintains
  # it by walking its flat buffer per event and reads the receiver filter
  # through the encoding accessor — no separate opportunities/compChange2 step.
  active_dyad_folded <- isTRUE(statsList$active_dyad_folded)
  # A standard- or ordinal-REM support_constraint folds both presences ∩ support
  # into a dense point `active_dyad`: the maintained matrix IS the
  # per-event risk mask, so the presence axis-reductions are skipped and it is
  # consumed directly as `active_dyad_mask`, replacing the standalone per-event mask.
  is_rem <- inherits(spec, c("rem_rate_spec", "rem_rate_ordered_spec"))
  # DyNAM coordination is two-sided (`getLikelihoodMM` pairs both directed
  # choices), so a folded coordination constraint — symmetrised into the dense
  # point `active_dyad` — is consumed as the full risk mask exactly
  # like REM, NOT via the one-sided-choice row accessor.
  is_coord <- inherits(spec, "dynam_choice_coord_spec")

  # check for parallelization
  # if (parallelize && require("snowfall", quietly = TRUE)) {
  #   snowfall::sfStop()
  #   snowfall::sfInit(parallel = TRUE, cpus = cpus)
  #   snowfall::sfExport("compute_event_contribution", namespace = "goldfish")
  # }

  # resolve the per-event step + contribution methods once
  step_fn <- bind_compute_step(spec)
  contribution_fn <- bind_event_contribution(spec)

  ctx <- list(
    statsList = statsList,
    nodes2 = nodes2,
    is_rate = is_rate,
    hasIntercept = hasIntercept,
    parameters = parameters,
    allowReflexive = allowReflexive,
    is_two_mode = is_two_mode,
    reduceArrayToMatrix = reduceArrayToMatrix,
    correctReflexive = correctReflexive,
    updatepresence = updatepresence,
    updatepresence2 = updatepresence2,
    updateopportunities = updateopportunities,
    compChange1 = compChange1,
    compChange2 = compChange2,
    opportunitiesList = opportunitiesList,
    active_sender_folded = active_sender_folded,
    active_sender_update = statsList$active_sender_update,
    active_sender_update_pointer = statsList$active_sender_update_pointer,
    active_dyad_folded = active_dyad_folded,
    active_dyad_update = statsList$active_dyad_update,
    active_dyad_update_pointer = statsList$active_dyad_update_pointer,
    active_dyad_encoding = statsList$active_dyad_encoding,
    is_rem = is_rem,
    is_coord = is_coord,
    returnIntervalLogL = returnIntervalLogL,
    returnEventProbabilities = returnEventProbabilities,
    return_event_scores = return_event_scores,
    contribution_fn = contribution_fn
  )

  # CHANGED Marion: fill in the loop! stats array needs to be computed
  # also changes for dependent and rc events!
  state <- list(
    statsArray = statsList$initialStats,
    time = statsList$startTime,
    flatPointer = 0L,
    bcPointer = 0L,
    oldTime = -Inf,
    presence = presence,
    presence2 = presence2,
    opportunities = rep(TRUE, nrow(nodes2)),
    logLikelihood = 0,
    score = rep(0, nParams),
    informationMatrix = matrix(0, nParams, nParams),
    eventLogL = if (returnIntervalLogL) numeric(nEvents) else NULL,
    event_scores = if (return_event_scores) {
      matrix(0, nEvents, nParams)
    } else {
      NULL
    },
    EventProbabilities = if (returnEventProbabilities) {
      vector(mode = "list", length = nEvents)
    } else {
      NULL
    }
  )

  for (i in seq_len(nEvents)) {
    state <- step_fn(spec, state, i, ctx)
  }

  returnList <- list(
    logLikelihood = state$logLikelihood,
    score = state$score,
    informationMatrix = state$informationMatrix
  )
  if (returnIntervalLogL) {
    returnList$eventLogL <- state$eventLogL
  }
  if (return_event_scores) {
    returnList$event_scores <- state$event_scores
  }
  if (returnEventProbabilities) {
    returnList$pMatrix <- state$EventProbabilities
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
  arrayDim <- dim(derivatives)
  nParams <- arrayDim[3]

  # take upper triangle of the likelihoods matrix so
  # that we do not count probabilities twice
  likelihoodsTriangle <- likelihoods * upper.tri(likelihoods)

  # In the Hessian formula H_{ijh} (7.11), p.111, each pair of derivative
  # slices is weighted by the (upper-triangle-masked) likelihood. Folding that
  # mask into the weight vector, the p^2 sum() closures over n^2 slices become
  # one weighted cross-product D^T diag(w) D on the flattened derivative cube.
  dim(derivatives) <- c(arrayDim[1] * arrayDim[2], nParams)
  weights <- as.vector(likelihoodsTriangle)
  crossprod(derivatives, derivatives * weights)
}


getMultinomialInformationMatrixM <- function(
  eventProbabilities,
  firstDerivatives
) {
  # firstDerivatives is the nActors x p matrix of log-likelihood derivatives;
  # sum_i p_i d_i d_i^T = D^T diag(p) D. crossprod preserves the p x p shape so
  # the single-parameter special case (dropped dims) no longer needs a branch.
  crossprod(firstDerivatives, firstDerivatives * eventProbabilities)
}


# Single-pass max-shift softmax. Given linear predictors `x` with
# excluded alternatives encoded as -Inf, returns in ONE exp pass both outputs
# the multinomial likelihood needs: the normalized `probabilities` and the
# log-probabilities log(p) = x - logNormalizer. Because the observed
# alternative's log-likelihood is then x_sel - logNormalizer rather than
# log(probabilities[sel]), it stays finite even when that probability underflows
# to 0 (where log(p_sel) would be -Inf). `rowwise = TRUE` normalizes each row of
# a matrix independently (per-sender choice); FALSE normalizes over all entries
# (global choice / REM). A fully-excluded row/vector (denominator 0) maps to
# probability 0 and log-probability -Inf, matching the pre-fold path that
# dropped absent senders before normalising.
stable_softmax <- function(x, rowwise = FALSE) {
  if (rowwise) {
    shift <- apply(x, 1L, max)
    zeroRow <- !is.finite(shift)
    shift[zeroRow] <- 0 # avoid -Inf shift on a fully-excluded row
    # column-major recycling subtracts shift[i] / divides by norm[i] per row i
    expShifted <- exp(x - shift)
    rowSum <- rowSums(expShifted)
    probabilities <- expShifted / rowSum
    logProbabilities <- x - (shift + log(rowSum))
    empty <- zeroRow | rowSum == 0
    if (any(empty)) {
      probabilities[empty, ] <- 0
      logProbabilities[empty, ] <- -Inf
    }
  } else {
    shift <- max(x)
    if (!is.finite(shift)) {
      shift <- 0
    }
    expShifted <- exp(x - shift)
    total <- sum(expShifted)
    probabilities <- expShifted / total
    logProbabilities <- x - (shift + log(total))
    if (total == 0) {
      probabilities[] <- 0
      logProbabilities[] <- -Inf
    }
  }
  list(probabilities = probabilities, logProbabilities = logProbabilities)
}

# Per-event reductions, mirroring `src/event_reductions.h` so the r backend
# produces the same primitives as the compiled ones, exactly as
# `stable_softmax()` above mirrors `stable_softmax_masked()`. A parity test
# (test-event_reductions.R) pins the two implementations to each other on
# constructed inputs, which is what keeps a mirror honest.
#
# Same contract as the header: `w` is a nonnegative weight vector over the
# event's risk set and ZERO outside it, `c` the scale whose product `c * w` is
# each alternative's expected-count contribution (`1 / sum(w)` for the
# probability scale in both families, the interval length for the exact-time
# compensator). `observed` indexes `w`, 1-based here as R code reads.

# 1 + the number of alternatives with a strictly greater weight; ties share the
# better rank. Scale-free, so `c` never enters.
rank_of_observed <- function(w, observed) {
  1L + sum(w > w[observed])
}

# Accumulate `expected[actor(j)] += c * w[j]` over the risk set, and
# `observed[actor(obs)] += 1` for a dependent event. `sides` is a list of
# `list(index = , observed = , expected = )`; `index` maps risk-set position to
# accumulator slot, or NULL when position IS the slot. Two sides are the REM /
# coordination shape (sender and receiver from one contribution).
accumulate_margins <- function(w, c, observed, dependent, sides) {
  contribution <- c * w
  lapply(sides, function(side) {
    slot <- if (is.null(side$index)) seq_along(w) else side$index
    # tapply-free scatter: one add per accumulator slot, order-independent.
    side$expected <- side$expected +
      vapply(
        seq_along(side$expected),
        function(k) sum(contribution[slot == k]),
        numeric(1)
      )
    if (dependent) {
      side$observed[slot[observed]] <- side$observed[slot[observed]] + 1
    }
    side
  })
}

# The event's score contribution: the observed statistic (dependent events only)
# minus the contribution-weighted mean `c * w'X`. `w` being zero outside the
# risk set is what lets this ignore the mask.
event_score_row <- function(x_mat, w, c, observed, dependent) {
  score <- -c * as.vector(w %*% x_mat)
  if (dependent) {
    score <- score + x_mat[observed, ]
  }
  score
}

# Function to calculate a matrix of i->j multinomial choice probabilities
# (non-logged) for one term of the model. Returns a list with `probabilities`
# and the matching stable `logProbabilities`: the excluded
# alternatives (reflexive diagonal, `active_dyad_mask`) enter as -Inf linear predictors,
# so the max-shift is taken over the included set only and they drop from the
# normalizer exactly as zeroing their utility did before.
getMultinomialProbabilities <- function(
  statsArray,
  activeDyad,
  parameters,
  actorNested = TRUE,
  allowReflexive = TRUE,
  is_two_mode = FALSE,
  active_dyad_mask = NULL
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
    # Linear predictor as one matrix product: reshape the n1 x n2 x p cube to
    # (n1*n2) x p without copying (column-major), multiply by the parameters,
    # and fold back to n1 x n2. Replaces the per-cell apply(., c(1,2), sum) over
    # a parameter-broadcast copy of the cube.
    dim(statsArray) <- c(matrixSize, length(parameters))
    linearPredictor <- statsArray %*% parameters
    dim(linearPredictor) <- c(nActors1, nActors2)
    if (!allowReflexive && !is_two_mode) {
      diag(linearPredictor) <- -Inf
    }
    # A support_constraint (ordinal REM / coordination) removes disallowed dyads
    # from the risk set exactly as the reflexive diagonal does: -Inf predictor
    # drops them from the denominator (and the probability-weighted score /
    # information sums), while the observed dyad's own term is untouched.
    # `active_dyad_mask` is the maintained dense n1 x n2 availability aligned with the
    # [sender, receiver] predictor (coordination folds both presences, so a
    # masked row is an absent / fully-gated sender).
    if (!is.null(active_dyad_mask)) {
      linearPredictor[!active_dyad_mask] <- -Inf
    }
    # actorNested: per-sender row softmax; else the global REM normalizer.
    stable_softmax(linearPredictor, rowwise = actorNested)
  } else {
    linearPredictor <- (statsArray %*% parameters)[, 1]
    # allow reflexive?
    if (!allowReflexive && !is_two_mode) {
      linearPredictor[activeDyad[1]] <- -Inf
    }
    stable_softmax(linearPredictor, rowwise = FALSE)
  }
}


#' Prepare the statistics list for estimation
#'
#' The only two transformations estimation applies to a preprocessed
#' object: dropping the effect columns listed in `excludeParameters` from
#' `initialStats`, and prepending the constant rate-intercept statistic
#' (a dummy for the theta_0 parameter) when the model carries a time
#' intercept. Replaces `modifyStatisticsList()` in the estimation entries;
#' the right-censoring and array reductions of `reduceStatisticsList()`
#' were no-ops at those call sites.
#'
#' @param statsList a `preprocessed.goldfish` object.
#' @param excludeParameters integer positions of effects to drop.
#' @param addInterceptEffect logical, whether to prepend the intercept
#'   statistic.
#' @param is_sender logical, whether the statistics are sender-indexed (a rate
#'   family). Supplied by the caller from the spec descriptor
#'   (`risk_set_axis(spec) == "sender"`) so the array shape is not used as a
#'   family indicator.
#'
#' @return the modified `statsList`.
#' @noRd
prepare_statslist <- function(
  statsList,
  excludeParameters = NULL,
  addInterceptEffect = FALSE,
  is_sender = FALSE
) {
  is_sender_stats <- isTRUE(is_sender)
  if (!is.null(excludeParameters)) {
    nEffects <- if (is_sender_stats) {
      ncol(statsList$initialStats)
    } else {
      dim(statsList$initialStats)[3]
    }
    unknownIndexes <- setdiff(excludeParameters, seq_len(nEffects))
    if (length(unknownIndexes) > 0) {
      stop(
        "Unknown parameter indexes in 'excludeIndexes': ",
        paste(unknownIndexes, collapse = " ")
      )
    }
    statsList$initialStats <- if (is_sender_stats) {
      statsList$initialStats[, -excludeParameters, drop = FALSE]
    } else {
      statsList$initialStats[,, -excludeParameters, drop = FALSE]
    }
  }
  if (addInterceptEffect) {
    dimensions <- dim(statsList$initialStats)
    statsList$initialStats <- if (is_sender_stats) {
      cbind(1, statsList$initialStats)
    } else {
      array(
        c(matrix(1, dimensions[1], dimensions[2]), statsList$initialStats),
        dim = dimensions + c(0, 0, 1)
      )
    }
  }
  statsList
}
