##################### ##
#
# Goldfish package
# Internal estimation routine
#
#################### ###

# The estimation-side counterpart of the front-end assembler: it turns the two
# contracts -- which coefficients are held at supplied values, which were seeded
# with starting values -- into the pieces the estimation loops need. Every
# estimation path reads them from here, so the alignment between a term and its
# position is decided once, where the coefficient count is known, instead of
# being re-derived from an encoded vector by each consumer.
#
# Seeding is applied first and fixing overwrites it, so a coefficient named by
# both ends up at its fixed value. `intercept_seeded` is what a data-derived
# intercept start must consult: seeding some other coefficient says nothing
# about the intercept, which is why "was any starting vector supplied at all"
# is not the question to ask.
resolve_coefficient_mask <- function(fixed_spec, initial_spec, n_params) {
  parameters <- numeric(n_params)
  if (!is.null(initial_spec)) {
    check_spec_positions(initial_spec, n_params, "seeded")
    parameters[initial_spec$idx] <- initial_spec$values
  }
  if (!is.null(fixed_spec)) {
    check_spec_positions(fixed_spec, n_params, "fixed")
    parameters[fixed_spec$idx] <- fixed_spec$values
  }
  id_fixed <- fixed_spec$idx
  list(
    parameters = parameters,
    id_fixed = id_fixed,
    id_unfixed = setdiff(seq_len(n_params), id_fixed),
    likelihood_only = length(id_fixed) == n_params && n_params > 0,
    intercept_fixed = 1L %in% id_fixed,
    intercept_seeded = 1L %in% initial_spec$idx
  )
}

check_spec_positions <- function(spec, n_params, what) {
  bad <- spec$idx > n_params | spec$idx < 1L
  if (!any(bad)) {
    return(invisible(NULL))
  }
  cli::cli_abort(c(
    "A {what} coefficient falls outside this model's {n_params}
     coefficient{?s}.",
    "x" = "Out of range: {.code {spec$names[bad]}} at
           position {.val {spec$idx[bad]}}.",
    "i" = "The model the values were written for is not the model being
           estimated."
  ))
}

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
  initial_spec = NULL,
  fixed_spec = NULL,
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
  return_ranks = FALSE,
  return_margins = FALSE,
  return_total_rate = FALSE,
  parallelize = FALSE,
  cpus = 6,
  verbose = FALSE,
  progress = FALSE,
  # restrictions of opportunity sets
  opportunitiesList = NULL
) {
  ## SET VARIABLES

  # preprocessing guarantees NA-free statistics
  stopifnot(!anyNA(statsList$initial_stats))

  minDampingFactor <- initialDamping
  nParams <- (if (is_rate_model) {
    ncol(statsList$initial_stats)
  } else {
    dim(statsList$initial_stats)[3]
  }) +
    hasIntercept
  # The same decode the compiled path reads: one helper, so the two backends
  # cannot drift on which coefficients they hold or where they start.
  mask <- resolve_coefficient_mask(
    fixed_spec,
    initial_spec,
    nParams
  )
  parameters <- mask$parameters
  id_unfixed <- mask$id_unfixed
  id_fixed <- mask$id_fixed
  likelihood_only <- mask$likelihood_only

  ## PARAMETER CHECKS

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
  # Applied unless the intercept itself carries a value: fixed, so there is
  # nothing to start, or seeded, so the user chose the start.
  if (
    inherits(spec, c("dynam_rate_spec", "dynami_rate_spec", "rem_rate_spec")) &&
      hasIntercept &&
      !mask$intercept_fixed &&
      !mask$intercept_seeded
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
    nParams = nParams,
    id_unfixed = id_unfixed,
    id_fixed = id_fixed,
    likelihood_only = likelihood_only,
    minDampingFactor = minDampingFactor,
    maxIterations = maxIterations,
    dampingIncreaseFactor = dampingIncreaseFactor,
    dampingDecreaseFactor = dampingDecreaseFactor,
    score_tol = score_tol,
    step_tol = step_tol,
    returnIntervalLogL = returnIntervalLogL,
    returnEventProbabilities = returnEventProbabilities,
    return_event_scores = return_event_scores,
    return_ranks = return_ranks,
    return_margins = return_margins,
    return_total_rate = return_total_rate,
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
      return_ranks = return_ranks,
      return_margins = return_margins,
      return_total_rate = return_total_rate,
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
  stdErrors[id_unfixed] <- sqrt(diag(inverseInformationUnfixed))

  # define, type and return result
  estimationResult <- list(
    parameters = parameters,
    standard_errors = stdErrors,
    log_likelihood = logLikelihood,
    final_score = score,
    final_information_matrix = informationMatrix,
    convergence = list(
      is_converged = isConverged,
      return_code = returnCode,
      max_abs_score = max(abs(score)),
      max_abs_update = max(abs(update)),
      score_rel_norm = max(abs(score)) / max(1, abs(logLikelihood))
    ),
    n_iterations = iIteration,
    n_events = nEvents
  )
  if (returnIntervalLogL) {
    estimationResult$interval_log_lik <- intervalLogL
  }
  if (return_event_scores) {
    estimationResult$event_scores <- event_scores
  }
  if (returnEventProbabilities) {
    estimationResult$event_probabilities <- eventProbabilities
  }
  if (return_ranks && !is.null(nr$observed_rank)) {
    estimationResult$observed_rank <- nr$observed_rank
  }
  if (return_margins && !is.null(nr$margins)) {
    # Same helper the compiled path calls: actor labels and the scale marker
    # are attached once, so the two backends cannot drift on either.
    estimationResult$margins <- label_margins(
      nr$margins,
      axis = risk_set_axis(spec),
      nodes = nodes,
      nodes2 = nodes2,
      is_exact_time = identical(risk_set_normalizer(spec), "poisson")
    )
  }
  # total_rate / conditional_logl exist only on the exact-time (Poisson)
  # contribution; the multinomial families leave them NULL, so they stay off
  # those fits exactly as on the cpp backend.
  if (return_total_rate && !is.null(nr$total_rate)) {
    estimationResult$total_rate <- nr$total_rate
    estimationResult$conditional_logl <- nr$conditional_logl
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
#' an algorithm variant reuses it by overriding `compute_step()` rather than
#' reimplementing the loop.
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
  nParams,
  id_unfixed,
  id_fixed,
  likelihood_only,
  minDampingFactor,
  maxIterations,
  dampingIncreaseFactor,
  dampingDecreaseFactor,
  score_tol,
  step_tol,
  returnIntervalLogL,
  returnEventProbabilities,
  return_event_scores,
  return_ranks = FALSE,
  return_margins = FALSE,
  return_total_rate = FALSE,
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
  # The starting point is the last accepted one until a step is accepted.
  parameters.old <- parameters
  score.old <- NULL
  informationMatrix.old <- NULL
  intervalLogL <- NULL
  event_scores <- NULL
  eventProbabilities <- NULL
  observed_rank <- NULL
  margins <- NULL
  total_rate <- NULL
  conditional_logl <- NULL

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
    if (return_ranks) {
      observed_rank <- res$observed_rank
    }
    if (return_margins) {
      margins <- res$margins
    }
    if (return_total_rate) {
      total_rate <- res$total_rate
      conditional_logl <- res$conditional_logl
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
        has_unexpected_na(res) &&
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
    if (likelihood_only) {
      inverseInformationUnfixed <- matrix(0, nParams, nParams)
      score <- rep(0, nParams)
      isConverged <- TRUE
      returnCode <- 1L
      break
    }

    # we don't consider the fixed components of the score.
    # It's for the fixing parameter feature. \
    score[id_fixed] <- 0

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

    step_accepted <- !has_unexpected_na(res) &&
      is.finite(logLikelihood) &&
      logLikelihood > logLikelihood.old
    if (!step_accepted) {
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
      informationMatrix[id_unfixed, id_unfixed]
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
    update[id_unfixed] <-
      (inverseInformationUnfixed %*% score[id_unfixed]) / dampingFactor

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
      step_accepted = step_accepted,
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
    eventProbabilities = eventProbabilities,
    observed_rank = observed_rank,
    margins = margins,
    total_rate = total_rate,
    conditional_logl = conditional_logl
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

  # One shifted exp pass yields both exact-time scales, mirroring the gather
  # Poisson kernel so the two backends share their overflow behavior. The
  # likelihood needs the total rate on the ABSOLUTE scale -- it enters as
  # -Dt * T, not as a ratio -- so T is recovered as exp(logNormalizer), which
  # overflows exactly where the raw sum of rates did. That is deliberate:
  # shifting the likelihood would be a different model, and an overflowing step
  # is already rejected by the estimation loop's is.finite() gate. What the
  # shift does buy is an exact probability vector, where the raw ratio
  # sum(rates) gives NaN on overflow or a spurious 1 when every rate is
  # subnormal.
  inRiskSet <- rep(TRUE, length(objectiveFunctions))
  inRiskSet[idEdgeNotConsidered] <- FALSE
  inRiskSet[maskedOut] <- FALSE
  shift <- if (any(inRiskSet)) max(objectiveFunctions[inRiskSet]) else 0
  if (!is.finite(shift)) {
    shift <- 0
  }
  weights <- exp(objectiveFunctions - shift)
  weights[!inRiskSet] <- 0
  shiftedTotal <- sum(weights)
  logNormalizer <- shift + log(shiftedTotal)
  totalRate <- exp(logNormalizer)
  probabilities <- weights / shiftedTotal
  if (shiftedTotal == 0) {
    probabilities[] <- 0
    logNormalizer <- -Inf
    totalRate <- 0
  }

  # The reduction runs on the probability scale with the compensator applied
  # once outside, since Dt * T * p_j == Dt * lambda_j (D20's factorization).
  # That keeps the only large factor in a scalar: a rate that overflowed inside
  # the vector would meet a mixed-sign statistic and give Inf - Inf = NaN.
  compensator <- timespan * totalRate
  # k vector with all p_k * s_k summed over all actors i
  weightedStatsSum <- colSums(probabilities * statsArray)

  # Probability-weighted Fisher information sum_i p_i s_i s_i^T as a single
  # weighted cross-product: crossprod(S, S * p) = S^T diag(p) S. This preserves
  # the nParams x nParams shape for every parameter count, so the former
  # single-parameter special-case loop (which patched the degenerate outer())
  # is no longer needed.
  weightedStatsStatsSum <- crossprod(statsArray, statsArray * probabilities)

  logL <- -compensator +
    if (!isRightCensored) objectiveFunctionOfSender else 0

  score <- -compensator *
    weightedStatsSum +
    if (!isRightCensored) statsOfSender else 0

  hessian <- -compensator * weightedStatsStatsSum
  # The Cox partial-likelihood contribution, computed as x_obs - lse rather
  # than reassembled from `intervalLogL - log T + Dt * T`: that identity cancels
  # a term against itself and loses digits in proportion to Dt * T, which is the
  # regime a diagnostic evaluated away from the MLE lives in. A right-censored
  # interval realizes no mover, so it has no observed alternative to condition
  # on and the component is NA there by design.
  conditionalLogL <- if (isRightCensored) {
    NA_real_
  } else {
    objectiveFunctionOfSender - logNormalizer
  }
  pVector <- probabilities
  if (isREM) {
    dim(pVector) <- c(dimMatrix[1], dimMatrix[2])
  }

  list(
    logLikelihood = logL,
    score = score,
    informationMatrix = -hessian,
    pMatrix = pVector,
    probabilities = probabilities,
    total_rate = totalRate,
    conditional_logl = conditionalLogL
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
    pMatrix = eventLikelihoods,
    # The per-cell unordered-dyad log-weights (log p(i->j) + log p(j->i), diag
    # -Inf). Ranks reduce over these rather than `eventLikelihoods` because the
    # product P(i->j)*P(j->i) underflows for low-probability dyads, collapsing
    # distinct dyads into spurious exact ties; the log-space weights stay
    # distinct and rank identically to the DyNAM_MM_default.cpp dyad softmax.
    logSymmetric = logSymmetric
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
# information into `state`. An algorithm variant overrides this method rather
# than the loop around it. `ctx` carries the loop-invariant context
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
  # Reduced-position -> global-actor slot maps for the margin reduction: an
  # entry k of the (reduced) risk set belongs to global actor `sender_slots[k]`
  # / `receiver_slots[k]`. `which(keepIn)` when an axis is filtered; identity
  # over the full node set otherwise. Captured before the second `keepIn`
  # assignment (receiver) shadows the first (sender).
  sender_slots <- NULL
  receiver_slots <- NULL
  if ((ctx$updatepresence || ctx$active_sender_folded) && !folded_full) {
    # || (updateopportunities && !is_two_mode)
    # When folded, `state$presence` already carries presence AND the sender
    # gate, so it is the sender filter directly.
    keepIn <- state$presence
    sender_slots <- which(keepIn)
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
    sender_slots <- seq_len(
      if (is_rate) nrow(statsArrayComp) else dim(statsArrayComp)[1]
    )
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
    receiver_slots <- which(keepIn)
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
    if (!is_rate) {
      receiver_slots <- seq_len(dim(statsArrayComp)[2])
    }
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
    state$EventProbabilities[[i]] <- expand_event_probabilities(
      eventValues$pMatrix,
      ctx$margin_axis,
      sender_slots,
      receiver_slots,
      ctx$n_actors1,
      ctx$n_actors2
    )
  }

  if (ctx$return_ranks || ctx$return_margins || ctx$return_total_rate) {
    state <- r_reduce_event(
      state,
      ctx,
      i,
      eventValues,
      activeDyad,
      isDependent,
      timespan,
      sender_slots,
      receiver_slots
    )
  }

  state$logLikelihood <- state$logLikelihood + eventValues$logLikelihood
  state$score <- state$score + eventValues$score
  state$informationMatrix <- state$informationMatrix +
    eventValues$informationMatrix

  state
}

# Scatter the contribution's reduced-risk-set probability vector over the whole
# node set, so that a position IS an actor id at every event regardless of which
# actors were present or in the risk set; alternatives absent from the risk set
# read 0. Without this the vector is uninterpretable downstream: the reduction
# is per event, so index 3 is a different actor at different events, and the
# reflexive receiver is dropped rather than zeroed even when nobody enters or
# leaves. The reduced -> global slot maps are the same `which(keepIn)` ones the
# margin accumulators already scatter through, which is what keeps the two
# primitives agreeing about what an index means.
expand_event_probabilities <- function(
  p_reduced,
  axis,
  sender_slots,
  receiver_slots,
  n_actors1,
  n_actors2
) {
  switch(
    axis,
    sender = {
      full <- numeric(n_actors1)
      full[sender_slots] <- as.numeric(p_reduced)
      full
    },
    receiver_given_sender = {
      full <- numeric(n_actors2)
      full[receiver_slots] <- as.numeric(p_reduced)
      full
    },
    # dyad (REM, REM_ordered) and dyad_symmetric (coordination) carry the
    # reduced n1r x n2r grid, which scatters into the whole n1 x n2 grid.
    {
      full <- matrix(0, n_actors1, n_actors2)
      full[sender_slots, receiver_slots] <- p_reduced
      full
    }
  )
}

# The per-event opt-in reductions on the r backend (ranks, margins,
# total_rate / conditional_logl), mirroring the shared C++ reduction
# (event_reductions.h) that the cpp and gather kernels run in-pass. Called from
# compute_step() with the reduced-risk-set probability vector already formed by
# the contribution, and the reduced-position -> global-actor-slot maps captured
# during the presence reduction. `w` is the flattened probability vector over
# the event's realized risk set (column-major for the two-sided dyad families,
# matching event_contribution_rate's linearization); `obs_pos` is the observed
# alternative's position in `w`, NA on a right-censored interval (no mover).
r_reduce_event <- function(
  state,
  ctx,
  i,
  eventValues,
  activeDyad,
  isDependent,
  timespan,
  sender_slots,
  receiver_slots
) {
  axis <- ctx$margin_axis
  # Coordination (DyNAM-MM) is a fourth geometry: its realized risk set is the
  # UNORDERED dyad list {a > b}, not the n1 x n2 grid (D14). Rank and margins
  # run over that list, matching DyNAM_MM_default.cpp, so it takes its own path.
  if (identical(axis, "dyad_symmetric")) {
    return(r_reduce_event_coordination(
      state,
      ctx,
      i,
      eventValues,
      activeDyad,
      isDependent,
      sender_slots
    ))
  }
  # rate / REM carry a flat `probabilities`; the multinomial families carry the
  # probability vector as `pMatrix` (a matrix for the two-sided ones).
  w <- if (!is.null(eventValues$probabilities)) {
    as.numeric(eventValues$probabilities)
  } else {
    as.numeric(eventValues$pMatrix)
  }
  n1r <- length(sender_slots)
  obs_pos <- if (isDependent) {
    switch(
      axis,
      sender = activeDyad[1],
      receiver_given_sender = activeDyad[2],
      # dyad / dyad_symmetric: column-major linear index into the n1r x n2r grid
      activeDyad[1] + (activeDyad[2] - 1L) * n1r
    )
  } else {
    NA_integer_
  }

  if (ctx$return_ranks && isDependent) {
    state$observed_rank[i] <- rank_of_observed(w, obs_pos)
  }

  if (ctx$return_total_rate && ctx$is_exact_time) {
    state$total_rate[i] <- eventValues$total_rate
    state$conditional_logl[i] <- eventValues$conditional_logl
  }

  if (ctx$return_margins) {
    two_sided <- identical(axis, "dyad")
    if (two_sided) {
      n2r <- length(receiver_slots)
      idx_i <- rep(sender_slots, times = n2r)
      idx_j <- rep(receiver_slots, each = n1r)
    } else if (identical(axis, "sender")) {
      idx_i <- sender_slots
    } else {
      idx_j <- receiver_slots
    }

    # Primary scale: compensator (Dt * total_rate) on exact-time sub-models,
    # probability (c = 1) on multinomial ones. Accumulated over ALL intervals;
    # the observed side is counted for dependent events only.
    primary_c <- if (ctx$is_exact_time) {
      timespan * eventValues$total_rate
    } else {
      1
    }
    if (identical(axis, "sender")) {
      sides <- accumulate_margins(
        w,
        primary_c,
        obs_pos,
        isDependent,
        list(list(
          index = idx_i,
          observed = state$m_obs_i,
          expected = state$m_exp_i
        ))
      )
      state$m_obs_i <- sides[[1]]$observed
      state$m_exp_i <- sides[[1]]$expected
    } else if (identical(axis, "receiver_given_sender")) {
      sides <- accumulate_margins(
        w,
        primary_c,
        obs_pos,
        isDependent,
        list(list(
          index = idx_j,
          observed = state$m_obs_j,
          expected = state$m_exp_j
        ))
      )
      state$m_obs_j <- sides[[1]]$observed
      state$m_exp_j <- sides[[1]]$expected
    } else {
      sides <- accumulate_margins(
        w,
        primary_c,
        obs_pos,
        isDependent,
        list(
          list(
            index = idx_i,
            observed = state$m_obs_i,
            expected = state$m_exp_i
          ),
          list(
            index = idx_j,
            observed = state$m_obs_j,
            expected = state$m_exp_j
          )
        )
      )
      state$m_obs_i <- sides[[1]]$observed
      state$m_exp_i <- sides[[1]]$expected
      state$m_obs_j <- sides[[2]]$observed
      state$m_exp_j <- sides[[2]]$expected
    }

    # Probability-scale variant (exact-time only), over DEPENDENT events only;
    # `dependent = FALSE` so the shared observed vectors are not double-counted.
    if (ctx$is_exact_time && isDependent) {
      if (identical(axis, "sender")) {
        sides_p <- accumulate_margins(
          w,
          1,
          obs_pos,
          FALSE,
          list(list(
            index = idx_i,
            observed = numeric(length(state$m_prob_i)),
            expected = state$m_prob_i
          ))
        )
        state$m_prob_i <- sides_p[[1]]$expected
      } else {
        sides_p <- accumulate_margins(
          w,
          1,
          obs_pos,
          FALSE,
          list(
            list(
              index = idx_i,
              observed = numeric(length(state$m_prob_i)),
              expected = state$m_prob_i
            ),
            list(
              index = idx_j,
              observed = numeric(length(state$m_prob_j)),
              expected = state$m_prob_j
            )
          )
        )
        state$m_prob_i <- sides_p[[1]]$expected
        state$m_prob_j <- sides_p[[2]]$expected
      }
    }
  }

  state
}

# Coordination (DyNAM-MM) reductions over the unordered-dyad risk set, matching
# DyNAM_MM_default.cpp. `eventValues$pMatrix` is the reduced n x n symmetric
# matrix of unordered-dyad probabilities `p_ab` (getLikelihoodMM: each cell of a
# dyad carries the same `p_ab`, the strict lower triangle sums to 1). Rank is
# over that triangle; margins credit each dyad's probability to BOTH members in
# ONE actor-set accumulator (so per-actor observed / expected each total the
# event count over the pair — 2 * n_events across the vector), exactly as the MM
# kernel does.
r_reduce_event_coordination <- function(
  state,
  ctx,
  i,
  eventValues,
  activeDyad,
  isDependent,
  sender_slots
) {
  likelihood_matrix <- eventValues$pMatrix
  lower <- lower.tri(likelihood_matrix)
  dyad_weights <- likelihood_matrix[lower]
  members <- which(lower, arr.ind = TRUE)
  member_a <- members[, 1] # the larger index (row > col)
  member_b <- members[, 2]

  obs_dyad <- NA_integer_
  if (isDependent) {
    ra <- max(activeDyad[1], activeDyad[2])
    rb <- min(activeDyad[1], activeDyad[2])
    obs_dyad <- which(member_a == ra & member_b == rb)
  }

  if (ctx$return_ranks && isDependent) {
    # Rank in log-space (see logSymmetric's doc on the coord contribution): the
    # product-space likelihoods tie underflowed dyads that the cpp kernel keeps
    # distinct.
    state$observed_rank[i] <- rank_of_observed(
      eventValues$logSymmetric[lower],
      obs_dyad,
      log_scale = TRUE
    )
  }

  if (ctx$return_margins) {
    # One actor-set accumulator; each dyad's probability credits both members,
    # so scatter the doubled (member, weight) list through the shared reduction.
    global_a <- sender_slots[member_a]
    global_b <- sender_slots[member_b]
    sides <- accumulate_margins(
      c(dyad_weights, dyad_weights),
      1,
      NA_integer_,
      FALSE,
      list(list(
        index = c(global_a, global_b),
        observed = state$m_obs_i,
        expected = state$m_exp_i
      ))
    )
    state$m_exp_i <- sides[[1]]$expected
    if (isDependent) {
      obs_actors <- sender_slots[c(activeDyad[1], activeDyad[2])]
      state$m_obs_i[obs_actors] <- state$m_obs_i[obs_actors] + 1
    }
  }

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
  return_ranks = FALSE,
  return_margins = FALSE,
  return_total_rate = FALSE,
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
    ncol(statsList$initial_stats)
  } else {
    dim(statsList$initial_stats)[3]
  }
  # Margin accumulators are per-actor over the WHOLE node set (not the
  # per-event reduced risk set), so the reduced-position -> global-slot mapping
  # `which(keepIn)` scatters into them. Sender axis = nodes; receiver / dyad
  # axes = nodes2 (two-mode) or nodes.
  n_actors1 <- nrow(nodes)
  n_actors2 <- nrow(nodes2)
  margin_axis <- risk_set_axis(spec)
  # Exact-time (Poisson) sub-models carry the compensator scale; all families
  # carry the probability scale. total_rate exists only on the Poisson kernel.
  is_exact_time <- identical(risk_set_normalizer(spec), "poisson")

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
    return_ranks = return_ranks,
    return_margins = return_margins,
    return_total_rate = return_total_rate,
    margin_axis = margin_axis,
    is_exact_time = is_exact_time,
    # Whole-node-set sizes, used to scatter the reduced per-event risk set back
    # onto actor ids: margins accumulate into them, probabilities expand to them.
    n_actors1 = n_actors1,
    n_actors2 = n_actors2,
    contribution_fn = contribution_fn
  )

  # CHANGED Marion: fill in the loop! stats array needs to be computed
  # also changes for dependent and rc events!
  state <- list(
    statsArray = statsList$initial_stats,
    time = statsList$start_time,
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
    },
    # Opt-in per-event primitives accumulated in the contribution loop, mirroring
    # the shared C++ reduction (event_reductions.h). observed_rank is NA on
    # right-censored intervals by design (no observed alternative to rank, D21).
    observed_rank = if (return_ranks) {
      rep(NA_integer_, nEvents)
    } else {
      NULL
    },
    total_rate = if (return_total_rate && is_exact_time) {
      numeric(nEvents)
    } else {
      NULL
    },
    conditional_logl = if (return_total_rate && is_exact_time) {
      numeric(nEvents)
    } else {
      NULL
    },
    # Per-actor margin accumulators over the WHOLE node set. `expected` is the
    # primary scale (compensator for exact-time, probability for multinomial);
    # `prob` is the extra probability-scale variant carried only by exact-time
    # fits (D12/D20). Which sides are populated is the risk-set axis (side i =
    # sender, side j = receiver), matching the cpp kernels.
    m_obs_i = if (return_margins) numeric(n_actors1) else NULL,
    m_exp_i = if (return_margins) numeric(n_actors1) else NULL,
    m_prob_i = if (return_margins) numeric(n_actors1) else NULL,
    m_obs_j = if (return_margins) numeric(n_actors2) else NULL,
    m_exp_j = if (return_margins) numeric(n_actors2) else NULL,
    m_prob_j = if (return_margins) numeric(n_actors2) else NULL
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
  if (return_ranks) {
    returnList$observed_rank <- state$observed_rank
  }
  if (return_total_rate && is_exact_time) {
    returnList$total_rate <- state$total_rate
    returnList$conditional_logl <- state$conditional_logl
  }
  if (return_margins) {
    returnList$margins <- assemble_r_margins(state, margin_axis, is_exact_time)
  }

  return(returnList)
}

# Shape the raw per-side margin accumulators into the result list the cpp
# backend also produces (cpp_interface.R): two accumulators for the two-sided
# families (dyad / dyad_symmetric -> sender + receiver), one for the single-
# sided ones. The primary `expected` is the compensator scale on exact-time
# fits and the probability scale on multinomial ones; exact-time fits carry the
# probability-scale variant additionally under `expected_probability*`.
assemble_r_margins <- function(state, margin_axis, is_exact_time) {
  # Only standard REM / REM_ordered (axis "dyad") are two-sided (distinct sender
  # and receiver accumulators). Coordination ("dyad_symmetric") credits both
  # endpoints into ONE actor set, so its result is single-sided like the sender
  # / receiver families.
  if (identical(margin_axis, "dyad")) {
    margins <- list(
      observed_sender = state$m_obs_i,
      expected_sender = state$m_exp_i,
      observed_receiver = state$m_obs_j,
      expected_receiver = state$m_exp_j
    )
    if (is_exact_time) {
      margins$expected_probability_sender <- state$m_prob_i
      margins$expected_probability_receiver <- state$m_prob_j
    }
  } else {
    one <- if (identical(margin_axis, "receiver_given_sender")) {
      list(obs = state$m_obs_j, exp = state$m_exp_j, prob = state$m_prob_j)
    } else {
      # sender or dyad_symmetric (coordination): the side-i accumulators.
      list(obs = state$m_obs_i, exp = state$m_exp_i, prob = state$m_prob_i)
    }
    margins <- list(observed = one$obs, expected = one$exp)
    if (is_exact_time) {
      margins$expected_probability <- one$prob
    }
  }
  margins
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
# produces the same primitives as the compiled ones. This is a *strict* mirror —
# same inputs, same outputs, pinned to the C++ by a parity test
# (test-event_reductions.R) on constructed values, which is what keeps a mirror
# honest. (`stable_softmax()` above is only an algorithmic cousin of
# `log_sum_exp_masked()`: same max-shift, different arguments and returns.)
#
# Same contract as the header: `w` is a nonnegative weight vector over the
# event's risk set and ZERO outside it, `c` the scale whose product `c * w` is
# each alternative's contribution. Callers pass the probability vector as `w` in
# every family, with `c = 1` for the probability scale and `c = Dt * total_rate`
# for the exact-time compensator. `observed` indexes `w`, 1-based here as R code
# reads.

# Relative tolerance of the rank tie rule. MUST hold the same number as
# `RANK_TIE_TOL` in `src/event_reductions.h`, which documents why the tolerance
# is relative rather than absolute (the kernels rank proportional but
# differently scaled vectors) and how it was sized.
RANK_TIE_TOL <- 1e-12

# 1 + the number of alternatives whose weight exceeds the observed one by more
# than `RANK_TIE_TOL` relatively; ties -- exact ones and near-ties within the
# tolerance -- share the better rank. Scale-free, so `c` never enters. On the
# log scale (`log_scale = TRUE`, which the coordination path ranks on) the same
# rule is additive, since log(1 + tol) = tol to first order.
rank_of_observed <- function(w, observed, log_scale = FALSE) {
  threshold <- if (log_scale) {
    w[observed] + RANK_TIE_TOL
  } else {
    w[observed] * (1 + RANK_TIE_TOL)
  }
  1L + sum(w > threshold)
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
#' The one transformation estimation applies to a preprocessed object:
#' prepending the constant rate-intercept statistic (a dummy for the theta_0
#' parameter) when the model carries a time intercept. Replaces
#' `modifyStatisticsList()` in the estimation entries; the right-censoring and
#' array reductions of `reduceStatisticsList()` were no-ops at those call
#' sites.
#'
#' @param statsList a `preprocessed.goldfish` object.
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
  addInterceptEffect = FALSE,
  is_sender = FALSE
) {
  is_sender_stats <- isTRUE(is_sender)
  if (addInterceptEffect) {
    dimensions <- dim(statsList$initial_stats)
    statsList$initial_stats <- if (is_sender_stats) {
      cbind(1, statsList$initial_stats)
    } else {
      array(
        c(matrix(1, dimensions[1], dimensions[2]), statsList$initial_stats),
        dim = dimensions + c(0, 0, 1)
      )
    }
  }
  statsList
}
