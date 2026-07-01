##################### ###
#
# Goldfish package
# Internal estimation routine
#
##################### ###

# Estimation
estimate_c_int <- function(
  statsList,
  nodes,
  nodes2,
  modelTypeCall = c(
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
  testing = FALSE,
  get_data_matrix = FALSE,
  impute = FALSE,
  opportunitiesList = NULL,
  engine = c("default_c", "gather_compute")
) {
  if (!is.null(opportunitiesList)) {
    stop(
      "opportunitiesList is not supported in the C interface.",
      call. = FALSE
    )
  }

  minDampingFactor <- initialDamping
  # CHANGED MARION
  # nParams: number of effects + 1 (if has intercept)
  is_rate_model <- modelTypeCall %in% c("DyNAM-M-Rate", "DyNAM-M-Rate-ordered")
  nParams <- (if (is_rate_model) {
    ncol(statsList$initialStats)
  } else {
    dim(statsList$initialStats)[3]
  }) -
    length(excludeParameters) +
    hasIntercept
  #

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

  modelTypeCall <- match.arg(modelTypeCall)
  engine <- match.arg(engine)

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
    addInterceptEffect = hasIntercept
  )

  ## PRESENCE UPDATES PRECOMPUTED DURING PREPROCESSING
  presence1_update <- statsList$presence1_update
  presence1_update_pointer <- statsList$presence1_update_pointer
  if (is.null(presence1_update)) {
    presence1_update <- matrix(0, 0, 0)
    presence1_update_pointer <- numeric(1)
  }

  presence2_update <- statsList$presence2_update
  presence2_update_pointer <- statsList$presence2_update_pointer
  if (is.null(presence2_update)) {
    presence2_update <- matrix(0, 0, 0)
    presence2_update_pointer <- numeric(1)
  }

  presence1_init <- statsList$active_mode1_init
  presence2_init <- statsList$active_mode2_init

  nEvents <- length(statsList$is_dependent)

  ## ADD INTERCEPT
  # CHANGED MARION
  # replace first parameter with an initial estimate of the intercept
  if (
    modelTypeCall %in%
      c("REM", "DyNAM-M-Rate") &&
      hasIntercept &&
      is.null(initialParameters) &&
      (is.null(fixedParameters) || is.na(fixedParameters[1]))
  ) {
    parameters[1] <- log(
      statsList$n_dep_events /
        statsList$total_time /
        statsList$avg_active_actors
    )
  }
  ## SET VARIABLES BASED ON STATSLIST
  twomode_or_reflexive <- (allowReflexive || is_two_mode)
  if (is_rate_model) {
    n_parameters <- ncol(statsList$initialStats)
    n_actors1 <- nrow(statsList$initialStats)
    n_actors2 <- 1L
    twomode_or_reflexive <- TRUE
  } else {
    n_parameters <- dim(statsList$initialStats)[3]
    n_actors1 <- dim(statsList$initialStats)[1]
    n_actors2 <- dim(statsList$initialStats)[2]
  }

  ## CONVERT UPDATES INTO THE FORMAT ACCEPTED BY C FUNCTIONS
  stat_mat_update <- statsList$stat_mat_update
  stat_mat_update_pointer <- statsList$stat_mat_pointer
  stat_mat_broadcast <- statsList$stat_mat_broadcast
  stat_mat_broadcast_pointer <- statsList$stat_mat_broadcast_pointer
  if (is.null(stat_mat_broadcast)) {
    stat_mat_broadcast <- matrix(0, 4L, 0L)
    stat_mat_broadcast_pointer <- numeric(length(stat_mat_update_pointer))
  }
  if (hasIntercept) {
    stat_mat_update[3, ] <- stat_mat_update[3, ] + 1
    if (ncol(stat_mat_broadcast) > 0L) {
      stat_mat_broadcast[3, ] <- stat_mat_broadcast[3, ] + 1
    }
  }

  ## CONVERT TYPES OF EVENTS AND TIMESPANS INTO THE FORMAT ACCEPTED
  ## BY C FUNCTIONS
  if (modelTypeCall %in% c("DyNAM-M-Rate", "REM")) {
    is_dependent <- as.logical(statsList$is_dependent)
    timespan <- statsList$intervals
  } else if (
    modelTypeCall %in%
      c("DyNAM-M-Rate-ordered", "REM-ordered", "DyNAM-MM")
  ) {
    is_dependent <- as.logical(statsList$is_dependent)
  } else {
    timespan <- NA
  }

  ## CONVERT INFOS OF SENDERS AND RECEIVERS INTO THE FORMAT ACCEPTED
  ##  BY C FUNCTIONS
  event_mat <- rbind(statsList$event_sender, statsList$event_receiver)

  ## CONVERT THE INITIALIZATION OF DATA MATRIX INTO THE FORMAT ACCEPTED
  ##  BY C FUNCTIONS
  if (is_rate_model) {
    stat_mat_init <- statsList$initialStats
  } else {
    stat_mat_init <- matrix(0, n_actors1 * n_actors2, n_parameters)
    for (i in seq_len(n_parameters)) {
      stat_mat_init[, i] <- t(statsList$initialStats[,, i])
    }
  }

  ## ESTIMATION: INITIALIZATION

  if (verbose) {
    cat("Estimating model type", modelTypeCall, ".\n")
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

  ## GATHERING INFO IF WE USE THE GATHER-COMPUTE ENGINE.
  if (engine == "gather_compute") {
    gathered_data <- gather_(
      modelTypeCall = modelTypeCall,
      event_mat = event_mat,
      timespan = timespan,
      is_dependent = is_dependent,
      stat_mat_init = stat_mat_init,
      stat_mat_update = stat_mat_update,
      stat_mat_update_pointer = stat_mat_update_pointer,
      stat_mat_broadcast = stat_mat_broadcast,
      stat_mat_broadcast_pointer = stat_mat_broadcast_pointer,
      presence1_init = presence1_init,
      presence1_update = presence1_update,
      presence1_update_pointer = presence1_update_pointer,
      presence2_init = presence2_init,
      presence2_update = presence2_update,
      presence2_update_pointer = presence2_update_pointer,
      n_actors1 = n_actors1,
      n_actors2 = n_actors2,
      twomode_or_reflexive = twomode_or_reflexive,
      verbose = progress, # output the progress of data gathering
      impute = impute
    )
    size_gathered_data <- utils::object.size(gathered_data)
  }

  while (TRUE) {
    ## CALCULATE THE LOGLIKELIHOOD,
    ## THE FISHER INFORMATION MATRIX, AND THE DERIVATIVE

    ## GATHER-COMPUTE ENGINE
    if (engine == "gather_compute") {
      res <- compute_(
        modelTypeCall = modelTypeCall,
        parameters = parameters,
        stat_all_events = gathered_data$stat_all_events,
        selected = gathered_data$selected,
        selected_actor1 = gathered_data$selected_actor1,
        selected_actor2 = gathered_data$selected_actor2,
        n_candidates = gathered_data$n_candidates,
        n_candidates1 = gathered_data$n_candidates1,
        n_candidates2 = gathered_data$n_candidates2,
        timespan = timespan,
        is_dependent = is_dependent,
        twomode_or_reflexive = twomode_or_reflexive
      )
    }

    ### DEFAULT_C ENGINE
    if (engine == "default_c") {
      res <- estimate_(
        modelTypeCall = modelTypeCall,
        parameters = parameters,
        event_mat = event_mat,
        timespan = timespan,
        is_dependent = is_dependent,
        stat_mat_init = stat_mat_init,
        stat_mat_update = stat_mat_update,
        stat_mat_update_pointer = stat_mat_update_pointer,
        stat_mat_broadcast = stat_mat_broadcast,
        stat_mat_broadcast_pointer = stat_mat_broadcast_pointer,
        presence1_init = presence1_init,
        presence1_update = presence1_update,
        presence1_update_pointer = presence1_update_pointer,
        presence2_init = presence2_init,
        presence2_update = presence2_update,
        presence2_update_pointer = presence2_update_pointer,
        n_actors1 = n_actors1,
        n_actors2 = n_actors2,
        twomode_or_reflexive = twomode_or_reflexive,
        impute = impute
      )
    }

    logLikelihood <- res$logLikelihood
    score <- as.numeric(res$derivative)
    informationMatrix <- res$fisher
    if (returnIntervalLogL) {
      intervalLogL <- as.numeric(res$intervalLogL)
    }

    if (returnEventProbabilities) {
      eventProbabilities <- if (is.null(res$pMatrix)) {
        paste("not implemented for model type", modelTypeCall)
      } else {
        res$pMatrix
      }
    }

    if (
      isInitialEstimation &&
        any(is.na(unlist(res))) &&
        !all(parameters[-1] == 0)
    ) {
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
    #  It's for the fixing parameter feature. \
    score[idFixedCompnents] <- 0

    if (verbose) {
      cat(
        "\n\nLikelihood: ",
        logLikelihood,
        " in iteration ",
        iIteration,
        "\n_parameters: ",
        toString(parameters),
        "\nScore: ",
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
        minDampingFactor /
          ifelse(isInitialEstimation, 1, dampingDecreaseFactor)
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
        " probably due to collinearity between parameters.",
        call. = FALSE
      )
    }

    update <- rep(0, nParams)
    update[idUnfixedCompnents] <-
      (inverseInformationUnfixed %*% score[idUnfixedCompnents]) /
      dampingFactor

    if (verbose) {
      cat(
        "\nUpdate: ",
        toString(update),
        "\nDamping factor:",
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
        "Max step: ",
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
  if (engine == "gather_compute") {
    estimationResult$sizeIntermediate <- size_gathered_data
    if (testing) estimationResult$intermediate <- gathered_data
  }
  # if (testing) estimationResult$intermediateData <-
  #  DataMatrixAndId$intermediate_data
  if (returnIntervalLogL) {
    estimationResult$intervalLogL <- intervalLogL
  }
  if (returnEventProbabilities) {
    estimationResult$eventProbabilities <- eventProbabilities
  }
  attr(estimationResult, "class") <- "result.goldfish"
  estimationResult
}

## ESTIMATE FOR DIFFERENT MODELS
estimate_ <- function(
  modelTypeCall,
  parameters,
  event_mat,
  timespan,
  is_dependent,
  stat_mat_init,
  stat_mat_update,
  stat_mat_update_pointer,
  stat_mat_broadcast,
  stat_mat_broadcast_pointer,
  presence1_init,
  presence1_update,
  presence1_update_pointer,
  presence2_init,
  presence2_update,
  presence2_update_pointer,
  n_actors1,
  n_actors2,
  twomode_or_reflexive,
  impute
) {
  if (modelTypeCall == "DyNAM-MM") {
    res <- estimate_DyNAM_MM(
      parameters,
      event_mat,
      stat_mat_init,
      stat_mat_update,
      stat_mat_update_pointer,
      stat_mat_broadcast,
      stat_mat_broadcast_pointer,
      presence1_init,
      presence1_update,
      presence1_update_pointer,
      presence2_init,
      presence2_update,
      presence2_update_pointer,
      n_actors1,
      n_actors2,
      twomode_or_reflexive,
      impute
    )
  }

  if (modelTypeCall == "DyNAM-M") {
    res <- estimate_DyNAM_choice(
      parameters,
      event_mat,
      stat_mat_init,
      stat_mat_update,
      stat_mat_update_pointer,
      stat_mat_broadcast,
      stat_mat_broadcast_pointer,
      presence2_init,
      presence2_update,
      presence2_update_pointer,
      n_actors1,
      n_actors2,
      twomode_or_reflexive,
      impute
    )
  }

  if (modelTypeCall == "REM-ordered") {
    res <- estimate_REM_ordered(
      parameters,
      event_mat,
      stat_mat_init,
      stat_mat_update,
      stat_mat_update_pointer,
      stat_mat_broadcast,
      stat_mat_broadcast_pointer,
      presence1_init,
      presence1_update,
      presence1_update_pointer,
      presence2_init,
      presence2_update,
      presence2_update_pointer,
      n_actors1,
      n_actors2,
      twomode_or_reflexive,
      impute
    )
  }

  if (modelTypeCall == "REM") {
    res <- estimate_REM(
      parameters,
      event_mat,
      timespan,
      is_dependent,
      stat_mat_init,
      stat_mat_update,
      stat_mat_update_pointer,
      stat_mat_broadcast,
      stat_mat_broadcast_pointer,
      presence1_init,
      presence1_update,
      presence1_update_pointer,
      presence2_init,
      presence2_update,
      presence2_update_pointer,
      n_actors1,
      n_actors2,
      twomode_or_reflexive,
      impute
    )
  }

  if (modelTypeCall == "DyNAM-M-Rate") {
    res <- estimate_DyNAM_rate(
      parameters,
      event_mat,
      timespan,
      is_dependent,
      stat_mat_init,
      stat_mat_update,
      stat_mat_update_pointer,
      stat_mat_broadcast,
      stat_mat_broadcast_pointer,
      presence1_init,
      presence1_update,
      presence1_update_pointer,
      presence2_init,
      presence2_update,
      presence2_update_pointer,
      n_actors1,
      n_actors2,
      twomode_or_reflexive,
      impute
    )
  }

  if (modelTypeCall == "DyNAM-M-Rate-ordered") {
    res <- estimate_DyNAM_rate_ordered(
      parameters,
      event_mat,
      stat_mat_init,
      stat_mat_update,
      stat_mat_update_pointer,
      stat_mat_broadcast,
      stat_mat_broadcast_pointer,
      presence1_init,
      presence1_update,
      presence1_update_pointer,
      presence2_init,
      presence2_update,
      presence2_update_pointer,
      n_actors1,
      n_actors2,
      twomode_or_reflexive,
      impute
    )
  }
  return(res)
}


## GATHER FOR DIFFERENT MODELS
#'
#' Native R expansion of the flat preprocessing buffer into the gather stack
#' (one row per event x alternative). This replaces the former C++ routines
#' `gather_sender_model()`, `gather_receiver_model()`, and
#' `gather_sender_receiver_model()` (removed in the writer refactor): the
#' gather stack is now produced once, in R, from `writer_default()`'s flat
#' output and consumed by both `engine = "gather_compute"` and
#' `gather_model_data()`. The per-iteration model fitting stays in C++ via
#' `compute_()`, so the one-time expansion in R does not affect the
#' estimation hot path. The `verbose` / `impute` arguments are retained for
#' call-site compatibility; `impute` is always `FALSE` in the current
#' code paths (the impute machinery was dropped from estimation).
#' @noRd
gather_ <- function(
  modelTypeCall,
  event_mat,
  timespan,
  is_dependent,
  stat_mat_init,
  stat_mat_update,
  stat_mat_update_pointer,
  stat_mat_broadcast,
  stat_mat_broadcast_pointer,
  presence1_init,
  presence1_update,
  presence1_update_pointer,
  presence2_init,
  presence2_update,
  presence2_update_pointer,
  n_actors1,
  n_actors2,
  twomode_or_reflexive,
  verbose,
  impute
) {
  if (modelTypeCall %in% c("REM-ordered", "REM", "DyNAM-MM")) {
    # For DyNAM-MM, we deal with twomode_or_reflexive in the estimation
    # for convenience.
    if (modelTypeCall == "DyNAM-MM") {
      twomode_or_reflexive <- TRUE
    }
    gathered_data <- gather_sender_receiver_model_r(
      event_mat,
      is_dependent,
      stat_mat_init,
      stat_mat_update,
      stat_mat_update_pointer,
      stat_mat_broadcast,
      stat_mat_broadcast_pointer,
      presence1_init,
      presence1_update,
      presence1_update_pointer,
      presence2_init,
      presence2_update,
      presence2_update_pointer,
      n_actors1,
      n_actors2,
      twomode_or_reflexive
    )
  } else if (modelTypeCall == "DyNAM-M") {
    gathered_data <- gather_receiver_model_r(
      event_mat,
      stat_mat_init,
      stat_mat_update,
      stat_mat_update_pointer,
      stat_mat_broadcast,
      stat_mat_broadcast_pointer,
      presence2_init,
      presence2_update,
      presence2_update_pointer,
      n_actors1,
      n_actors2,
      twomode_or_reflexive
    )
  } else if (modelTypeCall %in% c("DyNAM-M-Rate-ordered", "DyNAM-M-Rate")) {
    gathered_data <- gather_sender_model_r(
      event_mat,
      is_dependent,
      stat_mat_init,
      stat_mat_update,
      stat_mat_update_pointer,
      stat_mat_broadcast,
      stat_mat_broadcast_pointer,
      presence1_init,
      presence1_update,
      presence1_update_pointer,
      presence2_init,
      presence2_update,
      presence2_update_pointer,
      n_actors1,
      n_actors2,
      twomode_or_reflexive
    )
  }

  return(gathered_data)
}

# Apply the slice of flat stat updates for one event, in place.
# `upd` is the 4 x k flat buffer (rows node1, node2, effect, replace;
# all 0-indexed); columns `(from + 1):to` (1-indexed) are applied. Later
# writes to the same cell win, matching the sequential C++ assignment.
.gather_apply_stat <- function(stat_mat, upd, from, to, n2) {
  if (to <= from) {
    return(stat_mat)
  }
  cols <- (from + 1L):to
  rr <- upd[1, cols] * n2 + upd[2, cols] + 1
  cc <- upd[3, cols] + 1
  stat_mat[cbind(rr, cc)] <- upd[4, cols]
  stat_mat
}

# Apply the slice of broadcast (constant-value fan-out) updates for one event,
# in place, on the flattened n1*n2 x p `stat_mat`. `bc` is the 4 x M broadcast
# buffer (rows kind, fixed, effect, replace; fixed and effect 0-indexed);
# columns `(from + 1):to` (1-indexed) are applied. Mirrors the C++
# apply_broadcast_updates(): kind 1 over senders holding alter `fixed`, kind 2
# over alters holding ego `fixed`, kind 3 over all actors, skipping the
# reflexive diagonal cell when `!twomode_or_reflexive`.
.gather_apply_broadcast <- function(
  stat_mat,
  bc,
  from,
  to,
  n1,
  n2,
  twomode_or_reflexive
) {
  if (to <= from) {
    return(stat_mat)
  }
  for (b in (from + 1L):to) {
    kind <- bc[1, b]
    fixed <- bc[2, b]
    effect <- bc[3, b] + 1L
    value <- bc[4, b]
    if (kind == 1L) {
      rows <- if (twomode_or_reflexive) {
        seq_len(n1) - 1L
      } else {
        setdiff(seq_len(n1) - 1L, fixed)
      }
      stat_mat[rows * n2 + fixed + 1L, effect] <- value
    } else if (kind == 2L) {
      cols <- if (twomode_or_reflexive) {
        seq_len(n2) - 1L
      } else {
        setdiff(seq_len(n2) - 1L, fixed)
      }
      stat_mat[fixed * n2 + cols + 1L, effect] <- value
    } else {
      ij <- expand.grid(i = seq_len(n1) - 1L, j = seq_len(n2) - 1L)
      if (!twomode_or_reflexive) {
        ij <- ij[ij$i != ij$j, ]
      }
      stat_mat[ij$i * n2 + ij$j + 1L, effect] <- value
    }
  }
  stat_mat
}

# Apply the slice of presence (composition-change) updates for one event.
# `upd` row 1 is the 1-indexed node, row 2 the replacement value.
.gather_apply_presence <- function(presence, upd, from, to) {
  if (to <= from) {
    return(presence)
  }
  cols <- (from + 1L):to
  presence[upd[1, cols]] <- upd[2, cols]
  presence
}

# Reduce a n1*n2 x p stacked stat matrix to n1 x p by averaging over the
# receiver block of each sender (rate models). Mirrors the C++
# reduce_mat_to_vector(): when receivers are restricted (one-mode, not
# reflexive) the sender's own diagonal row is dropped and the average is
# over n2 - 1 receivers.
.gather_reduce <- function(stat_mat, n1, n2, twomode_or_reflexive) {
  if (n2 == 1L) {
    return(stat_mat)
  }
  n_parameters <- ncol(stat_mat)
  reduced <- matrix(0, n1, n_parameters)
  for (i in seq_len(n1)) {
    id_start <- (i - 1L) * n2
    block <- stat_mat[(id_start + 1L):(id_start + n2), , drop = FALSE]
    temp <- colSums(block)
    if (!twomode_or_reflexive) {
      temp <- temp - stat_mat[id_start + i, ]
      reduced[i, ] <- temp / (n2 - 1L)
    } else {
      reduced[i, ] <- temp / n2
    }
  }
  reduced
}

# Gather data for the sender-receiver models (REM, REM-ordered, DyNAM-MM):
# all present sender-receiver pairs become rows.
gather_sender_receiver_model_r <- function(
  event_mat,
  is_dependent,
  stat_mat_init,
  stat_mat_update,
  stat_mat_update_pointer,
  stat_mat_broadcast,
  stat_mat_broadcast_pointer,
  presence1_init,
  presence1_update,
  presence1_update_pointer,
  presence2_init,
  presence2_update,
  presence2_update_pointer,
  n_actors1,
  n_actors2,
  twomode_or_reflexive
) {
  stat_mat <- stat_mat_init
  n_events <- length(is_dependent)
  n_parameters <- ncol(stat_mat)
  has_cc1 <- length(presence1_update) > 0
  has_cc2 <- length(presence2_update) > 0
  presence1 <- presence1_init
  presence2 <- presence2_init
  update_id <- 0L
  bc_id <- 0L
  p1_id <- 0L
  p2_id <- 0L

  rows_list <- vector("list", n_events)
  selected <- numeric(n_events)
  selected_actor1 <- numeric(n_events)
  selected_actor2 <- numeric(n_events)
  n_candidates <- numeric(n_events)
  n_candidates1 <- numeric(n_events)
  n_candidates2 <- numeric(n_events)

  for (e in seq_len(n_events)) {
    ptr <- stat_mat_update_pointer[e]
    stat_mat <- .gather_apply_stat(
      stat_mat,
      stat_mat_update,
      update_id,
      ptr,
      n_actors2
    )
    update_id <- ptr
    bc_ptr <- stat_mat_broadcast_pointer[e]
    stat_mat <- .gather_apply_broadcast(
      stat_mat,
      stat_mat_broadcast,
      bc_id,
      bc_ptr,
      n_actors1,
      n_actors2,
      twomode_or_reflexive
    )
    bc_id <- bc_ptr
    if (has_cc1) {
      ptr1 <- presence1_update_pointer[e]
      presence1 <- .gather_apply_presence(
        presence1,
        presence1_update,
        p1_id,
        ptr1
      )
      p1_id <- ptr1
    }
    if (has_cc2) {
      ptr2 <- presence2_update_pointer[e]
      presence2 <- .gather_apply_presence(
        presence2,
        presence2_update,
        p2_id,
        ptr2
      )
      p2_id <- ptr2
    }

    id_sender <- event_mat[1, e] - 1L
    id_receiver <- event_mat[2, e] - 1L
    is_dep <- is_dependent[e]

    present1_ids <- which(presence1 == 1) - 1L
    present2_ids <- which(presence2 == 1) - 1L

    idx <- integer(0)
    n_present <- 0L
    n_p1 <- 0L
    n_p2_last <- 0L
    for (i in present1_ids) {
      not_allowed <- if (!twomode_or_reflexive) i else -1L
      allowed <- present2_ids[present2_ids != not_allowed]
      n_all <- length(allowed)
      idx <- c(idx, i * n_actors2 + allowed + 1L)
      if (is_dep && i == id_sender) {
        hit <- which(allowed == id_receiver)
        if (length(hit) > 0) {
          selected[e] <- n_present + (hit - 1L)
          selected_actor1[e] <- n_p1
          selected_actor2[e] <- hit - 1L
        }
      }
      n_present <- n_present + n_all
      n_p1 <- n_p1 + 1L
      n_p2_last <- n_all
    }
    rows_list[[e]] <- stat_mat[idx, , drop = FALSE]
    n_candidates[e] <- n_present
    n_candidates1[e] <- n_p1
    n_candidates2[e] <- n_p2_last
  }

  stat_all_events <- do.call(rbind, rows_list)
  if (is.null(stat_all_events)) {
    stat_all_events <- matrix(0, 0, n_parameters)
  }
  list(
    stat_all_events = stat_all_events,
    n_candidates = n_candidates,
    n_candidates1 = n_candidates1,
    n_candidates2 = n_candidates2,
    selected = selected,
    selected_actor1 = selected_actor1,
    selected_actor2 = selected_actor2
  )
}

# Gather data for the receiver model (DyNAM-M choice): for each event only the
# present receivers of the event's sender become rows.
gather_receiver_model_r <- function(
  event_mat,
  stat_mat_init,
  stat_mat_update,
  stat_mat_update_pointer,
  stat_mat_broadcast,
  stat_mat_broadcast_pointer,
  presence2_init,
  presence2_update,
  presence2_update_pointer,
  n_actors1,
  n_actors2,
  twomode_or_reflexive
) {
  stat_mat <- stat_mat_init
  n_events <- ncol(event_mat)
  n_parameters <- ncol(stat_mat)
  has_cc2 <- length(presence2_update) > 0
  presence2 <- presence2_init
  update_id <- 0L
  bc_id <- 0L
  p2_id <- 0L

  rows_list <- vector("list", n_events)
  selected <- numeric(n_events)
  n_candidates <- numeric(n_events)

  for (e in seq_len(n_events)) {
    ptr <- stat_mat_update_pointer[e]
    stat_mat <- .gather_apply_stat(
      stat_mat,
      stat_mat_update,
      update_id,
      ptr,
      n_actors2
    )
    update_id <- ptr
    bc_ptr <- stat_mat_broadcast_pointer[e]
    stat_mat <- .gather_apply_broadcast(
      stat_mat,
      stat_mat_broadcast,
      bc_id,
      bc_ptr,
      n_actors1,
      n_actors2,
      twomode_or_reflexive
    )
    bc_id <- bc_ptr
    if (has_cc2) {
      ptr2 <- presence2_update_pointer[e]
      presence2 <- .gather_apply_presence(
        presence2,
        presence2_update,
        p2_id,
        ptr2
      )
      p2_id <- ptr2
    }

    id_sender <- event_mat[1, e] - 1L
    id_receiver <- event_mat[2, e] - 1L
    not_allowed <- if (!twomode_or_reflexive) id_sender else -1L
    present2_ids <- which(presence2 == 1) - 1L
    allowed <- present2_ids[present2_ids != not_allowed]
    idx <- id_sender * n_actors2 + allowed + 1L
    rows_list[[e]] <- stat_mat[idx, , drop = FALSE]
    hit <- which(allowed == id_receiver)
    if (length(hit) > 0) {
      selected[e] <- hit - 1L
    }
    n_candidates[e] <- length(allowed)
  }

  stat_all_events <- do.call(rbind, rows_list)
  if (is.null(stat_all_events)) {
    stat_all_events <- matrix(0, 0, n_parameters)
  }
  list(
    stat_all_events = stat_all_events,
    n_candidates = n_candidates,
    selected = selected
  )
}

# Gather data for the sender models (DyNAM-M-Rate, DyNAM-M-Rate-ordered): the
# stacked stat matrix is reduced per sender, and present senders become rows.
gather_sender_model_r <- function(
  event_mat,
  is_dependent,
  stat_mat_init,
  stat_mat_update,
  stat_mat_update_pointer,
  stat_mat_broadcast,
  stat_mat_broadcast_pointer,
  presence1_init,
  presence1_update,
  presence1_update_pointer,
  presence2_init,
  presence2_update,
  presence2_update_pointer,
  n_actors1,
  n_actors2,
  twomode_or_reflexive
) {
  stat_mat <- stat_mat_init
  n_events <- ncol(event_mat)
  n_parameters <- ncol(stat_mat)
  has_cc1 <- length(presence1_update) > 0
  has_cc2 <- length(presence2_update) > 0
  presence1 <- presence1_init
  presence2 <- presence2_init
  update_id <- 0L
  bc_id <- 0L
  p1_id <- 0L
  p2_id <- 0L

  rows_list <- vector("list", n_events)
  selected <- numeric(n_events)
  n_candidates <- numeric(n_events)

  for (e in seq_len(n_events)) {
    ptr <- stat_mat_update_pointer[e]
    stat_mat <- .gather_apply_stat(
      stat_mat,
      stat_mat_update,
      update_id,
      ptr,
      n_actors2
    )
    update_id <- ptr
    bc_ptr <- stat_mat_broadcast_pointer[e]
    stat_mat <- .gather_apply_broadcast(
      stat_mat,
      stat_mat_broadcast,
      bc_id,
      bc_ptr,
      n_actors1,
      n_actors2,
      twomode_or_reflexive
    )
    bc_id <- bc_ptr
    if (has_cc1) {
      ptr1 <- presence1_update_pointer[e]
      presence1 <- .gather_apply_presence(
        presence1,
        presence1_update,
        p1_id,
        ptr1
      )
      p1_id <- ptr1
    }
    if (has_cc2) {
      ptr2 <- presence2_update_pointer[e]
      presence2 <- .gather_apply_presence(
        presence2,
        presence2_update,
        p2_id,
        ptr2
      )
      p2_id <- ptr2
    }

    reduced <- .gather_reduce(
      stat_mat,
      n_actors1,
      n_actors2,
      twomode_or_reflexive
    )
    id_sender <- event_mat[1, e] - 1L
    is_dep <- is_dependent[e]
    present1_ids <- which(presence1 == 1) - 1L
    rows_list[[e]] <- reduced[present1_ids + 1L, , drop = FALSE]
    if (is_dep) {
      hit <- which(present1_ids == id_sender)
      if (length(hit) > 0) {
        selected[e] <- hit - 1L
      }
    }
    n_candidates[e] <- length(present1_ids)
  }

  stat_all_events <- do.call(rbind, rows_list)
  if (is.null(stat_all_events)) {
    stat_all_events <- matrix(0, 0, n_parameters)
  }
  list(
    stat_all_events = stat_all_events,
    n_candidates = n_candidates,
    selected = selected
  )
}


## COMPUTE FOR DIFFERENT MODELS
compute_ <- function(
  modelTypeCall,
  parameters,
  stat_all_events,
  selected,
  selected_actor1,
  selected_actor2,
  n_candidates,
  n_candidates1,
  n_candidates2,
  timespan,
  is_dependent,
  twomode_or_reflexive
) {
  if (modelTypeCall %in% c("DyNAM-M", "REM-ordered", "DyNAM-M-Rate-ordered")) {
    res <- compute_multinomial_selection(
      parameters,
      stat_all_events,
      n_candidates,
      selected
    )
  }

  if (modelTypeCall %in% c("DyNAM-M-Rate", "REM")) {
    res <- compute_poisson_selection(
      parameters,
      stat_all_events,
      n_candidates,
      selected,
      timespan,
      is_dependent
    )
  }

  if (modelTypeCall == "DyNAM-MM") {
    res <- compute_coordination_selection(
      parameters,
      stat_all_events,
      n_candidates,
      n_candidates1,
      n_candidates2,
      selected,
      selected_actor1,
      selected_actor2,
      twomode_or_reflexive
    )
  }

  return(res)
}
