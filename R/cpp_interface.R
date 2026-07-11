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
  senderGate = NULL,
  remMask = NULL,
  supportMask = NULL,
  engine = c("default_c", "gather_compute")
) {
  if (!is.null(opportunitiesList)) {
    stop(
      "opportunitiesList is not supported in the C interface.",
      call. = FALSE
    )
  }
  # gather_compute consumes the mask via the R gather; default_c consumes it in
  # the per-model C++ estimator, wired for DyNAM-M (choice) only. Other
  # combinations are a caller error (they downgrade to the default engine
  # upstream).
  default_c_support_ok <- identical(modelTypeCall, "DyNAM-M")
  if (
    (!is.null(senderGate) || !is.null(remMask)) ||
      (!is.null(supportMask) &&
        identical(engine, "default_c") &&
        !default_c_support_ok)
  ) {
    stop(
      "support_constraint is not supported in this C interface engine.",
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
  active_sender_update <- statsList$active_sender_update
  active_sender_update_pointer <- statsList$active_sender_update_pointer
  if (is.null(active_sender_update)) {
    active_sender_update <- matrix(0, 0, 0)
    active_sender_update_pointer <- numeric(1)
  }

  active_dyad_update <- statsList$active_dyad_update
  active_dyad_update_pointer <- statsList$active_dyad_update_pointer
  if (is.null(active_dyad_update)) {
    active_dyad_update <- matrix(0, 0, 0)
    active_dyad_update_pointer <- numeric(1)
  }

  active_sender_init <- statsList$active_sender_init
  active_dyad_init <- statsList$active_dyad_init
  active_dyad_encoding <- if (is.null(statsList$active_dyad_encoding)) {
    "alter"
  } else {
    statsList$active_dyad_encoding
  }

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
        statsList$avg_active_entity
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
      active_sender_init = active_sender_init,
      active_sender_update = active_sender_update,
      active_sender_update_pointer = active_sender_update_pointer,
      active_dyad_init = active_dyad_init,
      active_dyad_update = active_dyad_update,
      active_dyad_update_pointer = active_dyad_update_pointer,
      n_actors1 = n_actors1,
      n_actors2 = n_actors2,
      twomode_or_reflexive = twomode_or_reflexive,
      verbose = progress, # output the progress of data gathering
      impute = impute,
      support = supportMask,
      active_dyad_encoding = active_dyad_encoding
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
        n_candidates = gathered_data$n_candidates,
        timespan = timespan,
        is_dependent = is_dependent,
        twomode_or_reflexive = twomode_or_reflexive,
        sender_of_row = gathered_data$sender_of_row,
        dyad_partner = gathered_data$dyad_partner
      )
    }

    ### DEFAULT_C ENGINE
    if (engine == "default_c") {
      # DyNAM-M (choice) and REM consume the folded `active_dyad` directly (design
      # D7): the C++ estimator maintains it from its flat buffer and reads the
      # event dyad's availability. At the point encoding `active_dyad_init` is a
      # dense n1 x n2 mask (choice: folded receiver presence n support n
      # opportunity; REM: both presences n support); flatten it sender-major (dyad
      # (i, j) at (i - 1) * n2 + j) so the estimator maintains it via the
      # (node1, node2, replace) buffer and reads cell-wise. Otherwise it is the
      # length-n2 receiver vector (choice alter / REM outer) consumed directly.
      dyad_is_point <- modelTypeCall %in%
        c("DyNAM-M", "REM", "REM-ordered", "DyNAM-MM") &&
        identical(active_dyad_encoding, "point")
      dyad_init_c <- if (dyad_is_point) {
        as.vector(t(active_dyad_init))
      } else {
        active_dyad_init
      }
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
        active_sender_init = active_sender_init,
        active_sender_update = active_sender_update,
        active_sender_update_pointer = active_sender_update_pointer,
        active_dyad_init = dyad_init_c,
        active_dyad_update = active_dyad_update,
        active_dyad_update_pointer = active_dyad_update_pointer,
        n_actors1 = n_actors1,
        n_actors2 = n_actors2,
        twomode_or_reflexive = twomode_or_reflexive,
        impute = impute,
        active_dyad_is_point = dyad_is_point
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
  active_sender_init,
  active_sender_update,
  active_sender_update_pointer,
  active_dyad_init,
  active_dyad_update,
  active_dyad_update_pointer,
  n_actors1,
  n_actors2,
  twomode_or_reflexive,
  impute,
  active_dyad_is_point = FALSE
) {
  # DyNAM-M (choice) consumes the folded `active_dyad` directly (design D7): at
  # the point encoding `active_dyad_init` is a flattened n1 x n2 mask with a
  # (node1, node2, replace) buffer; otherwise it is the length-n2 receiver vector.
  if (modelTypeCall == "DyNAM-MM") {
    res <- estimate_DyNAM_MM(
      parameters,
      event_mat,
      stat_mat_init,
      stat_mat_update,
      stat_mat_update_pointer,
      stat_mat_broadcast,
      stat_mat_broadcast_pointer,
      active_sender_init,
      active_sender_update,
      active_sender_update_pointer,
      active_dyad_init,
      active_dyad_update,
      active_dyad_update_pointer,
      n_actors1,
      n_actors2,
      twomode_or_reflexive,
      impute,
      active_dyad_is_point = active_dyad_is_point
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
      active_dyad_init,
      active_dyad_update,
      active_dyad_update_pointer,
      n_actors1,
      n_actors2,
      twomode_or_reflexive,
      impute,
      active_dyad_is_point = active_dyad_is_point
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
      active_sender_init,
      active_sender_update,
      active_sender_update_pointer,
      active_dyad_init,
      active_dyad_update,
      active_dyad_update_pointer,
      n_actors1,
      n_actors2,
      twomode_or_reflexive,
      impute,
      active_dyad_is_point = active_dyad_is_point
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
      active_sender_init,
      active_sender_update,
      active_sender_update_pointer,
      active_dyad_init,
      active_dyad_update,
      active_dyad_update_pointer,
      n_actors1,
      n_actors2,
      twomode_or_reflexive,
      impute,
      active_dyad_is_point = active_dyad_is_point
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
      active_sender_init,
      active_sender_update,
      active_sender_update_pointer,
      active_dyad_init,
      active_dyad_update,
      active_dyad_update_pointer,
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
      active_sender_init,
      active_sender_update,
      active_sender_update_pointer,
      active_dyad_init,
      active_dyad_update,
      active_dyad_update_pointer,
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
  active_sender_init,
  active_sender_update,
  active_sender_update_pointer,
  active_dyad_init,
  active_dyad_update,
  active_dyad_update_pointer,
  n_actors1,
  n_actors2,
  twomode_or_reflexive,
  verbose,
  impute,
  support = NULL,
  active_dyad_encoding = "alter"
) {
  if (modelTypeCall %in% c("REM-ordered", "REM", "DyNAM-MM")) {
    # DyNAM-MM (coordination) now emits the off-diagonal directed dyad list — no
    # forced `twomode_or_reflexive` and no reflexive rows (design D9/D13). The
    # dyad-triangle kernel reads the emitted index structures (per-sender groups
    # + the (i,j)<->(j,i) pairing), so a one-mode coordination model no longer
    # relies on a square n x n candidate grid.
    gathered_data <- gather_sender_receiver_model_r(
      event_mat,
      is_dependent,
      stat_mat_init,
      stat_mat_update,
      stat_mat_update_pointer,
      stat_mat_broadcast,
      stat_mat_broadcast_pointer,
      active_sender_init,
      active_sender_update,
      active_sender_update_pointer,
      active_dyad_init,
      active_dyad_update,
      active_dyad_update_pointer,
      n_actors1,
      n_actors2,
      twomode_or_reflexive,
      active_dyad_encoding = active_dyad_encoding,
      is_coordination = (modelTypeCall == "DyNAM-MM")
    )
  } else if (modelTypeCall == "DyNAM-M") {
    gathered_data <- gather_receiver_model_r(
      event_mat,
      stat_mat_init,
      stat_mat_update,
      stat_mat_update_pointer,
      stat_mat_broadcast,
      stat_mat_broadcast_pointer,
      active_dyad_init,
      active_dyad_update,
      active_dyad_update_pointer,
      n_actors1,
      n_actors2,
      twomode_or_reflexive,
      active_dyad_encoding = active_dyad_encoding
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
      active_sender_init,
      active_sender_update,
      active_sender_update_pointer,
      active_dyad_init,
      active_dyad_update,
      active_dyad_update_pointer,
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

# Apply one event's slice of a point-encoded `active_dyad` buffer into the dense
# n1 x n2 availability matrix. `upd` rows are (node1, node2, replace), all
# 1-indexed on the node axes; later writes to the same cell win.
.gather_apply_presence_point <- function(active_dyad, upd, from, to) {
  if (to <= from) {
    return(active_dyad)
  }
  cols <- (from + 1L):to
  active_dyad[cbind(upd[1, cols], upd[2, cols])] <- upd[3, cols]
  active_dyad
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
  active_sender_init,
  active_sender_update,
  active_sender_update_pointer,
  active_dyad_init,
  active_dyad_update,
  active_dyad_update_pointer,
  n_actors1,
  n_actors2,
  twomode_or_reflexive,
  active_dyad_encoding = "outer",
  is_coordination = FALSE
) {
  stat_mat <- stat_mat_init
  n_events <- length(is_dependent)
  n_parameters <- ncol(stat_mat)
  has_cc1 <- length(active_sender_update) > 0
  has_cc2 <- length(active_dyad_update) > 0
  # A folded standard-REM constraint rides `active_dyad` at the point encoding
  # (design D7/D11): a dense n1 x n2 risk mask (both presences n support)
  # maintained by a (node1, node2, replace) buffer, read per sender as its row.
  # The outer encoding keeps the length-n2 receiver vector (cell = f1[i] & f2[j]).
  is_point <- identical(active_dyad_encoding, "point")
  active_sender <- active_sender_init
  active_dyad <- active_dyad_init
  update_id <- 0L
  bc_id <- 0L
  p1_id <- 0L
  p2_id <- 0L

  rows_list <- vector("list", n_events)
  # Per-row actor identity in the shared index vocabulary (design D13): every
  # dyad row carries the sanitized 1-based sender / receiver ids.
  index_i_list <- vector("list", n_events)
  index_j_list <- vector("list", n_events)
  # Coordination-only ragged structures (design D9): `sender_of_row` groups each
  # event's rows by sender (0-based within event; the CSR grouping the per-sender
  # softmax consumes), and `dyad_partner` maps each directed row (i -> j) to the
  # within-event position of its partner (j -> i) so the kernel can form the
  # unordered-dyad log-weights. The symmetric fold guarantees the partner exists.
  sender_of_row_list <- vector("list", n_events)
  dyad_partner_list <- vector("list", n_events)
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
      ptr1 <- active_sender_update_pointer[e]
      active_sender <- .gather_apply_presence(
        active_sender,
        active_sender_update,
        p1_id,
        ptr1
      )
      p1_id <- ptr1
    }
    if (has_cc2) {
      ptr2 <- active_dyad_update_pointer[e]
      active_dyad <- if (is_point) {
        .gather_apply_presence_point(
          active_dyad,
          active_dyad_update,
          p2_id,
          ptr2
        )
      } else {
        .gather_apply_presence(active_dyad, active_dyad_update, p2_id, ptr2)
      }
      p2_id <- ptr2
    }

    id_sender <- event_mat[1, e] - 1L
    id_receiver <- event_mat[2, e] - 1L
    is_dep <- is_dependent[e]

    present1_ids <- which(active_sender == 1) - 1L
    # Outer: one receiver vector shared by every sender. Point: each sender reads
    # its own dense mask row (both presences n support already folded in).
    present2_ids <- if (is_point) NULL else which(active_dyad == 1) - 1L

    idx <- integer(0)
    n_present <- 0L
    for (i in present1_ids) {
      not_allowed <- if (!twomode_or_reflexive) i else -1L
      present2_i <- if (is_point) {
        which(active_dyad[i + 1L, ] == 1) - 1L
      } else {
        present2_ids
      }
      allowed <- present2_i[present2_i != not_allowed]
      n_all <- length(allowed)
      idx <- c(idx, i * n_actors2 + allowed + 1L)
      if (is_dep && i == id_sender) {
        hit <- which(allowed == id_receiver)
        if (length(hit) > 0) {
          selected[e] <- n_present + (hit - 1L)
        }
      }
      n_present <- n_present + n_all
    }
    rows_list[[e]] <- stat_mat[idx, , drop = FALSE]
    # Decode the flat row indices (i * n2 + j, 1-based) back to per-row sender /
    # receiver ids for the shared index vocabulary (design D13).
    flat0 <- idx - 1L
    i_vec <- flat0 %/% n_actors2
    j_vec <- flat0 %% n_actors2
    index_i_list[[e]] <- i_vec + 1L
    index_j_list[[e]] <- j_vec + 1L
    if (is_coordination) {
      # sender group (0-based position among present senders) and the
      # (i, j) -> (j, i) pairing, both 0-based within this event's row block.
      sender_of_row_list[[e]] <- match(i_vec, present1_ids) - 1L
      dyad_partner_list[[e]] <- match(j_vec * n_actors2 + i_vec, flat0) - 1L
    }
    n_candidates[e] <- n_present
  }

  stat_all_events <- do.call(rbind, rows_list)
  if (is.null(stat_all_events)) {
    stat_all_events <- matrix(0, 0, n_parameters)
  }
  # The rectangularity metadata (n_candidates1/n_candidates2, selected_actor1/2)
  # is retired (design D13): the ragged dyad list has no rectangular grid, and
  # the per-row index_i/index_j plus the coordination CSR groups / dyad pairing
  # carry all the row identity the kernel and the exports need.
  out <- list(
    stat_all_events = stat_all_events,
    n_candidates = n_candidates,
    selected = selected,
    index_i = as.integer(unlist(index_i_list)),
    index_j = as.integer(unlist(index_j_list))
  )
  if (is_coordination) {
    out$sender_of_row <- as.integer(unlist(sender_of_row_list))
    out$dyad_partner <- as.integer(unlist(dyad_partner_list))
  }
  out
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
  active_dyad_init,
  active_dyad_update,
  active_dyad_update_pointer,
  n_actors1,
  n_actors2,
  twomode_or_reflexive,
  active_dyad_encoding = "alter"
) {
  stat_mat <- stat_mat_init
  n_events <- ncol(event_mat)
  n_parameters <- ncol(stat_mat)
  has_cc2 <- length(active_dyad_update) > 0
  # A folded `support_constraint` / opportunity list rides `active_dyad` at the
  # point encoding (design D7/D13): a dense n1 x n2 availability maintained by a
  # (node1, node2, replace) buffer, read as the event sender's row. The alter
  # encoding keeps the length-n2 receiver vector.
  is_point <- identical(active_dyad_encoding, "point")
  active_dyad <- active_dyad_init
  update_id <- 0L
  bc_id <- 0L
  p2_id <- 0L

  rows_list <- vector("list", n_events)
  # Per-row actor identity (design D13): choice rows are the candidate receivers
  # of the event's (fixed) sender, so index_i is that constant sender and
  # index_j the receiver, both sanitized 1-based ids.
  index_i_list <- vector("list", n_events)
  index_j_list <- vector("list", n_events)
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
      ptr2 <- active_dyad_update_pointer[e]
      active_dyad <- if (is_point) {
        .gather_apply_presence_point(
          active_dyad,
          active_dyad_update,
          p2_id,
          ptr2
        )
      } else {
        .gather_apply_presence(active_dyad, active_dyad_update, p2_id, ptr2)
      }
      p2_id <- ptr2
    }

    id_sender <- event_mat[1, e] - 1L
    id_receiver <- event_mat[2, e] - 1L
    not_allowed <- if (!twomode_or_reflexive) id_sender else -1L
    dyad_row <- if (is_point) active_dyad[id_sender + 1L, ] else active_dyad
    present2_ids <- which(dyad_row == 1) - 1L
    allowed <- present2_ids[present2_ids != not_allowed]
    idx <- id_sender * n_actors2 + allowed + 1L
    rows_list[[e]] <- stat_mat[idx, , drop = FALSE]
    index_i_list[[e]] <- rep(id_sender + 1L, length(allowed))
    index_j_list[[e]] <- allowed + 1L
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
    selected = selected,
    index_i = as.integer(unlist(index_i_list)),
    index_j = as.integer(unlist(index_j_list))
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
  active_sender_init,
  active_sender_update,
  active_sender_update_pointer,
  active_dyad_init,
  active_dyad_update,
  active_dyad_update_pointer,
  n_actors1,
  n_actors2,
  twomode_or_reflexive
) {
  stat_mat <- stat_mat_init
  n_events <- ncol(event_mat)
  n_parameters <- ncol(stat_mat)
  has_cc1 <- length(active_sender_update) > 0
  active_sender <- active_sender_init
  update_id <- 0L
  bc_id <- 0L
  p1_id <- 0L

  rows_list <- vector("list", n_events)
  # Per-row actor identity (design D13): rate rows are the candidate senders, a
  # sender-set layout, so each row carries index_i (the sanitized 1-based sender)
  # and index_j = NA (no receiver axis after the per-sender reduction).
  index_i_list <- vector("list", n_events)
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
      ptr1 <- active_sender_update_pointer[e]
      active_sender <- .gather_apply_presence(
        active_sender,
        active_sender_update,
        p1_id,
        ptr1
      )
      p1_id <- ptr1
    }

    reduced <- .gather_reduce(
      stat_mat,
      n_actors1,
      n_actors2,
      twomode_or_reflexive
    )
    id_sender <- event_mat[1, e] - 1L
    is_dep <- is_dependent[e]
    # A rate support_constraint folds the "has >= 1 allowed present receiver"
    # sender gate into `active_sender` during preprocessing (design D4/D12), so
    # `active_sender` is the gated sender filter directly — no separate mask.
    present1_ids <- which(active_sender == 1) - 1L
    rows_list[[e]] <- reduced[present1_ids + 1L, , drop = FALSE]
    index_i_list[[e]] <- present1_ids + 1L
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
  index_i <- as.integer(unlist(index_i_list))
  list(
    stat_all_events = stat_all_events,
    n_candidates = n_candidates,
    selected = selected,
    index_i = index_i,
    index_j = rep(NA_integer_, length(index_i))
  )
}


## COMPUTE FOR DIFFERENT MODELS
compute_ <- function(
  modelTypeCall,
  parameters,
  stat_all_events,
  selected,
  n_candidates,
  timespan,
  is_dependent,
  twomode_or_reflexive,
  sender_of_row = NULL,
  dyad_partner = NULL
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
      selected,
      sender_of_row,
      dyad_partner
    )
  }

  return(res)
}
