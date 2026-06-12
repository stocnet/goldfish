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
  defaultNetworkName,
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
  ignoreRepParameter = NULL,
  testing = FALSE,
  get_data_matrix = FALSE,
  impute = FALSE,
  opportunitiesList = NULL,
  engine = c("default_c", "gather_compute"),
  prepEnvir = new.env()
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

  reduceArrayToMatrix <- modelTypeCall == "DyNAM-M"

  # CHANGED MARION: updated function
  # for rate model with intercept, add a table of all 1 to the
  #  statsList$initStats
  statsList <- modifyStatisticsList(
    statsList = statsList,
    modelType = modelTypeCall,
    reduceArrayToMatrix = reduceArrayToMatrix,
    excludeParameters = excludeParameters,
    addInterceptEffect = hasIntercept
  )

  ## CONVERT COMPOSITION CHANGES INTO THE FORMAT ACCEPTED BY C FUNCTIONS
  hasCompChange1 <- length(statsList$active_mode1_changes) > 0
  hasCompChange2 <- length(statsList$active_mode2_changes) > 0

  if (hasCompChange1) {
    compChange1 <- data.frame(
      time = vapply(statsList$active_mode1_changes, `[[`, double(1), "time"),
      node = vapply(statsList$active_mode1_changes, `[[`, integer(1), "node"),
      replace = vapply(
        statsList$active_mode1_changes,
        `[[`,
        logical(1),
        "replace"
      )
    )
    temp <- C_convert_composition_change(compChange1, statsList$event_time)
    presence1_update <- temp$presenceUpdate
    presence1_update_pointer <- temp$presenceUpdatePointer
  } else {
    presence1_update <- matrix(0, 0, 0)
    presence1_update_pointer <- numeric(1)
  }

  if (hasCompChange2) {
    compChange2 <- data.frame(
      time = vapply(statsList$active_mode2_changes, `[[`, double(1), "time"),
      node = vapply(statsList$active_mode2_changes, `[[`, integer(1), "node"),
      replace = vapply(
        statsList$active_mode2_changes,
        `[[`,
        logical(1),
        "replace"
      )
    )
    temp <- C_convert_composition_change(compChange2, statsList$event_time)
    presence2_update <- temp$presenceUpdate
    presence2_update_pointer <- temp$presenceUpdatePointer
  } else {
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
    totalTime <- sum(statsList$intervals, na.rm = TRUE)

    nActors <- sum(presence1_init)

    if (hasCompChange1) {
      # CHANGED MARION: remove the use of the events object
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
  dep_idx <- statsList$is_dependent == 1L
  rc_idx <- statsList$is_dependent == 0L

  expand_for_c <- function(changes_list) {
    lapply(changes_list, function(event_changes) {
      lapply(event_changes, function(ch) {
        if (is.null(ch)) {
          return(NULL)
        }
        if (!is.matrix(ch)) {
          ch <- matrix(ch, nrow = 1L, dimnames = list(NULL, names(ch)))
        }
        cbind(node1 = ch[, "node1"], node2 = 1L, replace = ch[, "replace"])
      })
    })
  }

  if (!is.null(statsList$stat_mat_update)) {
    temp <- split_flat_updates(
      statsList$stat_mat_update, statsList$stat_mat_pointer, dep_idx
    )
    stat_mat_update <- temp$mat
    stat_mat_update_pointer <- temp$pointer
    if (hasIntercept) {
      stat_mat_update[3, ] <- stat_mat_update[3, ] + 1
    }

    if (sum(rc_idx) == 0L) {
      stat_mat_rightcensored_update <- matrix(0, 4, 1)
      stat_mat_rightcensored_update_pointer <- numeric(1)
    } else {
      temp <- split_flat_updates(
        statsList$stat_mat_update, statsList$stat_mat_pointer, rc_idx
      )
      stat_mat_rightcensored_update <- temp$mat
      stat_mat_rightcensored_update_pointer <- temp$pointer
      if (hasIntercept) {
        stat_mat_rightcensored_update[3, ] <-
          stat_mat_rightcensored_update[3, ] + 1
      }
    }
  } else {
    dep_changes <- if (is_rate_model) {
      expand_for_c(statsList$stats_change[dep_idx])
    } else {
      statsList$stats_change[dep_idx]
    }
    temp <- convert_change(dep_changes)
    stat_mat_update <- temp$statMatUpdate
    stat_mat_update_pointer <- temp$statMatUpdatePointer
    if (hasIntercept) {
      stat_mat_update[3, ] <- stat_mat_update[3, ] + 1
    }

    if (sum(rc_idx) == 0L) {
      stat_mat_rightcensored_update <- matrix(0, 4, 1)
      stat_mat_rightcensored_update_pointer <- numeric(1)
    } else {
      rc_changes <- if (is_rate_model) {
        expand_for_c(statsList$stats_change[rc_idx])
      } else {
        statsList$stats_change[rc_idx]
      }
      temp <- convert_change(rc_changes)
      stat_mat_rightcensored_update <- temp$statMatUpdate
      stat_mat_rightcensored_update_pointer <- temp$statMatUpdatePointer
      if (hasIntercept) {
        stat_mat_rightcensored_update[3, ] <-
          stat_mat_rightcensored_update[3, ] + 1
      }
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
      stat_mat_rightcensored_update = stat_mat_rightcensored_update,
      stat_mat_rightcensored_update_pointer = stat_mat_rightcensored_update_pointer,
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
        stat_mat_rightcensored_update = stat_mat_rightcensored_update,
        stat_mat_rightcensored_update_pointer = stat_mat_rightcensored_update_pointer,
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
      maxAbsUpdate = max(abs(update))
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
  stat_mat_rightcensored_update,
  stat_mat_rightcensored_update_pointer,
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
      stat_mat_rightcensored_update,
      stat_mat_rightcensored_update_pointer,
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
      stat_mat_rightcensored_update,
      stat_mat_rightcensored_update_pointer,
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
gather_ <- function(
  modelTypeCall,
  event_mat,
  timespan,
  is_dependent,
  stat_mat_init,
  stat_mat_update,
  stat_mat_update_pointer,
  presence1_init,
  presence1_update,
  presence1_update_pointer,
  stat_mat_rightcensored_update,
  stat_mat_rightcensored_update_pointer,
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
    gathered_data <- gather_sender_receiver_model(
      event_mat,
      is_dependent,
      stat_mat_init,
      stat_mat_update,
      stat_mat_update_pointer,
      stat_mat_rightcensored_update,
      stat_mat_rightcensored_update_pointer,
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
    )
  }

  if (modelTypeCall == "DyNAM-M") {
    gathered_data <- gather_receiver_model(
      event_mat,
      stat_mat_init,
      stat_mat_update,
      stat_mat_update_pointer,
      presence2_init,
      presence2_update,
      presence2_update_pointer,
      n_actors1,
      n_actors2,
      twomode_or_reflexive,
      verbose, # output the progress of data gathering
      impute
    )
  }

  if (modelTypeCall %in% c("DyNAM-M-Rate-ordered", "DyNAM-M-Rate")) {
    gathered_data <- gather_sender_model(
      event_mat,
      is_dependent,
      stat_mat_init,
      stat_mat_update,
      stat_mat_update_pointer,
      stat_mat_rightcensored_update,
      stat_mat_rightcensored_update_pointer,
      presence1_init,
      presence1_update,
      presence1_update_pointer,
      presence2_init,
      presence2_update,
      presence2_update_pointer,
      n_actors1,
      n_actors2,
      twomode_or_reflexive,
      verbose, # verbose
      impute
    )
  }

  return(gathered_data)
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
