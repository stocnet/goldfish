##################### ###
#
# Goldfish package
# Internal estimation routine
#
##################### ###

# Both NA guards on the kernel result -- the initial-parameters check and the
# Newton loop's step acceptance -- read NA as "the likelihood could not be
# evaluated here". Some opt-in per-event components carry NA by DESIGN and must
# be excluded, or the guards report a numerical failure that did not happen:
# `observed_rank` is allocated NA-filled and written only for dependent events,
# so any model with a right-censored interval leaves NAs behind whenever ranks
# are requested. `conditional_logl` (the Cox partial-likelihood component of the
# exact-time loglik) is NA on right-censored intervals for the same reason: a
# censored interval realizes no mover, so log p_obs is undefined there. This is
# the ONLY place the by-design-NA exclusion list lives -- any future per-event
# NA scan MUST route through this guard, never an ad-hoc is.na() sweep.
diagnostic_components_with_na <- c(
  "observed_rank",
  "conditional_logl",
  "conditional_scores"
)

has_unexpected_na <- function(res) {
  scanned <- res[setdiff(names(res), diagnostic_components_with_na)]
  any(is.na(unlist(scanned)))
}

# One compiled-engine pass, as a closure over the buffers it needs
#
# Everything the engines take that does not depend on the parameter vector is
# prepared once here -- the reduced statistics list, the presence/availability
# buffers, the event matrix, the flattened initial statistics, and (on the
# gather backend) the gathered stack -- and `evaluate()` runs one pass at a
# supplied vector. Estimation builds this once and iterates `evaluate()`;
# `evaluate_model()` builds it and calls `evaluate()` exactly once, which is
# what keeps the two on the same code path by construction rather than by
# resemblance.
#
# `seed_intercept` is the caller's decision, not this function's: estimation
# starts an unfixed, unseeded time intercept at its data-derived value, while
# an evaluation at a supplied vector must use that vector verbatim.
make_engine_evaluator <- function(
  spec,
  stats_list,
  parameters,
  backend = c("cpp", "gather"),
  has_intercept = FALSE,
  is_rate_model = identical(risk_set_axis(spec), "sender"),
  is_two_mode = FALSE,
  allow_reflexive = FALSE,
  impute = FALSE,
  seed_intercept = TRUE,
  verbose = FALSE,
  progress = FALSE
) {
  backend <- match.arg(backend)

  ## REDUCE STATISTICS LIST

  if (verbose) {
    cat("Reducing data\n")
  }

  stats_list <- prepare_statslist(
    statsList = stats_list,
    addInterceptEffect = has_intercept,
    is_sender = is_rate_model
  )

  ## PRESENCE UPDATES PRECOMPUTED DURING PREPROCESSING
  active_sender_update <- stats_list$active_sender_update
  active_sender_update_pointer <- stats_list$active_sender_update_pointer
  if (is.null(active_sender_update)) {
    active_sender_update <- matrix(0, 0, 0)
    active_sender_update_pointer <- numeric(1)
  }

  active_dyad_update <- stats_list$active_dyad_update
  active_dyad_update_pointer <- stats_list$active_dyad_update_pointer
  if (is.null(active_dyad_update)) {
    active_dyad_update <- matrix(0, 0, 0)
    active_dyad_update_pointer <- numeric(1)
  }

  active_sender_init <- stats_list$active_sender_init
  active_dyad_init <- stats_list$active_dyad_init
  active_dyad_encoding <- if (is.null(stats_list$active_dyad_encoding)) {
    "alter"
  } else {
    stats_list$active_dyad_encoding
  }

  n_events <- length(stats_list$is_dependent)

  ## ADD INTERCEPT
  # CHANGED MARION
  # replace first parameter with an initial estimate of the intercept
  # The data-derived start applies unless the intercept itself was given a
  # value -- fixed, so there is nothing to start, or seeded, so the user chose
  # the start. Seeding some other coefficient leaves the intercept alone.
  if (
    identical(risk_set_normalizer(spec), "poisson") &&
      has_intercept &&
      seed_intercept
  ) {
    parameters[1] <- log(
      stats_list$n_dep_events /
        stats_list$total_time /
        stats_list$avg_active_entity
    )
  }
  ## SET VARIABLES BASED ON STATSLIST
  twomode_or_reflexive <- (allow_reflexive || is_two_mode)
  if (is_rate_model) {
    n_parameters <- ncol(stats_list$initial_stats)
    n_actors1 <- nrow(stats_list$initial_stats)
    n_actors2 <- 1L
    twomode_or_reflexive <- TRUE
  } else {
    n_parameters <- dim(stats_list$initial_stats)[3]
    n_actors1 <- dim(stats_list$initial_stats)[1]
    n_actors2 <- dim(stats_list$initial_stats)[2]
  }

  ## CONVERT UPDATES INTO THE FORMAT ACCEPTED BY C FUNCTIONS
  stat_mat_update <- stats_list$stat_mat_update
  stat_mat_update_pointer <- stats_list$stat_mat_pointer
  stat_mat_broadcast <- stats_list$stat_mat_broadcast
  stat_mat_broadcast_pointer <- stats_list$stat_mat_broadcast_pointer
  if (is.null(stat_mat_broadcast)) {
    stat_mat_broadcast <- matrix(0, 4L, 0L)
    stat_mat_broadcast_pointer <- numeric(length(stat_mat_update_pointer))
  }
  if (has_intercept) {
    stat_mat_update[3, ] <- stat_mat_update[3, ] + 1
    if (ncol(stat_mat_broadcast) > 0L) {
      stat_mat_broadcast[3, ] <- stat_mat_broadcast[3, ] + 1
    }
  }

  ## CONVERT TYPES OF EVENTS AND TIMESPANS INTO THE FORMAT ACCEPTED
  ## BY C FUNCTIONS
  # Poisson (rate / standard REM) carries a timespan; the ordinal and
  # coordination sender-set families need is_dependent but no timespan; choice
  # (receiver-given-sender) reads neither. The two stay NULL where the family
  # has no use for them: the dispatchers below forward only what each kernel
  # takes, so a NULL never reaches a kernel that would read it.
  is_dependent <- NULL
  timespan <- NA
  if (identical(risk_set_normalizer(spec), "poisson")) {
    is_dependent <- as.logical(stats_list$is_dependent)
    timespan <- stats_list$intervals
  } else if (!identical(risk_set_axis(spec), "receiver_given_sender")) {
    is_dependent <- as.logical(stats_list$is_dependent)
  }

  ## CONVERT INFOS OF SENDERS AND RECEIVERS INTO THE FORMAT ACCEPTED
  ##  BY C FUNCTIONS
  event_mat <- rbind(stats_list$event_sender, stats_list$event_receiver)

  ## CONVERT THE INITIALIZATION OF DATA MATRIX INTO THE FORMAT ACCEPTED
  ##  BY C FUNCTIONS
  if (is_rate_model) {
    stat_mat_init <- stats_list$initial_stats
  } else {
    stat_mat_init <- matrix(0, n_actors1 * n_actors2, n_parameters)
    for (i in seq_len(n_parameters)) {
      stat_mat_init[, i] <- t(stats_list$initial_stats[,, i])
    }
  }

  ## GATHERING INFO IF WE USE THE GATHER BACKEND.
  gathered_data <- NULL
  size_gathered_data <- NULL
  if (backend == "gather") {
    gathered_data <- gather_(
      spec = spec,
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
      active_dyad_encoding = active_dyad_encoding
    )
    size_gathered_data <- utils::object.size(gathered_data)
  }

  # Parameter-independent default_c buffer layout, hoisted out of the loop so
  # both the Newton-Raphson iterations and the maxLik adapter reuse it: at the
  # point encoding `active_dyad_init` is flattened sender-major to a
  # dense n1 x n2 mask; otherwise it is the length-n2 receiver vector.
  dyad_is_point <- !identical(risk_set_axis(spec), "sender") &&
    identical(active_dyad_encoding, "point")
  dyad_init_c <- if (dyad_is_point) {
    as.vector(t(active_dyad_init))
  } else {
    active_dyad_init
  }

  evaluate <- function(
    pars,
    need_scores,
    need_ranks = FALSE,
    need_margins = FALSE,
    need_total_rate = FALSE,
    need_probabilities = FALSE,
    need_availability = FALSE,
    need_conditional_scores = FALSE
  ) {
    if (backend == "gather") {
      # The gathered stack computes the exact-time components whenever the
      # family has them, so it takes no total-rate flag of its own.
      return(compute_(
        spec = spec,
        parameters = pars,
        stat_all_events = gathered_data$stat_all_events,
        selected = gathered_data$selected,
        n_candidates = gathered_data$n_candidates,
        timespan = timespan,
        is_dependent = is_dependent,
        twomode_or_reflexive = twomode_or_reflexive,
        index_i = gathered_data$index_i,
        index_j = gathered_data$index_j,
        n_actors_1 = n_actors1,
        n_actors_2 = n_actors2,
        return_event_scores = need_scores,
        return_ranks = need_ranks,
        return_margins = need_margins,
        return_probabilities = need_probabilities,
        return_availability = need_availability,
        return_conditional_scores = need_conditional_scores,
        sender_of_row = gathered_data$sender_of_row,
        dyad_partner = gathered_data$dyad_partner
      ))
    }
    estimate_(
      spec = spec,
      parameters = pars,
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
      active_dyad_is_point = dyad_is_point,
      return_event_scores = need_scores,
      return_ranks = need_ranks,
      return_margins = need_margins,
      return_total_rate = need_total_rate,
      return_probabilities = need_probabilities,
      return_availability = need_availability,
      return_conditional_scores = need_conditional_scores
    )
  }

  list(
    evaluate = evaluate,
    parameters = parameters,
    n_events = n_events,
    n_parameters = n_parameters,
    n_actors1 = n_actors1,
    n_actors2 = n_actors2,
    gathered = gathered_data,
    size_gathered = size_gathered_data
  )
}

# Estimation
estimate_c_int <- function(
  statsList,
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
  return_availability = FALSE,
  return_conditional_scores = FALSE,
  parallelize = FALSE,
  cpus = 6,
  verbose = FALSE,
  progress = FALSE,
  testing = FALSE,
  get_data_matrix = FALSE,
  impute = FALSE,
  opportunitiesList = NULL,
  spec = NULL,
  backend = c("cpp", "gather"),
  optimizer = "newton_raphson"
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
  is_rate_model <- identical(risk_set_axis(spec), "sender")
  nParams <- (if (is_rate_model) {
    ncol(statsList$initial_stats)
  } else {
    dim(statsList$initial_stats)[3]
  }) +
    hasIntercept
  #

  # Which coefficients are held at supplied values, which were seeded, and the
  # parameter vector both produce: decided once, by the shared helper every
  # estimation path reads.
  mask <- resolve_coefficient_mask(
    fixed_spec,
    initial_spec,
    nParams
  )
  parameters <- mask$parameters
  id_unfixed <- mask$id_unfixed
  id_fixed <- mask$id_fixed
  likelihood_only <- mask$likelihood_only

  backend <- match.arg(backend)

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

  engine <- make_engine_evaluator(
    spec = spec,
    stats_list = statsList,
    parameters = parameters,
    backend = backend,
    has_intercept = hasIntercept,
    is_rate_model = is_rate_model,
    is_two_mode = is_two_mode,
    allow_reflexive = allowReflexive,
    impute = impute,
    seed_intercept = !mask$intercept_fixed && !mask$intercept_seeded,
    verbose = verbose,
    progress = progress
  )
  parameters <- engine$parameters
  nEvents <- engine$n_events
  n_actors1 <- engine$n_actors1
  n_actors2 <- engine$n_actors2
  if (backend == "gather") {
    gathered_data <- engine$gathered
    size_gathered_data <- engine$size_gathered
  }

  ## ESTIMATION: INITIALIZATION

  if (verbose) {
    cat("Estimating model ", spec$model, " ", spec$sub_model, ".\n", sep = "")
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
  # The starting point is the last accepted one until a step is accepted.
  parameters.old <- parameters
  score.old <- NULL
  informationMatrix.old <- NULL

  # maxLik-backed optimizers replace the Newton-Raphson loop below:
  # the preprocessed data is fixed, the C++ evaluator is fed to maxLik through
  # memoized closures, and the maxLik result maps back into the standard result
  # object. Runs on the default_c evaluator only (guarded upstream).
  if (!identical(optimizer, "newton_raphson")) {
    return(estimate_via_maxlik(
      evaluate = engine$evaluate,
      optimizer = optimizer,
      start = parameters,
      id_fixed = id_fixed,
      n_params = nParams,
      n_events = nEvents,
      return_interval_loglik = returnIntervalLogL,
      return_event_scores = return_event_scores,
      verbose = verbose
    ))
  }

  while (TRUE) {
    ## CALCULATE THE LOGLIKELIHOOD,
    ## THE FISHER INFORMATION MATRIX, AND THE DERIVATIVE

    res <- engine$evaluate(
      parameters,
      return_event_scores,
      return_ranks,
      return_margins,
      return_total_rate,
      returnEventProbabilities,
      return_availability,
      return_conditional_scores
    )

    logLikelihood <- res$logLikelihood
    score <- as.numeric(res$derivative)
    informationMatrix <- res$fisher
    if (returnIntervalLogL) {
      intervalLogL <- as.numeric(res$intervalLogL)
    }
    if (return_event_scores) {
      event_scores <- res$event_scores
    }
    if (return_ranks) {
      observed_rank <- res$observed_rank
    }

    if (returnEventProbabilities) {
      # One per-event element, actor-indexed over the whole node set and zero
      # off the risk set, in the same shape the r backend produces: a vector on
      # the sender / receiver axes, an n1 x n2 grid on the dyad axes.
      eventProbabilities <- res$event_probabilities
    }

    if (
      isInitialEstimation &&
        has_unexpected_na(res) &&
        !all(parameters[-1] == 0)
    ) {
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
    #  It's for the fixing parameter feature. \
    score[id_fixed] <- 0

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
      informationMatrix[id_unfixed, id_unfixed]
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
    update[id_unfixed] <-
      (inverseInformationUnfixed %*% score[id_unfixed]) /
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
      step_accepted = step_accepted,
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
  if (backend == "gather") {
    estimationResult$size_intermediate <- size_gathered_data
    if (testing) estimationResult$intermediate <- gathered_data
  }
  # if (testing) estimationResult$intermediateData <-
  #  DataMatrixAndId$intermediate_data
  if (returnIntervalLogL) {
    estimationResult$interval_log_lik <- intervalLogL
  }
  if (return_event_scores) {
    estimationResult$event_scores <- event_scores
  }
  if (return_ranks) {
    estimationResult$observed_rank <- observed_rank
  }
  if (return_margins) {
    # Actor labels and the scale marker are attached by the shared helper both
    # backends call, so a fit's margins carry the same names and scales
    # whichever implementation produced them.
    margins <- label_margins(
      assemble_engine_margins(res),
      axis = risk_set_axis(spec),
      nodes = nodes,
      nodes2 = nodes2,
      is_exact_time = identical(risk_set_normalizer(spec), "poisson")
    )
    if (!is.null(margins)) estimationResult$margins <- margins
  }
  if (return_availability) {
    # Availability rides the same labeling the margins do -- one side rule, one
    # actor-label convention -- so a per-actor table can join the two without
    # reconciling two spellings of the same actor set.
    availability <- label_availability(
      assemble_engine_availability(res),
      axis = risk_set_axis(spec),
      nodes = nodes,
      nodes2 = nodes2
    )
    if (!is.null(availability)) estimationResult$availability <- availability
  }
  if (
    return_total_rate &&
      !is.null(res$total_rate) &&
      length(res$total_rate) > 0
  ) {
    # Only the exact-time engines (rate, REM) return a total rate; the
    # multinomial engines leave it empty, so it stays off their fits.
    estimationResult$total_rate <- as.numeric(res$total_rate)
  }
  if (
    return_total_rate &&
      !is.null(res$conditional_logl) &&
      length(res$conditional_logl) > 0
  ) {
    # The which/when split of the exact-time log-likelihood: the conditional
    # component is the Cox partial-likelihood contribution, the log probability
    # that the observed alternative is the one to move next. It rides the same
    # `loglik` primitive as total_rate, and is computed in the kernel as
    # x_obs - log_normalizer rather than reassembled from the identity, which
    # cancels a term against itself and loses digits away from the MLE.
    estimationResult$conditional_logl <- as.numeric(res$conditional_logl)
  }
  if (
    return_conditional_scores &&
      !is.null(res$conditional_scores) &&
      length(res$conditional_scores) > 0
  ) {
    # Only the exact-time kernels return conditional score rows; on a
    # multinomial family the stored `event_scores` ARE the conditional rows, so
    # the component is absent rather than duplicated.
    estimationResult$conditional_scores <- res$conditional_scores
  }
  if (returnEventProbabilities) {
    estimationResult$event_probabilities <- eventProbabilities
  }
  attr(estimationResult, "class") <- "result.goldfish"
  estimationResult
}

# Memoize a single evaluator call per parameter vector so the log-likelihood and
# gradient closures maxLik calls separately share one C++ pass. The
# cache holds the most recent evaluation, keyed by the (unnamed) parameter
# vector; the common logLik-then-grad-at-the-same-point call pattern hits it.
make_memoized_evaluator <- function(evaluate, need_scores) {
  cache <- new.env(parent = emptyenv())
  cache$key <- NULL
  cache$res <- NULL
  function(pars) {
    key <- unname(as.numeric(pars))
    if (is.null(cache$key) || !identical(cache$key, key)) {
      cache$res <- evaluate(pars, need_scores)
      cache$key <- key
    }
    cache$res
  }
}

#' maxLik-backed estimation adapter
#'
#' Drives `maxLik::maxLik()` over the fixed preprocessed data through memoized
#' closures on the `default_c` evaluator, then maps the result into the standard
#' `result.goldfish` object so `summary()` / `vcov()` / `logLik()` and the
#' post-estimation methods work unchanged. Only reached for
#' `optimizer != "newton_raphson"`, guarded upstream to the cpp backend
#' with maxLik installed.
#'
#' @param evaluate closure `function(pars, need_scores)` returning the C++
#'   evaluator list (`logLikelihood`, `derivative`, `fisher`, and, when
#'   `need_scores`, `event_scores` / `intervalLogL`).
#' @param optimizer one of `"bfgs"`, `"bhhh"`, `"nelder_mead"`.
#' @param start full-length initial parameter vector (fixed components already
#'   set to their values).
#' @param id_fixed integer indices of parameters held fixed (may be empty/NULL).
#' @param n_params,n_events problem dimensions.
#' @param return_interval_loglik,return_event_scores whether to attach the
#'   per-event outputs, evaluated at the optimum.
#' @param verbose passed through as the maxLik print level.
#' @return a `result.goldfish` list, structurally identical to the NR path.
#' @noRd
estimate_via_maxlik <- function(
  evaluate,
  optimizer,
  start,
  id_fixed,
  n_params,
  n_events,
  return_interval_loglik,
  return_event_scores,
  verbose
) {
  method <- switch(optimizer, bfgs = "BFGS", bhhh = "BHHH", nelder_mead = "NM")
  # BHHH needs observation-level gradients (the per-event score matrix); the
  # other methods use the aggregate score (BFGS) or none (Nelder-Mead), so the
  # extra score work runs only when BHHH requires it.
  use_scores <- identical(optimizer, "bhhh")
  eval_cached <- make_memoized_evaluator(evaluate, use_scores)

  loglik_fn <- function(pars) eval_cached(pars)$logLikelihood
  grad_fn <- if (identical(optimizer, "nelder_mead")) {
    NULL
  } else if (use_scores) {
    function(pars) eval_cached(pars)$event_scores
  } else {
    function(pars) as.numeric(eval_cached(pars)$derivative)
  }

  id_free <- setdiff(seq_len(n_params), id_fixed)
  if (length(id_free) == 0L) {
    # All parameters fixed: nothing to optimize, mirror the NR likelihood-only
    # exit and evaluate once at the fixed vector.
    fit_pars <- start
    n_iter <- 0L
    is_converged <- TRUE
    return_code <- 1L
  } else {
    ml <- maxLik::maxLik(
      logLik = loglik_fn,
      grad = grad_fn,
      start = start,
      method = method,
      fixed = if (length(id_fixed)) id_fixed else NULL,
      finalHessian = FALSE,
      printLevel = if (verbose) 2L else 0L
    )
    fit_pars <- as.numeric(stats::coef(ml))
    n_iter <- tryCatch(
      as.integer(maxLik::nIter(ml))[1],
      error = function(e) NA_integer_
    )
    return_code <- tryCatch(
      as.integer(maxLik::returnCode(ml)),
      error = function(e) NA_integer_
    )
    # maxLik success codes: 0 (optim-based BFGS/NM), 1 (gradient ~ 0) and 2
    # (successive params within tol) for the maxNR family, and 8 (successive
    # function values within relative tolerance).
    is_converged <- !is.na(return_code) &&
      return_code %in% c(0L, 1L, 2L, 8L)
  }

  # One evaluation at the optimum for the Fisher-based vcov and the optional
  # per-event outputs (vcov from the information at the optimum, as on the NR
  # path).
  final <- evaluate(fit_pars, return_event_scores)
  score <- as.numeric(final$derivative)
  score[id_fixed] <- 0
  information_matrix <- final$fisher
  log_likelihood <- final$logLikelihood

  std_errors <- rep(0, n_params)
  inverse_information_unfixed <- try(
    solve(information_matrix[id_free, id_free, drop = FALSE]),
    silent = TRUE
  )
  if (!inherits(inverse_information_unfixed, "try-error")) {
    std_errors[id_free] <- sqrt(diag(inverse_information_unfixed))
  }

  estimation_result <- list(
    parameters = fit_pars,
    standard_errors = std_errors,
    log_likelihood = log_likelihood,
    final_score = score,
    final_information_matrix = information_matrix,
    convergence = list(
      is_converged = is_converged,
      return_code = return_code,
      max_abs_score = max(abs(score)),
      max_abs_update = NA_real_,
      score_rel_norm = max(abs(score)) / max(1, abs(log_likelihood))
    ),
    n_iterations = n_iter,
    n_events = n_events
  )
  if (return_interval_loglik) {
    estimation_result$interval_log_lik <- as.numeric(final$intervalLogL)
  }
  if (return_event_scores) {
    estimation_result$event_scores <- final$event_scores
  }
  attr(estimation_result, "class") <- "result.goldfish"
  estimation_result
}

## ESTIMATE FOR DIFFERENT MODELS
# The C++ estimator is selected by the spec class (stage-boundary dispatch), not
# a model-type string; each family shapes its own argument list.
estimate_ <- function(
  spec,
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
  active_dyad_is_point = FALSE,
  return_event_scores = FALSE,
  return_ranks = FALSE,
  return_margins = FALSE,
  return_total_rate = FALSE,
  return_probabilities = FALSE,
  return_availability = FALSE,
  return_conditional_scores = FALSE
) {
  # DyNAM-M (choice) consumes the folded `active_dyad` directly: at
  # the point encoding `active_dyad_init` is a flattened n1 x n2 mask with a
  # (node1, node2, replace) buffer; otherwise it is the length-n2 receiver vector.
  if (inherits(spec, "dynam_choice_coord_spec")) {
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
      active_dyad_is_point = active_dyad_is_point,
      return_event_scores = return_event_scores,
      return_ranks = return_ranks,
      return_margins = return_margins,
      return_probabilities = return_probabilities,
      return_availability = return_availability
    )
  }

  if (inherits(spec, "dynam_choice_spec")) {
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
      active_dyad_is_point = active_dyad_is_point,
      return_event_scores = return_event_scores,
      return_ranks = return_ranks,
      return_margins = return_margins,
      return_probabilities = return_probabilities,
      return_availability = return_availability
    )
  }

  if (inherits(spec, "rem_rate_ordered_spec")) {
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
      active_dyad_is_point = active_dyad_is_point,
      return_event_scores = return_event_scores,
      return_ranks = return_ranks,
      return_margins = return_margins,
      return_probabilities = return_probabilities,
      return_availability = return_availability
    )
  }

  if (inherits(spec, "rem_rate_spec")) {
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
      active_dyad_is_point = active_dyad_is_point,
      return_event_scores = return_event_scores,
      return_ranks = return_ranks,
      return_margins = return_margins,
      return_total_rate = return_total_rate,
      return_probabilities = return_probabilities,
      return_availability = return_availability,
      return_conditional_scores = return_conditional_scores
    )
  }

  if (inherits(spec, "dynam_rate_spec")) {
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
      impute,
      return_event_scores = return_event_scores,
      return_ranks = return_ranks,
      return_margins = return_margins,
      return_total_rate = return_total_rate,
      return_probabilities = return_probabilities,
      return_availability = return_availability,
      return_conditional_scores = return_conditional_scores
    )
  }

  if (inherits(spec, "dynam_rate_ordered_spec")) {
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
      impute,
      return_event_scores = return_event_scores,
      return_ranks = return_ranks,
      return_margins = return_margins,
      return_probabilities = return_probabilities,
      return_availability = return_availability
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
#' output and consumed by both `backend = "gather"` and
#' `compute_statistics(output = "gather")`. The per-iteration model fitting
#' stays in C++ via `compute_()`, so the one-time expansion in R does not
#' affect the
#' estimation hot path. The `verbose` / `impute` arguments are retained for
#' call-site compatibility; `impute` is always `FALSE` in the current
#' code paths (the impute machinery was dropped from estimation).
#' @noRd
gather_ <- function(
  spec,
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
  active_dyad_encoding = "alter"
) {
  if (risk_set_is_dyadic(spec)) {
    # DyNAM-MM (coordination) now emits the off-diagonal directed dyad list — no
    # forced `twomode_or_reflexive` and no reflexive rows. The
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
      is_coordination = identical(risk_set_normalizer(spec), "coordination")
    )
  } else if (identical(risk_set_axis(spec), "receiver_given_sender")) {
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
  } else if (identical(risk_set_axis(spec), "sender")) {
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
  # a dense n1 x n2 risk mask (both presences n support)
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
  # Per-row actor identity in the shared index vocabulary: every
  # dyad row carries the sanitized 1-based sender / receiver ids.
  index_i_list <- vector("list", n_events)
  index_j_list <- vector("list", n_events)
  # Coordination-only ragged structures: `sender_of_row` groups each
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
    # receiver ids for the shared index vocabulary.
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
  # is retired: the ragged dyad list has no rectangular grid, and
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
  # point encoding: a dense n1 x n2 availability maintained by a
  # (node1, node2, replace) buffer, read as the event sender's row. The alter
  # encoding keeps the length-n2 receiver vector.
  is_point <- identical(active_dyad_encoding, "point")
  active_dyad <- active_dyad_init
  update_id <- 0L
  bc_id <- 0L
  p2_id <- 0L

  rows_list <- vector("list", n_events)
  # Per-row actor identity: choice rows are the candidate receivers
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
  # Per-row actor identity: rate rows are the candidate senders, a
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
    # sender gate into `active_sender` during preprocessing, so
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
# Translate a gather stack's 1-based per-row actor index into the 0-based slots
# the shared C++ reduction scatters margins into. An absent axis -- the sender
# models carry `index_j = NA` after reducing the receiver axis away -- becomes
# an empty vector, which the kernel reads as "this side does not exist".
margin_slots <- function(index) {
  if (is.null(index) || length(index) == 0 || all(is.na(index))) {
    return(integer(0))
  }
  as.integer(index) - 1L
}

# The likelihood kernel is selected by the spec's normalizer, not a model-type
# string: multinomial (choice / ordinal rate), Poisson (rate), or coordination.
compute_ <- function(
  spec,
  parameters,
  stat_all_events,
  selected,
  n_candidates,
  timespan,
  is_dependent,
  twomode_or_reflexive,
  index_i = NULL,
  index_j = NULL,
  n_actors_1 = 0L,
  n_actors_2 = 0L,
  return_event_scores = FALSE,
  return_ranks = FALSE,
  return_margins = FALSE,
  return_probabilities = FALSE,
  return_availability = FALSE,
  return_conditional_scores = FALSE,
  sender_of_row = NULL,
  dyad_partner = NULL
) {
  # Per-row actor slots for the margin sides, as the shared reduction wants
  # them: 0-based, and empty for an axis this shape does not have (the sender
  # models reduce away the receiver axis and carry index_j = NA).
  #
  # Which sides a model marginalises is its risk-set axis, NOT which indices
  # happen to exist. A choice model carries both a (constant) sender index and a
  # receiver index, but its cpp counterpart accumulates receiver margins only --
  # so handing the kernel both would give a gather fit a sender margin its cpp
  # counterpart does not have, and the two backends would disagree in SHAPE
  # while agreeing in every value. Blanking the unused side here keeps the
  # kernels free of model knowledge: they already read an empty index as "no
  # such side".
  axis <- risk_set_axis(spec)
  margin_i <- margin_slots(index_i)
  margin_j <- margin_slots(index_j)
  if (identical(axis, "receiver_given_sender")) {
    margin_i <- integer(0) # receiver margins only
  } else if (identical(axis, "sender")) {
    margin_j <- integer(0) # sender margins only
  }
  # dyad / dyad_symmetric keep both sides.

  if (identical(risk_set_normalizer(spec), "multinomial")) {
    res <- compute_multinomial_selection(
      parameters,
      stat_all_events,
      n_candidates,
      selected,
      margin_i,
      margin_j,
      n_actors_1,
      n_actors_2,
      return_event_scores,
      return_ranks,
      return_margins,
      return_probabilities,
      return_availability
    )
  }

  if (identical(risk_set_normalizer(spec), "poisson")) {
    res <- compute_poisson_selection(
      parameters,
      stat_all_events,
      n_candidates,
      selected,
      timespan,
      is_dependent,
      margin_i,
      margin_j,
      n_actors_1,
      n_actors_2,
      return_event_scores,
      return_ranks,
      return_margins,
      return_probabilities,
      return_availability,
      return_conditional_scores
    )
  }

  if (identical(risk_set_normalizer(spec), "coordination")) {
    res <- compute_coordination_selection(
      parameters,
      stat_all_events,
      n_candidates,
      selected,
      sender_of_row,
      dyad_partner,
      margin_i,
      margin_j,
      n_actors_1,
      return_event_scores,
      return_ranks,
      return_margins,
      return_probabilities,
      return_availability
    )
  }

  return(res)
}
