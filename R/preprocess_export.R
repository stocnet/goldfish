# #
# # Author(s): AU
# #
# #
# # Description: Helper functions to gather the preprocess data from a model


#' Gather model data from a formula
#'
#' Gather the preprocess data from a formula given a model and sub model,
#' where the output corresponds to the data structure used by the engine
#' `gather_compute`; see [estimate].
#'
#' It differs from the `estimate_dynam()`, `estimate_rem()` and
#' `estimate_dynami()` output when the argument `preprocessing_only`
#' is set to `TRUE` regarding the memory space requirement.
#' The `gather_model_data()` produces a list where the first element
#' is a matrix that could have up to the number of events times
#' the number of actors rows and the number of effects columns.
#' For medium to large datasets with thousands of events and
#' thousands of actors, the memory RAM requirements are large and,
#' therefore, errors are produced due to a lack of space.
#' The advantage of the data structure is that it can be adapted
#' to estimate the models (or extensions of them) using standard packages
#' for generalized linear models (or any other model)
#' that use tabular data as input.
#'
#' @inheritParams estimate
#'
#' @param formula a formula object that defines at the
#' left-hand side the dependent
#' network (see [make_dependent_events()]) and at the right-hand side the
#' effects and the variables for which the effects are expected to occur
#' (see `vignette("goldfishEffects")`).
#' @param model a character string defining the model type.
#' Current options include `"DyNAM"`, `"DyNAMi"` or `"REM"`
#' \describe{
#'  \item{DyNAM}{Dynamic Network Actor Models
#'  (Stadtfeld, Hollway and Block, 2017 and Stadtfeld and Block, 2017)}
#'  \item{DyNAMi}{Dynamic Network Actor Models for interactions
#'  (Hoffman et al., 2020)}
#'  \item{REM}{Relational Event Model (Butts, 2008)}
#' }
#' @param control_preprocessing An object of class
#'   `"preprocessing_options.goldfish"`, usually the result of a call to
#'   [set_preprocessing_opt()]. This object contains parameters that control
#'   the data preprocessing. See [set_preprocessing_opt()] for details on
#'   the available parameters.
#'
#' @return a list object including:
#'  \describe{
#'   \item{stat_all_events}{a matrix. The number of rows can be up to the number
#'    of events times the number of actors
#'    (square number of actors for the REM).
#'    Rigth-censored events are included when the model has an intercept.
#'    The number of columns is the number of effects in the model.
#'    Every row is the effect statistics at the time of the event for each actor
#'    in the choice set or the sender set.}
#'   \item{n_candidates}{
#'    a numeric vector with the number of rows related with an event.
#'    The length correspond to the number of events
#'    plus right censored events if any.}
#'   \item{selected}{a numeric vector with the position of the
#'    selected actor (choice model), sender actor (rate model), or
#'    active dyad (choice-coordination model, REM model).
#'    Indexing start at 1 for each event.}
#'   \item{sender, receiver}{
#'    a character vector with the label of the sender/receiver actor.
#'    For right-censored events the receiver values is not meaningful.}
#'   \item{has_intercept}{
#'    a logical value indicating if the model has an intercept.}
#'   \item{namesEffects}{a character vector with a short name of the effect.
#'   It includes the name of the object used to calculate the effects and
#'   modifiers of the effect, e.g., the type of effect, weighted effect.}
#'   \item{effectDescription}{
#'    a character matrix with the description of the effects.
#'    It includes the name of the object used to calculate the effects and
#'    additional information of the effect, e.g., the type of effect,
#'    weighted effect, transformation function, window length.}
#'  }
#'  If the model has an intercept and the sub_model is `rate` or model is `REM`,
#'  additional elements are included:
#'  \describe{
#'   \item{timespan}{
#'    a numeric vector with the time span between events,
#'    including right-censored events.}
#'   \item{isDependent}{
#'    a logical vector indicating if the event is dependent or right-censored.}
#'  }
#'
#' @export
#'
#' @examples
#' data("Fisheries_Treaties_6070")
#' states <- make_nodes(states)
#' states <- link_events(states, sovchanges, attribute = "present")
#' states <- link_events(states, regchanges, attribute = "regime")
#' states <- link_events(states, gdpchanges, attribute = "gdp")
#'
#' bilatnet <- make_network(bilatnet, nodes = states, directed = FALSE)
#' bilatnet <- link_events(bilatnet, bilatchanges, nodes = states)
#'
#' createBilat <- make_dependent_events(
#'   events = bilatchanges[bilatchanges$increment == 1, ],
#'   nodes = states, default_network = bilatnet
#' )
#'
#' fisheriesData <- make_data(createBilat)
#' 
#' gatheredData <- gather_model_data(
#'   createBilat ~ inertia(bilatnet) + trans(bilatnet) + tie(contignet),
#'   model = "DyNAM", sub_model = "choice_coordination",
#'   data = fisheriesData
#' )
#'
gather_model_data <- function(
    formula,
    model = c("DyNAM", "REM"),
    sub_model = c("choice", "choice_coordination", "rate"),
    data = NULL,
    control_preprocessing = set_preprocessing_opt(),
    progress = getOption("progress")
    ) {
  model <- match.arg(
    arg = if (length(model) > 1) model[1] else model,
    choices = c("DyNAM", "REM", "DyNAMRE")
  )
  sub_model <- match.arg(sub_model)

  check_model_par(
    model = model, sub_model = sub_model,
    model_list = c("DyNAM", "REM", "DyNAMRE"),
    sub_model_list = list(
      DyNAM = c("choice", "rate", "choice_coordination"),
      REM = "choice",
      DyNAMRE = c("choice", "choice_coordination")
    )
  )

  # Check class of control_preprocessing
  if (!inherits(control_preprocessing, "preprocessing_opt.goldfish")) {
    stop("'control_preprocessing' must be NULL or an object of class",
         " 'preprocessing_opt.goldfish' result of a call to",
         " 'set_preprocessing_opt()'.", call. = FALSE)
  }

  # Warning for opportunitiesList if still relevant
  if (!is.null(control_preprocessing$opportunities_list)) {
    warning(
      dQuote("gather_model_data"), " doesn't implement yet the ",
      dQuote("opportunities_list"),
      " functionality. This parameter will be ignored.",
      call. = FALSE, immediate. = TRUE
    )
  }

  if (is.null(progress)) progress <- FALSE

  # Create a working copy of the data environment to avoid side-effects
  work_env <- rlang::env_clone(data)
  
  ### 1. PARSE the formula----
  parsed_formula <- parse_formula(formula, envir = work_env)
  rhs_names <- parsed_formula$rhs_names
  dep_name <- parsed_formula$dep_name
  has_intercept <- parsed_formula$has_intercept
  window_parameters <- parsed_formula$window_parameters
  ignore_rep_parameter <- unlist(parsed_formula$ignore_rep_parameter)

  # # C implementation doesn't have ignore_repetitions option issue #105
  if (any(unlist(parsed_formula$ignore_rep_parameter))) {
    stop("gather_model_data ",
      " doesn't support ignore_repetitions effects (GH issue #105)!",
      call. = FALSE, immediate. = TRUE
    )
  }

  # Model-specific preprocessing initialization
  if (model == "DyNAMRE") {
    if (sub_model == "choice") model <- "REM" else model <- "DyNAM"
    altModel <- "DyNAMRE"
  } else {
    altModel <- NULL
  }

  if (model %in% c("DyNAM", "DyNAMi") &&
    sub_model %in% c("choice", "choice_coordination") &&
    parsed_formula$has_intercept) {
    warning("Model ", dQuote(model), " sub_model ", dQuote(sub_model),
      " ignores the time intercept.",
      call. = FALSE, immediate. = TRUE
    )
    parsed_formula$has_intercept <- FALSE
  }
  right_censored <- parsed_formula$has_intercept

  # if (progress && !all(vapply(windowParameters, is.null, logical(1)))) {
  #   cat("Creating window objects in global environment.\n")
  # }

  ### 2. INITIALIZE OBJECTS: effects, nodes, and link objects----

  if (progress) cat("Initializing objects.\n")

  ## 2.0 Set is_two_mode to define effects functions
  # get node sets of dependent variable
  .nodes <- attr(get(parsed_formula$dep_name, envir = work_env), "nodes")

  # two-mode networks(2 kinds of nodes)
  if (length(.nodes) == 2) {
    .nodes2 <- .nodes[2]
    .nodes <- .nodes[1]
    is_two_mode <- TRUE
  } else {
    .nodes2 <- .nodes
    is_two_mode <- FALSE
  }

  ## 2.1 INITIALIZE OBJECTS for all cases: preprocessingInit or not
  # enviroment from which get the objects
  effects <- create_effects_functions(
    parsed_formula$rhs_names, model, sub_model,
    envir = work_env
  )
  # Get links between objects and effects for printing results
  objects_effects_link <- get_objects_effects_link(parsed_formula$rhs_names)

  ## 2.2 INITIALIZE OBJECTS for preprocessingInit == NULL

  # Initialize events list and link to objects
  events <- get_events_and_objects_link(
    parsed_formula$dep_name, parsed_formula$rhs_names,
    .nodes, .nodes2,
    envir = work_env
  )[[1]]
  # moved cleanInteractionEvents in getEventsAndObjectsLink
  events_objects_link <- get_events_and_objects_link(
    parsed_formula$dep_name, parsed_formula$rhs_names,
    .nodes, .nodes2,
    envir = work_env
  )[[2]]
  events_effects_link <- get_events_effects_link(
    events, parsed_formula$rhs_names, events_objects_link
  )

  ## 3.2 PREPROCESS when preprocessingInit == NULL
  preprocessingStat <- preprocess_monolith(
    model = model,
    subModel = sub_model,
    events = events,
    effects = effects,
    windowParameters = parsed_formula$window_parameters,
    ignoreRepParameter = ignore_rep_parameter,
    eventsObjectsLink = events_objects_link, # for data update
    eventsEffectsLink = events_effects_link,
    objectsEffectsLink = objects_effects_link, # for parameterization
    # multipleParameter = multipleParameter,
    nodes = .nodes,
    nodes2 = .nodes2,
    is_two_mode = is_two_mode,
    startTime = control_preprocessing$start_time,
    endTime = control_preprocessing$end_time,
    rightCensored = right_censored,
    progress = progress,
    prepEnvir = work_env
  )

  # # 3.3 additional processing to flat array objects
  allowReflexive <- is_two_mode

  reduceArrayToMatrix <- FALSE

  if (!is.null(altModel) && sub_model == "choice") model <- "DyNAM"

  if (model == "REM") {
    modelTypeCall <- if (!parsed_formula$has_intercept) "REM-ordered" else "REM"
  } else if (model == "DyNAM") {
    if (sub_model == "rate" && !parsed_formula$has_intercept) {
      modelTypeCall <- "DyNAM-M-Rate-ordered"
    } else if (sub_model == "rate") {
      modelTypeCall <- "DyNAM-M-Rate"
    } else if (sub_model == "choice_coordination") {
      modelTypeCall <- "DyNAM-MM"
    } else {
      modelTypeCall <- "DyNAM-M"
      reduceArrayToMatrix <- TRUE
    }
  }

  is_rate_model <- modelTypeCall %in% c("DyNAM-M-Rate", "DyNAM-M-Rate-ordered")

  preprocessingStat <- modifyStatisticsList(
    preprocessingStat, modelTypeCall,
    reduceArrayToMatrix = reduceArrayToMatrix,
    excludeParameters = NULL,
    addInterceptEffect = parsed_formula$has_intercept
  )

  nodes <- get(.nodes, envir = data)
  nodes2 <- get(.nodes2, envir = data)

  ## SET VARIABLES BASED ON STATSLIST
  twomode_or_reflexive <- (allowReflexive || is_two_mode)
  if (is_rate_model) {
    n_parameters <- ncol(preprocessingStat$initialStats)
    n_actors1 <- nrow(preprocessingStat$initialStats)
    n_actors2 <- 1L
  } else {
    dimensions <- dim(preprocessingStat$initialStats)
    n_parameters <- dimensions[3]
    n_actors1 <- dimensions[1]
    n_actors2 <- dimensions[2]
  }

  dep_idx <- preprocessingStat$is_dependent == 1L
  rc_idx  <- preprocessingStat$is_dependent == 0L

  expand_for_c <- function(changes_list) {
    lapply(changes_list, function(event_changes) {
      lapply(event_changes, function(ch) {
        if (is.null(ch)) return(NULL)
        if (!is.matrix(ch)) ch <- matrix(ch, nrow = 1L, dimnames = list(NULL, names(ch)))
        cbind(node1 = ch[, "node1"], node2 = 1L, replace = ch[, "replace"])
      })
    })
  }

  dep_changes <- if (is_rate_model) expand_for_c(preprocessingStat$stats_change[dep_idx]) else preprocessingStat$stats_change[dep_idx]
  temp <- convert_change(dep_changes)
  stat_mat_update <- temp$statMatUpdate
  stat_mat_update_pointer <- temp$statMatUpdatePointer
  if (parsed_formula$has_intercept) stat_mat_update[3, ] <- stat_mat_update[3, ] + 1

  if (sum(rc_idx) > 0L) {
    rc_changes <- if (is_rate_model) expand_for_c(preprocessingStat$stats_change[rc_idx]) else preprocessingStat$stats_change[rc_idx]
    temp <- convert_change(rc_changes)
    stat_mat_rightcensored_update <- temp$statMatUpdate
    stat_mat_rightcensored_update_pointer <- temp$statMatUpdatePointer
    if (parsed_formula$has_intercept) stat_mat_rightcensored_update[3, ] <- stat_mat_rightcensored_update[3, ] + 1

    counts_dep <- diff(c(0L, stat_mat_update_pointer))
    counts_rc <- diff(c(0L, stat_mat_rightcensored_update_pointer))
    counts <- integer(length(dep_idx))
    counts[dep_idx] <- counts_dep
    counts[rc_idx] <- counts_rc
    ordering <- order(
      c(rep(which(dep_idx), counts_dep), rep(which(rc_idx), counts_rc)),
      method = "radix"
    )
    stat_mat_update <- cbind(
      stat_mat_update, stat_mat_rightcensored_update
    )[, ordering, drop = FALSE]
    stat_mat_update_pointer <- cumsum(counts)
  }

  ## CONVERT COMPOSITION CHANGES INTO THE FORMAT ACCEPTED BY C FUNCTIONS
  hasCompChange1 <- length(preprocessingStat$active_mode1_changes) > 0
  hasCompChange2 <- length(preprocessingStat$active_mode2_changes) > 0

  if (hasCompChange1) {
    compChange1 <- data.frame(
      time = vapply(preprocessingStat$active_mode1_changes, `[[`, double(1), "time"),
      node = vapply(preprocessingStat$active_mode1_changes, `[[`, integer(1), "node"),
      replace = vapply(preprocessingStat$active_mode1_changes, `[[`, logical(1), "replace")
    )
    temp <- C_convert_composition_change(compChange1, preprocessingStat$event_time)
    presence1_update <- temp$presenceUpdate
    presence1_update_pointer <- temp$presenceUpdatePointer
  } else {
    presence1_update <- matrix(0, 0, 0)
    presence1_update_pointer <- numeric(1)
  }

  if (hasCompChange2) {
    compChange2 <- data.frame(
      time = vapply(preprocessingStat$active_mode2_changes, `[[`, double(1), "time"),
      node = vapply(preprocessingStat$active_mode2_changes, `[[`, integer(1), "node"),
      replace = vapply(preprocessingStat$active_mode2_changes, `[[`, logical(1), "replace")
    )
    temp <- C_convert_composition_change(compChange2, preprocessingStat$event_time)
    presence2_update <- temp$presenceUpdate
    presence2_update_pointer <- temp$presenceUpdatePointer
  } else {
    presence2_update <- matrix(0, 0, 0)
    presence2_update_pointer <- numeric(1)
  }

  presence1_init <- preprocessingStat$active_mode1_init
  presence2_init <- preprocessingStat$active_mode2_init

  ## CONVERT TYPES OF EVENTS AND TIMESPANS INTO THE FORMAT ACCEPTED
  ## BY C FUNCTIONS
  if (modelTypeCall %in% c("DyNAM-M-Rate", "REM", "DyNAM-MM")) {
    is_dependent <- as.logical(preprocessingStat$is_dependent)
    timespan <- if (modelTypeCall != "DyNAM-MM") preprocessingStat$intervals else numeric(length(is_dependent))
  } else {
    timespan <- NA
  }

  ## CONVERT INFOS OF SENDERS AND RECEIVERS INTO THE FORMAT ACCEPTED
  ## BY C FUNCTIONS
  event_mat <- rbind(
    preprocessingStat$event_sender, preprocessingStat$event_receiver
  )

  ## CONVERT THE INITIALIZATION OF DATA MATRIX INTO THE FORMAT ACCEPTED
  ## BY C FUNCTIONS
  if (is_rate_model) {
    stat_mat_init <- preprocessingStat$initialStats
  } else {
    stat_mat_init <- matrix(0, n_actors1 * n_actors2, n_parameters)
    for (i in seq_len(n_parameters)) {
      stat_mat_init[, i] <- t(preprocessingStat$initialStats[, , i])
    }
  }

  gatheredData <- gather_(
    modelTypeCall = modelTypeCall,
    event_mat = event_mat,
    timespan = timespan,
    is_dependent = is_dependent,
    stat_mat_init = stat_mat_init,
    stat_mat_update = stat_mat_update,
    stat_mat_update_pointer = stat_mat_update_pointer,
    presence1_init = presence1_init,
    presence1_update = presence1_update,
    presence1_update_pointer = presence1_update_pointer,
    presence2_init = presence2_init,
    presence2_update = presence2_update,
    presence2_update_pointer = presence2_update_pointer,
    n_actors1 = n_actors1,
    n_actors2 = n_actors2,
    twomode_or_reflexive = twomode_or_reflexive,
    verbose = progress, # If not silent, output the progress of data gathering
    impute = FALSE
  )

  ## Add additional information
  gatheredData$sender <- nodes$label[preprocessingStat$event_sender]
  if (model == "REM" || (model == "DyNAM" && sub_model != "rate")) {
    gatheredData$receiver <-
      nodes2$label[preprocessingStat$event_receiver]
  } else if (model == "DyNAM" && sub_model == "rate" &&
    parsed_formula$has_intercept) {
    gatheredData$timespan <- timespan
    gatheredData$isDependent <- is_dependent
  }
  gatheredData$has_intercept <- parsed_formula$has_intercept

  gatheredData$selected <- gatheredData$selected +
    if (parsed_formula$has_intercept) (1 * is_dependent) else 1

  ### 4. PREPARE PRINTING----
  # functions_utility.R
  effectDescription <-
    GetDetailPrint(objects_effects_link, parsed_formula)
  hasWindows <- attr(effectDescription, "hasWindows")

  namesEffects <- CreateNames(effectDescription, sep = "_", joiner = "_")

  gatheredData$namesEffects <- namesEffects
  colnames(gatheredData$stat_all_events) <- namesEffects
  gatheredData$effectDescription <- effectDescription

  return(gatheredData)
}

#' Add labels and effect names to a native gather stack
#'
#' Completes the gather output produced by `gather_from_prep()` (via
#' `writer_gather()`) with the sender/receiver labels, the rate-model
#' `timespan` / `isDependent` fields, and the `namesEffects` /
#' `effectDescription` printing metadata, matching the field set and order of
#' the legacy `gather_model_data()` result. Internal carry attributes are
#' stripped so the returned list is value-comparable to the legacy output.
#'
#' @noRd
finalize_gather_output <- function(
  gathered, model, sub_model, has_intercept, nodes, nodes2,
  objects_effects_link, parsed_formula
) {
  event_sender <- attr(gathered, "event_sender")
  event_receiver <- attr(gathered, "event_receiver")
  is_dependent <- attr(gathered, "is_dependent")
  timespan <- attr(gathered, "timespan")

  gathered$sender <- nodes$label[event_sender]
  if (model == "REM" || (model == "DyNAM" && sub_model != "rate")) {
    gathered$receiver <- nodes2$label[event_receiver]
  } else if (model == "DyNAM" && sub_model == "rate" && has_intercept) {
    gathered$timespan <- timespan
    gathered$isDependent <- is_dependent
  }

  effectDescription <- GetDetailPrint(objects_effects_link, parsed_formula)
  namesEffects <- CreateNames(effectDescription, sep = "_", joiner = "_")

  gathered$namesEffects <- namesEffects
  colnames(gathered$stat_all_events) <- namesEffects
  gathered$effectDescription <- effectDescription

  attr(gathered, "event_sender") <- NULL
  attr(gathered, "event_receiver") <- NULL
  attr(gathered, "is_dependent") <- NULL
  attr(gathered, "timespan") <- NULL
  attr(gathered, "model_type_call") <- NULL
  gathered
}

#' Generate names for statistics effects
#'
#' Using the names data frame from `goldfish` generate compact names to the
#' columns for data frame or matrix
#'
#' @param names data frame from `goldfish`
#' @param sep string. Separator between different arguments and objects
#' @param joiner string. Separator to join multiple object names
#'
#' @return a string vector with the names.
#' @noRd
#'
#' @examples
#' names <- cbind(
#'   Object = c("bilatnet", "bilatnet", "contignet"),
#'   Weighted = c("W", "", "W")
#' )
#' rownames(names) <- c("inertia", "trans", "tie")
#' CreateNames(names, sep = "|")
CreateNames <- function(
    names, sep = " ", joiner = ", ") {
  isObjectD <- grepl("Object \\d+", colnames(names))
  if (any(isObjectD)) {
    object <- apply(
      names[, isObjectD], 1,
      function(z) {
        ret <- Filter(function(w) !is.na(w) & w != "", z)
        ret <- paste(ret, collapse = joiner)
        return(ret)
      }
    )
    newNames <- c("Object", colnames(names)[!isObjectD])
    names <- cbind(object, names[, !isObjectD])
    colnames(names) <- newNames
  }

  if ("fixed" %in% colnames(names)) {
    names[, "fixed"] <- ifelse(names[, "fixed"] == "TRUE", "Fx", "")
  }

  names <- cbind(effect = rownames(names), names)
  nombres <- apply(
    names, 1,
    function(z) {
      ret <- Filter(function(w) !is.na(w) & w != "", z)
      ret <- paste(ret, collapse = sep)
      return(ret)
    }
  )
  names(nombres) <- NULL

  return(nombres)
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
