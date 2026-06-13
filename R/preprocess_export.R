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
    choices = c("DyNAM", "REM")
  )
  sub_model <- match.arg(sub_model)
  if (is.null(progress)) progress <- FALSE

  compute_stats(
    formula = formula,
    data = data,
    model = model,
    sub_model = sub_model,
    output = "gather",
    control_preprocessing = control_preprocessing,
    progress = progress
  )
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
