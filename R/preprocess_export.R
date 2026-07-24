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
#' (see `vignette("goldfish_effects")`).
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
#'   `"preprocessing.goldfish"`, usually the result of a call to
#'   [set_preprocessing()]. This object contains parameters that control
#'   the data preprocessing. See [set_preprocessing()] for details on
#'   the available parameters.
#' @param max_length integer. Maximum number of characters for each produced
#'   effect/column name in `namesEffects` (default `63`, a database-safe value).
#'   Names are made valid and unique; the uniqueness suffix is applied after
#'   truncation so uniqueness is preserved.
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
#'   \item{index_i, index_j}{integer vectors, one entry per row of
#'    `stat_all_events`, giving each candidate row's actor identity as
#'    (1-based) node indices. Dyad rows (choice, REM, choice-coordination)
#'    carry both the sender `index_i` and the receiver `index_j`; sender-set
#'    rate rows carry `index_i` with `index_j = NA`. These decode each row to
#'    its actor(s) after the risk set has been filtered, where the positional
#'    `sender`/`receiver` (which name only the observed dyad) cannot. For
#'    one-mode choice-coordination the reflexive diagonal rows are not emitted,
#'    and a `support_constraint` emits only allowed rows.}
#'   \item{node_lookup}{a data frame mapping each modeled side's local index to
#'    original node identity, with columns `side` (1 sender-side, 2
#'    receiver-side), `local` (the 1-based index the row's `index_i`/`index_j`
#'    use), `global` (row index into the stocnet `nodes`), and `label`. Join
#'    `index_i` to `side == 1` rows and `index_j` to `side == 2` rows (a
#'    one-mode model carries side 1 only). Present on the stocnet data path;
#'    absent on the legacy environment path, which has no mode map.}
#'   \item{sender, receiver}{
#'    a character vector with the label of the sender/receiver actor.
#'    For right-censored events the receiver values is not meaningful.}
#'   \item{has_intercept}{
#'    a logical value indicating if the model has an intercept.}
#'   \item{namesEffects}{a character vector with a short name of the effect.
#'   It includes the name of the object used to calculate the effects and
#'   modifiers of the effect, e.g., the type of effect, weighted effect.}
#'   \item{effect_description}{
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
#' # The prebuilt `fisheries_treaties` stocnet (see `?fisheries_treaties`); the
#' # `treaties` layer names both the dependent process and its own network.
#' data("fisheries_treaties")
#'
#' gatheredData <- gather_model_data(
#'   treaties ~ inertia(treaties) + trans(treaties) + tie(contiguity),
#'   model = "DyNAM", sub_model = "choice_coordination",
#'   data = fisheries_treaties
#' )
#'
gather_model_data <- function(
  formula,
  model = c("DyNAM", "REM"),
  sub_model = c("choice", "choice_coordination", "rate"),
  data = NULL,
  control_preprocessing = set_preprocessing(),
  progress = getOption("progress"),
  max_length = 63L
) {
  model <- match.arg(
    arg = if (length(model) > 1) model[1] else model,
    choices = c("DyNAM", "REM")
  )
  sub_model <- match.arg(sub_model)
  if (is.null(progress)) {
    progress <- FALSE
  }

  compute_stats(
    formula = formula,
    data = data,
    model = model,
    sub_model = sub_model,
    output = "gather",
    control_preprocessing = control_preprocessing,
    progress = progress,
    max_length = max_length
  )
}

#' Add labels and effect names to a native gather stack
#'
#' Completes the gather output produced by `gather_from_prep()` (via
#' `writer_gather()`) with the sender/receiver labels, the rate-model
#' `timespan` / `isDependent` fields, and the `namesEffects` /
#' `effect_description` printing metadata, matching the field set and order of
#' the legacy `gather_model_data()` result. Internal carry attributes are
#' stripped so the returned list is value-comparable to the legacy output.
#'
#' @noRd
finalize_gather_output <- function(
  gathered,
  model,
  sub_model,
  has_intercept,
  nodes,
  nodes2,
  objects_effects_link,
  parsed_formula,
  max_length = 63L,
  effect_description = NULL
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

  # Single source of truth from the spec mapping when supplied;
  # recomputed only for callers without a spec_map (DyNAMi / legacy paths).
  effect_description <- effect_description
  if (is.null(effect_description)) {
    effect_description <- GetDetailPrint(objects_effects_link, parsed_formula)
  }
  namesEffects <- CreateNames(effect_description, max_length = max_length)

  gathered$namesEffects <- namesEffects
  colnames(gathered$stat_all_events) <- namesEffects
  gathered$effect_description <- effect_description

  attr(gathered, "event_sender") <- NULL
  attr(gathered, "event_receiver") <- NULL
  attr(gathered, "is_dependent") <- NULL
  attr(gathered, "timespan") <- NULL
  gathered
}

#' Generate names for statistics effects
#'
#' Using the names data frame from `goldfish` generate valid, unique and
#' length-bounded export names for the columns of a data frame or matrix.
#' Names are produced by the shared compact-term-string builder in export mode
#' (reading the persisted `.term_export` column when present), so dot-prefixed
#' decoder columns never leak into the output.
#'
#' @param names data frame from `goldfish`
#' @param max_length integer. Maximum length of each produced name; the
#'   uniqueness suffix is applied after truncation so uniqueness is preserved.
#'
#' @return a string vector with valid, unique names.
#' @noRd
#'
#' @examples
#' names <- cbind(
#'   Object = c("bilatnet", "bilatnet", "contignet"),
#'   weighted = c("W", "", "W")
#' )
#' rownames(names) <- c("inertia", "trans", "tie")
#' CreateNames(names)
CreateNames <- function(names, max_length = 63L) {
  hasCache <- !is.null(colnames(names)) &&
    ".term_export" %in% colnames(names)
  if (hasCache && isTRUE(max_length == 63L)) {
    nombres <- unname(names[, ".term_export"])
  } else {
    nombres <- unname(
      compact_term_strings(names, mode = "export", max_length = max_length)
    )
  }

  return(nombres)
}
