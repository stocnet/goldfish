##################### ###
#
# Goldfish package
# Diagnostic data objects: the labeled margins and the tables built from them
#
##################### ###

# Shaping and labeling the margins --------------------------------------------

# The raw margin accumulators of one compiled pass, shaped into the list both
# backends store. Two-sided sub-models (REM, REM_ordered) return sender and
# receiver margins; the single-sided ones return one pair -- rate, choice, and
# also coordination, whose kernel credits both endpoints into ONE actor set
# over `n_actors_1`. Tested on LENGTH, not on NULL: the gather kernels return
# every slot unconditionally and leave the absent side empty, so a NULL test
# would give a gather fit the two-sided shape where its cpp counterpart has the
# one-sided one.
assemble_engine_margins <- function(res) {
  has_length <- function(x) !is.null(x) && length(x) > 0
  margins <- if (has_length(res$margin_expected_sender)) {
    list(
      observed_sender = as.numeric(res$margin_observed_sender),
      expected_sender = as.numeric(res$margin_expected_sender),
      observed_receiver = as.numeric(res$margin_observed_receiver),
      expected_receiver = as.numeric(res$margin_expected_receiver)
    )
  } else if (has_length(res$margin_expected)) {
    list(
      observed = as.numeric(res$margin_observed),
      expected = as.numeric(res$margin_expected)
    )
  } else {
    NULL
  }
  # Exact-time sub-models additionally carry the probability-scale variant:
  # the compensator margins above total the event count only at the MLE, while
  # these total it at any parameter vector.
  if (!is.null(margins) && has_length(res$margin_probability)) {
    margins$expected_probability <- as.numeric(res$margin_probability)
  } else if (!is.null(margins) && has_length(res$margin_probability_sender)) {
    margins$expected_probability_sender <-
      as.numeric(res$margin_probability_sender)
    margins$expected_probability_receiver <-
      as.numeric(res$margin_probability_receiver)
  }
  margins
}

# The raw availability accumulators of one compiled pass, shaped into the list
# both backends store. The shape is the margins' shape by construction, so the
# same side rule and the same labeling helper serve both: two-sided sub-models
# return a sender and a receiver vector per quantity, the single-sided ones one
# vector each. Tested on LENGTH for the same reason the margins are: the gather
# kernels return every slot unconditionally and leave the absent side empty.
#
# `exposure` is exact-time only. Its absence on a multinomial family is not a
# gap but the fact that a compensator scale is not defined there, exactly as
# `total_rate` is absent from the same fits.
assemble_engine_availability <- function(res) {
  has_length <- function(x) !is.null(x) && length(x) > 0
  components <- list(
    exposure = res$availability_exposure,
    n_opportunities = res$availability_n_opportunities,
    exposure_sender = res$availability_exposure_sender,
    exposure_receiver = res$availability_exposure_receiver,
    n_opportunities_sender = res$availability_n_opportunities_sender,
    n_opportunities_receiver = res$availability_n_opportunities_receiver
  )
  components <- components[vapply(components, has_length, logical(1))]
  if (length(components) == 0) {
    return(NULL)
  }
  lapply(components, as.numeric)
}

# Labeling the stored margins -------------------------------------------------

# The margin accumulators leave every kernel as bare numerics: the actor a
# position stands for is implicit in the accumulator's side, and the scale of
# `expected` is implicit in the model family (a compensator on the exact-time
# families, a probability sum on the multinomial ones). Both are resolved here,
# at the single assembly point each backend passes through, so a consumer reads
# them off the stored object instead of re-deriving the family -- and so the
# labels cannot differ between backends.
#
# `margins` is the raw list either backend assembles, `axis` is
# `risk_set_axis(spec)`, and `is_exact_time` marks the Poisson families, which
# carry both scales.
label_margins <- function(margins, axis, nodes, nodes2, is_exact_time) {
  if (is.null(margins)) {
    return(NULL)
  }
  labels <- actor_side_labels(names(margins), axis, nodes, nodes2)
  for (component in names(margins)) {
    margins[[component]] <- label_margin_vector(
      margins[[component]],
      labels[[component]],
      margin_component_scale(component, is_exact_time)
    )
  }
  margins
}

# The availability vectors through the same side rule and the same labeling the
# margins go through -- one convention, so a per-actor join never has to
# reconcile two spellings of the same actor set. No scale marker: neither
# quantity is a fitted mass, so there is nothing for one to disambiguate.
label_availability <- function(availability, axis, nodes, nodes2) {
  if (is.null(availability)) {
    return(NULL)
  }
  labels <- actor_side_labels(names(availability), axis, nodes, nodes2)
  for (component in names(availability)) {
    availability[[component]] <- label_margin_vector(
      availability[[component]],
      labels[[component]],
      NA_character_
    )
  }
  availability
}

# Which node set each per-actor component indexes: a `_sender` accumulator is
# always side 1 and a `_receiver` one always side 2, while a single-sided family
# follows its axis -- choice accumulates over receivers, rate over senders, and
# coordination over the one actor set into which it credits both endpoints.
actor_side_labels <- function(components, axis, nodes, nodes2) {
  labels_sender <- node_labels(nodes)
  labels_receiver <- node_labels(nodes2)
  labels_single <- if (identical(axis, "receiver_given_sender")) {
    labels_receiver
  } else {
    labels_sender
  }
  stats::setNames(
    lapply(components, function(component) {
      if (endsWith(component, "_sender")) {
        labels_sender
      } else if (endsWith(component, "_receiver")) {
        labels_receiver
      } else {
        labels_single
      }
    }),
    components
  )
}

# The scale marker a component carries, or NA for the observed counts, which do
# not vary with the scale. `expected` is the compensator sum on an exact-time
# fit and the probability sum on a multinomial one -- the ambiguity this marker
# exists to settle.
margin_component_scale <- function(component, is_exact_time) {
  if (startsWith(component, "observed")) {
    return(NA_character_)
  }
  if (startsWith(component, "expected_probability")) {
    return("probability")
  }
  if (is_exact_time) "expected_count" else "probability"
}

# Names are attached only when the side's label vector matches the accumulator,
# so an unforeseen family shape stays unlabeled rather than mislabeled.
label_margin_vector <- function(x, labels, scale) {
  if (length(x) == length(labels)) {
    names(x) <- labels
  }
  if (!is.na(scale)) {
    attr(x, "scale") <- scale
  }
  x
}

node_labels <- function(nodes) {
  nodes$label %||% as.character(seq_len(nrow(nodes)))
}

# The diagnostic table constructor --------------------------------------------

#' The diagnostic table contract
#'
#' @description
#' Every diagnostic data object goldfish returns carries the producing
#' function's name at the head of its classes, and enough metadata for a print
#' or plot method to describe the object without reaching back into the fit it
#' came from. Where one table says everything the object *is* a
#' [tibble::tibble()] — `margin_table()`, `diagnose_outliers()`,
#' `diagnose_changepoints()`. Where it does not, the object is a classed list
#' whose components are each a tibble: `diagnose_onset()` carries a parameter
#' path, an accrual curve and a per-coefficient summary, which one rectangle
#' cannot hold. The metadata is the same either way.
#'
#' @details
#' The metadata travels as attributes:
#' \describe{
#'   \item{`diagnostic`}{a character value naming the producer, which is also
#'     the object's first class (`"margin_table"`).}
#'   \item{`context`}{a list describing the fit the table was computed from:
#'     `model`, `sub_model`, `backend`, `n_events`, the per-role observed
#'     totals, the node-set names (and, on a two-mode fit, `two_mode = TRUE`),
#'     `flavor` / `fid` (`NA` on a single-process fit), and `defined_scales`,
#'     the expected columns that are defined for the family.}
#'   \item{`params`}{a list of the producer's own arguments that shape how the
#'     table is read.}
#'   \item{`version`}{the goldfish version that produced the object.}
#'   \item{`defining`}{the columns without which the table is no longer the
#'     diagnostic object — the flag column a print counts and the series a
#'     plot draws.}
#' }
#'
#' The schema does not vary with the result. A `diagnose_outliers()` table
#' that flags nothing has the same columns and the same one row per interval
#' as one that flags ten, with `outlier` all `FALSE`; an expected column that
#' the model family does not define is `NA` rather than absent. Emptiness
#' lives in the content, never in the shape, so a consumer reads one schema in
#' every case.
#'
#' A tibble base keeps a diagnostic table usable with the ordinary data
#' verbs: subsetting with `[`, and dplyr's `filter()`, `mutate()` and
#' `arrange()`, preserve the class and the metadata. A `group_by()` plus
#' `summarise()` drops them, which is correct -- the summary is no longer the
#' diagnostic object. **An operation that removes a defining column drops
#' them for the same reason**: the object returns as a plain tibble, the way
#' a `grouped_df` demotes when its groups are gone. Without that, a column
#' subset would keep the class while the print method counted a flag column
#' that is no longer there, reporting no findings on a table that has them.
#'
#' # A multi-process fit
#'
#' A fit holding several processes returns the same object, with its
#' per-process tables stacked and two columns appended: `flavor` and `family`,
#' saying which process each row came from. Appended rather than prepended, so
#' the diagnostic's own columns stay where a single-process table puts them.
#' The blocks come flavor-major, the order the fit prints its components in, so
#' two diagnostics of one fit never need reordering against each other.
#'
#' `flavor` and `family` are **identity** columns and not defining ones.
#' Dropping them leaves a table that is still the diagnostic — one that has
#' merely stopped saying which process each row is from — so the class, the
#' metadata and the plot method survive it. A defining column is one the object
#' cannot be read without; knowing the provenance of a row is not that.
#'
#' A diagnostic that does not fit in one rectangle returns a classed list of
#' tibbles instead, and there each component carries the two columns. The
#' metadata is the container's own rather than any one process's: a field
#' differing between processes, such as `sub_model`, holds the set of its
#' values, and the counts total over the processes the object holds.
#'
#' @name diagnostic-tables
#' @seealso [margin_table()], the producer of the first diagnostic table, and
#'   [diagnostic-requirements] for what each diagnostic needs from a fit.
NULL

#' What a diagnostic needs from a fit
#'
#' @description
#' The diagnostics divide into three entry costs, and which one a function has
#' is the first thing to know about it: some read what the fit already carries,
#' some need a primitive that had to be requested at estimation, and some need
#' the model's statistics and one evaluation pass over them. This page is the
#' shared vocabulary; each function's own page states where it falls.
#'
#' @details
#' # What a diagnostic needs
#'
#' Three things can be asked of a fit, in increasing order of what they cost to
#' have available.
#'
#' \describe{
#'   \item{**Always present**}{the coefficients, the total information matrix,
#'     the interval clock (`intervals`, `start_time`, `end_time`), the event
#'     times and the right-censoring indicator. These ride on every fit
#'     regardless of what was requested, so a diagnostic reading only these has
#'     no requirement to state.}
#'   \item{**A stored primitive**}{a per-interval quantity accumulated during
#'     estimation and kept on the result, requested through
#'     `set_algorithm_newton(diagnostics = )`: `"loglik"`, `"scores"`,
#'     `"ranks"`, `"margins"`, `"availability"`, `"probabilities"`,
#'     `"conditional_scores"`. A primitive not requested is not recoverable
#'     from the fit — it was never computed — so a diagnostic needing one
#'     aborts naming the primitive, and the remedy is to re-estimate with it in
#'     the `diagnostics` vector. The default is `c("loglik", "scores")`.}
#'   \item{**The model's statistics**}{the preprocessed design the model was
#'     estimated from, which a fit does not carry by default because it is
#'     large. Two routes make it available: `estimate_*(return_preprocessed =
#'     TRUE)` attaches it to the result, and `preprocessed = ` supplies an
#'     equivalent object from [compute_statistics()] to the diagnostic
#'     directly. A diagnostic needing it aborts naming **both** routes.}
#' }
#'
#' # What a pass costs
#'
#' A diagnostic that needs the statistics needs them because it recomputes
#' something the fit does not store, and that recomputation is **one
#' evaluation pass** — a single walk of the event sequence through
#' [evaluate_model()], with no Newton-Raphson iterations. It is the cost of one
#' likelihood evaluation, not of a re-fit. What forces a pass is a per-interval
#' quantity no primitive holds: the per-interval expected information is the
#' clearest case, since a fit keeps only the total.
#'
#' # The family side by side
#'
#' \tabular{lll}{
#'   **Function** \tab **Stored primitive** \tab **Statistics / passes** \cr
#'   [test_gof()] \tab `"scores"` \tab none \cr
#'   [test_parameter()] \tab none \tab required, 1 pass \cr
#'   [test_time()] \tab none \tab required, 1 pass \cr
#'   [diagnose_outliers()] \tab `"loglik"` \tab none \cr
#'   [diagnose_changepoints()] \tab `"loglik"` or `"scores"` \tab none \cr
#'   [diagnose_onset()] \tab `"scores"` \tab `information = "expected"` only,
#'     1 pass \cr
#'   [residuals()] \tab varies by `type` \tab varies by `type` \cr
#'   [fitted()] / [predict()] \tab none \tab required, 1 pass \cr
#'   [margin_table()] \tab `"margins"` \tab none \cr
#'   [evaluate_model()] \tab none \tab required, 1 pass \cr
#' }
#'
#' Two readings the table is for. The `test_*` family's entry costs are
#' **uneven** — `test_gof()` needs a primitive and no pass, the other two need
#' the statistics — which is a consequence of what each statistic is rather
#' than an inconsistency: a cumulative score process is a reduction of stored
#' score rows, while an augmented-model score test needs per-interval
#' information that is stored nowhere. And a `residuals()` `type` spans the
#' whole range on its own, from `"cox_snell"` (the interval clock and
#' `"loglik"`, no pass) to `"dfbeta"` (the statistics and a pass).
#'
#' # A fit holding several processes
#'
#' A multi-flavor specification fits one process per flavor and sub-model, and
#' every diagnostic above works on the container as well as on one process. The
#' return takes one of two shapes, and which one it takes follows from what the
#' diagnostic returns rather than from any choice made per function:
#'
#' \describe{
#'   \item{**A table** — [augment()], [diagnose_outliers()],
#'     [diagnose_changepoints()], [model_terms()], [margin_table()], and the
#'     `test_*` family}{the per-process tables are row-bound, with `flavor` and
#'     `family` appended after the existing columns. A column can carry the
#'     identity, so one rectangle still holds the answer.}
#'   \item{**A list keyed by process label** — [residuals()], [fitted()],
#'     [predict()], [evaluate_model()]}{these return vectors, matrices and
#'     lists, and none of which can carry a column saying where it came from.
#'     The entries are named as the fit prints its components, in the same
#'     flavor-major order every other view uses.}
#' }
#'
#' The four list-shaped methods take `flavor =`, which narrows the selection.
#' Where a flavor is fitted with a single sub-model — the ordinary case —
#' that leaves one process and the return is the plain single-fit shape, so
#' the familiar object is one argument away rather than an extraction. Where a
#' flavor spans several sub-models it leaves several, and the return is the list
#' restricted to them: `flavor =` selects, it does not ask which sub-model was
#' meant. Naming a flavor the fit does not carry is an error that lists the ones
#' it does.
#'
#' A diagnostic that refuses on one process names that process. Each carries its
#' own formula and its own stored primitives, so one can lack a term or a
#' statistic the others have, and a message that does not say which of several
#' processes refused is not actionable.
#'
#' @name diagnostic-requirements
#' @seealso [set_algorithm_newton()] for requesting a primitive,
#'   [compute_statistics()] for producing the statistics, [evaluate_model()]
#'   for the pass itself, and [diagnostic-tables] for what the returned object
#'   carries.
NULL

new_diagnostic_table <- function(
  df,
  class,
  context = list(),
  params = list(),
  defining = character()
) {
  out <- tibble::as_tibble(df)
  class(out) <- c(class, class(out))
  stamp_diagnostic_metadata(out, class, context, params, defining)
}

# The same contract where one rectangle does not hold the object: a classed
# list whose components are each a tibble. The metadata is identical, so a
# consumer reads `diagnostic` / `context` / `params` off either shape without
# first asking which one it received.
new_diagnostic_list <- function(
  components,
  class,
  context = list(),
  params = list()
) {
  out <- components
  class(out) <- c(class, "list")
  stamp_diagnostic_metadata(out, class, context, params)
}

stamp_diagnostic_metadata <- function(
  out,
  class,
  context,
  params,
  defining = character()
) {
  attr(out, "diagnostic") <- class
  attr(out, "context") <- context
  attr(out, "params") <- params
  attr(out, "version") <- as.character(utils::packageVersion("goldfish"))
  if (length(defining) > 0) {
    attr(out, "defining") <- defining
  }
  out
}

# Demotion: what happens when an operation returns a table missing a column
# that defines it. Class preservation through `[` is the feature -- a filtered
# or reordered table is still the diagnostic object -- and it is also the
# hazard, because a print or plot method then reads a column that is gone and
# reports a wrong number instead of failing. Losing the defining column is
# losing the object, which is how a `grouped_df` treats losing its groups.
demote_if_incomplete <- function(out) {
  defining <- attr(out, "defining")
  if (is.null(defining) || all(defining %in% names(out))) {
    return(out)
  }
  for (name in c("diagnostic", "context", "params", "version", "defining")) {
    attr(out, name) <- NULL
  }
  class(out) <- c("tbl_df", "tbl", "data.frame")
  out
}

# One rule for every diagnostic table, reached by two routes: base subsetting
# and, when dplyr is attached, the reconstruction step its verbs restore
# attributes through.
#' @export
`[.diagnose_outliers` <- function(x, ...) {
  out <- NextMethod()
  demote_if_incomplete(out)
}

#' @export
`[.diagnose_changepoints` <- function(x, ...) {
  out <- NextMethod()
  demote_if_incomplete(out)
}

#' @export
`[.margin_table` <- function(x, ...) {
  out <- NextMethod()
  demote_if_incomplete(out)
}

# dplyr is not a goldfish dependency, so the reconstruction method is
# registered only if dplyr is loaded -- and then the same rule applies to
# `select()`, `filter()` and the rest of the verbs that route through it.
dplyr_reconstruct_diagnostic <- function(data, template) {
  out <- NextMethod()
  demote_if_incomplete(out)
}

# The margins accessor --------------------------------------------------------

#' Observed and expected per-actor event counts
#'
#' @description
#' Presents the margins a fit stored (`diagnostics = "margins"`) as one table
#' with the same five columns for every model family, so a calibration
#' comparison of observed against expected actor activity reads the same way on
#' a DyNAM rate, a DyNAM choice, a coordination and a REM fit.
#'
#' These are *descriptives*, not per-actor tests: the differences are plug-in
#' quantities and are negatively correlated across actors. Read them as a map
#' screening for unmodeled actor heterogeneity, and test a candidate
#' explanation by adding the corresponding effect to the model.
#'
#' @details
#' The two expected columns are the two scales on which an actor's expected
#' activity is defined:
#' \describe{
#'   \item{`expected_probability`}{the sum, over the dependent events, of the
#'     fitted probability that this actor is the one involved in the next
#'     event. It totals the number of events at *any* parameter vector, so
#'     `observed / expected_probability` is a calibration ratio that can be
#'     read on a model that has not converged.}
#'   \item{`expected_count`}{the compensator: the fitted intensity integrated
#'     over the actor's exposure time, accumulated over all intervals including
#'     the right-censored ones. `observed - expected_count` is the per-actor
#'     martingale residual. It totals the number of events at the maximum
#'     likelihood estimate, through the time-intercept score equation.}
#' }
#'
#' `expected_count` is `NA` on the multinomial families (DyNAM choice, the
#' ordinal rate and REM sub-models, and choice coordination). The `NA` means the
#' compensator scale **is not defined for the model class**, never that it was
#' not computed: those models condition on an event having occurred and carry no
#' exposure-time term. Such a fit therefore has a calibration ratio but no
#' martingale residual. Both columns are always present, so the schema does not
#' change with the family.
#'
#' The `role` column names what an actor did in the events being counted:
#' \describe{
#'   \item{`sender`}{rate sub-models, and the sender side of a REM fit.}
#'   \item{`receiver`}{choice sub-models, and the receiver side of a REM fit.}
#'   \item{`endpoint`}{choice coordination, where each event credits both
#'     members of the pair -- so the observed column totals twice the event
#'     count.}
#' }
#' A REM fit contributes both a `sender` and a `receiver` row per actor from
#' the same fit: a tie-oriented model implies both the out- and the in-degree
#' margin.
#'
#' @param x a fitted model of class `"result.goldfish"` or
#'   `"flavored_result.goldfish"`, estimated with `"margins"` among the
#'   [set_algorithm_newton()] `diagnostics` primitives.
#' @param dispersion whether to add the `dispersion` column, which needs one
#'   evaluation pass over the model's statistics. `FALSE` by default, so the
#'   ordinary call stays a read of what estimation already stored.
#' @param preprocessed a `preprocessed.goldfish` object, as returned by
#'   [compute_statistics()]. Read only when `dispersion = TRUE`, and defaulting
#'   to the object attached by `estimate_*(return_preprocessed = TRUE)`.
#' @param ... additional arguments passed to or from other methods (currently
#'   unused).
#'
#' @section Level and shape:
#' `observed` and `expected_count` are a **first-moment** view: they say whether
#' an actor acted as often as the model expected. An actor whose events are
#' correctly counted but bunched into a burst is calibrated on both of them, and
#' `dispersion` is what separates the two readings:
#'
#' ```
#'   well-timed   spans (1.0, 1.0, 1.0, 1.0)      sum 4, n 4   dispersion 0.00
#'   bursty       spans (0.01, 0.02, 0.01, 3.96)  sum 4, n 4   dispersion 3.90
#' ```
#'
#' It is the variance of that actor's own compensators — the same series
#' `residuals(type = "cox_snell", level = "actor")` returns — over the spans
#' between its consecutive events. Each is unit exponential under a correct
#' model, so the column reads against **one** the way `observed` reads against
#' `expected_count`.
#'
#' Two things it is not. It is **not defined below two completed spans**, so an
#' actor with fewer than two events is `NA`, and on a real event stream that is
#' most actors: it is read beside `observed` and never on its own. And the final
#' span, running to the end of the observation window, is excluded — it closes
#' no event, so counting it would drag an actor's variance toward zero in
#' proportion to how early it stopped acting.
#'
#' @return A [tibble::tibble()] of class `margin_table` with columns
#'   \describe{
#'     \item{actor}{the actor label, from the node set the margin indexes.}
#'     \item{role}{`"sender"`, `"receiver"` or `"endpoint"`, as above.}
#'     \item{observed}{the actor's tabulated count in the dependent events.}
#'     \item{expected_probability}{the probability-scale expected count.}
#'     \item{expected_count}{the compensator-scale expected count, `NA` on the
#'       multinomial families.}
#'     \item{dispersion}{present only when `dispersion = TRUE`: the variance of
#'       the actor's own compensators, `NA` on the multinomial families (which
#'       define no waiting time) and wherever the actor completed fewer than two
#'       spans. See \emph{Level and shape}.}
#'   }
#'   A multi-process (flavored) fit adds `flavor` and `family` columns from the
#'   fit's process map. The object carries the diagnostic metadata described in
#'   [diagnostic-tables].
#'
#' @examples
#' data("social_evolution")
#' choice <- estimate_dynam(
#'   calls ~ inertia + recip + trans,
#'   sub_model = "choice",
#'   data = social_evolution,
#'   control_algo = set_algorithm_newton(
#'     diagnostics = c("loglik", "margins")
#'   )
#' )
#' margin_table(choice)
#'
#' @seealso [estimate_dynam()] and [estimate_rem()] for fitting,
#'   [set_algorithm_newton()] for requesting the primitive, and
#'   [diagnostic-tables] for the metadata a diagnostic table carries.
#' @export
margin_table <- function(x, ...) {
  UseMethod("margin_table")
}

#' @export
margin_table.default <- function(x, ...) {
  cli::cli_abort(c(
    "{.fn margin_table} needs a fitted goldfish model.",
    "x" = "{.arg x} is {.obj_type_friendly {x}}.",
    "i" = "Fit one with {.fn estimate_dynam} or {.fn estimate_rem}."
  ))
}

#' @export
margin_table.result.goldfish <- function(
  x,
  dispersion = FALSE,
  preprocessed = NULL,
  ...
) {
  rows <- margin_rows(x, dispersion = dispersion, preprocessed = preprocessed)
  new_diagnostic_table(
    rows$table,
    class = "margin_table",
    context = margin_context(x, rows),
    params = list(scales = rows$defined_scales),
    defining = "observed"
  )
}

#' @export
margin_table.flavored_result.goldfish <- function(
  x,
  dispersion = FALSE,
  preprocessed = NULL,
  ...
) {
  map <- x$process_map
  processes <- flavored_processes(x)
  per_fid <- lapply(processes, function(process) {
    rows <- margin_rows(
      process$fit,
      flavor = process$flavor,
      family = process$family,
      dispersion = dispersion,
      preprocessed = preprocessed
    )
    rows$table <- append_process_identity(rows$table, process)
    rows
  })
  tables <- lapply(per_fid, `[[`, "table")
  context <- margin_context(processes[[1]]$fit, per_fid[[1]])
  # The combined table describes the whole multi-process fit, so the per-fid
  # identity is what varies: the context keeps the map columns rather than one
  # fid's own flavor, and the defined scales are the union over the processes.
  # Flavor-major throughout, matching the rows: a context listing the processes
  # in a different order from the table it describes is the same disagreement
  # this method was fixed for, one level down.
  context$model <- x$model
  context$sub_model <- unique(vapply(processes, `[[`, character(1), "family"))
  context$flavor <- unique(vapply(processes, `[[`, character(1), "flavor"))
  context$fid <- vapply(processes, `[[`, map$fid[1], "fid")
  context$n_events <- vapply(
    processes,
    function(process) as.integer(process$fit$n_events),
    integer(1)
  )
  defined <- unique(unlist(lapply(per_fid, `[[`, "defined_scales")))
  context$defined_scales <- defined
  context$role_totals <- role_totals(do.call(rbind, tables))
  new_diagnostic_table(
    do.call(rbind, tables),
    class = "margin_table",
    context = context,
    params = list(scales = defined),
    defining = "observed"
  )
}

# One fit's margins as the five-column table, plus the facts the metadata needs.
# `flavor` / `family` name the process this fit is, on a multi-process fit.
margin_rows <- function(
  x,
  flavor = NA_character_,
  family = NA_character_,
  dispersion = FALSE,
  preprocessed = NULL,
  call = rlang::caller_env()
) {
  margins <- x$margins
  if (is.null(margins)) {
    cli::cli_abort(
      c(
        "This fit stored no margins.",
        "i" = "Refit with {.arg diagnostics} including {.val margins} in
               {.fn set_algorithm_newton}."
      ),
      call = call
    )
  }
  sides <- margin_sides(margins, risk_set_axis(x), call = call)
  spread <- if (isTRUE(dispersion)) {
    actor_dispersion(x, preprocessed, call = call)
  } else {
    NULL
  }
  tables <- lapply(sides, function(side) {
    margin_side_table(margins, side, spread)
  })
  list(
    table = do.call(rbind, lapply(tables, `[[`, "table")),
    defined_scales = unique(unlist(lapply(tables, `[[`, "defined_scales"))),
    flavor = flavor,
    family = family
  )
}

# The sides a family's margins carry, in table order. Only REM / REM_ordered
# (axis "dyad") accumulate separate sender and receiver vectors; the sender,
# receiver and coordination families each carry one pair, whose role the axis
# names.
margin_sides <- function(margins, axis, call = rlang::caller_env()) {
  if ("observed_sender" %in% names(margins)) {
    return(list(
      list(suffix = "_sender", role = "sender"),
      list(suffix = "_receiver", role = "receiver")
    ))
  }
  role <- switch(
    axis %||% "",
    sender = "sender",
    receiver_given_sender = "receiver",
    dyad_symmetric = "endpoint",
    cli::cli_abort(
      c(
        "Cannot name the margin role of this fit.",
        "x" = "Unknown risk-set axis {.val {axis}}."
      ),
      call = call
    )
  )
  list(list(suffix = "", role = role))
}

margin_side_table <- function(margins, side, spread = NULL) {
  observed <- margins[[paste0("observed", side$suffix)]]
  expected <- margins[[paste0("expected", side$suffix)]]
  probability <- margins[[paste0("expected_probability", side$suffix)]]
  # Which scale `expected` is on: read the marker the estimation pass attaches.
  # A fit stored before the marker existed is resolved from the presence of the
  # separate probability variant, which only the exact-time families carry.
  scale <- attr(expected, "scale") %||%
    if (is.null(probability)) "probability" else "expected_count"
  if (identical(scale, "expected_count")) {
    expected_count <- expected
    expected_probability <- probability
  } else {
    expected_count <- NULL
    expected_probability <- expected
  }
  n <- length(observed)
  defined <- c(
    if (!is.null(expected_probability)) "expected_probability",
    if (!is.null(expected_count)) "expected_count"
  )
  out <- list(
    table = tibble::tibble(
      actor = names(observed) %||% as.character(seq_len(n)),
      role = rep(side$role, n),
      observed = unname(as.numeric(observed)),
      expected_probability = margin_column(expected_probability, n),
      expected_count = margin_column(expected_count, n)
    ),
    defined_scales = defined
  )
  if (!is.null(spread)) {
    values <- spread[[side$role]] %||% spread[["single"]]
    out$table$dispersion <- margin_column(values, n)
  }
  out
}

# A scale the family does not define is an all-NA column of the right length,
# never an absent one: the five-column schema does not vary with the family.
margin_column <- function(x, n) {
  if (is.null(x)) rep(NA_real_, n) else unname(as.numeric(x))
}

margin_context <- function(fit, rows) {
  spec <- fit$model_spec
  list(
    model = fit$model,
    sub_model = fit$sub_model,
    flavor = rows$flavor,
    fid = NA_integer_,
    backend = fit$backend %||% NA_character_,
    n_events = fit$n_events,
    role_totals = role_totals(rows$table),
    defined_scales = rows$defined_scales,
    nodes = spec$nodes,
    nodes2 = spec$nodes2,
    two_mode = isTRUE(spec$is_two_mode)
  )
}

role_totals <- function(table) {
  vapply(
    unique(table$role),
    function(role) sum(table$observed[table$role == role]),
    numeric(1)
  )
}

#' @export
#' @method print margin_table
#' @noRd
print.margin_table <- function(x, ...) {
  context <- attr(x, "context")
  cli::cli_rule(left = "{.cls margin_table}")
  cli::cli_text(
    "Model {.val {context$model}} ·
     sub-model {.val {context$sub_model}} ·
     backend {.val {context$backend}}"
  )
  # A multi-process table row-binds processes whose event counts are not
  # commensurable (a rate process counts intervals, its choice counterpart
  # counts events), so the flavors and the process count are reported in place
  # of a total that would sum unlike things.
  if (all(is.na(context$flavor))) {
    cli::cli_text("{context$n_events} event{?s}")
  } else {
    cli::cli_text(
      "{length(context$flavor)} flavor{?s} {.val {context$flavor}} over
       {length(context$fid)} process{?es}"
    )
  }
  totals <- paste(
    names(context$role_totals),
    format(unname(context$role_totals))
  )
  cli::cli_text("Observed by role: {totals}")
  if (!"expected_count" %in% context$defined_scales) {
    cli::cli_text(
      "{.field expected_count} is not defined for this model class."
    )
  }
  body <- x
  class(body) <- setdiff(class(body), "margin_table")
  print(body, ...)
  invisible(x)
}

# Each actor's dispersion: the variance of its own stratified compensators.
#
# The margins are a first-moment view -- an actor gets a count and an expected
# count -- so an actor whose events are correctly counted but clustered in time
# is calibrated on every column the table otherwise carries. Under a correct
# model each completed span is unit exponential, whose variance is one, so this
# column reads as a ratio against one in the way `observed` / `expected_count`
# reads as a ratio against one.
#
# `NULL` for a family defining no waiting time, which the caller turns into the
# all-NA column the schema requires. That NA means the same thing
# `expected_count`'s does: not defined for the model class.
actor_dispersion <- function(x, preprocessed, call = rlang::caller_env()) {
  if (!is_exact_time_fit(x)) {
    return(list(single = NULL))
  }
  spans <- actor_cox_snell_residuals(x, preprocessed, call = call)
  if (all(c("sender", "receiver") %in% names(spans))) {
    return(list(
      sender = span_variance(spans$sender),
      receiver = span_variance(spans$receiver)
    ))
  }
  list(single = span_variance(spans))
}

# The censored final span is excluded rather than counted: it closes no event,
# so it is a partial waiting time and including it would drag every actor's
# variance toward zero in proportion to how early it stopped acting. An actor
# with fewer than two completed spans has no variance to report and gets NA --
# which is a great many actors on a real event stream, and the reason the
# column is read beside `observed` rather than on its own.
span_variance <- function(per_actor) {
  vapply(
    per_actor,
    function(spans) {
      closed <- spans[!attr(spans, "right_censored")]
      if (length(closed) < 2L) NA_real_ else stats::var(closed)
    },
    numeric(1)
  )
}
