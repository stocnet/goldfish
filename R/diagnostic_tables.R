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
  labels_sender <- node_labels(nodes)
  labels_receiver <- node_labels(nodes2)
  # Which node set a component indexes: a `_sender` accumulator is always side
  # 1 and a `_receiver` one always side 2, while a single-sided family follows
  # its axis -- choice accumulates over receivers, rate over senders, and
  # coordination over the one actor set into which it credits both endpoints.
  labels_single <- if (identical(axis, "receiver_given_sender")) {
    labels_receiver
  } else {
    labels_sender
  }
  for (component in names(margins)) {
    labels <- if (endsWith(component, "_sender")) {
      labels_sender
    } else if (endsWith(component, "_receiver")) {
      labels_receiver
    } else {
      labels_single
    }
    margins[[component]] <- label_margin_vector(
      margins[[component]],
      labels,
      margin_component_scale(component, is_exact_time)
    )
  }
  margins
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
#' Every diagnostic data object goldfish returns is a [tibble::tibble()] with
#' the producing function's name prepended to its classes, carrying enough
#' metadata for a print or plot method to describe the object without reaching
#' back into the fit it came from. `margin_table()` is the first such producer.
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
#' }
#'
#' The tibble base keeps a diagnostic table usable with the ordinary data
#' verbs: subsetting with `[`, and dplyr's `filter()`, `mutate()` and
#' `arrange()`, preserve the class and the metadata. A `group_by()` plus
#' `summarise()` drops them, which is correct -- the summary is no longer the
#' diagnostic object.
#'
#' @name diagnostic-tables
#' @seealso [margin_table()], the producer of the first diagnostic table.
NULL

new_diagnostic_table <- function(df, class, context = list(), params = list()) {
  out <- tibble::as_tibble(df)
  attr(out, "diagnostic") <- class
  attr(out, "context") <- context
  attr(out, "params") <- params
  attr(out, "version") <- as.character(utils::packageVersion("goldfish"))
  class(out) <- c(class, class(out))
  out
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
#' @param ... additional arguments passed to or from other methods (currently
#'   unused).
#'
#' @return A [tibble::tibble()] of class `margin_table` with columns
#'   \describe{
#'     \item{actor}{the actor label, from the node set the margin indexes.}
#'     \item{role}{`"sender"`, `"receiver"` or `"endpoint"`, as above.}
#'     \item{observed}{the actor's tabulated count in the dependent events.}
#'     \item{expected_probability}{the probability-scale expected count.}
#'     \item{expected_count}{the compensator-scale expected count, `NA` on the
#'       multinomial families.}
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
margin_table.result.goldfish <- function(x, ...) {
  rows <- margin_rows(x)
  new_diagnostic_table(
    rows$table,
    class = "margin_table",
    context = margin_context(x, rows),
    params = list(scales = rows$defined_scales)
  )
}

#' @export
margin_table.flavored_result.goldfish <- function(x, ...) {
  map <- x$process_map
  per_fid <- lapply(seq_len(nrow(map)), function(i) {
    fit <- x$results[[as.character(map$fid[i])]]
    rows <- margin_rows(fit, flavor = map$flavor[i], family = map$family[i])
    rows$table$flavor <- map$flavor[i]
    rows$table$family <- map$family[i]
    rows
  })
  tables <- lapply(per_fid, `[[`, "table")
  context <- margin_context(x$results[[as.character(map$fid[1])]], per_fid[[1]])
  # The combined table describes the whole multi-process fit, so the per-fid
  # identity is what varies: the context keeps the map columns rather than one
  # fid's own flavor, and the defined scales are the union over the processes.
  context$model <- x$model
  context$sub_model <- unique(map$family)
  context$flavor <- unique(map$flavor)
  context$fid <- map$fid
  context$n_events <- vapply(
    as.character(map$fid),
    function(key) as.integer(x$results[[key]]$n_events),
    integer(1),
    USE.NAMES = FALSE
  )
  defined <- unique(unlist(lapply(per_fid, `[[`, "defined_scales")))
  context$defined_scales <- defined
  context$role_totals <- role_totals(do.call(rbind, tables))
  new_diagnostic_table(
    do.call(rbind, tables),
    class = "margin_table",
    context = context,
    params = list(scales = defined)
  )
}

# One fit's margins as the five-column table, plus the facts the metadata needs.
# `flavor` / `family` name the process this fit is, on a multi-process fit.
margin_rows <- function(
  x,
  flavor = NA_character_,
  family = NA_character_,
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
  tables <- lapply(sides, function(side) margin_side_table(margins, side))
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

margin_side_table <- function(margins, side) {
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
  list(
    table = tibble::tibble(
      actor = names(observed) %||% as.character(seq_len(n)),
      role = rep(side$role, n),
      observed = unname(as.numeric(observed)),
      expected_probability = margin_column(expected_probability, n),
      expected_count = margin_column(expected_count, n)
    ),
    defined_scales = defined
  )
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
