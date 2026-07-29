##################### ###
#
# Goldfish package
# The onset diagnostic: what the left-censored start of the sequence did
# to the estimate
#
##################### ###

#' Diagnose the onset of the observed sequence
#'
#' @description
#' At the start of an event sequence the history is left-censored: the
#' endogenous statistics are still at their initial values, so every
#' alternative looks alike, the model predicts at the per-event null
#' benchmark, and the endogenous score contributions are *exactly* zero.
#' `diagnose_onset()` measures how far that opening segment moved the
#' estimate, and when the data began to identify the endogenous
#' parameters.
#'
#' It reads the stored per-event scores and the fit's information matrix,
#' so it costs no evaluation pass and needs no preprocessed statistics.
#'
#' @details
#' Two curves, both indexed by how many **dependent events** the initial
#' segment holds, rather than by how many intervals it spans: on a rate or
#' REM fit most of the intervals are right-censored, and an axis counting
#' window closures says little about how much history has accumulated.
#'
#' \describe{
#'   \item{the parameter path}{\eqn{\hat\theta_{-[1:m]} \approx \hat\theta
#'     - I^{-1} \sum_{k \le m} s_k}, the one-step estimate obtained by
#'     leaving the first `m` intervals' likelihood terms out, for every `m`
#'     at once. Flat means the onset is harmless; drift that then settles
#'     means the opening segment carried the estimate up to the point it
#'     settles.}
#'   \item{the information-accrual curve}{the cumulative share of the
#'     per-event information the sequence has delivered by `m`. Its initial
#'     flat stretch is the segment over which the data says nothing about
#'     the endogenous parameters.}
#' }
#'
#' The path is a bridge in the same sense the cumulative score processes of
#' `test_gof()` are: at `m =` the whole sequence it returns to
#' `coef(object)`, because the total score is zero at the maximum. Only its
#' shape over the opening segment is read.
#'
#' The summary is **descriptive**. No p-value is computed and none is
#' implied: a path that drifts is a statement about which events carry the
#' estimate, not evidence against the model.
#'
#' @section Which intervals take part:
#' All of them. Unlike [diagnose_outliers()] and [diagnose_changepoints()],
#' this function has no `include_censored` argument, and the omission is
#' deliberate: the score of a right-censored interval is a genuine
#' contribution to the gradient, and the gradient sums to zero over *all*
#' intervals. Restricting the cumulative sums to the dependent intervals
#' would leave a series with no null to be read against — on a windowed
#' rate model of the `social_evolution` calls the cumulative score of the
#' in-degree effect ends at -0.0003 over all intervals and at 142.3 over the
#' dependent ones, because a window's lifetime *is* the right-censored
#' interval and about half the endogenous score mass lives there. The
#' dependent events enter as the axis the path is reported on instead.
#'
#' @section Remedies:
#' \describe{
#'   \item{warm-start the history}{link the pre-observation events to the
#'     networks and make only the later events dependent, so the statistics
#'     are already populated when the observation window opens. This is the
#'     goldfish idiom, and it keeps the early events as history rather than
#'     discarding them.}
#'   \item{exclude the opening segment}{drop the initial events from the
#'     dependent set and compare the estimate with and without them.}
#' }
#'
#' Both are conditioning on early history, which is standard practice for
#' relational event models; the influence measures here are
#' **likelihood deletion, not history deletion** — see the caveats in
#' [residuals.result.goldfish()].
#'
#' @section Why changepoint detection does not find this:
#' Running [diagnose_changepoints()] on the per-interval log-likelihood
#' does not reliably surface the onset phase, for four reasons: the segment
#' is usually short against the penalty and the `minseglen` default; the
#' opening is a plateau *at* the per-event null benchmark rather than away
#' from it, so the level shift the detector looks for is small; the
#' endogenous score contributions there are exactly zero, a series the
#' detector never sees; and on a rate or REM fit the pooled series is
#' dominated by the alternation of dependent and right-censored intervals.
#'
#' @param x a fitted model of class `"result.goldfish"`, estimated with
#'   `"scores"` among the [set_algorithm_newton()] `diagnostics`
#'   primitives.
#' @param information how the accrual curve measures per-event
#'   information. `"opg"` (default) is the outer-product form
#'   \eqn{\sum_k s_k' s_k}, available from the stored score rows alone.
#'   `"expected"` — per-event Fisher contributions — is not available yet;
#'   it needs an accumulation inside the estimation kernels that no stored
#'   primitive carries, and requesting it aborts rather than quietly
#'   returning the outer-product curve. The distinction matters little
#'   here: the calibration weakness of the outer-product form is a property
#'   of tests, and a cumulative share is not one.
#' @param tolerance the half-width, in standard errors, of the band a
#'   coefficient's path must stay inside for the remainder of the sequence
#'   to count as stabilized.
#'
#' @return An object of class `diagnose_onset`: a list of three
#'   [tibble::tibble()]s, carrying the metadata described in
#'   [diagnostic-tables].
#'   \describe{
#'     \item{`path`}{one row per coefficient and initial-segment length,
#'       with `dropped_intervals` and `dropped_events` naming the segment,
#'       `estimate` the one-step path value, and `reference` / `std_error`
#'       the fitted estimate and its standard error, so a panel is
#'       self-contained.}
#'     \item{`accrual`}{one row per initial-segment length, with `share`
#'       the cumulative share of the information delivered by it.}
#'     \item{`summary`}{one row per coefficient, describing its opening
#'       excursion: the largest drift from the estimate in standard
#'       errors and the segment length at which it occurred, the segment
#'       length at which the path came back inside the band
#'       (`stabilized_at`, `0` when it never left), and the share of the
#'       information accrued by then.}
#'   }
#'
#' @examples
#' data("social_evolution")
#' fit <- estimate_dynam(
#'   calls ~ inertia + recip,
#'   sub_model = "choice",
#'   data = social_evolution,
#'   control_algo = set_algorithm_newton(diagnostics = "scores")
#' )
#' onset <- diagnose_onset(fit)
#' onset
#' head(onset$accrual)
#'
#' @seealso [diagnose_outliers()] and [diagnose_changepoints()] for the
#'   per-interval log-likelihood diagnostics,
#'   [residuals.result.goldfish()] for the per-event influence measures the
#'   path is the cumulative form of, and [diagnostic-tables] for the
#'   metadata a diagnostic object carries.
#' @export
diagnose_onset <- function(
  x,
  information = c("opg", "expected"),
  tolerance = 0.1
) {
  abort_if_not_diagnosable(
    x,
    "The onset diagnostic",
    component = "event_scores",
    primitive = "scores"
  )
  abort_if_stale_result(x, "onset diagnostics")
  information <- match.arg(information)
  if (identical(information, "expected")) {
    cli::cli_abort(c(
      "{.code information = \"expected\"} has not shipped yet.",
      "x" = "It needs the per-interval Fisher contributions, which no
             stored primitive carries: the fit keeps the total information
             matrix only.",
      "i" = "Use {.code information = \"opg\"}, the outer-product form,
             which is what a cumulative share needs."
    ))
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1L || tolerance <= 0) {
    cli::cli_abort("{.arg tolerance} must be a single positive number.")
  }

  scores <- x$event_scores
  dropped_events <- c(0L, cumsum(!x$right_censored_events))
  path <- onset_path_matrix(x, scores)
  accrual <- onset_accrual(scores)

  new_diagnostic_list(
    list(
      path = onset_path_table(x, path, dropped_events),
      accrual = tibble::tibble(
        dropped_intervals = seq_along(dropped_events) - 1L,
        dropped_events = dropped_events,
        share = accrual
      ),
      summary = onset_summary_table(x, path, dropped_events, accrual, tolerance)
    ),
    "diagnose_onset",
    context = list(
      model = x$model,
      sub_model = x$sub_model,
      backend = x$backend,
      n_intervals = nrow(scores),
      n_events = sum(!x$right_censored_events)
    ),
    params = list(information = information, tolerance = tolerance)
  )
}

# The leave-initial-segment-out path, all segment lengths at once. Row `m + 1`
# is the estimate with the first `m` intervals' likelihood terms deleted, so
# row 1 is the estimate itself and the last row returns to it -- the total
# score being zero at the maximum, which is the same bridge structure the
# cumulative score processes have.
#
# A coefficient held fixed does not move, so it keeps its value and is
# excluded from the inverse, which is also what keeps the information
# invertible on a fit with offsets.
onset_path_matrix <- function(x, scores) {
  is_fixed <- GetFixed(x)
  inverse <- invert_free_information(x)
  estimate <- stats::coef(x, complete = TRUE)
  cumulative <- rbind(0, apply(scores, 2, cumsum))
  path <- matrix(
    estimate,
    nrow = nrow(cumulative),
    ncol = length(estimate),
    byrow = TRUE,
    dimnames = list(NULL, names(estimate))
  )
  path[, !is_fixed] <- path[, !is_fixed] -
    cumulative[, !is_fixed, drop = FALSE] %*% inverse
  path
}

# The outer-product information accrual: `tr(s_k' s_k)` is the squared norm of
# a score row, so the cumulative share needs no matrix product at all. Indexed
# like the path, starting at zero for the empty initial segment.
onset_accrual <- function(scores) {
  contribution <- rowSums(scores^2)
  c(0, cumsum(contribution) / sum(contribution))
}

# The path in long form: one row per coefficient and segment length, which is
# what a faceted panel maps over. `reference` and `std_error` repeat the
# coefficient's own constants so a panel draws its band without a join.
onset_path_table <- function(x, path, dropped_events) {
  n_steps <- nrow(path)
  # Not named `estimate`: tibble() evaluates its columns in order and lets a
  # later one see an earlier one, so a local of that name would be shadowed by
  # the `estimate` column below.
  fitted <- stats::coef(x, complete = TRUE)
  tibble::tibble(
    index = rep(seq_len(ncol(path)), each = n_steps),
    term = rep(onset_term_labels(x), each = n_steps),
    coefficient = rep(
      term_label(x$names, ".coef_name", "coef"),
      each = n_steps
    ),
    fixed = rep(unname(GetFixed(x)), each = n_steps),
    dropped_intervals = rep(seq_len(n_steps) - 1L, times = ncol(path)),
    dropped_events = rep(dropped_events, times = ncol(path)),
    estimate = as.vector(path),
    reference = rep(unname(fitted), each = n_steps),
    std_error = rep(unname(x$standard_errors), each = n_steps)
  )
}

# Where each coefficient's opening excursion ends, and how much of the
# information had accrued by then.
#
# Read from the start of the sequence forward, which is what makes this an
# onset summary: the path leaves the band as the events that carry the
# estimate are deleted, and `stabilized_at` is where it first comes back. The
# alternative reading -- the last segment length after which it never leaves
# again -- is not a statement about the onset at all: this path is a bridge
# returning to the estimate at the end, so deleting most of the sequence
# leaves few events and a wide late excursion, and that reading would report
# the end of the sequence on almost every fit.
onset_summary_table <- function(x, path, dropped_events, accrual, tolerance) {
  estimate <- stats::coef(x, complete = TRUE)
  is_fixed <- unname(GetFixed(x))
  standard_errors <- unname(x$standard_errors)
  drift <- abs(sweep(path, 2, estimate)) /
    rep(standard_errors, each = nrow(path))
  # A fixed coefficient does not move and has no standard error to divide by,
  # so its drift is zero by construction rather than the 0/0 the division
  # produces.
  drift[, is_fixed] <- 0
  excursion <- apply(drift, 2, onset_excursion_end, tolerance = tolerance)
  peak <- vapply(
    seq_len(ncol(drift)),
    function(j) which.max(drift[seq_len(excursion[j]), j]),
    integer(1)
  )
  tibble::tibble(
    index = seq_len(ncol(path)),
    term = onset_term_labels(x),
    coefficient = term_label(x$names, ".coef_name", "coef"),
    fixed = is_fixed,
    estimate = unname(estimate),
    std_error = standard_errors,
    max_drift = drift[cbind(peak, seq_len(ncol(path)))],
    max_drift_at = dropped_events[peak],
    stabilized_at = dropped_events[excursion],
    stabilized_share = accrual[excursion]
  )
}

# The row at which one coefficient's opening excursion ends: the first row
# back inside the band once the path has left it. Row 1 -- the whole sequence
# kept -- if it never leaves, which is the flat, harmless-onset case. A return
# always exists, the last row being the estimate itself.
onset_excursion_end <- function(drift, tolerance) {
  outside <- which(drift > tolerance)
  if (length(outside) == 0L) {
    return(1L)
  }
  inside <- which(drift[outside[1]:length(drift)] <= tolerance)
  outside[1] + inside[1] - 1L
}

# The full compact strings, never the console-abbreviated ones: these label
# plot panels, where the console width is not what the labels have to fit.
onset_term_labels <- function(x) {
  unname(compact_term_strings(x$names, "console", width = Inf))
}

#' @param x a `diagnose_onset` object.
#' @param ... Additional arguments passed to or from other methods
#'   (currently unused).
#' @return The object, invisibly.
#' @rdname diagnose_onset
#' @method print diagnose_onset
#' @export
print.diagnose_onset <- function(x, ...) {
  context <- attr(x, "context")
  params <- attr(x, "params")
  cli::cli_rule(left = "{.cls diagnose_onset}")
  cli::cli_text(
    "Model {.val {context$model}} ·
     sub-model {.val {context$sub_model}} ·
     backend {.val {context$backend}}"
  )
  cli::cli_text(
    "{context$n_intervals} interval{?s}, {context$n_events} dependent
     event{?s}; {.val {params$information}} information accrual."
  )
  settled <- max(x$summary$stabilized_at)
  if (settled == 0L) {
    cli::cli_text(
      "No path leaves the {params$tolerance} standard-error band around its
       estimate: the onset costs nothing here."
    )
  } else {
    cli::cli_text(
      "Every path is back within {params$tolerance} standard error{?s} of its
       estimate by the first {settled} event{?s}, by when
       {round(100 * max(x$summary$stabilized_share))}% of the information has
       accrued."
    )
  }
  cli::cli_text(
    "{.emph Descriptive}: this is a reading of which events carry the
     estimate, not a test, and no p-value is computed."
  )
  print(x$summary)
  invisible(x)
}
