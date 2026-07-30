##################### ###
#
# Goldfish package
# The cumulative-score goodness-of-fit test
#
##################### ###

#' Goodness of fit from the cumulative score processes
#'
#' @description
#' Tests whether each effect's contribution is spread over the event sequence
#' the way the model assumes. At the maximum the per-event scores sum to zero,
#' so each effect's cumulative score process starts and ends at zero: under a
#' correctly specified model it is a Brownian bridge, and a process that
#' wanders far from zero in between is an effect whose contribution is
#' concentrated somewhere in the sequence — a time-varying coefficient, a
#' misspecified functional form, an unmodeled regime.
#'
#' It reads the stored per-event scores, so it costs no evaluation pass and
#' needs no preprocessed statistics.
#'
#' @details
#' For effect \eqn{d} with per-event score contributions \eqn{s_{kd}}, the
#' standardized cumulative process is
#' \deqn{W_d(u) = \hat J_d^{-1/2} n^{-1/2} \sum_{k \le \lfloor nu \rfloor}
#'   s_{kd},}
#' with \eqn{\hat J_d} the empirical per-event variance of the centered
#' contributions — the per-effect outer-product (OPG) scale. The observed
#' information diagonal \eqn{I_{dd}/n} estimates the same per-event quantity
#' and is asymptotically equivalent; it is not what is used, because the two
#' differ materially at realistic event counts and the reference
#' implementations of this test standardize by the outer product.
#'
#' The statistic is \eqn{T_d = \sup_u |W_d(u)|}, and the omnibus over effects
#' is the Cauchy combination
#' \eqn{T_o = \frac{1}{L}\sum_l \tan(\pi(0.5 - P_l))} with
#' \eqn{p = \frac{1}{2} - \arctan(T_o)/\pi}, which is valid under arbitrary
#' dependence between the effect-level p-values — and they are dependent, the
#' processes being cumulative sums of the same score rows.
#'
#' Every interval contributes, including the right-censored ones: their score
#' rows are genuine contributions to the gradient, and it is over *all*
#' intervals that the gradient is zero. A process restricted to the dependent
#' events would have no bridge property to be read against.
#'
#' @section What the clock changes, and what it does not:
#' A supremum reads the values a path takes, never the positions they are
#' plotted at, so `clock` **does not change the statistic**: `T_d` is
#' identical under both settings, by construction. What it selects is the
#' `u`-axis stored with the process, and — the substantive part — the
#' reference distribution the p-value comes from.
#'
#' \describe{
#'   \item{`"event"` (default)}{places increment \eqn{k} at \eqn{u_k = k/n}
#'     and reads the p-value from the analytic Kolmogorov distribution,
#'     \eqn{p(t) = 2\sum_{j\ge1}(-1)^{j-1} e^{-2j^2t^2}}. This is the
#'     normalization of the test as published. Its accuracy needs the
#'     per-event information to accrue roughly proportionally over the
#'     sequence, so that the discrete process is observed on a near-uniform
#'     grid.}
#'   \item{`"information"`}{places increment \eqn{k} at
#'     \eqn{u_k = \mathrm{OPG}_d(k)/\mathrm{OPG}_d(n)}, the cumulative share
#'     of that effect's outer-product information, and computes the p-value
#'     from a reference simulated **on that observed grid**: Gaussian
#'     increments with variances proportional to \eqn{s_{kd}^2}, centered to
#'     end at zero, the supremum recorded per replication. This is
#'     Lin-Wei-Ying (1993) multiplier resampling specialized to the observed
#'     path, and the analytic Kolmogorov formula is its proportional-accrual
#'     special case. It runs from the stored scores alone — no evaluation
#'     pass — and draws through the session RNG, so [base::set.seed()]
#'     reproduces it.}
#' }
#'
#' Cold-start endogenous statistics are the typical violation of proportional
#' accrual: while the history is empty every alternative looks alike and the
#' score contributions are exactly zero, so the information arrives in the
#' later part of the sequence. The true null law of the discrete supremum is
#' then that of a bridge observed on the concentrated grid, which is
#' stochastically smaller than the continuous supremum the analytic formula
#' assumes — event-clock p-values deviate toward 1, and the test is
#' conservative rather than anti-conservative.
#'
#' [diagnose_onset()]'s information-accrual curve is the diagnostic for
#' choosing between them: it is the clock map itself, so its shape says in
#' advance whether the two references will separate. With near-uniform accrual
#' they coincide and the choice does not matter.
#'
#' @section Which effects are tested:
#' The free coefficients. A coefficient held fixed through `offset()` has no
#' bridge to test — its score component is not zero at the optimum, precisely
#' because it was not optimized — so offsets are excluded from the default set
#' and naming one in `effects` is an error pointing at [test_parameter()],
#' which is the test of an imposed value.
#'
#' @param object a fitted model of class `"result.goldfish"`, estimated with
#'   `"scores"` among the [set_algorithm_newton()] `diagnostics` primitives;
#'   for the print method, the `test_gof` object it renders.
#' @param effects an optional selection of the terms to test, given by any
#'   name a term answers to (the compact string the summary prints, the export
#'   form, the `coef()` label) or by position; [model_terms()] lists them.
#'   Defaults to every free coefficient.
#' @param clock which reference the p-values come from, `"event"` (default) or
#'   `"information"` — see the section above. The statistic is the same under
#'   both.
#' @param n_sim the number of replications of the simulated reference on the
#'   information clock. Ignored on the event clock, whose p-value is analytic.
#' @param ... additional arguments passed to or from other methods (currently
#'   unused).
#'
#' @return An object of class `test_gof`: a list of three
#'   [tibble::tibble()]s, carrying the metadata described in
#'   [diagnostic-tables].
#'   \describe{
#'     \item{`effects`}{one row per tested effect, with `statistic` the
#'       supremum, `p_value` its p-value under the selected reference, and
#'       `scale` the standardizing constant \eqn{\sqrt{n \hat J_d}}.}
#'     \item{`process`}{the standardized process paths in long form: one row
#'       per effect and step, with `u` the process-time axis, `clock` naming
#'       which clock produced it, and `process` the value of \eqn{W_d(u)}.
#'       Step `0` is the origin, so every path starts at `u = 0`, `W = 0`.}
#'     \item{`omnibus`}{one row: the Cauchy combination of the effect-level
#'       p-values, with the number of effects combined.}
#'   }
#'
#' @references
#' Boschi, M. and Wit, E. C. (2024). Goodness of fit of relational event
#' models. \doi{10.48550/arXiv.2407.08599}.
#'
#' Lin, D. Y., Wei, L. J. and Ying, Z. (1993). Checking the Cox model with
#' cumulative sums of martingale-based residuals. \emph{Biometrika}, 80(3),
#' 557-572. \doi{10.1093/biomet/80.3.557}.
#'
#' @examples
#' data("social_evolution")
#' fit <- estimate_dynam(
#'   calls ~ inertia + recip + trans,
#'   sub_model = "choice",
#'   data = social_evolution,
#'   control_algo = set_algorithm_newton(diagnostics = "scores")
#' )
#' test_gof(fit)
#'
#' # The statistic does not move with the clock; the reference does.
#' set.seed(1)
#' test_gof(fit, clock = "information")$effects
#'
#' @seealso [test_time()] for the directed alternative of a coefficient
#'   changing over the sequence, [test_parameter()] for an imposed value,
#'   [diagnose_onset()] for the accrual curve that chooses the clock, and
#'   [diagnostic-tables] for the metadata a diagnostic object carries.
#' @method test_gof result.goldfish
#' @export
test_gof.result.goldfish <- function(
  object,
  effects = NULL,
  clock = c("event", "information"),
  n_sim = 1000,
  ...
) {
  abort_if_stale_result(object, "goodness-of-fit tests")
  abort_if_not_diagnosable(
    object,
    "The goodness-of-fit test",
    component = "event_scores",
    primitive = "scores"
  )
  clock <- match.arg(clock)
  n_sim <- check_replication_count(n_sim)

  tested <- gof_tested_effects(object, effects)
  columns <- object$event_scores[, tested, drop = FALSE]
  paths <- gof_processes(columns, clock, tested = tested)
  statistic <- apply(abs(paths$standardized), 2, max)
  p_value <- if (identical(clock, "event")) {
    kolmogorov_p(statistic)
  } else {
    gof_simulated_p(columns, paths, statistic, n_sim)
  }

  labels <- gof_term_labels(object, tested)
  new_diagnostic_list(
    list(
      effects = tibble::tibble(
        index = tested,
        term = labels$term,
        coefficient = labels$coefficient,
        statistic = unname(statistic),
        p_value = unname(p_value),
        scale = unname(paths$scale)
      ),
      process = gof_process_table(paths, labels, tested, clock),
      omnibus = cauchy_omnibus(p_value)
    ),
    "test_gof",
    context = list(
      model = object$model,
      sub_model = object$sub_model,
      backend = object$backend,
      n_intervals = nrow(object$event_scores),
      n_events = sum(!object$right_censored_events)
    ),
    params = list(clock = clock, n_sim = n_sim)
  )
}

# Which coefficients the test ranges over. A fixed one is excluded rather than
# reported as insignificant: its score process is not a bridge, because the
# coefficient was never moved to make the total score zero, so the reference
# distribution does not apply to it at all. Naming one is therefore an error
# with a destination -- the test of an imposed value is a different test.
gof_tested_effects <- function(object, effects, call = rlang::caller_env()) {
  is_fixed <- GetFixed(object)
  if (is.null(effects)) {
    tested <- which(!is_fixed)
    if (length(tested) == 0L) {
      cli::cli_abort(
        c(
          "This fit has no free coefficient to test.",
          "x" = "Every term is held fixed through {.fn offset}.",
          "i" = "{.fn test_parameter} tests coefficients at their imposed
                 values."
        ),
        call = call
      )
    }
    return(unname(tested))
  }
  tested <- resolve_term_index(effects, object$names, "effects", call = call)
  fixed <- tested[is_fixed[tested]]
  if (length(fixed) > 0) {
    labels <- gof_term_labels(object, fixed)$term
    cli::cli_abort(
      c(
        "{.arg effects} names {length(fixed)} term{?s} held fixed through
         {.fn offset}: {.val {labels}}.",
        "x" = "A fixed coefficient's cumulative score process is not a
               bridge — it is not zero at the optimum, the coefficient never
               having been moved there.",
        "i" = "{.fn test_parameter} tests a coefficient at the value
               {.fn offset} imposed."
      ),
      call = call
    )
  }
  tested
}

# The standardized processes, their axis, and the constant that standardized
# them. `scale` is `sqrt(n * J_d)` with `J_d` the empirical per-event variance
# of the centered contributions, which is the same as the root of their
# summed squares -- so the `n` of the normalization cancels and the statistic
# does not depend on whether intervals or events are counted.
#
# The origin is carried as step 0 rather than left implicit: a path that starts
# at zero and ends at zero is the whole reading, and a plot method should not
# have to prepend it.
gof_processes <- function(columns, clock, tested, call = rlang::caller_env()) {
  n <- nrow(columns)
  centered <- sweep(columns, 2, colMeans(columns))
  scale <- sqrt(colSums(centered^2))
  opg <- colSums(columns^2)
  degenerate <- which(scale == 0 | opg == 0)
  if (length(degenerate) > 0) {
    cli::cli_abort(
      c(
        "{length(degenerate)} tested effect{?s} contribute{?s/} no score at
         all.",
        "x" = "{cli::qty(length(degenerate))}{?Its/Their} cumulative
               process{?es} {?is/are} identically zero, so there is nothing to
               standardize and no statistic to read.",
        "i" = "Check the model for an effect that is constant across every
               risk set."
      ),
      call = call
    )
  }
  cumulative <- rbind(
    0,
    matrix(
      apply(columns, 2, cumsum),
      nrow = n,
      dimnames = NULL
    )
  )
  standardized <- sweep(cumulative, 2, scale, "/")
  axis <- if (identical(clock, "event")) {
    matrix(seq.int(0, n) / n, nrow = n + 1L, ncol = ncol(columns))
  } else {
    rbind(
      0,
      sweep(
        matrix(apply(columns^2, 2, cumsum), nrow = n),
        2,
        opg,
        "/"
      )
    )
  }
  list(standardized = standardized, axis = axis, scale = scale, opg = opg)
}

# The Lin-Wei-Ying reference on the observed grid: Gaussian increments whose
# standard deviations are the observed |s_k| on the standardizing scale, so a
# replication has the null variance profile of the observed process, then
# centered into a bridge. The bridge correction subtracts `u * walk(1)`, which
# is the right centering only when `u` is that same variance profile
# normalized -- the information axis, which is why this is the information
# clock's reference and not a general one. Chunked over replications because
# the raw draw is an n x n_sim matrix and only the per-replication supremum
# survives it.
gof_simulated_p <- function(columns, paths, statistic, n_sim, chunk = 200L) {
  n <- nrow(columns)
  vapply(
    seq_along(statistic),
    function(d) {
      increment_sd <- abs(columns[, d]) / paths$scale[d]
      u <- paths$axis[-1L, d]
      exceed <- 0L
      remaining <- n_sim
      while (remaining > 0) {
        size <- min(chunk, remaining)
        draws <- matrix(stats::rnorm(n * size), nrow = n, ncol = size)
        walk <- apply(draws * increment_sd, 2, cumsum)
        bridge <- matrix(walk, nrow = n) - outer(u, walk[n, ])
        exceed <- exceed + sum(apply(abs(bridge), 2, max) >= statistic[d])
        remaining <- remaining - size
      }
      # The plus-one form: a p-value of exactly zero would claim more than
      # `n_sim` replications can support.
      (1 + exceed) / (1 + n_sim)
    },
    numeric(1)
  )
}

# The Kolmogorov distribution of the supremum of a standard Brownian bridge,
# `p(t) = 2 sum_j (-1)^(j-1) exp(-2 j^2 t^2)`. The series alternates and its
# terms fall off as `exp(-2 j^2 t^2)`, so a hundred of them are far beyond
# double precision for any `t` worth a p-value; the clamp covers the small-`t`
# tail, where the truncated series can leave the unit interval.
kolmogorov_p <- function(t, terms = 100L) {
  j <- seq_len(terms)
  vapply(
    t,
    function(one) {
      if (!is.finite(one) || one <= 0) {
        return(1)
      }
      min(max(2 * sum((-1)^(j - 1) * exp(-2 * j^2 * one^2)), 0), 1)
    },
    numeric(1)
  )
}

# The Cauchy combination test: the p-values enter as Cauchy quantiles, whose
# mean is Cauchy again whatever their dependence -- which is what makes it the
# right omnibus here, the processes being cumulative sums of the same rows.
cauchy_omnibus <- function(p_value) {
  statistic <- mean(tan(pi * (0.5 - p_value)))
  # Counted before the call: tibble() evaluates its columns in order and lets a
  # later one see an earlier one, so `length(p_value)` inside it would count
  # the omnibus p-value column rather than the effects that went into it.
  n_effects <- length(p_value)
  tibble::tibble(
    statistic = statistic,
    p_value = 0.5 - atan(statistic) / pi,
    n_effects = n_effects
  )
}

# The paths in long form: one row per effect and step, which is what a faceted
# panel maps over. The clock rides along as a column so the axis cannot be
# separated from the label saying what it is -- the pointwise band a plot draws
# around a bridge is honest on one of them and not on the other.
gof_process_table <- function(paths, labels, tested, clock) {
  n_steps <- nrow(paths$standardized)
  tibble::tibble(
    index = rep(tested, each = n_steps),
    term = rep(labels$term, each = n_steps),
    coefficient = rep(labels$coefficient, each = n_steps),
    step = rep(seq_len(n_steps) - 1L, times = length(tested)),
    u = as.vector(paths$axis),
    clock = clock,
    process = as.vector(paths$standardized)
  )
}

# The full compact strings, never the console-abbreviated ones: these label
# plot panels, where the console width is not what the labels have to fit.
gof_term_labels <- function(object, tested) {
  list(
    term = unname(
      compact_term_strings(object$names, "console", width = Inf)
    )[tested],
    coefficient = term_label(object$names, ".coef_name", "coef")[tested]
  )
}

check_replication_count <- function(n_sim, call = rlang::caller_env()) {
  if (!is.numeric(n_sim) || length(n_sim) != 1L || is.na(n_sim) || n_sim < 1) {
    cli::cli_abort(
      "{.arg n_sim} must be a single positive number.",
      call = call
    )
  }
  as.integer(n_sim)
}

#' @return The object, invisibly.
#' @rdname test_gof.result.goldfish
#' @method print test_gof
#' @export
print.test_gof <- function(x, ...) {
  context <- attr(x, "context")
  params <- attr(x, "params")
  cli::cli_rule(left = "{.cls test_gof}")
  cli::cli_text(
    "Model {.val {context$model}} ·
     sub-model {.val {context$sub_model}} ·
     backend {.val {context$backend}}"
  )
  cli::cli_text(
    "{context$n_intervals} interval{?s}, {context$n_events} dependent
     event{?s}; {nrow(x$effects)} effect{?s} tested."
  )
  cli::cli_text(
    "Supremum of the standardized cumulative score process, against
     {gof_reference_label(params)}."
  )
  omnibus <- x$omnibus
  cli::cli_text(
    "Cauchy omnibus over {omnibus$n_effects} effect{?s}:
     {.field p} = {format.pval(omnibus$p_value, digits = 3)}"
  )
  print(x$effects)
  invisible(x)
}

# Which reference produced the p-values, in words: the two are not variants of
# one calculation, so a printed result says which of them it is.
gof_reference_label <- function(params) {
  if (identical(params$clock, "event")) {
    "the Kolmogorov distribution on the event clock"
  } else {
    cli::format_inline(
      "{params$n_sim} simulated bridges on the information clock"
    )
  }
}
