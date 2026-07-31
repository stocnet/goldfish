##################### ###
#
# Goldfish package
# The cumulative-score goodness-of-fit test
#
##################### ###

#' Goodness of fit from the cumulative score processes
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
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
#' @section What is experimental here:
#' The per-effect test is validated: its null coverage is measured by
#' simulation, under both clocks, on a fixture whose information accrual is
#' verified concentrated first. Two things beside it are not, and the badge is
#' for them.
#'
#' **The omnibus combination is computed but not reported.** The object carries
#' the per-block Cauchy combination in `omnibus` and the joint one in
#' `attr(x, "context")$joint`, and the print methods show neither. It has had
#' no null-calibration or power study, and it is dominated by its most extreme
#' input in *both* directions — one effect with a very small p-value drags it
#' toward 0, and one whose process is unusually flat drags it toward 1, far
#' enough to mask a significant effect elsewhere. Read the per-effect rows.
#' The quantity is kept on the object rather than removed so that the study,
#' when it runs, can use saved fits.
#'
#' **The intercept row extends the cited test rather than reproducing it.** The
#' reference implementations fit a case-control differenced (conditional
#' logistic) design, in which a constant differences to zero, so they have no
#' intercept to test — the same reason a Cox partial likelihood has no
#' baseline. goldfish's parametric baseline makes the row available, and its
#' reading is given below.
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
#' @section The intercept row is a different question:
#' On the exact-time sub-models the time intercept is tested like any other
#' free coefficient, but what its row means is not the same thing.
#'
#' The intercept reaches the engine as a column of ones, so its per-interval
#' score is `dN_k - Dt_k * total_rate_k` — the interval's event indicator
#' minus its compensator, which is exactly the `cox_snell` residual of the
#' same interval. Its cumulative process is therefore
#' \eqn{N(t) - \Lambda(t)}, the counting-process martingale, and the score
#' equation that pins it to zero at the end is \eqn{N(T) = \Lambda(T)}: the
#' fitted model reproduces the observed number of events.
#' Every other effect's cumulative score is that same object
#' weighted by the effect's own statistic, so the intercept is the
#' **unweighted** member of the family.
#'
#' The practical consequence is that it asks about a different part of the
#' model:
#' \describe{
#'   \item{a covariate row}{asks whether the model is right about *who* acts —
#'     whether that statistic's contribution is spread over the sequence the
#'     way a fixed coefficient implies.}
#'   \item{the intercept row}{asks whether it is right about *how many* events
#'     occur and *when* — whether the fitted intensity reproduces the observed
#'     event flow. It is the classical counting-process goodness-of-fit check.}
#' }
#'
#' A wandering intercept process points at a non-constant baseline (a trend,
#' burn-in or saturation), a periodic rhythm the model does not carry, a
#' missing global time-varying covariate, an incorrect presence or exposure
#' schedule — the normalizer sums over the *active* actors, so a wrong
#' composition schedule lands here — or temporal clustering beyond the fitted
#' intensity. A conspicuously *flat* one points at degenerate timing: ties,
#' rounded timestamps, or a deterministic grid. The remedies differ from a
#' covariate row's accordingly: a time-varying baseline, a period comparison
#' through [test_time()], or a corrected presence schedule, rather than another
#' effect in the choice model.
#'
#' It is also the row most likely to want `clock = "information"`. Its
#' increment variance is approximately the interval's compensator, which swings
#' with interval length, so on irregular event times it is observed on the
#' coarsest grid of any effect.
#'
#' The ordinal sub-models (`rate_ordered`, and the choice families) carry no
#' intercept at all: they condition on the event times, an intercept cancels in
#' the softmax, and a constant column there would have identically zero score.
#' No such row exists on those fits, and none is missing.
#'
#' @param object a fitted model of class `"result.goldfish"`, estimated with
#'   `"scores"` among the [set_algorithm_newton()] `diagnostics` primitives;
#'   for the print method, the `test_gof` object it renders.
#' @param effects an optional selection of the terms to test, given by any
#'   name a term answers to (the compact string the summary prints, the export
#'   form, the `coef()` label) or by position; [model_terms()] lists them.
#'   A bare **effect** name selects every term of that effect, so
#'   `effects = "inertia"` tests all of `inertia`, `inertia(friendship)` and
#'   `inertia(calls, weighted = TRUE)` on a model carrying the three.
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
#'       p-values, with the number of effects combined. Present on the object
#'       and **deliberately not printed** — see the experimental section.}
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
#' @inheritSection diagnostic-requirements What a diagnostic needs
#'
#' @section What this test needs:
#'
#' The `"scores"` primitive, and **no evaluation pass**: both the statistic and
#' the simulated reference are arithmetic on the stored score rows. It is the
#' cheapest member of the `test_*` family to have available, and the only one
#' that runs on a fit carrying no statistics.
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
  tested <- resolve_term_index(
    effects,
    object$names,
    "effects",
    expand_family = TRUE,
    call = call
  )
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
cauchy_omnibus <- function(p_value, n_blocks = NULL) {
  # The transform has poles at 0 and 1, and both are reachable: the Kolmogorov
  # series is clamped into the unit interval, and the simulated p-value is
  # `(1 + exceed) / (1 + n_sim)`, which is exactly 1 when every replication
  # exceeds. Nudging off the pole keeps the value finite and deterministic
  # rather than whatever `tan()` returns a machine epsilon from pi/2.
  #
  # It does not stop a single near-degenerate p-value from dominating -- that
  # is the combination working as defined, in both directions: one effect
  # whose process is unusually flat drags the omnibus toward 1 exactly as one
  # significant effect drags it toward 0. Read the per-effect rows, not only
  # the combination.
  eps <- .Machine$double.eps
  statistic <- mean(tan(pi * (0.5 - pmin(pmax(p_value, eps), 1 - eps))))
  # Counted before the call: tibble() evaluates its columns in order and lets a
  # later one see an earlier one, so `length(p_value)` inside it would count
  # the omnibus p-value column rather than the effects that went into it.
  n_effects <- length(p_value)
  tibble::tibble(
    statistic = statistic,
    p_value = 0.5 - atan(statistic) / pi,
    n_effects = n_effects,
    # Present only where the combination spans blocks, so the single-fit
    # omnibus keeps the three columns it has always had.
    n_blocks = n_blocks
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

# The specification (multi-process) fit ---------------------------------------

#' Goodness of fit of a multi-process specification
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Tests each process of a specification fit exactly as [test_gof()] tests a
#' single fit, and reports the individual per-block results.
#'
#' A flavored specification is K *independent* fits — the competing-flavor
#' likelihood factorizes, which is why nothing here is pooled. Each block is a
#' process: one flavor's one sub-model, with its own effect set, its own event
#' count and its own standardizing constants. A shared constant across blocks
#' would assert a joint model that was never estimated.
#'
#' @details
#' The components are the same three tibbles a single-fit result carries, each
#' row-bound over processes with `flavor` and `family` **appended**, so a plot
#' method facets on those columns instead of needing a separate flavored
#' method, and so a consumer never has to ask whether a flavored result nested
#' one level deeper.
#'
#' `omnibus` gains one row per block — that block's Cauchy combination over
#' its own effects. The **joint** omnibus is a property of the whole object
#' rather than of any row, so it lives in the metadata, reachable as
#' `attr(x, "context")$joint`. Neither is printed: the printed report is the
#' per-block individual tests, for the reason given under
#' [test_gof.result.goldfish()]'s experimental section.
#'
#' The joint combination is taken over the **effect-level** p-values of every
#' block, not over the per-block omnibus values. A block carrying more effects
#' therefore contributes more of the combination, which is the reading that
#' treats an effect rather than a process as the unit being combined. The
#' Cauchy combination is valid under arbitrary dependence either way.
#'
#' It is also dominated by its most extreme input, in **both** directions: one
#' effect with a very small p-value drags the combination toward 0, and one
#' whose process is unusually flat — a p-value near 1 — drags it toward 1,
#' far enough to mask a significant effect elsewhere. That is the combination
#' behaving as defined rather than a defect, but it means the per-block and
#' per-effect rows are what to read when the joint value is uninformative.
#'
#' `effects` is matched against each process separately, since the processes
#' have different formulas. Naming a term some process does not carry is an
#' error that says which process, rather than a silent omission.
#'
#' @inheritParams test_gof.result.goldfish
#' @param object a multi-process fit of class `"flavored_result.goldfish"`, as
#'   returned by estimating a [make_specification()] with more than one process.
#'
#' @return An object of class `test_gof`, shaped exactly as the single-fit
#'   result and documented at [test_gof.result.goldfish()], with `flavor` and
#'   `family` columns appended to each component and the joint omnibus in the
#'   metadata. The printed report shows the per-block individual tests; no
#'   combination is rendered at either level.
#'
#' @seealso [test_gof.result.goldfish()] for what each block's test is.
#' @method test_gof flavored_result.goldfish
#' @export
test_gof.flavored_result.goldfish <- function(
  object,
  effects = NULL,
  clock = c("event", "information"),
  n_sim = 1000,
  ...
) {
  clock <- match.arg(clock)
  n_sim <- check_replication_count(n_sim)
  map <- object$process_map
  # Flavor-major, the order the container itself prints in, so a reader
  # comparing the two tables never has to reorder one of them.
  rows <- flavored_row_order(object)

  per_block <- lapply(rows, function(i) {
    fit <- object$results[[as.character(map$fid[i])]]
    label <- render_process_label(map, map$fid[i])
    block <- gof_block(fit, label, effects, clock, n_sim)
    lapply(block, function(component) {
      component$flavor <- map$flavor[i]
      component$family <- map$family[i]
      component
    })
  })

  components <- stats::setNames(
    lapply(
      c("effects", "process", "omnibus"),
      function(name) do.call(rbind, lapply(per_block, `[[`, name))
    ),
    c("effects", "process", "omnibus")
  )
  new_diagnostic_list(
    components,
    "test_gof",
    context = gof_flavored_context(object, map, rows, components$effects),
    params = list(clock = clock, n_sim = n_sim)
  )
}

# One block's test, with the process named if it fails. Each process has its
# own formula, so an `effects` selection valid for one can be absent from
# another, and "unknown term" without saying where is not actionable on a fit
# with four processes.
gof_block <- function(
  fit,
  label,
  effects,
  clock,
  n_sim,
  call = rlang::caller_env()
) {
  tryCatch(
    test_gof(fit, effects = effects, clock = clock, n_sim = n_sim),
    error = function(e) {
      cli::cli_abort(
        "{.fn test_gof} could not test process {.val {label}}.",
        parent = e,
        call = call
      )
    }
  )
}

# What the multi-process table describes. The per-process identity is what
# varies, so the context keeps the map columns rather than one process's own
# flavor, and the per-block counts are vectors in the table's row order --
# a rate process counts intervals where its choice counterpart counts events,
# and a total would sum unlike things.
gof_flavored_context <- function(object, map, rows, effects) {
  fits <- lapply(rows, function(i) object$results[[as.character(map$fid[i])]])
  list(
    model = object$model,
    layer = object$layer,
    sub_model = unique(map$family[rows]),
    flavor = unique(map$flavor[rows]),
    fid = map$fid[rows],
    backend = fits[[1]]$backend,
    n_intervals = vapply(fits, function(f) nrow(f$event_scores), integer(1)),
    n_events = vapply(
      fits,
      function(f) sum(!f$right_censored_events),
      integer(1)
    ),
    joint = cauchy_omnibus(effects$p_value, n_blocks = length(rows))
  )
}

#' @return The object, invisibly.
#' @rdname test_gof.result.goldfish
#' @method print test_gof
#' @export
print.test_gof <- function(x, ...) {
  context <- attr(x, "context")
  params <- attr(x, "params")
  # One print for both shapes, told apart by the column the flavored form
  # appends -- the same branch `print.margin_table` makes.
  blocked <- "flavor" %in% names(x$effects)
  cli::cli_rule(left = "{.cls test_gof}")
  if (blocked) {
    cli::cli_text(
      "Model {.val {context$model}} · layer {.val {context$layer}} ·
       {length(context$flavor)} flavor{?s} over {length(context$fid)}
       process{?es}"
    )
  } else {
    cli::cli_text(
      "Model {.val {context$model}} ·
       sub-model {.val {context$sub_model}} ·
       backend {.val {context$backend}}"
    )
    cli::cli_text(
      "{context$n_intervals} interval{?s}, {context$n_events} dependent
       event{?s}; {nrow(x$effects)} effect{?s} tested."
    )
  }
  cli::cli_text(
    "Supremum of the standardized cumulative score process, against
     {gof_reference_label(params)}."
  )
  # The Cauchy combination is computed and carried on the object, and
  # deliberately not shown: the per-effect test is validated by simulation and
  # the combination is not, and it is dominated by its most extreme input in
  # BOTH directions -- one effect whose process is unusually flat drags it
  # toward 1 far enough to mask a significant effect elsewhere. Suppressing at
  # the print rather than dropping the component keeps the plot-data contract
  # stable and lets the validation study run against saved objects.
  if (blocked) {
    gof_print_blocks(x)
  } else {
    print(x$effects)
  }
  invisible(x)
}

# The per-block listing: one section per process, in the container's own
# flavor-major order. The identity columns are dropped from each section's
# table because the header just said them, and repeating a constant down every
# row is what makes a blocked print unreadable. The block ordering comes from
# `omnibus`, which carries exactly one row per block in that order -- the
# combination it also carries is not shown, for the reason stated at the print.
gof_print_blocks <- function(x) {
  omnibus <- x$omnibus
  for (i in seq_len(nrow(omnibus))) {
    flavor <- omnibus$flavor[i]
    family <- omnibus$family[i]
    cli::cli_text("")
    cli::cli_text("{.strong {flavor}} · {.field {family}}")
    rows <- x$effects$flavor == flavor & x$effects$family == family
    block <- x$effects[rows, c("term", "statistic", "p_value")]
    print(block)
  }
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
