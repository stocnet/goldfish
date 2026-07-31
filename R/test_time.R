##################### ###
#
# Goldfish package
# Time-heterogeneity tests: is a coefficient constant over the sequence?
#
##################### ###

#' Test whether an effect's coefficient is constant over the sequence
#'
#' @description
#' A fitted coefficient is one number standing for the whole observation
#' window. `test_time()` asks whether that is tenable: whether the effect the
#' model estimated once was in fact the same effect throughout.
#'
#' Both methods are **exact score tests of an augmented model** — the fitted
#' model plus an interaction between the effect and time — evaluated at the
#' fitted coefficients, where the augmentation's score is what the fitted model
#' left on the table. `"trend"` augments with a smooth function of time and
#' asks whether the coefficient drifts; `"periods"` augments with period
#' indicators and asks whether it differs between regimes.
#'
#' @details
#' # What the two methods are
#'
#' \describe{
#'   \item{`"trend"`}{augments the model with `x_d * g(t)` for a time
#'     transform `g`, and tests the interaction's coefficient against zero:
#'     one degree of freedom per effect, a directed alternative, most powerful
#'     against a coefficient that moves monotonically. This is the
#'     proportional-hazards test of survival analysis, and on a sub-model whose
#'     classical twin is a Cox partial likelihood it agrees with
#'     `survival::cox.zph()`.}
#'   \item{`"periods"`}{augments it with `x_d * 1{k in period j}` for
#'     `j = 2..J`, the first period being the reference: `J - 1` degrees of
#'     freedom per effect, an undirected alternative, and the method to reach
#'     for when a regime change is expected at a known time rather than a drift.
#'     Its per-period one-step deltas are the interpretable readout — what the
#'     coefficient would move to in each period.}
#' }
#'
#' # Which clock the transform is applied to
#'
#' The time axis is **the one the sub-model's own likelihood runs on**, not
#' wall-clock time in every case. An ordinal sub-model conditions on the order
#' of events and nothing else, so its clock is the event index; an exact-time
#' sub-model integrates a compensator over elapsed time, so its clock is the
#' event time. Applying a transform to an axis the likelihood never saw gives a
#' number that answers no question the model poses — and it is not a small
#' effect: on the package's own Cox-equivalent fixture, using wall-clock times
#' where the likelihood is ordinal moves the `"identity"` statistic by 24%.
#'
#' A consequence worth stating: on an ordinal sub-model `"identity"` and
#' `"rank"` are the same transform, because the event index already is the
#' rank.
#'
#' # What it requires
#'
#' One [evaluate_model()] pass, and therefore the model's statistics — attached
#' by `estimate_*(return_preprocessed = TRUE)` or supplied through
#' `preprocessed`. Without either it aborts naming both routes. The reason is
#' that the augmented model's information needs the **per-interval** expected
#' information, which no fitted object stores: a fit keeps the total.
#'
#' There is deliberately no outer-product option. An outer-product form would
#' need no pass, but it is an approximation of the quantity being tested, and a
#' p-value is a claim: the cheap variants available here are a Lagrange
#' multiplier test that over-rejects in finite samples, and the
#' scaled-Schoenfeld regression that `survival` itself retired when `cox.zph()`
#' was rewritten as an exact test. [diagnose_onset()] keeps its outer-product
#' default because a cumulative share is a description, not a test.
#'
#' # What this test is not
#'
#' It is not a test of a **windowed statistic**. `inertia(net, window = 300)`
#' asks whether the *statistic* should forget old events — a hypothesis about
#' memory, which changes the design matrix and therefore needs preprocessing.
#' Fit both and compare, or hold the windowed variant at zero with
#' `offset(..., coef = 0)` and use [test_parameter()]. `test_time()` holds the
#' statistic fixed and asks about the coefficient on it.
#'
#' @param x a fitted model of class `"result.goldfish"`; for the print method,
#'   the `test_time` object it renders.
#' @param method `"trend"` (default) for a smooth drift in the coefficient, or
#'   `"periods"` for a difference between regimes.
#' @param transform the time transform `g` used by `"trend"`: `"identity"`
#'   (default) or `"rank"`. Ignored by `"periods"`.
#' @param periods what defines the regimes for `"periods"`, in one of three
#'   forms told apart by shape:
#'   \describe{
#'     \item{a single whole number `J`}{splits the sequence into `J` groups of
#'       approximately equal **dependent-event** counts. This is the default
#'       form (`4L`).}
#'     \item{cut times}{a numeric vector of two or more values, or a single
#'       fractional one, read as boundaries on the model's own clock and giving
#'       right-open intervals. A count and a single cut time are distinguished
#'       by something a count cannot be — a count is whole — and a cut falling
#'       *between* two events is in any case the only kind that splits the
#'       sequence unambiguously, so `periods = 200.5` is one cut and
#'       `periods = 200` is a request for two hundred periods.}
#'     \item{a grouping}{a vector or factor with one entry per interval, used
#'       directly, which is how an exogenous regime is supplied.}
#'   }
#'   Ignored by `"trend"`.
#' @inheritParams test_gof.result.goldfish
#' @param preprocessed a `preprocessed.goldfish` object to evaluate over, as
#'   returned by [compute_statistics()]. Defaults to the object attached by
#'   `estimate_*(return_preprocessed = TRUE)`.
#' @param ... additional arguments passed to or from other methods (currently
#'   unused).
#'
#' @return An object of class `test_time`: a list of three
#'   [tibble::tibble()]s, carrying the metadata described in
#'   [diagnostic-tables].
#'   \describe{
#'     \item{`effects`}{one row per tested effect — `statistic`, `df` and
#'       `p_value` of its augmentation, under the term's compact string.}
#'     \item{`residuals`}{plot-ready per-interval data: the model's clock, the
#'       transformed clock, the scaled Schoenfeld residual of each tested
#'       effect, and the fitted estimate it should scatter around. Under
#'       `"periods"` it also carries the interval's period.}
#'     \item{`periods`}{the one-step per-period coefficient deltas, one row per
#'       tested effect and period, with the reference period at zero. Empty
#'       under `"trend"`, which has no periods.}
#'   }
#'   The joint test over all tested effects is in `attr(x, "context")$global`,
#'   and the print method reports it.
#'
#' @examples
#' data("social_evolution")
#' fit <- estimate_dynam(
#'   calls ~ inertia + recip,
#'   sub_model = "choice",
#'   data = social_evolution,
#'   return_preprocessed = TRUE
#' )
#' test_time(fit)
#'
#' # A regime split, with the coefficient's one-step move in each period.
#' regimes <- test_time(fit, method = "periods", periods = 2L)
#' regimes$periods
#'
#' @inheritSection diagnostic-requirements What a diagnostic needs
#'
#' @section What this test needs:
#'
#' The model's **statistics**, and **one evaluation pass**. No stored primitive:
#' the augmented model's information needs the per-interval expected
#' information, which no fit carries — a fit keeps the total.
#'
#' @seealso [test_gof()] for whether an effect's contribution is spread over
#'   the sequence as the model assumes, [test_parameter()] for a coefficient
#'   held at an imposed value, [diagnose_changepoints()] for locating a shift
#'   rather than testing one, and [diagnostic-tables] for the metadata a
#'   diagnostic object carries.
#' @method test_time result.goldfish
#' @export
test_time.result.goldfish <- function(
  x,
  method = c("trend", "periods"),
  transform = c("identity", "rank"),
  periods = 4L,
  effects = NULL,
  preprocessed = NULL,
  ...
) {
  abort_if_stale_result(x, "a time-heterogeneity test")
  method <- match.arg(method)
  transform <- match.arg(transform)
  tested <- time_tested_effects(x, effects)
  prep <- resolve_preprocessed(preprocessed, x)

  clock <- time_clock(x)
  grouping <- NULL
  if (identical(method, "trend")) {
    weights <- time_trend_weights(clock, transform, x)
  } else {
    grouping <- time_period_grouping(periods, clock, x)
    weights <- time_period_weights(grouping)
  }

  pass <- evaluate_model(
    x,
    at = stats::coef(x, complete = TRUE),
    return = c("information", "weighted_information", "event_scores"),
    preprocessed = prep,
    weights = weights
  )

  free <- which(!GetFixed(x))
  fitted <- pass$information[free, free, drop = FALSE]
  scores <- pass$event_scores[, free, drop = FALSE]
  blocks <- pass$weighted_information[free, free, , drop = FALSE]
  at <- match(tested, free)

  augmented <- if (identical(method, "trend")) {
    time_trend_tests(fitted, blocks, scores, weights, at)
  } else {
    time_period_tests(fitted, blocks, scores, weights, at)
  }

  labels <- gof_term_labels(x, tested)
  new_diagnostic_list(
    list(
      effects = tibble::tibble(
        index = tested,
        term = labels$term,
        coefficient = labels$coefficient,
        statistic = augmented$statistic,
        df = augmented$df,
        p_value = stats::pchisq(
          augmented$statistic,
          df = augmented$df,
          lower.tail = FALSE
        )
      ),
      residuals = time_residual_table(
        x,
        prep,
        clock,
        weights,
        tested,
        labels,
        transform = transform,
        method = method,
        grouping = grouping
      ),
      periods = time_delta_table(augmented$delta, tested, labels, grouping)
    ),
    "test_time",
    context = list(
      model = x$model,
      sub_model = x$sub_model,
      backend = x$backend,
      n_intervals = nrow(scores),
      n_events = sum(!x$right_censored_events),
      global = augmented$global
    ),
    params = list(
      method = method,
      transform = if (identical(method, "trend")) transform else NA_character_,
      n_periods = if (is.null(grouping)) NA_integer_ else nlevels(grouping)
    )
  )
}

# Which coefficients the test ranges over. A fixed one is excluded because the
# augmentation is a question about a coefficient the model chose, and a fixed
# coefficient was not chosen: its base score is not zero, so the augmented
# score test would be conditioning on a block that is not at its maximum.
time_tested_effects <- function(x, effects, call = rlang::caller_env()) {
  is_fixed <- GetFixed(x)
  if (is.null(effects)) {
    tested <- which(!is_fixed)
    if (length(tested) == 0L) {
      cli::cli_abort(
        c(
          "This fit has no estimated coefficient to test.",
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
    x$names,
    "effects",
    expand_family = TRUE,
    call = call
  )
  fixed <- tested[is_fixed[tested]]
  if (length(fixed) > 0) {
    labels <- gof_term_labels(x, fixed)$term
    cli::cli_abort(
      c(
        "{.arg effects} names {length(fixed)} term{?s} held fixed through
         {.fn offset}: {.val {labels}}.",
        "x" = "This test asks whether an estimated coefficient stayed put, and
               an imposed one never moved to begin with.",
        "i" = "{.fn test_parameter} tests a coefficient at the value
               {.fn offset} imposed."
      ),
      call = call
    )
  }
  tested
}

# The clock the sub-model's likelihood runs on. See the "Which clock" section:
# an ordinal sub-model conditions on order alone, so its axis is the event
# index; an exact-time one integrates over elapsed time, so its axis is the
# event time. Length is the interval count, right-censored intervals included,
# because those carry information too.
time_clock <- function(x) {
  if (is_exact_time_fit(x)) {
    return(as.numeric(x$event_time))
  }
  seq_along(x$event_time)
}

# The two weight columns the trend test needs: the transform and its square.
#
# Centering is done here rather than in the kernel, and the constant is
# arbitrary: adding `c` to `g` adds `c * x_d` to the augmented column, which
# the base block already spans, so the score test is invariant to it. It is
# subtracted anyway because it keeps the augmented information far better
# conditioned on a clock whose origin is a POSIXct epoch.
time_trend_weights <- function(clock, transform, x) {
  g <- switch(transform, identity = clock, rank = rank(clock))
  g <- g - mean(g[!x$right_censored_events])
  cbind(g = g, g_squared = g^2)
}

# The period indicators, one column per period AFTER the first: the first is
# the reference, exactly as a treatment-coded factor drops its first level.
#
# Indicators are idempotent and disjoint, so `sum_k 1{k in j} 1{k in j'} Cov_k`
# is the period-j block when `j == j'` and zero otherwise -- which is why J
# columns suffice where a general pair of augmentations would need their
# products too.
time_period_weights <- function(grouping) {
  levels <- levels(grouping)
  out <- vapply(
    levels[-1],
    function(level) as.numeric(grouping == level),
    numeric(length(grouping))
  )
  matrix(
    out,
    nrow = length(grouping),
    dimnames = list(NULL, levels[-1])
  )
}

# What defines a period. Three accepted forms, told apart by shape rather than
# by a mode argument: one number is a count of equal-sized groups, a short
# vector is a set of cut times, and a full-length vector is the grouping
# itself. The three are distinguishable because a grouping has one entry per
# interval and cut times cannot (a cut per interval would define no interior).
time_period_grouping <- function(
  periods,
  clock,
  x,
  call = rlang::caller_env()
) {
  n <- length(clock)
  if (is.factor(periods) || length(periods) == n) {
    if (length(periods) != n) {
      cli::cli_abort(
        c(
          "{.arg periods} must have one entry per interval when it is a
           grouping.",
          "x" = "It has {length(periods)}; the fit has {n}."
        ),
        call = call
      )
    }
    return(droplevels(as.factor(periods)))
  }
  if (!is.numeric(periods)) {
    cli::cli_abort(
      "{.arg periods} must be a count, cut times, or a grouping.",
      call = call
    )
  }
  # A single number is genuinely ambiguous -- 3 could ask for three periods or
  # for one cut at t = 3 -- so the two are told apart by something a count
  # cannot be: a count is whole, a cut need not be. A cut falling BETWEEN two
  # events is also the only kind that splits the sequence unambiguously, so the
  # fractional form is the one a caller wants anyway. Two or more numbers are
  # always cut times, a count being a single number.
  if (length(periods) == 1L && periods == round(periods)) {
    return(time_equal_count_periods(periods, clock, x, call = call))
  }
  # Cut times: right-open intervals on the model's own clock, with the window
  # ends supplied so the caller names only the interior boundaries.
  breaks <- sort(unique(c(-Inf, as.numeric(periods), Inf)))
  grouping <- cut(clock, breaks = breaks, right = FALSE)
  time_check_periods(droplevels(grouping), call = call)
}

# J groups of approximately equal DEPENDENT-event counts. Equal event counts,
# not equal intervals: the right-censored intervals of an exact-time fit carry
# no observed alternative, and a period holding many of them and few events
# would be a period the test has little to say about.
time_equal_count_periods <- function(n_periods, clock, x, call) {
  n_periods <- as.integer(n_periods)
  if (is.na(n_periods) || n_periods < 2L) {
    cli::cli_abort(
      c(
        "{.arg periods} must ask for at least two periods.",
        "x" = "It asks for {n_periods}."
      ),
      call = call
    )
  }
  dependent <- !x$right_censored_events
  rank_of_event <- cumsum(dependent)
  n_events <- sum(dependent)
  edges <- ceiling(seq_len(n_periods - 1L) * n_events / n_periods)
  grouping <- cut(
    rank_of_event,
    breaks = c(-Inf, edges, Inf),
    right = TRUE,
    labels = paste0("period_", seq_len(n_periods))
  )
  time_check_periods(droplevels(grouping), call = call)
}

time_check_periods <- function(grouping, call) {
  if (nlevels(grouping) < 2L) {
    cli::cli_abort(
      c(
        "{.arg periods} defines a single period.",
        "x" = "There is nothing to compare it against.",
        "i" = "Supply cut times inside the observation window, or a count of
               two or more."
      ),
      call = call
    )
  }
  grouping
}

# The trend statistic. The augmented model for effect `d` is `[X, x_d * g]`,
# so its score is zero on the base block (the fit is at its maximum there) and
# `sum_k g_k s_kd` on the interaction. The information blocks are the total,
# the `g`-weighted sum and the `g^2`-weighted sum -- the evaluator's return.
#
# The quadratic form is `score_statistic()` / `joint_score_test()`, shared with
# `test_parameter()` rather than written again: mark the base block "free" and
# the interaction "fixed", and the same "invert over the free block plus the
# tested coordinates" rule produces the efficient score test of the
# interaction.
time_trend_tests <- function(fitted, blocks, scores, weights, at) {
  n_free <- ncol(fitted)
  interaction_score <- colSums(scores * weights[, "g"])
  score <- c(rep(0, n_free), interaction_score)
  information <- rbind(
    cbind(fitted, blocks[,, "g"]),
    cbind(blocks[,, "g"], blocks[,, "g_squared"])
  )
  is_fixed <- c(rep(FALSE, n_free), rep(TRUE, n_free))
  statistic <- vapply(
    at,
    function(d) score_statistic(score, information, n_free + d, is_fixed),
    numeric(1)
  )
  list(
    statistic = unname(statistic),
    df = rep(1L, length(at)),
    global = joint_score_test(score, information, n_free + at, is_fixed),
    delta = NULL
  )
}

# The periods statistic. The augmented model for effect `d` carries one column
# per period after the first, so the test is on `J - 1` degrees of freedom.
# Disjointness is what keeps the interaction block simple: two different
# periods share no interval, so their cross-block is zero and the interaction
# block is block-diagonal in the period.
time_period_tests <- function(fitted, blocks, scores, weights, at) {
  n_free <- ncol(fitted)
  n_periods <- ncol(weights)
  labels <- colnames(weights)
  per_effect <- lapply(at, function(d) {
    cross <- vapply(labels, function(j) blocks[, d, j], numeric(n_free))
    interaction <- diag(
      vapply(labels, function(j) blocks[d, d, j], numeric(1)),
      nrow = n_periods
    )
    information <- rbind(
      cbind(fitted, matrix(cross, nrow = n_free)),
      cbind(t(matrix(cross, nrow = n_free)), interaction)
    )
    score <- c(
      rep(0, n_free),
      vapply(labels, function(j) sum(scores[weights[, j] == 1, d]), numeric(1))
    )
    is_fixed <- c(rep(FALSE, n_free), rep(TRUE, n_periods))
    test <- joint_score_test(
      score,
      information,
      n_free + seq_len(n_periods),
      is_fixed
    )
    # The one-step Newton move from zero: what the interaction coefficient
    # would become after a single update, which is the readable form of the
    # same score and information the test reads.
    step <- solve(information, score)
    list(test = test, delta = step[n_free + seq_len(n_periods)])
  })
  list(
    statistic = vapply(per_effect, function(e) e$test$statistic, numeric(1)),
    df = rep(as.integer(n_periods), length(at)),
    global = time_period_global(fitted, blocks, scores, weights, at),
    delta = do.call(rbind, lapply(per_effect, function(e) e$delta))
  )
}

# The joint test over every tested effect's period interactions at once. The
# cross-block between two effects within the same period is that period's own
# information block; across periods it is zero, disjointness again.
time_period_global <- function(fitted, blocks, scores, weights, at) {
  n_free <- ncol(fitted)
  labels <- colnames(weights)
  n_periods <- length(labels)
  n_tested <- length(at)
  size <- n_free + n_tested * n_periods
  information <- matrix(0, size, size)
  information[seq_len(n_free), seq_len(n_free)] <- fitted
  score <- numeric(size)
  for (j in seq_len(n_periods)) {
    block <- blocks[,, labels[j]]
    slot <- n_free + (j - 1L) * n_tested + seq_len(n_tested)
    information[seq_len(n_free), slot] <- block[, at, drop = FALSE]
    information[slot, seq_len(n_free)] <- t(block[, at, drop = FALSE])
    information[slot, slot] <- block[at, at, drop = FALSE]
    score[slot] <- colSums(scores[weights[, j] == 1, at, drop = FALSE])
  }
  is_fixed <- c(rep(FALSE, n_free), rep(TRUE, n_tested * n_periods))
  joint_score_test(
    score,
    information,
    n_free + seq_len(size - n_free),
    is_fixed
  )
}

# The plot-ready per-interval data. The residual is the scaled Schoenfeld one
# the residuals method already defines, taken from there rather than restated,
# so a scatter drawn from this table and one drawn from `residuals()` are the
# same numbers.
time_residual_table <- function(
  x,
  prep,
  clock,
  weights,
  tested,
  labels,
  transform,
  method,
  grouping
) {
  scaled <- stats::residuals(
    x,
    type = "scaled_schoenfeld",
    preprocessed = prep
  )
  estimate <- stats::coef(x, complete = TRUE)
  n <- length(clock)
  out <- lapply(seq_along(tested), function(i) {
    d <- tested[i]
    tibble::tibble(
      index = d,
      term = labels$term[i],
      interval = seq_len(n),
      clock = clock,
      transformed = if (identical(method, "trend")) {
        weights[, "g"]
      } else {
        rep(NA_real_, n)
      },
      period = if (is.null(grouping)) {
        rep(NA_character_, n)
      } else {
        as.character(grouping)
      },
      residual = as.numeric(scaled[, d]),
      reference = unname(estimate[d])
    )
  })
  do.call(rbind, out)
}

# The one-step deltas in long form, with the reference period carried at zero
# so a panel reads as J values rather than J - 1 plus an implicit baseline.
time_delta_table <- function(delta, tested, labels, grouping) {
  if (is.null(delta)) {
    return(tibble::tibble(
      index = integer(),
      term = character(),
      period = character(),
      delta = numeric()
    ))
  }
  levels <- levels(grouping)
  tibble::tibble(
    index = rep(tested, each = length(levels)),
    term = rep(labels$term, each = length(levels)),
    period = rep(levels, times = length(tested)),
    delta = as.numeric(t(cbind(0, delta)))
  )
}

#' @rdname test_time.result.goldfish
#' @method test_time flavored_result.goldfish
#' @export
test_time.flavored_result.goldfish <- function(
  x,
  method = c("trend", "periods"),
  transform = c("identity", "rank"),
  periods = 4L,
  effects = NULL,
  preprocessed = NULL,
  ...
) {
  method <- match.arg(method)
  transform <- match.arg(transform)
  map <- x$process_map
  # Flavor-major, the order the container prints in, so two tables from the
  # same fit never need reordering against each other.
  rows <- flavored_row_order(x)
  per_block <- lapply(rows, function(i) {
    fit <- x$results[[as.character(map$fid[i])]]
    label <- render_process_label(map, map$fid[i])
    block <- time_block(
      fit,
      label,
      method,
      transform,
      periods,
      effects,
      preprocessed
    )
    lapply(block, function(component) {
      component$flavor <- map$flavor[i]
      component$family <- map$family[i]
      component
    })
  })
  names(per_block) <- NULL
  components <- stats::setNames(
    lapply(
      c("effects", "residuals", "periods"),
      function(name) do.call(rbind, lapply(per_block, `[[`, name))
    ),
    c("effects", "residuals", "periods")
  )
  new_diagnostic_list(
    components,
    "test_time",
    context = list(
      model = x$model,
      layer = x$layer,
      sub_model = unique(map$family[rows]),
      flavor = unique(map$flavor[rows]),
      fid = map$fid[rows],
      backend = x$results[[as.character(map$fid[rows[1]])]]$backend
    ),
    params = list(
      method = method,
      transform = if (identical(method, "trend")) transform else NA_character_
    )
  )
}

# One process's test, with the process named if it fails. Each process has its
# own formula, so an `effects` selection valid for one may be absent from
# another, and "unknown term" without saying where is not actionable on a fit
# carrying four processes.
time_block <- function(
  fit,
  label,
  method,
  transform,
  periods,
  effects,
  preprocessed,
  call = rlang::caller_env()
) {
  tryCatch(
    test_time(
      fit,
      method = method,
      transform = transform,
      periods = periods,
      effects = effects,
      preprocessed = preprocessed
    ),
    error = function(e) {
      cli::cli_abort(
        "{.fn test_time} could not test process {.val {label}}.",
        parent = e,
        call = call
      )
    }
  )
}

#' @rdname test_time.result.goldfish
#' @method print test_time
#' @export
print.test_time <- function(x, ...) {
  context <- attr(x, "context")
  params <- attr(x, "params")
  # One print for both shapes, told apart by the column the flavored form
  # appends -- the same branch `print.test_gof` makes.
  blocked <- "flavor" %in% names(x$effects)
  cli::cli_rule(left = "{.cls test_time}")
  if (blocked) {
    cli::cli_text(
      "Model {.val {context$model}} · layer {.val {context$layer}} ·
       {length(context$flavor)} flavor{?s} over {length(context$fid)}
       process{?es}"
    )
  } else {
    cli::cli_text(
      "Model {.val {context$model}} · sub-model {.val {context$sub_model}} ·
       backend {.val {context$backend}}"
    )
    cli::cli_text(
      "{context$n_intervals} interval{?s}, {context$n_events} dependent
       event{?s}; {nrow(x$effects)} effect{?s} tested."
    )
  }
  if (identical(params$method, "trend")) {
    cli::cli_text(
      "Score test of a {.val {params$transform}} time trend in each
       coefficient."
    )
  } else if (blocked) {
    # A blocked result carries no single period count: each process is split
    # on its own event sequence, so the counts need not agree.
    cli::cli_text("Score test of a coefficient difference across periods.")
  } else {
    cli::cli_text(
      "Score test of a coefficient difference across
       {params$n_periods} period{?s}."
    )
  }
  print(x$effects)
  # The joint test is per process, so a blocked result has no single one to
  # report: each block's own is in its `effects` rows, and combining across
  # processes would assert a joint model that was never estimated.
  if (!blocked) {
    global <- context$global
    cli::cli_text("")
    cli::cli_text(
      "Joint test: chi-squared {sprintf('%.4g', global$statistic)} on
       {global$df} df, p {sprintf('%.4g', global$p_value)}."
    )
  }
  invisible(x)
}
