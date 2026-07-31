##################### ###
#
# Goldfish package
# The score (LM) test of a coefficient held at an imposed value
#
##################### ###

#' Score test of a coefficient held at an imposed value
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Tests whether the value an `offset()` term imposed on a coefficient is
#' consistent with the data, without ever estimating that coefficient. The
#' question it answers is "would freeing this term move the fit?", and it
#' answers it from the constrained fit alone.
#'
#' @details
#' A term written `offset(recip, coef = 0)` enters the model held at zero: its
#' statistics are computed, it contributes to every risk-set probability, and
#' its coefficient is not estimated. At the constrained maximum the *free*
#' coefficients have score zero by definition, while the held one generally
#' does not — and the size of that leftover score is exactly the evidence that
#' the imposed value is wrong.
#'
#' The statistic is the efficient score (Lagrange multiplier) form
#' \deqn{LM = U_S^\top \left[ I^{-1} \right]_{SS} U_S,}
#' with \eqn{U} the score and \eqn{I} the information of the full model
#' evaluated at the fitted vector, \eqn{S} the tested coefficients, and the
#' inverse taken over the free block together with \eqn{S} — the untested
#' `offset()` terms are constants of the model rather than nuisance
#' parameters, so they leave the parameter space entirely. Under the null it
#' is chi-square on `length(S)` degrees of freedom. When every offset is
#' tested at once this is the familiar \eqn{U^\top I^{-1} U}, the free block's
#' score being zero.
#'
#' Each row of the result is one term's own one-degree-of-freedom test, taken
#' the same way, so a row is exact rather than marginal: freeing that one
#' coefficient is a well-defined alternative whatever the other offsets are
#' doing.
#'
#' @section Why this costs a pass:
#' The fitted object cannot answer the question. Estimation zeroes the score
#' at the fixed coefficients before every Newton step — the update indexes the
#' free block, so the zeros are never read, and the convergence test needs
#' them, since the score at a held coefficient is generally nonzero at the
#' constrained optimum and `max|score| < tol` would otherwise never be met.
#' One vector serves both the update and the report, so the score this test
#' needs exists nowhere on the fit.
#'
#' It is therefore recomputed, which needs the model's statistics: either
#' attached by `estimate_*(return_preprocessed = TRUE)` or supplied through
#' `preprocessed =`. One evaluation pass, no preprocessing pass. Storing the
#' unmasked score on the fit instead was considered and rejected: one rule for
#' which diagnostics need the statistics is worth more than saving a pass on
#' one of them.
#'
#' @section Testing a term the formula does not contain:
#' Not available, and the way to do it today is to put the term in the model
#' held at the value you want to test:
#'
#' ```
#' fit <- estimate_dynam(
#'   calls ~ inertia + offset(recip, coef = 0),
#'   sub_model = "choice", data = social_evolution,
#'   return_preprocessed = TRUE)
#' test_parameter(fit)
#' ```
#'
#' That fit *is* the constrained fit the test needs, and its statistics are
#' preprocessed in the same pass. Testing a genuinely absent effect would
#' instead require preprocessing an augmented model over the whole event
#' sequence — roughly the cost of the original fit, against one evaluation
#' pass here — which is why it is deferred rather than offered.
#'
#' For comparing two models that were both estimated, this is the wrong tool:
#' use [lmtest::lrtest()] or [lmtest::waldtest()], whose generics goldfish fits
#' already satisfy.
#'
#' @param x a fitted model of class `"result.goldfish"` carrying at least one
#'   `offset()` term, or a multi-process fit; for the print method, the
#'   `test_parameter` object it renders.
#' @param effects an optional selection of the held terms to test, given by any
#'   name a term answers to or by position; [model_terms()] lists them. A bare
#'   **effect** name selects every term of that effect, so `effects =
#'   "inertia"` tests each held variant of `inertia` the formula carries.
#'   Defaults to every `offset()` term the fit carries.
#' @param preprocessed a `preprocessed.goldfish` object to evaluate from, as
#'   returned by [compute_statistics()]. Defaults to the object attached by
#'   `estimate_*(return_preprocessed = TRUE)`.
#' @param ... additional arguments passed to or from other methods (currently
#'   unused).
#'
#' @return An object of class `test_parameter`: a [tibble::tibble()] with one
#'   row per tested term — `imposed` the value the formula fixed it at, `score`
#'   the leftover score there, `statistic` the one-degree-of-freedom LM,
#'   `df` and `p_value` — carrying the metadata described in
#'   [diagnostic-tables]. The joint test over all tested terms is in
#'   `attr(x, "context")$joint`, and the print method reports it.
#'
#' @examples
#' data("social_evolution")
#' fit <- estimate_dynam(
#'   calls ~ inertia + offset(recip, coef = 0) + trans,
#'   sub_model = "choice",
#'   data = social_evolution,
#'   return_preprocessed = TRUE
#' )
#' test_parameter(fit)
#'
#' @seealso [test_gof()] for whether an estimated effect's contribution is
#'   spread over the sequence, [test_time()] for whether it is constant, and
#'   [diagnostic-tables] for the metadata a diagnostic object carries.
#' @method test_parameter result.goldfish
#' @export
test_parameter.result.goldfish <- function(
  x,
  effects = NULL,
  preprocessed = NULL,
  ...
) {
  abort_if_stale_result(x, "a score test")
  tested <- parameter_tested_effects(x, effects)
  # Resolved here rather than inside `evaluate_model()` so the "no statistics"
  # abort names this function, which is the one the user called.
  prep <- resolve_preprocessed(preprocessed, x)
  pass <- evaluate_model(
    x,
    at = stats::coef(x, complete = TRUE),
    return = c("score", "information"),
    preprocessed = prep
  )

  is_fixed <- GetFixed(x)
  score <- pass$score
  information <- pass$information
  statistic <- vapply(
    tested,
    function(d) score_statistic(score, information, d, is_fixed),
    numeric(1)
  )
  labels <- gof_term_labels(x, tested)
  new_diagnostic_table(
    tibble::tibble(
      index = tested,
      term = labels$term,
      coefficient = labels$coefficient,
      imposed = unname(x$parameters[tested]),
      score = unname(score[tested]),
      statistic = unname(statistic),
      df = 1L,
      p_value = stats::pchisq(statistic, df = 1L, lower.tail = FALSE)
    ),
    "test_parameter",
    context = list(
      model = x$model,
      sub_model = x$sub_model,
      backend = x$backend,
      n_events = sum(!x$right_censored_events),
      joint = joint_score_test(score, information, tested, is_fixed)
    ),
    params = list(n_fixed = sum(is_fixed)),
    defining = c("statistic", "p_value")
  )
}

# Which held coefficients the test ranges over. Three ways to have nothing to
# test, and each names the idiom, because "you have no offset terms" is only
# useful next to "here is how to make one".
parameter_tested_effects <- function(
  x,
  effects,
  call = rlang::caller_env()
) {
  is_fixed <- GetFixed(x)
  if (is.null(effects)) {
    tested <- which(is_fixed)
    if (length(tested) == 0L) {
      abort_needs_offset(
        "This fit holds no coefficient at an imposed value.",
        "x" = "Every term was estimated, and an estimated coefficient's score
               is zero at the maximum — there is nothing left to test.",
        call = call
      )
    }
    return(unname(tested))
  }
  tested <- tryCatch(
    resolve_term_index(
      effects,
      x$names,
      "effects",
      expand_family = TRUE,
      call = call
    ),
    error = function(e) {
      abort_needs_offset(
        "{.arg effects} names a term this model does not contain.",
        "x" = "A term absent from the formula has no statistics, so there is
               no score to test it with.",
        parent = e,
        call = call
      )
    }
  )
  free <- tested[!is_fixed[tested]]
  if (length(free) > 0) {
    labels <- gof_term_labels(x, free)$term
    cli::cli_abort(
      c(
        "{.arg effects} names {length(free)} estimated coefficient{?s}:
         {.val {labels}}.",
        "x" = "This test reads the score left over at a value the formula
               imposed, and an estimated coefficient's score is zero at the
               maximum by construction.",
        "i" = "{.fn test_gof} tests whether an estimated effect's contribution
               is spread over the sequence as the model assumes."
      ),
      call = call
    )
  }
  tested
}

# The one exit every "nothing to test here" abort takes: name the idiom, and
# name what it costs not to have used it.
abort_needs_offset <- function(message, ..., parent = NULL, call) {
  cli::cli_abort(
    c(
      message,
      ...,
      "i" = "Put the term in the model held at the value you want to test:
             {.code offset(term, coef = 0)}. Its statistics are preprocessed
             in the same pass, and the resulting fit is the constrained one
             this test needs.",
      "i" = "Testing a term absent from the formula would instead need a
             preprocessing pass over the whole event sequence, which is why it
             is not offered."
    ),
    parent = parent,
    call = call
  )
}

# The efficient score statistic for one held coefficient. The parameter space
# is the free block plus the tested coefficient: the OTHER held terms are
# constants of the model rather than nuisance parameters, so they leave the
# inverse entirely and the row is an exact one-degree-of-freedom test rather
# than a marginal one.
#
# The block of the INVERSE is used directly, never inverted a second time:
# `[I^-1]_SS` is already the inverse of the efficient information
# `I_SS - I_SF I_FF^-1 I_FS`, so a further `solve()` would report the quadratic
# form in the efficient information itself. The two differ by more than a
# constant, which is what makes the `t(Delta) I Delta` equivalence test worth
# having.
score_statistic <- function(score, information, d, is_fixed) {
  keep <- which(!is_fixed | seq_along(is_fixed) == d)
  inverse <- invert_information_block(information, keep)
  position <- match(d, keep)
  score[d]^2 * inverse[position, position]
}

# The same construction over every tested coefficient at once: chi-square on
# as many degrees of freedom as there are of them. With all the offsets tested
# this is the familiar `t(U) I^-1 U`, the free block's score being zero.
joint_score_test <- function(score, information, tested, is_fixed) {
  keep <- which(!is_fixed | seq_along(is_fixed) %in% tested)
  inverse <- invert_information_block(information, keep)
  positions <- match(tested, keep)
  block <- inverse[positions, positions, drop = FALSE]
  statistic <- drop(t(score[tested]) %*% block %*% score[tested])
  tibble::tibble(
    statistic = statistic,
    df = length(tested),
    p_value = stats::pchisq(statistic, df = length(tested), lower.tail = FALSE)
  )
}

invert_information_block <- function(
  information,
  keep,
  call = rlang::caller_env()
) {
  tryCatch(
    solve(information[keep, keep, drop = FALSE]),
    error = function(e) {
      cli::cli_abort(
        c(
          "The information matrix of this fit cannot be inverted.",
          "x" = "The score test needs it over the estimated coefficients
                 together with the tested one, and collinear effects make it
                 singular.",
          "i" = "Check the model for redundant effects."
        ),
        call = call
      )
    }
  )
}

#' @export
`[.test_parameter` <- function(x, ...) {
  out <- NextMethod()
  demote_if_incomplete(out)
}

#' @return The object, invisibly.
#' @rdname test_parameter.result.goldfish
#' @method print test_parameter
#' @export
print.test_parameter <- function(x, ...) {
  context <- attr(x, "context")
  cli::cli_rule(left = "{.cls test_parameter}")
  cli::cli_text(
    "Model {.val {context$model}} ·
     sub-model {.val {context$sub_model}} ·
     backend {.val {context$backend}}"
  )
  cli::cli_text(
    "Score test of {nrow(x)} coefficient{?s} held at an imposed value,
     over {context$n_events} event{?s}."
  )
  joint <- context$joint
  cli::cli_text(
    "Joint: {.field LM} = {format(joint$statistic, digits = 4)} on
     {joint$df} degree{?s} of freedom,
     {.field p} = {format.pval(joint$p_value, digits = 3)}"
  )
  body <- x
  class(body) <- setdiff(class(body), "test_parameter")
  print(body, ...)
  invisible(x)
}

#' Score test on a multi-process specification
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Tests each process of a specification fit against **its own** `offset()`
#' terms, exactly as [test_parameter()] tests a single fit.
#'
#' @details
#' No candidate argument is needed or accepted per process: each process
#' formula already declares which of its terms are held, so the tested set is
#' read off the fit rather than supplied. A process carrying no `offset()`
#' term contributes no rows.
#'
#' The per-process joint tests are carried in the metadata, one row per
#' process. They are **not** combined across processes. The processes are
#' independent, so a sum of chi-squares would be a legitimate statistic — but
#' it is not one the specification asks for, and a combination nobody has
#' asked to interpret is not worth reporting.
#'
#' @inheritParams test_parameter.result.goldfish
#' @param x a multi-process fit of class `"flavored_result.goldfish"`.
#'
#' @return An object of class `test_parameter`, shaped as the single-fit result
#'   and documented at [test_parameter.result.goldfish()], with `flavor` and
#'   `family` columns appended and one joint row per process in the metadata.
#'
#' @seealso [test_parameter.result.goldfish()] for what each process's test is.
#' @method test_parameter flavored_result.goldfish
#' @export
test_parameter.flavored_result.goldfish <- function(
  x,
  effects = NULL,
  preprocessed = NULL,
  ...
) {
  map <- x$process_map
  rows <- flavored_row_order(x)
  per_process <- lapply(rows, function(i) {
    fit <- x$results[[as.character(map$fid[i])]]
    if (!any(GetFixed(fit))) {
      return(NULL)
    }
    label <- render_process_label(map, map$fid[i])
    one <- parameter_block(fit, label, effects, preprocessed)
    table <- tibble::as_tibble(one)
    table$flavor <- map$flavor[i]
    table$family <- map$family[i]
    joint <- attr(one, "context")$joint
    joint$flavor <- map$flavor[i]
    joint$family <- map$family[i]
    list(table = table, joint = joint, fit = fit)
  })
  per_process <- per_process[!vapply(per_process, is.null, logical(1))]
  if (length(per_process) == 0L) {
    cli::cli_abort(c(
      "No process of this fit holds a coefficient at an imposed value.",
      "i" = "Add {.code offset(term, coef = 0)} to the process formula whose
             term you want to test."
    ))
  }
  new_diagnostic_table(
    do.call(rbind, lapply(per_process, `[[`, "table")),
    "test_parameter",
    context = list(
      model = x$model,
      layer = x$layer,
      sub_model = unique(map$family[rows]),
      flavor = unique(map$flavor[rows]),
      backend = per_process[[1]]$fit$backend,
      n_events = vapply(
        per_process,
        function(p) sum(!p$fit$right_censored_events),
        integer(1)
      ),
      joint = do.call(rbind, lapply(per_process, `[[`, "joint"))
    ),
    params = list(n_processes = length(per_process)),
    defining = c("statistic", "p_value")
  )
}

# One process's test, with the process named if it fails -- each process has
# its own formula, so a selection valid for one can be absent from another.
parameter_block <- function(
  fit,
  label,
  effects,
  preprocessed,
  call = rlang::caller_env()
) {
  tryCatch(
    test_parameter(fit, effects = effects, preprocessed = preprocessed),
    error = function(e) {
      cli::cli_abort(
        "{.fn test_parameter} could not test process {.val {label}}.",
        parent = e,
        call = call
      )
    }
  )
}
