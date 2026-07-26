#' Control Parameters for the Newton-type Estimation Algorithm
#'
#' Specifies the algorithm and its control parameters for the model estimation
#' process in `[estimate]`. The name records the algorithm family: direct
#' maximization with Newton-type steps (damped Newton-Raphson, BFGS, BHHH;
#' Nelder-Mead is the derivative-free exception), as opposed to the
#' ascent-based Monte Carlo algorithms of other model families.
#'
#' The damping factors arguments control the step size at each iteration of
#' the Newton-Raphson algorithm. They have a bigger impact in the first
#' iterations of the algorithm and will decrease by half after each iteration.
#' In particular, the increase factor is the one that is expected to play
#' a role during the first iterations where it's easier to improve
#' the log-likelihood.
#' In scenarios where the model is fit in a large dataset, for example,
#' when the number of actors in the system is large,
#' the `damping_increase_factor` and the `damping_decrease_factor` arguments
#' can be increased from the default values (e.g., 4 or 6) to speed up the
#' estimation process with large changes in the coefficients.
#' However, this should have the opposite effect in small datasets producing
#' large changes in the coefficients that would create more iterations
#' (in a similar vein to the step size parameter in gradient descendent).

#'
#' @param initial_parameters A numeric vector. It includes initial parameter
#'   values used to initialize the estimation process.
#'   Default is `NULL`, which means parameters are initialized at zero,
#'   except for the rate intercept when present.
#' @param fixed_parameters `r lifecycle::badge("superseded")` A numeric vector
#'   of the same length as the number of parameters to be estimated in the model.
#'   `NA` values indicate parameters to be estimated,
#'   while numeric values indicate parameters to be fixed at the given value.
#'   For example, if the vector is `c(2, NA)` then the first component of the
#'   parameter is fixed to 2 during the estimation process.
#'   Default is `NULL` (all parameters are estimated).
#'   Superseded by wrapping the term in `offset()` in the model formula and
#'   supplying its value through `offset_coef`, which aligns values to terms by
#'   name instead of by counting coefficient positions.
#' @param offset_coef A numeric vector giving the fixed coefficient value(s) for
#'   the `offset()` term(s) in the model formula, aligned to the offset terms in
#'   formula order. For example, `~ inertia + offset(ego(sex)) + recip` with
#'   `offset_coef = 2` holds the `ego(sex)` coefficient at 2 while estimating the
#'   rest. Default is `NULL` (no offsets).
#' @param max_iterations An integer non-negative.
#'   The maximum number of iterations in the Gauss-Fisher scoring algorithm.
#'   Default is `20`.
#' @param score_tol A positive numeric value.
#'   Scale-invariant gradient convergence criterion.
#'   The algorithm stops if
#'   `max(abs(score)) / max(1, abs(logLik)) <= score_tol`.
#'   Default is `1e-6`.
#' @param step_tol A positive numeric value.
#'   Damped Newton step size convergence criterion.
#'   The algorithm stops if `max(abs(update)) <= step_tol`.
#'   Default is `1e-8`.
#' @param convergence_criterion `r lifecycle::badge("deprecated")`.
#'   Use `score_tol` instead. This argument is ignored.
#' @param initial_damping A positive numeric value.
#'   The initial damping factor for the Gauss-Fisher scoring algorithm.
#'   Default is `NULL`, which allows `estimate_dynam()`, `estimate_rem()` and
#'   `estimate_dynami()` to set
#'   a context-dependent default
#'   (e.g., 30 or 10 based on wheter the model has windows effects).
#'   If set, this value is used directly.
#' @param damping_increase_factor A positive numeric value.
#'   Factor by which damping is increased when improvements
#'   in the estimation are found. Must be >= 1. Default is `2`.
#' @param damping_decrease_factor A positive numeric value.
#'   Factor by which damping is decreased when no improvements
#'   in the estimation are found. Must be >= 1. Default is `3`.
#' @param return_interval_loglik `r lifecycle::badge("deprecated")` Superseded by
#'   `diagnostics = "loglik"`. Whether to keep and return the log-likelihood for
#'   each event.
#' @param return_probabilities `r lifecycle::badge("deprecated")` Superseded by
#'   `diagnostics = "probabilities"`. Whether to keep and return the
#'   probabilities for all alternatives for each event.
#'   * When `sub_model = "choice"` the probabilities correspond to all actors in
#'     the choice set present at the time of the event.
#'   * When `model = "REM"` the probabilities correspond to all dyads present at
#'     the time of the event.
#' @param return_event_scores `r lifecycle::badge("deprecated")` Superseded by
#'   `diagnostics = "scores"`. Whether to keep and return the per-event score
#'   matrix (one row per
#'   dependent event, one column per effect) evaluated at the returned
#'   parameter estimates, stored as the `event_scores` component of the result.
#'   Each row is the observation-level gradient contribution whose column sums
#'   equal the aggregate score. The matrix supports downstream diagnostics
#'   (implemented by other tools, not here): robust sandwich and clustered
#'   standard errors from the outer product of gradients `crossprod(event_scores)`,
#'   per-effect score-process diagnostics that localize where individual effects
#'   drift over the event sequence, and event-influence measures. Only the
#'   `"cpp"` and `"r"` backends support it; `"gather"` aborts.
#' @param diagnostics Names the per-event diagnostic *primitives* estimation
#'   stores on the fitted result, superseding the three `return_*` flags above.
#'   Accepts a character vector drawn from
#'   `c("loglik", "scores", "ranks", "margins", "probabilities")`, or the
#'   shorthands `TRUE` (equivalent to `c("loglik", "scores")`), `"all"` (all
#'   five), and `FALSE` / `character(0)` (none). Each primitive maps to a stored
#'   component of the result: `"loglik"` to `intervalLogL` (and `total_rate` on
#'   exact-time submodels), `"scores"` to `event_scores`, `"ranks"` to
#'   `observed_rank`, `"margins"` to per-actor observed and expected counts, and
#'   `"probabilities"` to per-event probability vectors. Unknown names abort with
#'   an error listing the valid primitives. Default is `c("loglik", "scores")`,
#'   preserving today's stored log-likelihood and adding the (free) scores. For
#'   fits with more than 100,000 events a one-time message reports the
#'   approximate footprint of the per-event vectors and names `diagnostics =
#'   FALSE` as the opt-out.
#' @param optimizer `r lifecycle::badge("experimental")` A character string
#'   naming the optimization algorithm. Options are:
#'   \describe{
#'      \item{newton_raphson}{The built-in damped Newton-Raphson / Fisher
#'       scoring loop (default).}
#'      \item{bfgs}{Quasi-Newton BFGS, via `maxLik::maxLik()`.}
#'      \item{bhhh}{Berndt-Hall-Hall-Hausman, using the per-event score matrix
#'       as observation-level gradients, via `maxLik::maxLik()`.}
#'      \item{nelder_mead}{Derivative-free Nelder-Mead, via `maxLik::maxLik()`.}
#'    }
#'   Any value other than `"newton_raphson"` requires the \pkg{maxLik} package
#'   (in `Suggests`) and runs only on the `"cpp"` backend. Default is
#'   `"newton_raphson"`.
#' @param backend A character string naming the computational implementation
#'   that runs the estimation. Options are:
#'   \describe{
#'      \item{cpp}{`C++` based implementation using RcppEigen
#'       and RcppParallel.}
#'      \item{r}{R-based reference implementation.}
#'      \item{gather}{`C++` based implementation with a different data
#'       structure that reduces the time but it can increase the memory usage.}
#'    }
#'   Default is `"cpp"`.
#' @param engine `r lifecycle::badge("deprecated")` Renamed to `backend`, whose
#'   values name what runs instead of recording implementation history:
#'   `"default_c"` is now `"cpp"`, `"default"` is now `"r"`, and
#'   `"gather_compute"` is now `"gather"`. The legacy values are still accepted
#'   and mapped, with one warning naming the new spelling.
#'
#' @return An object of class
#'  `c("algorithm_newton.goldfish", "algorithm.goldfish", "list")`,
#'  where the components values are the default values or the values provided
#'  to the function. The `algorithm.goldfish` superclass is the shared gate
#'  estimators validate against, so every algorithm object passes the same
#'  check. The list object has the following components:
#'   \item{initial_parameters}{Initial parameter values used during
#'      the estimation process.}
#'   \item{fixed_parameters}{Values for parameters fixed during
#'      the estimation process.}
#'   \item{max_iterations}{Maximum number of iterations in the
#'      estimation process.}
#'   \item{score_tol}{Scale-invariant gradient convergence criterion for
#'      the estimation process.}
#'   \item{step_tol}{Damped Newton step size convergence criterion for
#'      the estimation process.}
#'   \item{initial_damping}{Initial damping factor for the
#'      estimation process.}
#'   \item{damping_increase_factor}{Factor by which damping is
#'      increased when improvements in the estimation are found.}
#'   \item{damping_decrease_factor}{Factor by which damping is
#'      decreased when no improvements in the estimation are found.}
#'   \item{return_interval_loglik}{Logical value indicating whether to
#'      return the log-likelihood for each event.}
#'   \item{return_event_scores}{Logical value indicating whether to
#'      return the per-event score matrix.}
#'   \item{diagnostics}{Character vector of the diagnostic primitives to store
#'      on the fitted result.}
#'   \item{optimizer}{Optimization algorithm used in the estimation process.}
#'   \item{backend}{Computational implementation used in the estimation
#'      process, one of `"cpp"`, `"r"` or `"gather"`.}
#' @export
#' @examples
#' est_ctrl <- set_algorithm_newton(
#'   max_iterations = 50,
#'   score_tol = 1e-7,
#'   step_tol = 1e-9
#' )
set_algorithm_newton <- function(
  initial_parameters = NULL,
  fixed_parameters = NULL,
  offset_coef = NULL,
  max_iterations = 20,
  convergence_criterion = deprecated(),
  score_tol = 1e-6,
  step_tol = 1e-8,
  initial_damping = NULL,
  damping_increase_factor = 2,
  damping_decrease_factor = 3,
  return_interval_loglik = deprecated(),
  return_probabilities = deprecated(),
  return_event_scores = deprecated(),
  diagnostics = c("loglik", "scores"),
  optimizer = c("newton_raphson", "bfgs", "bhhh", "nelder_mead"),
  backend = c("cpp", "r", "gather"),
  engine = deprecated()
) {
  backend_supplied <- !missing(backend)
  engine_present <- lifecycle::is_present(engine)
  # `backend` wins when both are supplied, as `fold_renamed_arg()` decides.
  selected <- if (backend_supplied || !engine_present) backend else engine
  backend <- fold_renamed_arg(
    backend,
    backend_supplied,
    engine,
    "set_algorithm_newton",
    "engine",
    "backend",
    details = legacy_backend_note(selected)
  )
  # A legacy value on the new argument is the half-migrated call; it warns on
  # its own only when the argument fold above has not already named it.
  if (!engine_present) {
    warn_legacy_backend_value(backend)
  }
  backend <- resolve_backend(backend)
  optimizer <- match.arg(optimizer)
  diagnostics_supplied <- !missing(diagnostics)
  diagnostics <- resolve_diagnostics(diagnostics)
  resolved <- reconcile_legacy_diagnostics(
    diagnostics,
    diagnostics_supplied,
    return_interval_loglik,
    return_probabilities,
    return_event_scores
  )
  diagnostics <- resolved$diagnostics
  # `diagnostics` is the single source of truth for per-event storage. The three
  # legacy booleans are derived from it (reconcile has already folded any supplied
  # return_* flag into `diagnostics`), so the default `c("loglik", "scores")`
  # stores the per-event scores.
  return_interval_loglik <- "loglik" %in% diagnostics
  return_probabilities <- "probabilities" %in% diagnostics
  return_event_scores <- "scores" %in% diagnostics

  # Emit the soft-deprecation from this frame so lifecycle attributes it to the
  # direct caller (a nested helper frame reads as an internal, silent call).
  for (flag_name in resolved$deprecated) {
    primitive <- LEGACY_DIAGNOSTIC_FLAGS[[flag_name]]
    lifecycle::deprecate_soft(
      when = "1.9.11",
      what = paste0("set_algorithm_newton(", flag_name, ")"),
      with = "set_algorithm_newton(diagnostics)",
      details = c(
        "i" = paste0(
          "Request the ",
          encodeString(primitive, quote = "\""),
          " primitive via diagnostics = ",
          encodeString(primitive, quote = "\""),
          "."
        )
      )
    )
  }

  if (lifecycle::is_present(convergence_criterion)) {
    lifecycle::deprecate_warn(
      when = "1.7.2",
      what = "set_algorithm_newton(convergence_criterion)",
      details = paste0(
        "`convergence_criterion` is ignored; ",
        "please use `score_tol` instead (default: 1e-6)."
      )
    )
  }

  # Argument checks
  if (!is.null(initial_parameters) && !is.numeric(initial_parameters)) {
    stop(
      "'initial_parameters' must be a numeric vector or NULL.",
      call. = FALSE
    )
  }
  if (!is.null(fixed_parameters)) {
    if (!is.numeric(fixed_parameters)) {
      stop(
        "'fixed_parameters' must be a numeric vector or NULL.",
        call. = FALSE
      )
    }
    lifecycle::deprecate_soft(
      when = "1.8.4",
      what = "set_algorithm_newton(fixed_parameters)",
      details = c(
        "!" = "Wrap the term in `offset()` in the model formula and supply its
               value through `offset_coef` instead.",
        "i" = "`offset()` aligns fixed values to terms by name rather than by
               counting coefficient positions."
      )
    )
  }
  if (!is.null(offset_coef) && !is.numeric(offset_coef)) {
    stop("'offset_coef' must be a numeric vector or NULL.", call. = FALSE)
  }
  if (!is.null(fixed_parameters) && !is.null(offset_coef)) {
    cli::cli_abort(c(
      "{.arg fixed_parameters} and {.arg offset_coef} cannot both be supplied.",
      "i" = "Use {.arg offset_coef} with {.fn offset} terms in the formula
             ({.arg fixed_parameters} is superseded)."
    ))
  }
  if (
    !rlang::is_scalar_integerish(max_iterations, finite = TRUE) ||
      max_iterations < 0
  ) {
    stop(
      "'max_iterations' must be a single non-negative integer.",
      call. = FALSE
    )
  }
  if (!rlang::is_scalar_double(score_tol) || score_tol <= 0) {
    stop("'score_tol' must be a single positive numeric value.", call. = FALSE)
  }
  if (!rlang::is_scalar_double(step_tol) || step_tol <= 0) {
    stop("'step_tol' must be a single positive numeric value.", call. = FALSE)
  }
  if (
    !is.null(initial_damping) &&
      (!is.numeric(initial_damping) ||
        length(initial_damping) != 1 ||
        initial_damping <= 0)
  ) {
    stop(
      "'initial_damping' must be a single positive numeric value or NULL.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(damping_increase_factor) ||
      length(damping_increase_factor) != 1 ||
      damping_increase_factor < 1
  ) {
    stop(
      "'damping_increase_factor' must be a single numeric value >= 1.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(damping_decrease_factor) ||
      length(damping_decrease_factor) != 1 ||
      damping_decrease_factor < 1
  ) {
    stop(
      "'damping_decrease_factor' must be a single numeric value >= 1.",
      call. = FALSE
    )
  }
  control_list <- list(
    initial_parameters = initial_parameters,
    fixed_parameters = fixed_parameters,
    offset_coef = offset_coef,
    max_iterations = max_iterations,
    score_tol = score_tol,
    step_tol = step_tol,
    initial_damping = initial_damping,
    damping_increase_factor = damping_increase_factor,
    damping_decrease_factor = damping_decrease_factor,
    return_interval_loglik = return_interval_loglik,
    return_probabilities = return_probabilities,
    return_event_scores = return_event_scores,
    diagnostics = diagnostics,
    optimizer = optimizer,
    backend = backend
  )

  class(control_list) <- c(
    "algorithm_newton.goldfish",
    "algorithm.goldfish",
    "list"
  )
  return(control_list)
}

# The backend vocabulary, paired with the pre-2.0.0 engine token each value
# replaced. Nothing downstream reads a token any more: the pair survives as the
# input-side compatibility map (the `engine =` sentinel, its legacy values, and
# the read shim for control objects built before 2.0.0), plus the key the frozen
# coefficient baselines were written under.
BACKEND_ENGINE_TOKENS <- c(
  cpp = "default_c",
  r = "default",
  gather = "gather_compute"
)
BACKEND_VALUES <- names(BACKEND_ENGINE_TOKENS)
# The inverse map: the pre-2.0.0 `engine` values, keyed by token, valued by the
# backend that replaced each one.
LEGACY_ENGINE_BACKENDS <- stats::setNames(
  BACKEND_VALUES,
  BACKEND_ENGINE_TOKENS
)

# The backend an algorithm-control object selects. A control list built before
# 2.0.0 carries only the legacy `engine` token (`set_estimation_opt()` since
# 1.7.0, or `set_algorithm_newton()` in the 1.9 line) — restored from an .rds or
# built once in a long-lived script — so its token resolves here on read.
# Deliberately silent: the deprecated surface already warned when the object was
# constructed, and warning again at estimation time would charge a serialized
# object twice for one mistake.
algo_backend <- function(control_algo) {
  if (!is.null(control_algo$backend)) {
    return(control_algo$backend)
  }
  unname(LEGACY_ENGINE_BACKENDS[[control_algo$engine]])
}

# Translate a pre-2.0.0 `engine` value to its backend spelling, leaving anything
# else (including the untouched default vector) for `resolve_backend()` to
# judge.
map_legacy_backend <- function(value) {
  if (length(value) == 1L && value %in% names(LEGACY_ENGINE_BACKENDS)) {
    return(unname(LEGACY_ENGINE_BACKENDS[[value]]))
  }
  value
}

# Settle `backend` to one of the three values: the untouched default vector
# resolves to its first element, a legacy value maps, anything else aborts
# naming the vocabulary the user should pick from.
resolve_backend <- function(backend, call = rlang::caller_env()) {
  if (identical(backend, BACKEND_VALUES)) {
    return(BACKEND_VALUES[[1L]])
  }
  backend <- map_legacy_backend(backend)
  if (!rlang::is_string(backend) || !backend %in% BACKEND_VALUES) {
    cli::cli_abort(
      c(
        "{.arg backend} must be one of {.val {BACKEND_VALUES}}.",
        "x" = "You supplied {.val {backend}}."
      ),
      call = call
    )
  }
  backend
}

# Warn for a pre-2.0.0 value supplied to `backend` itself. `user_env` reaches
# past this helper and `set_algorithm_newton()` so lifecycle attributes the
# warning to the user's own call.
warn_legacy_backend_value <- function(value, user_env = rlang::caller_env(2)) {
  if (length(value) != 1L || !value %in% names(LEGACY_ENGINE_BACKENDS)) {
    return(invisible())
  }
  # `I()` because lifecycle's `fn(arg = "...")` spec reads what follows the `=`
  # as a reason, not as the deprecated value.
  lifecycle::deprecate_soft(
    when = "2.0.0",
    what = I(paste0(
      "The `set_algorithm_newton()` backend value ",
      encodeString(value, quote = "\"")
    )),
    with = I(encodeString(LEGACY_ENGINE_BACKENDS[[value]], quote = "\"")),
    user_env = user_env
  )
}

# The bullet that carries the value half of the rename, so a call using both the
# old argument and an old value gets one warning naming the final spelling
# rather than being pointed at `backend = "default_c"`, itself deprecated.
legacy_backend_note <- function(value) {
  if (
    !lifecycle::is_present(value) ||
      length(value) != 1L ||
      !value %in% names(LEGACY_ENGINE_BACKENDS)
  ) {
    return(NULL)
  }
  c(
    "i" = paste0(
      "The value ",
      encodeString(value, quote = "\""),
      " is now ",
      encodeString(LEGACY_ENGINE_BACKENDS[[value]], quote = "\""),
      "."
    )
  )
}

# The per-event diagnostic primitives estimation can store, in the order the
# `diagnostics =` help and error messages list them.
DIAGNOSTIC_PRIMITIVES <- c(
  "loglik",
  "scores",
  "ranks",
  "margins",
  "probabilities"
)

# Which backends produce which primitive: the one source of truth, consulted
# once before any preprocessing. Every cell is currently supported, because each
# primitive is a reduction of the same per-event weight vector and so is
# computable wherever that vector is formed.
#
# The table stays even so. It replaced three different reactions to the same
# class of problem -- an abort for one primitive, a silent drop for another, an
# unmarked absence for two more, plus a backend substitution that made the
# verdict depend on what else was requested alongside. Encoding availability as
# data means the next primitive that is not universal on day one gets the one
# existing failure mode rather than a fourth invented one.
DIAGNOSTIC_BACKEND_SUPPORT <- list(
  loglik = BACKEND_VALUES,
  scores = BACKEND_VALUES,
  ranks = BACKEND_VALUES,
  margins = BACKEND_VALUES,
  probabilities = BACKEND_VALUES
)

# Abort if the chosen backend cannot produce a requested primitive, naming the
# backends that can. Nothing is dropped, downgraded, or rerouted: the caller
# chose a backend, and silently honoring the request somewhere else is what the
# `probabilities` redirect used to do.
check_diagnostic_support <- function(
  diagnostics,
  backend,
  support = DIAGNOSTIC_BACKEND_SUPPORT,
  call = rlang::caller_env()
) {
  supported <- vapply(
    diagnostics,
    function(primitive) {
      backends <- support[[primitive]]
      is.null(backends) || backend %in% backends
    },
    logical(1)
  )
  if (all(supported)) {
    return(invisible(NULL))
  }
  # Report the first unsupported primitive in the vocabulary's own order, so the
  # verdict does not depend on the order the user happened to request them in.
  unsupported <- diagnostics[!supported]
  primitive <- intersect(DIAGNOSTIC_PRIMITIVES, unsupported)[1]
  # Pre-format one code span per backend: interpolating the vector inside a
  # single `{.code}` collapses it into one span reading `backend = "cpp" or "r"`,
  # which is not a call anyone can copy.
  alternatives <- paste0(
    "backend = ",
    encodeString(support[[primitive]], quote = "\"")
  )
  cli::cli_abort(
    c(
      "The {.val {primitive}} diagnostic is not available with
       {.code backend = {.val {backend}}}.",
      "i" = "Use {.or {.code {alternatives}}} to store it."
    ),
    call = call
  )
}

# Resolve the user-facing `diagnostics =` value (TRUE/FALSE/"all"/name vector)
# to the canonical character vector of primitive names stored in the options.
resolve_diagnostics <- function(diagnostics, call = rlang::caller_env()) {
  if (is.logical(diagnostics)) {
    if (!rlang::is_scalar_logical(diagnostics) || is.na(diagnostics)) {
      cli::cli_abort(
        "{.arg diagnostics} must be a single {.code TRUE} or {.code FALSE}, or a
         character vector of primitive names.",
        call = call
      )
    }
    return(if (diagnostics) c("loglik", "scores") else character(0))
  }
  if (is.null(diagnostics) || length(diagnostics) == 0) {
    return(character(0))
  }
  if (!is.character(diagnostics)) {
    cli::cli_abort(
      c(
        "{.arg diagnostics} must be a character vector, {.code TRUE},
         {.code FALSE}, or {.val all}.",
        "x" = "You supplied a {.cls {class(diagnostics)}} vector."
      ),
      call = call
    )
  }
  if ("all" %in% diagnostics) {
    return(DIAGNOSTIC_PRIMITIVES)
  }
  diagnostics <- unique(diagnostics)
  unknown <- setdiff(diagnostics, DIAGNOSTIC_PRIMITIVES)
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(
        "Unknown {.arg diagnostics} primitive{?s} {.val {unknown}}.",
        "i" = "Valid primitives are {.val {DIAGNOSTIC_PRIMITIVES}}, or one of
               {.code TRUE} / {.code FALSE} / {.val all}."
      ),
      call = call
    )
  }
  diagnostics
}

# The one-to-one mapping from each deprecated return_* flag to the diagnostics
# primitive it stores; the third element is the flag's historical default.
LEGACY_DIAGNOSTIC_FLAGS <- c(
  return_interval_loglik = "loglik",
  return_probabilities = "probabilities",
  return_event_scores = "scores"
)
LEGACY_DIAGNOSTIC_DEFAULTS <- c(
  return_interval_loglik = TRUE,
  return_probabilities = FALSE,
  return_event_scores = FALSE
)

# Reconcile the deprecated return_* flags with the `diagnostics` vector.
# Each flag drives exactly one primitive; a flag left unset falls back to its
# historical default so the returned booleans preserve the pre-deprecation
# storage behavior downstream. When any flag is supplied the diagnostics vector
# is rebuilt from those booleans so the two surfaces agree. Supplying any flag
# together with an explicit `diagnostics` aborts, since the two surfaces would
# then both drive the same storage. The soft-deprecation itself is emitted by
# the caller so lifecycle attributes it to the direct user.
reconcile_legacy_diagnostics <- function(
  diagnostics,
  diagnostics_supplied,
  return_interval_loglik,
  return_probabilities,
  return_event_scores,
  call = rlang::caller_env()
) {
  values <- list(
    return_interval_loglik = return_interval_loglik,
    return_probabilities = return_probabilities,
    return_event_scores = return_event_scores
  )
  present <- vapply(values, lifecycle::is_present, logical(1))

  if (any(present) && diagnostics_supplied) {
    cli::cli_abort(
      c(
        "Cannot supply {.arg diagnostics} together with the deprecated
         {.arg {names(present)[present]}} flag{?s}.",
        "i" = "Use {.arg diagnostics} alone; it supersedes the {.code return_*}
               flags."
      ),
      call = call
    )
  }

  flags <- vapply(
    names(values),
    function(name) {
      flag <- if (present[[name]]) {
        values[[name]]
      } else {
        LEGACY_DIAGNOSTIC_DEFAULTS[[name]]
      }
      if (!rlang::is_scalar_logical(flag) || is.na(flag)) {
        cli::cli_abort(
          "{.arg {name}} must be a single {.code TRUE} or {.code FALSE}.",
          call = call
        )
      }
      flag
    },
    logical(1)
  )

  # When a legacy flag was used, the diagnostics vector mirrors the flag view so
  # the two surfaces agree; otherwise the resolved `diagnostics` stands.
  if (any(present)) {
    diagnostics <- unname(LEGACY_DIAGNOSTIC_FLAGS[names(flags)[flags]])
  }

  list(
    diagnostics = diagnostics,
    deprecated = names(present)[present],
    return_interval_loglik = flags[["return_interval_loglik"]],
    return_probabilities = flags[["return_probabilities"]],
    return_event_scores = flags[["return_event_scores"]]
  )
}

#' Control Parameters for Preprocessing
#'
#' Specifies control parameters for the data preprocessing stage,
#' used by `estimate_dynam()`, `estimate_rem()` and `estimate_dynami()`
#' (when `preprocessed` is not a
#' `preprocessed.goldfish` object) and `gather_model_data()`.
#'
#' @param start_time A numerical value or a date-time character string
#'   (parsable by `as.POSIXct`) indicating the starting time when the events
#'   are considered for likelihood computation.
#'   All the events that happen before the `start_time` are used to compute
#'   the initial values of the effects statistics in the model.
#'   It's useful to set this parameter when the model has windowed effects or
#'   effects that depends of previous order of events (e.g., `trans()` and
#'   `cycle()` when `history` argument is set to sequential or consecutive,
#'   as they are initialized with empty values.
#'   Default is `NULL` (start from the first event).
#' @param end_time A numerical value or a date-time character string
#'   (parsable by `as.POSIXct`) indicating the end time when the events
#'   are not to be considered for likelihood computation.
#'   The preprocessing stage won't stop at this time and will continue
#'   processing events after this time.
#'   Default is `NULL` (end with the last event).
#' @param opportunities_list `r lifecycle::badge("deprecated")` A list object.
#'   For choice models, this list specifies, for each dependent event,
#'   the set of available nodes in the choice set.
#'   The list should have the same length as the number of events in the
#'   dependent events objects created with `make_dependent_events()`.
#'   Default is `NULL`, so the choice set is the set of all nodes present at the
#'   time of the event. Superseded by the `support_constraint` argument of
#'   [estimate_dynam()] / [make_specification()], which generalizes it to a
#'   per-`(sender, receiver)` risk-set restriction and works on every engine; an
#'   equivalent constraint over an allowed-dyad network reproduces the
#'   opportunity-list coefficients.
#' @param impute An optional named character vector declaring a per-attribute
#'   imputation policy, keyed by nodal attribute name (e.g.
#'   `impute = c(party = "as_category")`). Every attribute not named uses the
#'   default summary contract documented in the *Missing data* section of
#'   [goldfish_data], so omitting `impute` leaves behavior unchanged. Supported
#'   values: `"summary"` (the default rule) and `"as_category"` (recode a
#'   factor or character attribute's missing values to a reserved `"(missing)"`
#'   level, so missingness by design survives to the summarizers). `"locf"` is
#'   reserved for a future estimator and currently aborts as unimplemented.
#'   Default is `NULL`.
#' @param db A `DBIConnection` object or `NULL` (default). When supplied
#'   together with `compute_statistics(..., output = "db")`, the gather statistics
#'   are streamed to the database table named by `db_table` instead of being
#'   held in memory.
#' @param db_table A single character string naming the database table to
#'   write to when a `db` connection is configured. Default is `"stats"`.
# @param keep_sender_index A logical value. If `TRUE`, the sender index,
#   the index in the nodeset, of the potential senders of the events is
#   kept in the preprocessed data.
# @param keep_receiver_index A logical value. If `TRUE`, the receiver index,
#  the index in the nodeset, of the potential receivers of the events is
#  kept in the preprocessed data.
#' @return An object of class `preprocessing.goldfish` (a list object), with
#'  where the components values are the default values or the values provided
#'  to the function. The list object has the following components:
#'   \item{start_time}{Value from `start_time` argument.}
#'   \item{end_time}{Value from `end_time` argument.}
#'   \item{opportunities_list}{Value from `opportunities_list` argument.}
#'   \item{impute}{Value from `impute` argument.}
#' @export
#' @examples
#' prep_ctrl <- set_preprocessing(
#'   start_time = "2000-01-01 00:00:00",
#'   end_time = "2000-12-31 23:59:59"
#' )
set_preprocessing <- function(
  start_time = NULL,
  end_time = NULL,
  opportunities_list = NULL,
  impute = NULL,
  db = NULL,
  db_table = "stats"
) {
  # Argument checks
  classesAllowed <- c("numeric", "character", "POSIXlt", "POSIXct", "POSIXt")
  if (!is.null(start_time)) {
    if (length(start_time) != 1) {
      stop("'start_time' must be NULL or a single value.", call. = FALSE)
    }
    if (!any(check_classes(start_time, classesAllowed))) {
      stop(
        "'start_time' must be NULL or an object of class: ",
        paste(classesAllowed, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }
  if (!is.null(end_time)) {
    if (length(end_time) != 1) {
      stop("'end_time' must be NULL or a single value.", call. = FALSE)
    }
    if (!any(check_classes(end_time, classesAllowed))) {
      stop(
        "'end_time' must be NULL or an object of class: ",
        paste(classesAllowed, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }
  if (!is.null(opportunities_list)) {
    lifecycle::deprecate_warn(
      when = "1.8.6",
      what = "set_preprocessing(opportunities_list)",
      details = c(
        i = paste(
          "Use the `support_constraint` argument of `estimate_dynam()` /",
          "`make_specification()` instead."
        )
      )
    )
    if (!is.list(opportunities_list)) {
      stop(
        "'opportunities_list' must be a list or NULL.",
        call. = FALSE
      )
    }
    # check every element is a integer or character vector
    is_vector <- vapply(
      opportunities_list,
      \(x) any(check_classes(x, c("integer", "character"))),
      logical(1)
    )
    if (!all(is_vector)) {
      stop(
        "'opportunities_list' must be a list of integer or character vectors.",
        call. = FALSE
      )
    }
  }

  # if (!rlang::is_scalar_logical(keep_sender_index)) {
  #   stop(
  #     "'keep_sender_index' must be a single logical value.",
  #     call. = FALSE
  #   )
  # }
  #
  # if (!rlang::is_scalar_logical(keep_receiver_index)) {
  #   stop(
  #     "'keep_receiver_index' must be a single logical value.",
  #     call. = FALSE
  #   )
  # }

  if (!is.null(impute)) {
    if (
      !is.character(impute) ||
        is.null(names(impute)) ||
        any(names(impute) == "" | is.na(names(impute)))
    ) {
      cli::cli_abort(c(
        "{.arg impute} must be {.code NULL} or a named character vector.",
        "i" = "Key each entry by the attribute name, e.g.
               {.code impute = c(party = \"as_category\")}."
      ))
    }
    reserved <- impute %in% IMPUTATION_POLICY_RESERVED
    if (any(reserved)) {
      cli::cli_abort(c(
        "Imputation policy {.val {unique(impute[reserved])}} is reserved but
         not yet implemented.",
        "i" = "Supported values are
               {.val {IMPUTATION_POLICY_SUPPORTED}}."
      ))
    }
    unknown <- !impute %in% IMPUTATION_POLICY_SUPPORTED
    if (any(unknown)) {
      cli::cli_abort(c(
        "Unknown imputation policy {.val {unique(impute[unknown])}}.",
        "i" = "Supported values are
               {.val {IMPUTATION_POLICY_SUPPORTED}}."
      ))
    }
  }

  if (!is.null(db) && !inherits(db, "DBIConnection")) {
    stop("'db' must be NULL or a DBI connection object.", call. = FALSE)
  }
  if (!is.character(db_table) || length(db_table) != 1L) {
    stop("'db_table' must be a single character string.", call. = FALSE)
  }

  control_list <- list(
    start_time = start_time,
    end_time = end_time,
    opportunities_list = opportunities_list,
    impute = impute,
    db = db,
    db_table = db_table
  )

  class(control_list) <- c("preprocessing.goldfish", "list")
  return(control_list)
}
