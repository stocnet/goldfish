#' Control Parameters for the Sequence Augmenter
#'
#' Specifies how the ascent-based Monte Carlo EM (ABMCEM) loop driven by
#' `[estimate_dynes]` augments the pool of endpoint-hitting event sequences.
#' Nested under `[set_algorithm_em]`'s `augmenter` argument; validated here
#' against its own arguments only, the cross-object rules (the augmenter
#' \eqn{\times} weighting validity matrix, the cold-start construction guard)
#' being enforced by `set_algorithm_em()`.
#'
#' @param routine A character string naming the augmentation routine,
#'   matching the `augment_seq_*()` constructor it drives. One of `"mcmc"`
#'   (default), `"random"`, or `"sim"`.
#' @param burn_in,thinning Single non-negative integers counting sweeps (one
#'   sweep = as many proposals as there are panel events), so the same
#'   setting means the same thing at every dataset size. MCMC-only. Default
#'   `NULL` on both: the tuned default is a `dynes-augmentation` Phase-1
#'   tuning outcome not yet pinned; the arguments ship now so the signature
#'   does not churn when that study lands, and `NULL` leaves the choice to
#'   the `"mcmc"` routine itself. `thinning`, when supplied, must be at
#'   least `1L`.
#' @param initialize A character string picking the first chain's starting
#'   draw: `"random"` (default) via `augment_seq_random()`, or `"sim"` via a
#'   constrained-simulation draw, typically nearer the target and so
#'   shortening the unavoidable burn-in.
#' @param initial_sequence An optional user-supplied starting sequence,
#'   overriding `initialize`. Its shape is validated by the augmenter against
#'   the specification (endpoint-hitting, chain order, within-wave times) —
#'   this constructor only carries it through. Default `NULL`.
#' @param warm_start A single logical. When `TRUE`, before the first EM
#'   iteration the loop draws one augmentation and runs, per fid, an
#'   independent internal `[set_algorithm_newton]` fit at its defaults on
#'   that fid's drawn sequence, concatenating the estimates into
#'   \eqn{\theta_0} instead of `set_algorithm_em()`'s `initial_parameters`.
#'   Warn-and-ignored when `initial_parameters` is also supplied (the
#'   explicit start wins). Default `FALSE`.
#' @param move_probs A named numeric vector giving the MCMC move-type mix
#'   (`"mcmc"` only), normalized internally so only the relative weights
#'   matter. Names must be drawn from `"permute"` and `"shift"`. Default
#'   `c(permute = 0.5, shift = 0.5)`.
#'
#' @return An object of class `c("goldfishAugmenterCtrl", "list")` with one
#'   component per argument above.
#' @export
#' @examples
#' aug_ctrl <- set_augmenter_options(
#'   routine = "mcmc",
#'   burn_in = 50,
#'   thinning = 5
#' )
set_augmenter_options <- function(
  routine = c("mcmc", "random", "sim"),
  burn_in = NULL,
  thinning = NULL,
  initialize = c("random", "sim"),
  initial_sequence = NULL,
  warm_start = FALSE,
  move_probs = c(permute = 0.5, shift = 0.5)
) {
  routine <- rlang::arg_match(routine)
  initialize <- rlang::arg_match(initialize)

  if (!is.null(burn_in)) {
    if (!rlang::is_scalar_integerish(burn_in, finite = TRUE) || burn_in < 0) {
      cli::cli_abort(
        "{.arg burn_in} must be {.code NULL} or a single non-negative integer."
      )
    }
    burn_in <- as.integer(burn_in)
  }
  if (!is.null(thinning)) {
    if (!rlang::is_scalar_integerish(thinning, finite = TRUE) || thinning < 1) {
      cli::cli_abort(
        "{.arg thinning} must be {.code NULL} or a single positive integer."
      )
    }
    thinning <- as.integer(thinning)
  }
  if (!rlang::is_scalar_logical(warm_start) || is.na(warm_start)) {
    cli::cli_abort(
      "{.arg warm_start} must be a single {.code TRUE}/{.code FALSE}."
    )
  }
  move_probs <- check_move_probs(move_probs)

  control_list <- list(
    routine = routine,
    burn_in = burn_in,
    thinning = thinning,
    initialize = initialize,
    initial_sequence = initial_sequence,
    warm_start = warm_start,
    move_probs = move_probs
  )

  class(control_list) <- c("goldfishAugmenterCtrl", "list")
  control_list
}

# `move_probs` must be a named numeric vector over the known move types
# ("permute", "shift"), all entries non-negative, with at least one positive
# so normalizing is well-defined; it is then normalized to sum to 1 (design
# D2: "MCMC-only... normalized internally").
check_move_probs <- function(move_probs, call = rlang::caller_env()) {
  valid_moves <- c("permute", "shift")
  if (
    !is.numeric(move_probs) ||
      is.null(names(move_probs)) ||
      any(names(move_probs) == "" | is.na(names(move_probs)))
  ) {
    cli::cli_abort(
      c(
        "{.arg move_probs} must be a named numeric vector.",
        "i" = "Name each entry from {.val {valid_moves}}, e.g.
               {.code c(permute = 0.5, shift = 0.5)}."
      ),
      call = call
    )
  }
  unknown <- setdiff(names(move_probs), valid_moves)
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(
        "Unknown {.arg move_probs} move type{?s} {.val {unknown}}.",
        "i" = "Valid move types are {.val {valid_moves}}."
      ),
      call = call
    )
  }
  if (any(move_probs < 0) || sum(move_probs) <= 0) {
    cli::cli_abort(
      "{.arg move_probs} entries must be non-negative and sum to a positive
       value.",
      call = call
    )
  }
  move_probs / sum(move_probs)
}

#' Control Parameters for the E-step Weighting Scheme
#'
#' Specifies how the ascent-based Monte Carlo EM (ABMCEM) loop driven by
#' `[estimate_dynes]` weights and resamples the pooled sequences at the
#' E-step. Nested under `[set_algorithm_em]`'s `weights` argument; validated
#' here against its own arguments only, the cross-object augmenter
#' \eqn{\times} weighting validity matrix being enforced by
#' `set_algorithm_em()`.
#'
#' @param weighting A character string: `"importance"` (default) reweights
#'   each pooled sequence by the likelihood ratio against its own reference
#'   record (the Casella & Levine 2001 Monte-Carlo-EM sample-reuse update);
#'   `"uniform"` treats every sequence as equally weighted, valid only when
#'   the pool is drawn exactly from the model's conditional distribution at
#'   the current parameters.
#' @param use A character string naming what the normalized weights feed:
#'   `"importance"` (default, weighted expectations directly) or
#'   `"resampling"` (a resampled unweighted multiset, layered on top of the
#'   importance weights).
#' @param resampling_scheme A character string naming the resampling scheme
#'   used when `use = "resampling"`: `"stratified"` (default), `"residual"`,
#'   or `"random"`.
#' @param transformation A character string naming the pre-normalization
#'   transformation applied to the raw log-weights: `"identity"` (default),
#'   `"clipping"`, or `"smoothing"`. A named enum rather than a user-supplied
#'   function, so the choice is validatable and snapshot-testable at
#'   construction.
#' @param refresh A single logical. When `TRUE`, the whole pool is redrawn
#'   every EM iteration at its grown size and cross-iteration reweighting,
#'   the ESS guard, and staleness transformations become inapplicable.
#'   Default `FALSE` (a persistent, reweighted pool).
#' @param ess_threshold A single numeric value in `(0, 1]`. With a persistent
#'   pool (`refresh = FALSE`), a full redraw fires when the effective sample
#'   size falls below `ess_threshold` times the pool size. Default `0.5`.
#'
#' @return An object of class `c("goldfishWeightsCtrl", "list")` with one
#'   component per argument above.
#' @export
#' @examples
#' weights_ctrl <- set_weights_options(
#'   weighting = "importance",
#'   use = "resampling"
#' )
set_weights_options <- function(
  weighting = c("importance", "uniform"),
  use = c("importance", "resampling"),
  resampling_scheme = c("stratified", "residual", "random"),
  transformation = c("identity", "clipping", "smoothing"),
  refresh = FALSE,
  ess_threshold = 0.5
) {
  weighting <- rlang::arg_match(weighting)
  use <- rlang::arg_match(use)
  resampling_scheme <- rlang::arg_match(resampling_scheme)
  transformation <- rlang::arg_match(transformation)

  if (!rlang::is_scalar_logical(refresh) || is.na(refresh)) {
    cli::cli_abort(
      "{.arg refresh} must be a single {.code TRUE}/{.code FALSE}."
    )
  }
  if (
    !rlang::is_scalar_double(ess_threshold) ||
      is.na(ess_threshold) ||
      ess_threshold <= 0 ||
      ess_threshold > 1
  ) {
    cli::cli_abort(
      "{.arg ess_threshold} must be a single numeric value in (0, 1]."
    )
  }

  control_list <- list(
    weighting = weighting,
    use = use,
    resampling_scheme = resampling_scheme,
    transformation = transformation,
    refresh = refresh,
    ess_threshold = ess_threshold
  )

  class(control_list) <- c("goldfishWeightsCtrl", "list")
  control_list
}

#' Control Parameters for the SGD M-step Optimizer
#'
#' Specifies the stochastic-gradient M-step the ascent-based Monte Carlo EM
#' (ABMCEM) loop driven by `[estimate_dynes]` uses to update the concatenated
#' parameter vector. Nested under `[set_algorithm_em]`'s `optimizer`
#' argument; validated here against its own arguments only.
#'
#' @param variant A character string: `"minibatch"` (default) draws a batch
#'   per update; `"full"` uses the whole pool (or resampled multiset) every
#'   update.
#' @param batch_size A single positive integer, or `NULL` (default). The
#'   batch size is a fixed absolute count held constant across EM iterations
#'   even as the pool grows. An unset `NULL` is resolved once by
#'   `[set_algorithm_em]` — the only object that knows `n_sequences` — to
#'   `min(n_sequences, 10)` and then frozen for the run.
#' @param batch_scheme A character string: `"weighted"` (default) draws
#'   batch members with replacement proportional to the normalized weights
#'   and applies the unweighted batch mean; `"cyclic"` rotates fixed batches
#'   deterministically and importance-weights the within-batch gradient.
#' @param step_size A single positive numeric value, the base SGD step size
#'   (\eqn{\eta_0} under `step_schedule = "decay"`). Default `0.001`.
#' @param step_schedule A character string naming the step-size schedule:
#'   `"constant"` (default), `"decay"` (the Bottou schedule
#'   \eqn{\eta_0 / (1 + \eta_0 t)}), `"adagrad"`, `"adam"`, or `"momentum"`
#'   (the last three at fixed literature-default hyperparameters).
#' @param max_iterations A single positive integer, the M-step's own
#'   iteration budget (distinct from `[set_algorithm_em]`'s outer
#'   `max_iterations`). Default `100L`.
#' @param sgd_tolerance A single positive numeric value, the M-step's
#'   gradient-norm convergence tolerance. Named distinctly from
#'   `set_algorithm_em()`'s EM-stop `tolerance` — a different scale
#'   (gradient-norm vs Q-scale), so a user setting one never silently sets
#'   the other. Default `1e-3`.
#' @param convergence A character string selecting the M-step stopping mode:
#'   `"gradient"` (default) tests the gradient norm against `sgd_tolerance`;
#'   `"iterations"` runs to `max_iterations` unconditionally, with
#'   `sgd_tolerance` unused.
#'
#' @return An object of class `c("goldfishSgdCtrl", "list")` with one
#'   component per argument above.
#' @export
#' @examples
#' sgd_ctrl <- set_sgd_options(step_size = 0.01, step_schedule = "decay")
set_sgd_options <- function(
  variant = c("minibatch", "full"),
  batch_size = NULL,
  batch_scheme = c("weighted", "cyclic"),
  step_size = 0.001,
  step_schedule = c("constant", "decay", "adagrad", "adam", "momentum"),
  max_iterations = 100L,
  sgd_tolerance = 1e-3,
  convergence = c("gradient", "iterations")
) {
  variant <- rlang::arg_match(variant)
  batch_scheme <- rlang::arg_match(batch_scheme)
  step_schedule <- rlang::arg_match(step_schedule)
  convergence <- rlang::arg_match(convergence)

  if (!is.null(batch_size)) {
    if (
      !rlang::is_scalar_integerish(batch_size, finite = TRUE) ||
        batch_size < 1
    ) {
      cli::cli_abort(
        "{.arg batch_size} must be {.code NULL} or a single positive integer."
      )
    }
    batch_size <- as.integer(batch_size)
  }
  if (
    !rlang::is_scalar_double(step_size) || is.na(step_size) || step_size <= 0
  ) {
    cli::cli_abort("{.arg step_size} must be a single positive numeric value.")
  }
  if (
    !rlang::is_scalar_integerish(max_iterations, finite = TRUE) ||
      max_iterations < 1
  ) {
    cli::cli_abort(
      "{.arg max_iterations} must be a single positive integer."
    )
  }
  max_iterations <- as.integer(max_iterations)
  if (
    !rlang::is_scalar_double(sgd_tolerance) ||
      is.na(sgd_tolerance) ||
      sgd_tolerance <= 0
  ) {
    cli::cli_abort(
      "{.arg sgd_tolerance} must be a single positive numeric value."
    )
  }

  control_list <- list(
    variant = variant,
    batch_size = batch_size,
    batch_scheme = batch_scheme,
    step_size = step_size,
    step_schedule = step_schedule,
    max_iterations = max_iterations,
    sgd_tolerance = sgd_tolerance,
    convergence = convergence
  )

  class(control_list) <- c("goldfishSgdCtrl", "list")
  control_list
}
