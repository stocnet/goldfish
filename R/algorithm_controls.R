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

#' Control Parameters for the Ascent-based Monte Carlo EM Algorithm
#'
#' Specifies the ascent-based Monte Carlo EM (ABMCEM) loop `[estimate_dynes]`
#' runs, nesting one option bundle per algorithm concern:
#' `[set_augmenter_options]` (how endpoint-hitting sequences are drawn),
#' `[set_weights_options]` (how the pool is weighted and resampled at the
#' E-step), and `[set_sgd_options]` (the stochastic-gradient M-step). This
#' constructor validates its own arguments and the nested controls' classes,
#' then enforces the **cross-object rules** the child constructors cannot see on
#' their own: the augmenter \eqn{\times} weighting validity matrix, the
#' refresh-inapplicability and transformation precedence, and the
#' warm-start precedence. Inconsistent-but-ignorable combinations warn and are
#' corrected; impossible ones (a nested control of the wrong class, a
#' non-`goldfishParams` starting point) abort.
#'
#' @param n_sequences A single positive integer, the **initial** pool size (the
#'   loop's \eqn{m_0} / K — the number of endpoint-hitting sequences the first
#'   iteration draws). Default `100L`.
#' @param max_iterations A single positive integer, the outer EM iteration
#'   budget. Exhausting it returns the last accepted parameters with a
#'   non-convergence warning and `converged = FALSE`, never an abort. Default
#'   `50L`.
#' @param accept_quantile,growth_quantile,stop_quantile Single numeric values
#'   in \eqn{(0, 1)}, the tail probabilities of the ascent stop rule — the
#'   paper's \eqn{\alpha} (accept), \eqn{\beta} (growth), and \eqn{\gamma}
#'   (stop). A pass accepts when the ascent lower bound clears zero, sizes the
#'   next pool from the growth quantile, and stops when the upper bound falls
#'   within `tolerance`. Defaults `0.2`, `0.2`, `0.1`.
#' @param tolerance A single positive numeric value, the EM-stop threshold on
#'   the Q-scale. Named distinctly from `[set_sgd_options]`'s `sgd_tolerance`
#'   (the M-step gradient-norm scale) so the two are never set by the same
#'   argument. Default `1e-3`.
#' @param stop_count A single positive integer, the number of **consecutive**
#'   stop-rule satisfactions required to terminate (checked once per decision
#'   pass; any miss resets the streak). Default `1L`.
#' @param max_retries A single positive integer, the within-iteration
#'   pool-growth retry budget. Exhausting it aborts the run (the ascent cannot
#'   be certified at that iteration), carrying the accumulated trace as
#'   condition data. Default `20L`.
#' @param max_pool A single positive integer, the ceiling clamping the
#'   post-acceptance pool-sizing rule (whose \eqn{Q^2} denominator can request
#'   an astronomical pool near the optimum). Default `5000L`.
#' @param seed A single integer, or `NULL` (default). The **only** seed: it
#'   seeds one L'Ecuyer-CMRG root from which augmentation draws, batch
#'   selection, and resampling all take their streams. `NULL` draws a seed from
#'   the session's RNG and records it on the result. At a fixed seed the drawn
#'   sequences do not depend on `n_cores`; see "Reproducibility".
#' @param initial_parameters A `goldfishParams` (from `[set_parameters]` over
#'   the joint specification), or `NULL` (default). It is \eqn{\theta_0}: its
#'   free (non-offset `NA`) slots are the parameters estimated, and `NULL` means
#'   zero over the free set. There is deliberately **no** `fixed_parameters`
#'   argument — fixed effects are the specification's formula offsets, not a
#'   control setting. A bare numeric vector or a parameter *provider* is
#'   rejected (build a `goldfishParams` with `[set_parameters]`).
#' @param em_trace_se A single logical. When `TRUE`, the per-iteration
#'   `em_trace` also carries parameter standard errors (one extra Fisher pass
#'   per accepted iteration). Default `FALSE`.
#' @param n_cores A single positive integer, the core budget for the internal
#'   per-sequence map (augmentation draws, pool preprocessing, and evaluation).
#'   Left at its default (`2L`, within CRAN's two-core cap) the map runs
#'   serially. An explicit value above 1 runs it on that many mirai daemons,
#'   with workers times BLAS threads kept within `n_cores`; if mirai (a
#'   Suggests package) is not installed, it warns once and runs serially. The
#'   worker count never changes the drawn sequences; see "Reproducibility".
#' @param augmenter,weights,optimizer The nested option bundles, from
#'   `[set_augmenter_options]`, `[set_weights_options]`, and `[set_sgd_options]`
#'   respectively. Each defaults to its own constructor's defaults.
#'
#' @return An object of class `c("goldfishAlgoEm", "goldfishAlgo", "list")`
#'   carrying the resolved EM settings and the (possibly corrected) nested
#'   controls. The nested `[set_sgd_options]` `batch_size`, if left `NULL`, is
#'   resolved once here to `min(n_sequences, 10L)` and frozen for the run.
#'
#' @section Reproducibility:
#' `seed` makes a run reproducible, in two parts:
#'
#' * **The drawn sequences** are identical for any `n_cores`, serial included,
#'   and on any machine. Each draw runs on its own random-number substream,
#'   keyed by its EM iteration and its position among that iteration's draws,
#'   never by the worker that runs it. The draws are pure R and call no BLAS.
#' * **The estimates** are identical for any `n_cores` on the same machine and
#'   BLAS build. Per-sequence results are combined in sequence order, never in
#'   the order workers return them. Across machines, compiled code and
#'   multithreaded BLAS round differently, so bit-for-bit identical estimates
#'   are not promised there.
#'
#' @seealso [set_augmenter_options()], [set_weights_options()],
#'   [set_sgd_options()]
#' @export
#' @examples
#' em_ctrl <- set_algorithm_em(
#'   n_sequences = 200L,
#'   max_iterations = 100L,
#'   augmenter = set_augmenter_options(routine = "mcmc"),
#'   weights = set_weights_options(weighting = "importance")
#' )
set_algorithm_em <- function(
  n_sequences = 100L,
  max_iterations = 50L,
  accept_quantile = 0.2,
  growth_quantile = 0.2,
  stop_quantile = 0.1,
  tolerance = 1e-3,
  stop_count = 1L,
  max_retries = 20L,
  max_pool = 5000L,
  seed = NULL,
  initial_parameters = NULL,
  em_trace_se = FALSE,
  n_cores = 2L,
  augmenter = set_augmenter_options(),
  weights = set_weights_options(),
  optimizer = set_sgd_options()
) {
  n_cores_explicit <- !missing(n_cores)

  n_sequences <- check_em_count(n_sequences, "n_sequences")
  max_iterations <- check_em_count(max_iterations, "max_iterations")
  stop_count <- check_em_count(stop_count, "stop_count")
  max_retries <- check_em_count(max_retries, "max_retries")
  max_pool <- check_em_count(max_pool, "max_pool")
  n_cores <- check_em_count(n_cores, "n_cores")

  check_em_quantile(accept_quantile, "accept_quantile")
  check_em_quantile(growth_quantile, "growth_quantile")
  check_em_quantile(stop_quantile, "stop_quantile")

  if (
    !rlang::is_scalar_double(tolerance) || is.na(tolerance) || tolerance <= 0
  ) {
    cli::cli_abort("{.arg tolerance} must be a single positive numeric value.")
  }
  if (!rlang::is_scalar_logical(em_trace_se) || is.na(em_trace_se)) {
    cli::cli_abort(
      "{.arg em_trace_se} must be a single {.code TRUE}/{.code FALSE}."
    )
  }
  if (!is.null(seed)) {
    if (!rlang::is_scalar_integerish(seed, finite = TRUE)) {
      cli::cli_abort("{.arg seed} must be {.code NULL} or a single integer.")
    }
    seed <- as.integer(seed)
  }
  if (!is.null(initial_parameters)) {
    # Accepts only a goldfishParams; the abort names set_parameters(). The
    # object is stored as-is — the loop projects its free set (offsets excluded)
    # when it has the specification.
    initial_parameters <- accept_joint_parameters(
      initial_parameters,
      arg = "initial_parameters"
    )
  }

  augmenter <- check_nested_control(
    augmenter,
    "goldfishAugmenterCtrl",
    "augmenter",
    "set_augmenter_options"
  )
  weights <- check_nested_control(
    weights,
    "goldfishWeightsCtrl",
    "weights",
    "set_weights_options"
  )
  optimizer <- check_nested_control(
    optimizer,
    "goldfishSgdCtrl",
    "optimizer",
    "set_sgd_options"
  )

  # Cold-start construction guard (design D4, dynes-augmentation D1/D14):
  # captured before the warm-start precedence branch below can clear the flag,
  # since either a non-zero explicit theta0 or a requested warm start is
  # equally a risk for the "random" routine.
  warm_start_requested <- isTRUE(augmenter$warm_start)

  # Cross-object rules. Warm-start precedence first: an explicit start wins over
  # a warm-start fit, so disable the toggle before it can reach the loop.
  if (!is.null(initial_parameters) && isTRUE(augmenter$warm_start)) {
    cli::cli_warn(c(
      "{.arg initial_parameters} was supplied, so the augmenter's
       {.arg warm_start} is ignored.",
      "i" = "The explicit starting values take precedence over a warm-start
             fit."
    ))
    augmenter$warm_start <- FALSE
  }

  # Validity matrix (design D4): uniform weighting is exact only when the
  # proposal is the model's conditional distribution — the mcmc routine with a
  # per-iteration pool refresh. Every other cell warns and switches to
  # importance weighting.
  uniform_valid <- identical(augmenter$routine, "mcmc") &&
    isTRUE(weights$refresh)
  if (identical(weights$weighting, "uniform") && !uniform_valid) {
    cli::cli_warn(c(
      "{.arg weighting} = {.val uniform} requires {.arg routine} = {.val mcmc}
       with pool {.arg refresh} = {.code TRUE}.",
      "i" = "Using {.arg weighting} = {.val importance} instead."
    ))
    weights$weighting <- "importance"
  }

  # Refresh-inapplicability (design D5): a whole-pool redraw each iteration
  # makes the staleness defenses moot. A non-identity transformation is reset
  # (state-changing); a non-default ESS threshold is warned as unused.
  if (isTRUE(weights$refresh)) {
    if (!identical(weights$transformation, "identity")) {
      cli::cli_warn(c(
        "{.arg transformation} = {.val {weights$transformation}} is inapplicable
         when {.arg refresh} = {.code TRUE}.",
        "i" = "Weight transformations act on a persistent pool; the redrawn pool
               uses {.val identity}."
      ))
      weights$transformation <- "identity"
    }
    if (!isTRUE(weights$ess_threshold == 0.5)) {
      cli::cli_warn(c(
        "{.arg ess_threshold} is inapplicable when {.arg refresh} =
         {.code TRUE}.",
        "i" = "The ESS redraw guard does not run on a fully redrawn pool."
      ))
    }
  } else if (
    # Non-identity transform feeds resampling probabilities their truncation
    # bias (design D5). Warn-only — the transformation is still applied.
    !identical(weights$transformation, "identity") &&
      identical(weights$use, "resampling")
  ) {
    cli::cli_warn(c(
      "A non-identity {.arg transformation} = {.val {weights$transformation}}
       with {.arg use} = {.val resampling} truncates the weights before
       resampling.",
      "i" = "Resampling probabilities inherit the transformation's bias
             (truncated-IS caveat)."
    ))
  }

  # Batch-scheme coupling (design D6): resampled batches carry unweighted
  # gradients, so the cyclic scheme's importance-weighting has nothing to act
  # on.
  if (
    identical(weights$use, "resampling") &&
      identical(optimizer$batch_scheme, "cyclic")
  ) {
    cli::cli_warn(c(
      "{.arg batch_scheme} = {.val cyclic} importance-weights the gradient,
       which is inapplicable when {.arg use} = {.val resampling}.",
      "i" = "Resampled batches use unweighted gradients."
    ))
  }

  # Cold-start construction guard (design D4, dynes-augmentation D1/D14):
  # "random" draws uniformly over orderings/times, which sits far from the
  # target model at a non-zero starting point and risks E-step weight collapse
  # at iteration 1. Settle-at-construction complement to the runtime cold-start
  # ESS diagnostic (D5); valid, not aborted, and silent at theta0 = 0 (the
  # "random" + default-theta0 path is near the model).
  if (
    identical(augmenter$routine, "random") &&
      (!is_zero_initial_parameters(initial_parameters) || warm_start_requested)
  ) {
    cli::cli_warn(c(
      "{.arg routine} = {.val random} draws uniformly over orderings/times,
       which sits far from the target model at a non-zero starting point.",
      "i" = "E-step importance weights may collapse at iteration 1; consider a
             model-driven {.arg routine} ({.val sim} or {.val mcmc}), or start
             from {.arg initial_parameters} = {.code NULL} (theta0 = 0)."
    ))
  }

  # Resolve the fixed absolute batch size once (design D6): the only object that
  # knows n_sequences freezes NULL to min(n_sequences, 10L); an explicit value
  # is kept verbatim. The floor keeps the batch from exceeding a tiny pool.
  if (is.null(optimizer$batch_size)) {
    optimizer$batch_size <- min(n_sequences, 10L)
  }

  control_list <- list(
    n_sequences = n_sequences,
    max_iterations = max_iterations,
    accept_quantile = accept_quantile,
    growth_quantile = growth_quantile,
    stop_quantile = stop_quantile,
    tolerance = tolerance,
    stop_count = stop_count,
    max_retries = max_retries,
    max_pool = max_pool,
    seed = seed,
    initial_parameters = initial_parameters,
    em_trace_se = em_trace_se,
    n_cores = n_cores,
    n_cores_explicit = n_cores_explicit,
    augmenter = augmenter,
    weights = weights,
    optimizer = optimizer
  )

  class(control_list) <- c("goldfishAlgoEm", "goldfishAlgo", "list")
  control_list
}

# A single positive integer (the EM loop's count-valued controls), coerced to
# integer. `arg` names the offending argument in the abort.
check_em_count <- function(value, arg, call = rlang::caller_env()) {
  if (!rlang::is_scalar_integerish(value, finite = TRUE) || value < 1) {
    cli::cli_abort(
      "{.arg {arg}} must be a single positive integer.",
      call = call
    )
  }
  as.integer(value)
}

# A single numeric stop-rule quantile in the open interval (0, 1) -- the tails
# feed qnorm(), so 0 and 1 (infinite z) are rejected.
check_em_quantile <- function(value, arg, call = rlang::caller_env()) {
  if (
    !rlang::is_scalar_double(value) ||
      is.na(value) ||
      value <= 0 ||
      value >= 1
  ) {
    cli::cli_abort(
      "{.arg {arg}} must be a single numeric value in (0, 1).",
      call = call
    )
  }
  invisible(value)
}

# A nested option bundle of the expected class; the abort names the constructor
# that mints it so the fix is obvious.
check_nested_control <- function(
  value,
  cls,
  arg,
  constructor,
  call = rlang::caller_env()
) {
  if (!inherits(value, cls)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a {.cls {cls}}.",
        "x" = "A {.cls {class(value)[1]}} was supplied.",
        "i" = "Build one with {.fn {constructor}}."
      ),
      call = call
    )
  }
  value
}

# Whether a supplied initial_parameters object (or its absence) represents
# theta0 = 0 over the free set for the cold-start construction guard: NULL
# defaults to zero (design D2), and an unpinned (NA) free slot also defaults
# to zero at the loop, so only a genuinely nonzero pin counts as non-zero.
is_zero_initial_parameters <- function(x) {
  if (is.null(x)) {
    return(TRUE)
  }
  values <- x$free
  values[is.na(values)] <- 0
  all(values == 0)
}
