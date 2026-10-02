# E-step weight state for the ascent-based Monte Carlo EM (ABMCEM) loop driven
# by estimate_dynes(). Pure functions over a pool of endpoint-hitting sequences:
# the permanent per-sequence reference record, the Levine & Casella (2001)
# likelihood-ratio reweighting, the named-enum log-weight transformation, the
# effective sample size, the three resampling schemes, and the ESS staleness
# guard. The estimation loop owns the redraw/refresh/growth mechanics; these
# functions only compute weights and diagnose degeneracy.

# A permanent per-sequence reference record, kept on the log scale: the
# parameters the sequence was drawn under (`theta_ref`), its log-likelihood
# there (`loglik_ref`), and its log proposal density (`log_proposal`). For MCMC
# draws the last two coincide; for a restart-coupled draw `log_proposal` is
# already restart-normalized by the augmenter and is consumed verbatim here. The
# record travels with the sequence for the whole run, so reweighting at any
# later parameter value is a pure recomputation, never a redraw.
new_reference_record <- function(
  theta_ref,
  loglik_ref,
  log_proposal,
  call = rlang::caller_env()
) {
  if (!is.numeric(theta_ref)) {
    cli::cli_abort("{.arg theta_ref} must be a numeric vector.", call = call)
  }
  if (!rlang::is_scalar_double(loglik_ref) || !is.finite(loglik_ref)) {
    cli::cli_abort(
      "{.arg loglik_ref} must be a single finite numeric value.",
      call = call
    )
  }
  if (!rlang::is_scalar_double(log_proposal) || !is.finite(log_proposal)) {
    cli::cli_abort(
      "{.arg log_proposal} must be a single finite numeric value.",
      call = call
    )
  }
  list(
    theta_ref = theta_ref,
    loglik_ref = loglik_ref,
    log_proposal = log_proposal
  )
}

# The pool's stored log proposal densities in sequence order -- the denominator
# of the importance weights.
pool_log_proposal <- function(reference_records) {
  vapply(reference_records, function(r) r$log_proposal, numeric(1))
}

# Raw (un-normalized, pre-transformation) log importance weights: the current
# per-sequence total log-likelihood (summed across sub-models) minus each
# sequence's stored log proposal density. This is the Levine & Casella (2001)
# Monte-Carlo-EM sample-reuse update -- for MCMC draws `log_proposal` is the
# log-likelihood at the sequence's own reference parameters, so the weight is
# exactly the likelihood ratio against that reference. Mixed-reference pools
# (which arise once growth appends sequences drawn at a later parameter value)
# are combined as self-normalized importance sampling with heterogeneous
# proposals: each sequence weighted by its OWN ratio, self-normalized across the
# pool. A shared mixture denominator (true multiple importance sampling) is a
# recorded future refinement, not this version.
reweight_log_weights <- function(loglik_current, log_proposal) {
  loglik_current - log_proposal
}

# The pre-normalization transformation applied to the raw log-weights -- a named
# enum, not a user-supplied function, so the choice is validatable and its
# resampling caveat can fire at construction:
#   * "identity"  -- unchanged.
#   * "clipping"  -- winsorize the upper tail at its 0.9 quantile (truncated
#     importance sampling), capping any single sequence from dominating.
#     Clipping a quantile commutes with the log/linear scale, so winsorizing the
#     log-weights matches clipping the linear-scale weights.
#   * "smoothing" -- temper the log-weights by gamma = 2. Under the softmax
#     normalization below this is identical to raising the linear-scale weights
#     to the power gamma and renormalizing, since softmax(gamma * lw) =
#     normalize(w^gamma).
# Non-finite log-weights (a degenerate sequence) are excluded from the clipping
# quantile and pass through as -Inf, normalizing to zero downstream.
transform_log_weights <- function(
  log_weights,
  transformation = c("identity", "clipping", "smoothing")
) {
  transformation <- rlang::arg_match(transformation)
  switch(
    transformation,
    identity = log_weights,
    clipping = {
      finite <- is.finite(log_weights)
      if (any(finite)) {
        threshold <- stats::quantile(
          log_weights[finite],
          probs = 0.9,
          names = FALSE
        )
        log_weights[finite] <- pmin(log_weights[finite], threshold)
      }
      log_weights
    },
    smoothing = 2 * log_weights
  )
}

# Normalize log-weights to a probability vector via the package's max-shift
# softmax. A fully non-finite (all -Inf) input normalizes to all-zero rather
# than NaN, so a degenerate pool degrades gracefully.
normalize_log_weights <- function(log_weights) {
  stable_softmax(log_weights)$probabilities
}

# The canonical normalized E-step weights for a pool. Uniform weighting -- valid
# only on the MCMC + refresh path, enforced upstream in set_algorithm_em() --
# ignores the proposal and spreads the weight equally over the sequences with a
# finite current log-likelihood: a degenerate one has no density to average,
# and weighting it would turn the uniform Q into NaN. Importance weighting (the
# default) forms the own-ratio log-weights, applies the transformation, and
# softmax-normalizes. `loglik_current` is the per-sequence total log-likelihood.
compute_pool_weights <- function(
  loglik_current,
  log_proposal,
  weighting = c("importance", "uniform"),
  transformation = c("identity", "clipping", "smoothing")
) {
  weighting <- rlang::arg_match(weighting)
  transformation <- rlang::arg_match(transformation)
  n <- length(loglik_current)
  if (identical(weighting, "uniform")) {
    finite <- is.finite(loglik_current)
    return(as.numeric(finite) / max(sum(finite), 1L))
  }
  log_weights <- reweight_log_weights(loglik_current, log_proposal)
  log_weights <- transform_log_weights(log_weights, transformation)
  normalize_log_weights(log_weights)
}

# Effective sample size of a normalized weight vector, ESS = 1 / sum(w^2). A
# fully degenerate pool (all weights zero, e.g. every sequence non-finite)
# floors at 0 rather than dividing by zero.
effective_sample_size <- function(weights) {
  denom <- sum(weights^2)
  if (denom <= 0) {
    return(0)
  }
  1 / denom
}

# Convert a weight vector into a length-`size` multiset of pool indices under
# one of the three schemes. Stratified and residual are low-variance resamplers;
# random is plain multinomial draws.
resample_pool <- function(
  weights,
  scheme = c("stratified", "residual", "random"),
  size = length(weights)
) {
  scheme <- rlang::arg_match(scheme)
  weights <- weights / sum(weights)
  switch(
    scheme,
    stratified = stratified_resample(cumsum(weights), size),
    residual = residual_resample(weights, size),
    random = sample.int(
      length(weights),
      size = size,
      replace = TRUE,
      prob = weights
    )
  )
}

# Systematic-within-stratum resampling: one uniform draw per equal-probability
# stratum keeps the sample spread across the weight support with lower variance
# than plain multinomial draws.
stratified_resample <- function(cum_weights, size = length(cum_weights)) {
  u <- (stats::runif(size) + seq_len(size) - 1) / size
  findInterval(u, cum_weights) + 1L
}

# Residual resampling: allocate the guaranteed integer copies floor(size * w)
# deterministically, then fill the remainder by multinomial draws on the
# fractional residual -- lower variance than plain multinomial by fixing the
# integer part.
residual_resample <- function(weights, size = length(weights)) {
  num_copies <- floor(size * weights)
  indices <- rep(seq_along(weights), num_copies)
  remainder <- size - length(indices)
  if (remainder > 0) {
    residual <- size * weights - num_copies
    residual <- residual / sum(residual)
    extra <- sample.int(
      length(weights),
      size = remainder,
      replace = TRUE,
      prob = residual
    )
    indices <- c(indices, extra)
  }
  as.integer(indices)
}

# The classed E-step object, built by the ABMCEM loop from the per-sequence
# quantities evaluate_sequence_pool() returns (the evaluator itself stays
# scheme-agnostic). The loop consumes only compute_q()/compute_ase(), so it is
# written once, scheme-agnostically, and adding a weighting scheme
# means adding methods, not editing the loop. Three schemes, three subclasses
# sharing the `goldfishDynesEstep` parent:
#   * "importance" -> goldfishEstepIS         (default; no resampling)
#   * "resampling" -> goldfishEstepResampling (one resample per pass into an
#                     unweighted multiset feeding Q/ASE and the M-step batches)
#   * "uniform"    -> goldfishEstepUniform    (MCMC + refresh; plain mean)
# `lambda` are the per-sequence log-likelihood differences Lambda_i (summed
# across sub-models); `weights` are the normalized E-step weights (on the
# uniform path they only mark, by a zero, the sequences left out of the mean).
# Resampling draws its multiset here (via `resample_pool()`) unless a `resample_indices` multiset is supplied verbatim (the loop's control
# RNG substream owns the draw at run time; supplying it keeps the estimators
# deterministically testable). When `mcmc = TRUE` a lag-1 autocorrelation index
# of the Lambda series is recorded for the trace (the ASE treats retained draws
# as independent given the thinning; a high index earns a raise-thinning hint).
# `score` (a K x p matrix) and `fisher` (a list of K p x p matrices) are the
# per-sequence complete-data score and information at the parameters Lambda
# was evaluated at. Q and its ASE do not read them, but the sandwich covariance
# does -- its meat sums the per-sequence score outer products and its bread the
# per-sequence information -- and neither can be recovered from Lambda.
new_dynes_estep <- function(
  lambda,
  weights = NULL,
  scheme = c("importance", "resampling", "uniform"),
  resampling_scheme = c("stratified", "residual", "random"),
  resample_indices = NULL,
  mcmc = FALSE,
  score = NULL,
  fisher = NULL,
  call = rlang::caller_env()
) {
  scheme <- rlang::arg_match(scheme)
  resampling_scheme <- rlang::arg_match(resampling_scheme)
  if (!is.numeric(lambda)) {
    cli::cli_abort("{.arg lambda} must be a numeric vector.", call = call)
  }
  k <- length(lambda)
  if (scheme != "uniform") {
    if (!is.numeric(weights) || length(weights) != k) {
      cli::cli_abort(
        "{.arg weights} must be a numeric vector as long as {.arg lambda}
         ({.val {k}}) under the {.val {scheme}} scheme.",
        call = call
      )
    }
  }
  check_estep_derivatives(score, fisher, k, call = call)
  estep <- list(
    lambda = lambda,
    pool_size = k,
    weights = weights,
    score = score,
    fisher = fisher,
    autocorrelation = if (isTRUE(mcmc)) {
      lag1_autocorrelation(lambda)
    } else {
      NA_real_
    }
  )
  subclass <- switch(
    scheme,
    importance = "goldfishEstepIS",
    resampling = "goldfishEstepResampling",
    uniform = "goldfishEstepUniform"
  )
  if (scheme == "resampling") {
    if (is.null(resample_indices)) {
      resample_indices <- resample_pool(weights, resampling_scheme, size = k)
    }
    estep$resample_indices <- as.integer(resample_indices)
    estep$counts <- tabulate(estep$resample_indices, nbins = k)
    estep$resampling_scheme <- resampling_scheme
  }
  structure(estep, class = c(subclass, "goldfishDynesEstep"))
}

# The per-sequence score and information, when supplied, must cover the `k`
# pooled sequences on one parameter dimension.
check_estep_derivatives <- function(score, fisher, k, call) {
  if (is.null(score) && is.null(fisher)) {
    return(invisible())
  }
  if (!is.matrix(score) || nrow(score) != k) {
    cli::cli_abort(
      "{.arg score} must be a matrix with one row per pooled sequence
       ({.val {k}}).",
      call = call
    )
  }
  if (!is.list(fisher) || length(fisher) != k) {
    cli::cli_abort(
      "{.arg fisher} must be a list with one matrix per pooled sequence
       ({.val {k}}).",
      call = call
    )
  }
  p <- ncol(score)
  square <- vapply(
    fisher,
    function(f) is.matrix(f) && identical(dim(f), c(p, p)),
    logical(1)
  )
  if (!all(square)) {
    bad <- which(!square)
    cli::cli_abort(
      c(
        "Each {.arg fisher} matrix must be {p} x {p}, matching the columns of
         {.arg score}.",
        "x" = "{cli::qty(length(bad))}Sequence{?s} {?does/do} not match:
               {.val {bad}}."
      ),
      call = call
    )
  }
  invisible()
}

# Lag-1 autocorrelation of a scalar series in draw order -- the MCMC mixing
# diagnostic recorded in `em_trace`. A constant (zero-variance) series has no
# defined correlation and reports 0; a series shorter than two draws reports NA.
lag1_autocorrelation <- function(x) {
  n <- length(x)
  if (n < 2L) {
    return(NA_real_)
  }
  centered <- x - mean(x)
  denom <- sum(centered^2)
  if (denom <= 0) {
    return(0)
  }
  sum(centered[-1L] * centered[-n]) / denom
}

# The Q (expected complete-data log-likelihood increment) estimator, dispatching
# on the E-step scheme.
compute_q <- function(estep, ...) {
  UseMethod("compute_q")
}

# Its Monte-Carlo standard error, dispatching on the same object. Every method
# returns the ASE carrying an `ase_floored` attribute (read via
# `ase_floored()`): TRUE only when a variance estimate was floored (resampling).
compute_ase <- function(estep, ...) {
  UseMethod("compute_ase")
}

#' @exportS3Method
compute_q.default <- function(estep, ...) {
  cli::cli_abort(
    "{.fun compute_q} has no method for an object of class
     {.cls {class(estep)}}; expected a {.cls goldfishDynesEstep}."
  )
}

#' @exportS3Method
compute_ase.default <- function(estep, ...) {
  cli::cli_abort(
    "{.fun compute_ase} has no method for an object of class
     {.cls {class(estep)}}; expected a {.cls goldfishDynesEstep}."
  )
}

# Whether an ASE returned by compute_ase() was floored at 0 (a negative variance
# estimate below the resolution floor). The pass records this in `em_trace`.
ase_floored <- function(ase) {
  isTRUE(attr(ase, "ase_floored"))
}

# Importance (default): Q_hat = sum w_i Lambda_i; ASE^2 = sum w_i^2 (Lambda_i -
# Q_hat)^2 -- the self-normalized IS delta-method variance. A sum of squares is
# non-negative, so this form never floors.
#' @exportS3Method
compute_q.goldfishEstepIS <- function(estep, ...) {
  sum(estep$weights * estep$lambda)
}

#' @exportS3Method
compute_ase.goldfishEstepIS <- function(estep, ...) {
  q <- compute_q(estep)
  ase <- sqrt(sum(estep$weights^2 * (estep$lambda - q)^2))
  structure(ase, ase_floored = FALSE)
}

# Resampling: the multinomial counts c_i turn the pool into an unweighted
# multiset. Q_hat = K^-1 sum c_i Lambda_i; the ASE is the prototype's
# ratio/delta form sigma^2 = Q_hat^2 [ sum c L^2 / (sum c L)^2 - 1/K ], which
# includes the resampling noise. The bracket is 0 in exact arithmetic when the
# Lambda_i are constant across the multiset and can dip negative under
# floating-point roundoff in that near-convergence regime; the variance is
# floored at 0 (ASE = 0) rather than propagating NaN from sqrt(negative),
# degrading the decision bounds to the honest |Q| < tolerance / Q > 0 limit,
# and the flooring is flagged for the trace.
#' @exportS3Method
compute_q.goldfishEstepResampling <- function(estep, ...) {
  sum(estep$counts * estep$lambda) / estep$pool_size
}

#' @exportS3Method
compute_ase.goldfishEstepResampling <- function(estep, ...) {
  counts <- estep$counts
  lambda <- estep$lambda
  k <- estep$pool_size
  weighted_sum <- sum(counts * lambda)
  if (weighted_sum == 0) {
    return(structure(0, ase_floored = FALSE))
  }
  q <- weighted_sum / k
  var_est <- q^2 * (sum(counts * lambda^2) / weighted_sum^2 - 1 / k)
  floored <- var_est < 0
  structure(sqrt(max(var_est, 0)), ase_floored = floored)
}

# Uniform (MCMC + refresh only): Q_hat = mean(Lambda); ASE^2 = s^2(Lambda) / K,
# the sample standard error of the mean, both over the K positive-weight
# sequences. The zero weights come from a non-finite *current* log-likelihood,
# not from Lambda, so a sequence with no likelihood at the proposal stays in
# the mean and makes Q = -Inf, a rejection.
#' @exportS3Method
compute_q.goldfishEstepUniform <- function(estep, ...) {
  mean(uniform_lambda(estep))
}

#' @exportS3Method
compute_ase.goldfishEstepUniform <- function(estep, ...) {
  lambda <- uniform_lambda(estep)
  ase <- sqrt(stats::var(lambda) / length(lambda))
  structure(ase, ase_floored = FALSE)
}

uniform_lambda <- function(estep) {
  if (is.null(estep$weights)) {
    return(estep$lambda)
  }
  estep$lambda[estep$weights > 0]
}

# The ESS staleness guard on a persistent pool. It reports the ESS, whether it
# fell below `ess_threshold` * `pool_size`, and whether the loop should redraw
# the pool. Two warnings, one code path: when the guard is enabled and ESS
# collapses, it requests a redraw and warns that new draws are generated; when
# disabled but the condition is met, it warns without redrawing. It never warns
# or redraws at startup (a redraw of the first pool is statistically pointless).
# `enabled` is supplied by the loop; on a fully refreshed pool the guard is
# inapplicable and is not consulted.
ess_guard <- function(
  weights,
  ess_threshold = 0.5,
  pool_size = length(weights),
  enabled = TRUE,
  at_startup = FALSE
) {
  ess <- effective_sample_size(weights)
  below <- ess < ess_threshold * pool_size
  decision <- list(ess = ess, below_threshold = below, redraw = FALSE)
  if (at_startup || !below) {
    return(decision)
  }
  floor_value <- round(ess_threshold * pool_size, 2)
  if (enabled) {
    cli::cli_warn(c(
      "!" = "Effective sample size {.val {round(ess, 2)}} fell below
             {.arg ess_threshold} × pool size ({.val {floor_value}}); redrawing
             the pool at the current parameters.",
      "i" = "Importance weights have degenerated on the persistent pool."
    ))
    decision$redraw <- TRUE
  } else {
    cli::cli_warn(c(
      "!" = "Effective sample size {.val {round(ess, 2)}} fell below
             {.arg ess_threshold} × pool size ({.val {floor_value}}), but the
             redraw guard is disabled; the degenerate pool is kept.",
      "i" = "Consider enabling the ESS guard or raising {.arg n_sequences}."
    ))
  }
  decision
}
