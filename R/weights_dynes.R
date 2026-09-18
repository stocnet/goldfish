# E-step weight state for the ascent-based Monte Carlo EM (ABMCEM) loop driven
# by estimate_dynes(). Pure functions over a pool of endpoint-hitting sequences:
# the permanent per-sequence reference record, the Casella & Levine (2001)
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
# sequence's stored log proposal density. This is the Casella & Levine (2001)
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
# ignores the proposal and returns equal weights. Importance weighting (the
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
    return(rep(1 / n, n))
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
