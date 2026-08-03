#ifndef GOLDFISH_LOG_SUM_EXP_H
#define GOLDFISH_LOG_SUM_EXP_H

#include <RcppArmadillo.h>

// Single-pass max-shift log-sum-exp over the allowed entries of a
// linear-predictor vector. This is the numerically stable *primitive*: the
// log-softmax `x_obs - log_normalizer` is then free and exact, and the softmax
// itself is a transformation of the returned weights. The R `stable_softmax()`
// helper shares this max-shift (not this interface — it returns the two derived
// scales instead), so the backends have the same numerics.
//
// `lin_pred` (length N) holds x_k = beta^T s_k; `allowed` is a 0/1 risk-set
// mask of length N, or EMPTY to mean every position is in the risk set (the
// gather backend's slices are pre-masked, so it has no mask to pass) — the same
// convention as event_reductions.h. On return `weights` (length N) holds
// exp(x_k - m) for allowed entries and 0 elsewhere, where m = max_{allowed} x_k;
// the return value is the log-normalizer m + log(sum_k weights_k). Then
// sum(weights) is the softmax denominator and the observed alternative's
// log-probability is x_obs - log_normalizer, which stays finite even when that
// probability underflows (log(0) = -inf is avoided). With no allowed entry the
// weights are all 0 and the return value is -inf.
//
// Scope note. Which quantities may use this is decided PER QUANTITY, not per
// kernel: anything entering as a ratio or a log of the normalizer (multinomial
// log-likelihood contributions, probabilities, ranks, the exact-time
// conditional log-probability) is shift-invariant and belongs here. The
// exact-time likelihood's total rate does NOT: it enters as -Dt * T on an
// absolute scale, so shifting it would change the model rather than stabilize
// it. That path recovers T = exp(log_normalizer) on the linear scale instead.
double log_sum_exp_masked(
    const arma::vec& lin_pred,
    const arma::vec& allowed,
    arma::vec& weights
);

#endif
