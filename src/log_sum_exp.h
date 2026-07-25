#ifndef GOLDFISH_LOG_SUM_EXP_H
#define GOLDFISH_LOG_SUM_EXP_H

#include <RcppArmadillo.h>

// Single-pass max-shift softmax over the allowed entries of a linear-predictor
// vector, mirroring the R `stable_softmax()` helper so the `default`
// and `default_c` engines share the same numerics. `lin_pred` (length N) holds
// x_k = beta^T s_k; `allowed` (length N) is a 0/1 risk-set mask. On return
// `weights` (length N) holds exp(x_k - m) for allowed entries and 0 elsewhere,
// where m = max_{allowed} x_k; the return value is the log-normalizer
// m + log(sum_k weights_k). Then sum(weights) is the softmax denominator and the
// observed alternative's log-probability is x_obs - log_normalizer, which stays
// finite even when that probability underflows (log(0) = -inf is avoided). With
// no allowed entry the weights are all 0 and the return value is -inf.
//
// Scope: the multinomial (shift-invariant) contributions only. The timed
// rate/REM hazard path keeps plain exp() — its scale is absolute (Non-Goal).
double log_sum_exp_masked(
    const arma::vec& lin_pred,
    const arma::vec& allowed,
    arma::vec& weights
);

#endif
