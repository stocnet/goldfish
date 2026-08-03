#include "log_sum_exp.h"

// [[Rcpp::depends(RcppArmadillo)]]

double log_sum_exp_masked(
    const arma::vec& lin_pred,
    const arma::vec& allowed,
    arma::vec& weights
) {
    const arma::uword n = lin_pred.n_elem;
    weights.zeros(n);
    // An empty mask means every position is in the risk set -- the gather
    // backend's slices are pre-masked, so it has no mask to hand over. Same
    // convention as event_reductions.h.
    const bool masked = allowed.n_elem > 0;
    // max over the allowed alternatives (the shift), ignoring masked entries
    double shift = -arma::datum::inf;
    for (arma::uword k = 0; k < n; ++k) {
        if ((!masked || allowed(k) == 1) && lin_pred(k) > shift) {
            shift = lin_pred(k);
        }
    }
    if (!std::isfinite(shift)) {
        // no allowed alternative: zero weights, -inf normalizer
        return -arma::datum::inf;
    }
    double total = 0.0;
    for (arma::uword k = 0; k < n; ++k) {
        if (!masked || allowed(k) == 1) {
            double w = std::exp(lin_pred(k) - shift);
            weights(k) = w;
            total += w;
        }
    }
    return shift + std::log(total);
}
