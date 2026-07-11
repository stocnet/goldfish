#include "stable_softmax.h"

// [[Rcpp::depends(RcppArmadillo)]]

double stable_softmax_masked(
    const arma::vec& lin_pred,
    const arma::vec& allowed,
    arma::vec& weights
) {
    const arma::uword n = lin_pred.n_elem;
    weights.zeros(n);
    // max over the allowed alternatives (the shift), ignoring masked entries
    double shift = -arma::datum::inf;
    for (arma::uword k = 0; k < n; ++k) {
        if (allowed(k) == 1 && lin_pred(k) > shift) {
            shift = lin_pred(k);
        }
    }
    if (!std::isfinite(shift)) {
        // no allowed alternative: zero weights, -inf normalizer
        return -arma::datum::inf;
    }
    double total = 0.0;
    for (arma::uword k = 0; k < n; ++k) {
        if (allowed(k) == 1) {
            double w = std::exp(lin_pred(k) - shift);
            weights(k) = w;
            total += w;
        }
    }
    return shift + std::log(total);
}
