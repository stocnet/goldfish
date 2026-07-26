#include "event_reductions.h"
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// See event_reductions.h for the (w, c) contract these three share.

int rank_of_observed(
    const arma::vec& w,
    const arma::vec& allowed,
    arma::uword obs
) {
  const bool masked = allowed.n_elem > 0;
  const double obs_weight = w(obs);
  int rank = 1;
  for (arma::uword j = 0; j < w.n_elem; ++j) {
    if (masked && allowed(j) != 1) continue;
    // Strict `>` excludes the observed alternative itself and makes ties share
    // the better rank, the same rule in every kernel and in the R mirror.
    if (w(j) > obs_weight) rank++;
  }
  return rank;
}

void accumulate_margins(
    const arma::vec& w,
    double c,
    const arma::vec& allowed,
    arma::uword obs,
    bool dependent,
    const std::vector<margin_side>& sides
) {
  const bool masked = allowed.n_elem > 0;
  for (arma::uword j = 0; j < w.n_elem; ++j) {
    if (masked && allowed(j) != 1) continue;
    const double contribution = c * w(j);
    for (std::size_t s = 0; s < sides.size(); ++s) {
      const margin_side& side = sides[s];
      const arma::uword slot = side.index == nullptr ? j : (*side.index)(j);
      (*side.expected)(slot) += contribution;
    }
  }
  if (!dependent) return;
  for (std::size_t s = 0; s < sides.size(); ++s) {
    const margin_side& side = sides[s];
    const arma::uword slot = side.index == nullptr ? obs : (*side.index)(obs);
    (*side.observed)(slot) += 1;
  }
}

arma::rowvec event_score_row(
    const arma::mat& X,
    const arma::vec& w,
    double c,
    arma::uword obs,
    bool dependent
) {
  // `w` is zero outside the risk set (header contract), so the weighted mean
  // needs no mask and stays a single BLAS reduction.
  arma::rowvec score = -c * (w.t() * X);
  if (dependent) score += X.row(obs);
  return score;
}

Rcpp::RObject scatter_event_probabilities(
    const arma::vec& probabilities,
    const arma::uvec& index_i,
    const arma::uvec& index_j,
    arma::uword n_actors_1,
    arma::uword n_actors_2
) {
  const bool axis_i = index_i.n_elem > 0;
  const bool axis_j = index_j.n_elem > 0;
  // With neither index there is no actor identity to scatter onto and the
  // shape is undefined. Fail loudly: silently returning an empty or unscattered
  // vector would put a meaningless per-event entry on the fit.
  if (!axis_i && !axis_j) {
    Rcpp::stop("scatter_event_probabilities(): no actor index for either axis");
  }
  if (axis_i && axis_j) {
    arma::mat grid(n_actors_1, n_actors_2, fill::zeros);
    for (arma::uword r = 0; r < probabilities.n_elem; ++r) {
      grid(index_i(r), index_j(r)) = probabilities(r);
    }
    return Rcpp::wrap(grid);
  }
  const arma::uvec& index = axis_j ? index_j : index_i;
  arma::vec full(axis_j ? n_actors_2 : n_actors_1, fill::zeros);
  for (arma::uword r = 0; r < probabilities.n_elem; ++r) {
    full(index(r)) = probabilities(r);
  }
  return Rcpp::wrap(full);
}

//' Exercise the shared per-event reductions from R
//'
//' Test-only entry point: it feeds constructed `(w, c, X, obs)` inputs to the
//' three reductions so the R mirror can be pinned to this implementation
//' directly, rather than only through a kernel that happens to call them.
//' Supplying `index_b` adds a second margin side, which is how the two-sided
//' kernels (REM, REM_ordered, MM) use `accumulate_margins()`.
//' @noRd
// [[Rcpp::export]]
List event_reductions_probe(
    const arma::mat& X,
    const arma::vec& w,
    double c,
    arma::uword obs,
    const arma::vec& allowed,
    bool dependent,
    const arma::uvec& index_a,
    const arma::uvec& index_b,
    arma::uword n_a,
    arma::uword n_b
) {
  arma::vec observed_a(n_a, fill::zeros);
  arma::vec expected_a(n_a, fill::zeros);
  arma::vec observed_b(n_b, fill::zeros);
  arma::vec expected_b(n_b, fill::zeros);

  std::vector<margin_side> sides;
  sides.push_back(margin_side(
    &observed_a, &expected_a, index_a.n_elem > 0 ? &index_a : nullptr
  ));
  if (n_b > 0) {
    sides.push_back(margin_side(
      &observed_b, &expected_b, index_b.n_elem > 0 ? &index_b : nullptr
    ));
  }
  accumulate_margins(w, c, allowed, obs, dependent, sides);

  return List::create(
    Named("rank") = rank_of_observed(w, allowed, obs),
    Named("score") = event_score_row(X, w, c, obs, dependent),
    Named("observed_a") = observed_a,
    Named("expected_a") = expected_a,
    Named("observed_b") = observed_b,
    Named("expected_b") = expected_b
  );
}
