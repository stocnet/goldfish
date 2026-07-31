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
  // Strict `>` against a relatively-widened observed weight: it excludes the
  // observed alternative itself, makes exact ties share the better rank, and
  // collapses blocks that only floating-point noise has split -- the same rule
  // in every kernel and in the R mirror. Without the tolerance a nearly empty
  // network, where large blocks of alternatives carry identical statistics,
  // has its top ranks decided by whichever way the last bit of each
  // log-normalizer happened to fall, differently per backend.
  const double threshold = obs_weight * (1.0 + RANK_TIE_TOL);
  int rank = 1;
  for (arma::uword j = 0; j < w.n_elem; ++j) {
    if (masked && allowed(j) != 1) continue;
    if (w(j) > threshold) rank++;
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

void mark_availability(
    arma::uword n_positions,
    const arma::vec& allowed,
    const arma::uvec* index,
    arma::vec& seen
) {
  const bool masked = allowed.n_elem > 0;
  for (arma::uword j = 0; j < n_positions; ++j) {
    if (masked && allowed(j) != 1) continue;
    seen(index == nullptr ? j : (*index)(j)) = 1;
  }
}

void accumulate_availability(
    const arma::vec& seen,
    double dt,
    bool dependent,
    arma::vec* exposure,
    arma::vec* n_opportunities
) {
  if (exposure != nullptr) *exposure += dt * seen;
  if (dependent && n_opportunities != nullptr) *n_opportunities += seen;
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

void accumulate_event_information(
    const arma::mat& block,
    double scale,
    arma::uword id_event,
    const arma::mat& weights,
    arma::cube& weighted,
    arma::vec& trace_out
) {
  if (!weighted.is_empty()) {
    for (arma::uword m = 0; m < weights.n_cols; ++m) {
      const double w = weights(id_event, m);
      if (w == 0.0) continue;
      weighted.slice(m) += (w * scale) * block;
    }
  }
  if (!trace_out.is_empty()) {
    trace_out(id_event) = scale * arma::trace(block);
  }
}

arma::mat as_event_weights(
    const Rcpp::Nullable<Rcpp::NumericMatrix>& weights,
    arma::uword n_events
) {
  if (weights.isNull()) return arma::mat();
  arma::mat out = Rcpp::as<arma::mat>(weights.get());
  // The row count is the interval count, not the dependent-event count: the
  // exact-time families carry right-censored intervals that contribute to the
  // information, and a caller whose weights skipped them would silently
  // misalign every column.
  if (out.n_rows != n_events) {
    Rcpp::stop(
      "event weights have %u rows; the pass has %u intervals",
      (unsigned) out.n_rows, (unsigned) n_events
    );
  }
  return out;
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
