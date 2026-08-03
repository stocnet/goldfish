#ifndef GOLDFISH_EVENT_REDUCTIONS_H
#define GOLDFISH_EVENT_REDUCTIONS_H

#include <RcppArmadillo.h>
#include <vector>

// Per-event reductions shared by every backend, so a primitive is written once
// rather than once per kernel. The R mirror in `estimation_core.R` implements
// the same three reductions, exactly as the R `stable_softmax()` helper mirrors
// `log_sum_exp_masked()`, and a parity test pins the two together.
//
// The contract. At each event every backend forms a nonnegative weight vector
// `w` over that event's risk set and a scalar scale `c`, and the contribution
// of alternative j is m_j = c * w_j. Callers pass the **probability vector** as
// `w` in every family, taken from the stable log-sum-exp
// (`p_j = exp(x_j - lse)`, so it is exact whether or not the rates overflow or
// underflow); `c` alone carries the family difference:
//
//   family / scale                                w_j    c
//   --------------------------------------------  -----  ------------------
//   multinomial (choice, rate_ordered,             p_j    1
//     REM_ordered, coordination)
//   exact-time (rate, REM), probability scale      p_j    1
//   exact-time (rate, REM), compensator scale      p_j    Dt * total_rate
//
// so sum_j m_j is 1 on the probability scale in both families, and
// Dt * total_rate on the exact-time compensator — where m_j = Dt * rate_j
// exactly, since Dt * T * p_j = Dt * lambda_j. Splitting it this way keeps the
// only large factor in a scalar: an overflowing rate inside `w` would meet a
// mixed-sign statistic and give Inf - Inf = NaN, whereas p is bounded in [0, 1].
//
// **`w` MUST be zero outside the event's risk set.** Every caller already
// satisfies this: `log_sum_exp_masked()` returns 0 for masked entries (the
// four multinomial event-loop kernels), REM zeroes explicitly before use, the
// rate kernel only ever writes active senders, and the gather backend's slices
// are pre-masked.
//
// Given that invariant, `allowed` — a 0/1 mask, or empty — is a *skip hint*,
// not a semantic switch: it lets the accumulation loop step over positions
// known to contribute nothing, and results are identical whether or not it is
// supplied. It must agree with the zeros in `w`. `event_score_row()` therefore
// does not take it at all: `w' X` already ignores masked positions, and
// reducing through BLAS beats masking the vector first.
//
// `obs` is the 0-based position of the observed alternative within the risk-set
// vectors. `dependent` is false for a right-censored interval, which contributes
// its timing term but no observed alternative.

// Relative tolerance of the rank tie rule below. Two alternatives whose
// weights agree to this precision are treated as tied. Relative, not absolute,
// because the kernels rank different but proportional vectors — some the raw
// exp(linear predictor), some the normalized probabilities — and only a
// relative comparison is invariant to that per-event constant, which is what
// makes the rule reproducible across backends. The R mirror in
// `estimation_core.R` (`RANK_TIE_TOL`) MUST hold the same number; on the log
// scale that mirror ranks the coordination family on, the equivalent form is
// additive (log(1 + tol) = tol to first order).
//
// Sized from the measured floating-point spread: the backends agree on
// per-event probabilities to a few parts in 1e13, while genuinely distinct
// levels on the fixtures are separated by factors of order 10, so 1e-12 sits
// above the noise and far below any real structure.
static const double RANK_TIE_TOL = 1e-12;

// Rank of the observed alternative: 1 + the number of alternatives whose
// weight exceeds the observed one by more than `RANK_TIE_TOL` relatively
// (rank 1 = most likely). Ties — exact ones, and near-ties within the
// tolerance — share the better rank. Scale-free, so `c` never enters and the
// rule is identical in both families and on either of the two proportional
// scales a kernel may hold. Masked positions hold w = 0 and so can only
// outrank an observed alternative whose own weight underflowed, which no scale
// reaches.
int rank_of_observed(
    const arma::vec& w,
    const arma::vec& allowed,
    arma::uword obs
);

// One side of a margin accumulation: a mapping from risk-set position to
// accumulator index, plus the observed/expected accumulators it feeds. Three of
// the six event-loop kernels are two-sided — REM and REM_ordered scatter one
// contribution into sender and receiver margins, MM into both endpoints of a
// coordination tie — so the reduction takes a list of sides and walks the risk
// set once, rather than being called per side and walking REM's n1 x n2 set
// twice.
//
// `index` may be null when position IS the accumulator index (the single-sided
// dense kernels), which avoids materializing 0..n-1 per event.
struct margin_side {
  const arma::uvec* index;
  arma::vec* observed;
  arma::vec* expected;

  margin_side(arma::vec* observed_, arma::vec* expected_,
              const arma::uvec* index_ = nullptr)
      : index(index_), observed(observed_), expected(expected_) {}
};

// Accumulate expected[actor(j)] += c * w_j over the risk set, and
// observed[actor(obs)] += 1 for a dependent event.
void accumulate_margins(
    const arma::vec& w,
    double c,
    const arma::vec& allowed,
    arma::uword obs,
    bool dependent,
    const std::vector<margin_side>& sides
);

// Per-actor availability over ONE interval's realized risk set, in two steps
// because the family shapes group differently: REM feeds two accumulator pairs
// from one dyad walk, coordination feeds ONE accumulator pair from two
// endpoint maps of the same dyad walk, and the actor-oriented families feed
// one pair from one walk in which position already IS actor.
//
// `mark_availability()` sets seen(slot) = 1 for every position in the risk set,
// so an actor at risk in many dyads of the same interval is marked once —
// membership, not position count. That distinction is the whole point: a REM
// risk set over 84 actors holds 6 972 dyads of which 166 contain any given
// actor, so counting positions would report an exposure 166 times the
// observation window, and `observed[j] / exposure[j]` would stop being an event
// rate per unit time at risk. The CALLER zeroes `seen` before the first mark,
// which is what lets coordination mark both endpoint maps into one indicator.
//
// Membership is read from `allowed` when there is one; an empty mask means
// every position is at risk, which is the gather backend's pre-masked slice.
// Unlike the weight-driven reductions above this cannot fall back on "w is
// zero off the risk set": an at-risk actor whose fitted weight underflows is
// still at risk.
void mark_availability(
    arma::uword n_positions,
    const arma::vec& allowed,
    const arma::uvec* index,
    arma::vec& seen
);

// Fold a marked membership indicator into the two availability quantities:
// `dt` into the exposure of every at-risk actor (EVERY interval,
// right-censored included — the compensator integrates over all exposure time)
// and 1 into its opportunity count (dependent events only — an interval that
// realizes no mover offered nobody an opportunity). The two mirror the two
// margins scales, and either accumulator may be null: a multinomial family
// defines no exposure time.
void accumulate_availability(
    const arma::vec& seen,
    double dt,
    bool dependent,
    arma::vec* exposure,
    arma::vec* n_opportunities
);

// The event's score contribution: X_obs (for a dependent event) minus the
// contribution-weighted mean statistic c * w' X. This is the increment the
// estimators already accumulate into the running derivative; storing it per
// event is what the "scores" primitive is.
arma::rowvec event_score_row(
    const arma::mat& X,
    const arma::vec& w,
    double c,
    arma::uword obs,
    bool dependent
);

// Fold one interval's expected-information block into the two on-demand
// returns of a weighted pass.
//
// `block` is the per-event Fisher every kernel already forms and discards, and
// `scale` is the family's per-interval factor -- 1 for the multinomial
// families, the timespan or the compensator for the exact-time ones -- so
// `scale * block` is exactly the I_k the running `fisher` receives. The two
// MUST be formed from the same product: a weight column of ones is what
// reproduces the total information, and that identity is the contract.
//
// `weights` is n_intervals x m and empty when no weighted return was asked
// for; `weighted` carries one p x p slice per column. A zero weight is
// skipped, which is what keeps a set of J period indicators O(n p^2) instead
// of O(n J p^2) and removes any need for a grouping-specialized path.
//
// `trace_out` is empty when the per-interval trace was not asked for. The
// trace cannot be expressed as a weighted sum of blocks -- asking for one
// block per interval would be the per-event storage this design refuses -- so
// it is accumulated beside the weights rather than through them.
void accumulate_event_information(
    const arma::mat& block,
    double scale,
    arma::uword id_event,
    const arma::mat& weights,
    arma::cube& weighted,
    arma::vec& trace_out
);

// Whether a weighted pass is running, and the accumulators sized for it. The
// nine kernels each need the same three lines of setup, so they live here: an
// m-slice cube when weights were supplied, and a length-n vector when the
// trace was asked for. Both stay empty otherwise, and the accumulator above
// reads emptiness as "not requested".
arma::mat as_event_weights(
    const Rcpp::Nullable<Rcpp::NumericMatrix>& weights,
    arma::uword n_events
);

// Scatter a per-event probability vector onto the whole node set, so a position
// is an actor id at every event rather than a position in that event's risk
// set. Positions off the risk set stay 0.
//
// This exists for the gather kernels, whose rows ARE the realized risk set and
// so carry no actor identity of their own; the event-loop kernels already hold
// full-length vectors and store them directly. `index_i` / `index_j` are the
// same 0-based per-row actor slots the margin reduction scatters through, and
// which of them are non-empty is what fixes the shape: both give the
// n_actors_1 x n_actors_2 grid of the dyad families, one alone gives a vector
// on that axis.
Rcpp::RObject scatter_event_probabilities(
    const arma::vec& probabilities,
    const arma::uvec& index_i,
    const arma::uvec& index_j,
    arma::uword n_actors_1,
    arma::uword n_actors_2
);

#endif
