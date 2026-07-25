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

// Rank of the observed alternative: 1 + the number of alternatives with a
// strictly greater weight (rank 1 = most likely). Ties share the better rank.
// Scale-free, so `c` never enters and the rule is identical in both families —
// and identical on the probability scale, since w -> p is a strictly monotone
// per-event rescaling. Masked positions hold w = 0 and so can only outrank an
// observed alternative whose own weight underflowed, which no scale reaches.
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

#endif
