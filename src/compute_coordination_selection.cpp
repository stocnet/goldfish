#include <RcppArmadillo.h>
#include "log_sum_exp.h"
#include "event_reductions.h"
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// @inherit estimate_REM return

//' Estimate a DyNAM-coordination model with gathered data
//'
//' Given the gathered and distilled data, it outputs the derivative of
//'     the loglikelihood, the Fisher information matrix, the logLikelihood,
//'     and the loglikelihood of each event  for DyNAM-coordination models.
//'
//' @param parameters An n_effects by 1 matrix, which is the input parameter
//' @param stat_all_events An matrix with n_effects columns.
//'     Each row represent the values of all effects of
//'     a sender-receiver pair in an event.
//'     For example, for a model with 2 effects, stat_all_events 
//'     might looks like this.
//'     \tabular{rr}{
//'       3.2 \tab 1.9\cr
//'       3.2 \tab 4.5\cr
//'       1.2 \tab 5.2\cr
//'       4.3 \tab 3.1\cr
//'       2.4 \tab 4.7\cr
//'       9.2 \tab 5.6\cr
//'       2.9 \tab 8.9\cr
//'       ... \tab ...\cr
//'     }
//'     The first row means in a event, the values of the two effects of
//'     a candidate pair is (3.2,1.9).
//' @param n_candidates An n_events by 1 matrix.
//'     It record how many candidate sender-receiver pairs are in each event.
//'     It indicates which row in stat_all_events belongs to which event.
//'     For example if the first two element of n_candidates are (2, 4),
//'     then the first 2 rows of stat_all_events correspond to
//'     the candidate pairs in the first event, which is
//'     \tabular{rr}{
//'       3.2 \tab 1.9\cr
//'       3.2 \tab 4.5\cr
//'     }
//'     And the 3rd-6th rows correspond to the candidate pairs in
//'     the second event, which is
//'     \tabular{rr}{
//'       1.2 \tab 5.2\cr
//'       4.3 \tab 3.1\cr
//'       2.4 \tab 4.7\cr
//'       9.2 \tab 5.6\cr
//'     }
//' @param selected An n_events by 1 matrix.
//'     It records the position of the selected candidate sender-receiver pair
//'     in each event.
//'     For example  if the first two element of n_candidates are 2, 4,
//'     and the first two elements of selected is (0,2).
//'     Then the value of the effects of the pair selected in the first event is
//'     (3.2,1.9), which is the 1st(=0+1) row of
//'     \tabular{rr}{
//'       3.2 \tab 1.9\cr
//'       3.2 \tab 4.5\cr
//'     }
//'     And the value of the effects of the pair selected in the second event is
//'     (2.4, 4.7), which is the 3rd(=2+1) row of
//'     \tabular{rr}{
//'       1.2 \tab 5.2\cr
//'       4.3 \tab 3.1\cr
//'       2.4 \tab 4.7\cr
//'       9.2 \tab 5.6\cr
//'     }
//'     `selected` is the within-event 0-based position of the observed directed
//'     dyad (sender -> receiver) row.
//' @param sender_of_row An integer vector, one entry per row of
//'     `stat_all_events`: the 0-based sender-group index of that row WITHIN its
//'     event (the CSR grouping the per-sender softmax consumes; rows are stored
//'     grouped by sender, so this is non-decreasing within an event).
//' @param dyad_partner An integer vector, one entry per row of
//'     `stat_all_events`: the within-event 0-based position of the partner row
//'     (j -> i) of each directed row (i -> j). The symmetric risk-set fold
//'     guarantees the partner exists, so each unordered dyad has exactly two
//'     rows pointing at each other.
//' @noRd
// [[Rcpp::export]]
List compute_coordination_selection(
    arma::colvec& parameters,
    const arma::mat& stat_all_events,
    const arma::uvec& n_candidates,
    const arma::uvec& selected,
    const arma::uvec& sender_of_row,
    const arma::uvec& dyad_partner,
    const arma::uvec& index_i,
    const arma::uvec& index_j,
    const arma::uword n_actors_1,
    const bool return_event_scores,
    const bool return_ranks,
    const bool return_margins,
    const bool return_probabilities = false
) {
    int n_events = selected.size();
    int n_parameters = parameters.size();
    // declare return variables
    arma::mat fisher(n_parameters, n_parameters, fill::zeros);
    arma::mat derivative(1, n_parameters, fill::zeros);
    double logLikelihood = 0;
    arma::vec intervalLogL(n_events, fill::zeros);

    // Linear predictors beta^T s for every candidate dyad in every event (one
    // GEMV); the staged dyad-triangle softmax below works in log space.
    arma::vec lin_pred_all = stat_all_events * parameters;

    // Ragged dyad-triangle buffers sized to the largest event and reused: the
    // kernel reads ONLY the emitted index structures — the risk set
    // (off-diagonal directed dyads, masked rows already dropped at emit) is the
    // row list itself, grouped by sender via `sender_of_row` and paired via
    // `dyad_partner`; there is no n1 x n2 assumption and no square reshape.
    // `E` per-sender expected statistics (row = sender group), `logZ` the
    // per-sender softmax log-normalizers, `D` the compact d x p deviation buffer.
    const arma::uword max_n_groups =
      sender_of_row.empty() ? 0 : sender_of_row.max() + 1;
    const arma::uword max_n_cand = n_candidates.empty() ? 0 : n_candidates.max();
    const arma::uword max_n_dyads = max_n_cand / 2;
    arma::mat E(max_n_groups, n_parameters);
    arma::vec logZ(max_n_groups);
    arma::mat D(max_n_dyads, n_parameters);
    arma::vec logw_dyad(max_n_dyads);
    arma::vec ones_mask(max_n_cand, fill::ones);
    arma::vec sender_weights;
    arma::vec dyad_weights;
    // Opt-in per-event primitives. The event's realized risk set is the DYAD
    // list, not the row list and not the per-sender CSR groups -- the score,
    // the likelihood and the softmax below all range over dyads -- so the
    // reductions run on the dyad-level probability vector (D14).
    arma::mat event_scores;
    if (return_event_scores) event_scores.set_size(n_events, n_parameters);
    IntegerVector observed_rank;
    if (return_ranks) observed_rank = IntegerVector(n_events, NA_INTEGER);
    arma::vec margin_observed, margin_expected;
    const bool do_margins = return_margins && index_i.n_elem > 0;
    // The dyad endpoints are also what an actor-indexed probability grid needs,
    // so they are built whenever EITHER primitive is requested.
    const bool need_endpoints =
      do_margins || (return_probabilities && index_i.n_elem > 0);
    if (do_margins) {
        margin_observed = arma::vec(n_actors_1, fill::zeros);
        margin_expected = arma::vec(n_actors_1, fill::zeros);
    }
    // A dyad credits BOTH its endpoints, into the same accumulator pair -- one
    // actor set, two contributions per dyad, matching the cpp MM kernel. Two
    // reduction sides over one pair of vectors expresses exactly that.
    arma::uvec dyad_endpoint_a(max_n_dyads);
    arma::uvec dyad_endpoint_b(max_n_dyads);
    arma::vec dyad_probabilities;
    // Opt-in per-event probability grid, actor-indexed over the whole node set
    // and zero off the risk set. Allocated only when requested.
    List event_probabilities(return_probabilities ? n_events : 0);
    // start address in stat_all_events of current events
    int id_start = 0;

    // Go through all events
    for (int id_event = 0; id_event < n_events; id_event++) {
        const int n_rows = n_candidates(id_event);
        const int id_end = id_start + n_rows;

        // per-sender softmax over the contiguous sender groups:
        // log-normalizer logZ_g and expected statistic E_g via one GEMV each.
        int row = 0;
        while (row < n_rows) {
            const int gid = sender_of_row(id_start + row);
            const int start = row;
            while (row < n_rows &&
                   static_cast<int>(sender_of_row(id_start + row)) == gid) {
                ++row;
            }
            const int gsize = row - start;
            arma::vec lin_pred_g(
                lin_pred_all.memptr() + id_start + start, gsize, false);
            arma::vec allowed_g(ones_mask.memptr(), gsize, false);
            logZ(gid) = log_sum_exp_masked(lin_pred_g, allowed_g, sender_weights);
            double norm_g = accu(sender_weights);
            E.row(gid) = (sender_weights.t() *
              stat_all_events.rows(id_start + start, id_start + row - 1)) / norm_g;
        }

        // dyad list: one row per unordered dyad, taken at its canonical directed
        // row (r < partner). log w = (x_r - logZ_{g(r)}) + (x_pr - logZ_{g(pr)}),
        // D_d = s_r + s_pr - E_{g(r)} - E_{g(pr)} = grad log w.
        const int n_dyads = n_rows / 2;
        const arma::uword id_obs_row = selected(id_event);
        int idx = 0;
        int idx_obs = 0;
        for (int r = 0; r < n_rows; ++r) {
            const arma::uword pr = dyad_partner(id_start + r);
            if (static_cast<arma::uword>(r) < pr) {
                const int g_r = sender_of_row(id_start + r);
                const int g_pr = sender_of_row(id_start + pr);
                logw_dyad(idx) =
                  (lin_pred_all(id_start + r) - logZ(g_r)) +
                  (lin_pred_all(id_start + pr) - logZ(g_pr));
                D.row(idx) =
                  stat_all_events.row(id_start + r) +
                  stat_all_events.row(id_start + pr) -
                  E.row(g_r) - E.row(g_pr);
                if (static_cast<arma::uword>(r) == id_obs_row ||
                    pr == id_obs_row) {
                    idx_obs = idx;
                }
                if (need_endpoints) {
                    // The canonical directed row carries both endpoints.
                    dyad_endpoint_a(idx) = index_i(id_start + r);
                    dyad_endpoint_b(idx) = index_j(id_start + r);
                }
                ++idx;
            }
        }

        // d-alternative dyad softmax, then one weighted-crossprod GEMM Fisher
        arma::vec logw_event(logw_dyad.memptr(), n_dyads, false);
        arma::vec allowed_event(ones_mask.memptr(), n_dyads, false);
        double log_normalizer =
          log_sum_exp_masked(logw_event, allowed_event, dyad_weights);
        double normalizer = accu(dyad_weights);
        arma::subview<double> D_event = D.rows(0, n_dyads - 1);
        // expected gradient g = sum_d P_d D_d; score = grad log w_obs - g
        arma::rowvec g = (dyad_weights.t() * D_event) / normalizer;
        derivative += D.row(idx_obs) - g;
        // Fisher: sum_d P_d D_d D_d^T - g^T g
        fisher += (D_event.each_col() % dyad_weights).t() * D_event /
          normalizer - g.t() * g;
        // Opt-in primitives, all reductions of the dyad-level probability
        // vector on the probability scale (c = 1).
        if (
          return_event_scores || return_ranks || do_margins ||
          return_probabilities
        ) {
            dyad_probabilities = dyad_weights / normalizer;
        }
        if (return_ranks) {
            observed_rank[id_event] =
              rank_of_observed(dyad_probabilities, allowed_event, idx_obs);
        }
        if (do_margins) {
            arma::uvec endpoint_a(dyad_endpoint_a.memptr(), n_dyads, false);
            arma::uvec endpoint_b(dyad_endpoint_b.memptr(), n_dyads, false);
            std::vector<margin_side> sides;
            sides.push_back(margin_side(
              &margin_observed, &margin_expected, &endpoint_a
            ));
            sides.push_back(margin_side(
              &margin_observed, &margin_expected, &endpoint_b
            ));
            accumulate_margins(
              dyad_probabilities, 1.0, allowed_event, idx_obs, true, sides
            );
        }
        if (return_event_scores) {
            event_scores.row(id_event) = event_score_row(
              D_event, dyad_probabilities, 1.0, idx_obs, true
            );
        }
        if (return_probabilities) {
            // Coordination's risk set is the unordered-pair list, so the grid
            // is symmetric: each dyad's probability lands on both sides of the
            // diagonal and the event totals 2, one per pair. Not the shared
            // scatter helper, which writes one direction per row.
            arma::mat grid(n_actors_1, n_actors_1, fill::zeros);
            for (int d = 0; d < n_dyads; ++d) {
                const arma::uword a = dyad_endpoint_a(d);
                const arma::uword b = dyad_endpoint_b(d);
                grid(a, b) = dyad_probabilities(d);
                grid(b, a) = dyad_probabilities(d);
            }
            event_probabilities[id_event] = wrap(grid);
        }
        // logLikelihood from the shifted predictor (finite under underflow)
        intervalLogL(id_event) = logw_dyad(idx_obs) - log_normalizer;
        logLikelihood += intervalLogL(id_event);

        // renew the starting index
        id_start = id_end;
    }

    return List::create(
      Named("derivative") = derivative,
      Named("fisher") = fisher,
      Named("logLikelihood") = logLikelihood,
      Named("intervalLogL") = intervalLogL,
      Named("event_scores") = event_scores,
      Named("observed_rank") = observed_rank,
      Named("margin_observed") = margin_observed,
      Named("margin_expected") = margin_expected,
      Named("event_probabilities") = event_probabilities
    );
}
