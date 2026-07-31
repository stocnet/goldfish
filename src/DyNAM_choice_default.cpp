#include <RcppArmadillo.h>
#include "broadcast_updates.h"
#include "event_reductions.h"
#include "flat_updates.h"
#include "log_sum_exp.h"
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// @inherit estimate_REM params return description

//' Calculation for estimating an DyNAM choice model
//' @noRd
// [[Rcpp::export]]
List estimate_DyNAM_choice(
    const arma::vec& parameters,
    const arma::mat& dep_event_mat,
    const arma::mat& stat_mat_init,
    const arma::mat& stat_mat_update,
    const arma::vec& stat_mat_update_pointer,
    const arma::mat& stat_mat_broadcast,
    const arma::vec& stat_mat_broadcast_pointer,
    const arma::vec& active_dyad_init,
    const arma::mat& active_dyad_update,
    const arma::vec& active_dyad_update_pointer,
    const int n_actors_1,
    const int n_actors_2,
    const bool twomode_or_reflexive,
    bool impute,
    const bool active_dyad_is_point,
    const bool return_event_scores = false,
    const bool return_ranks = false,
    const bool return_margins = false,
    const bool return_probabilities = false,
    const bool return_availability = false,
    const Rcpp::Nullable<Rcpp::NumericMatrix> event_weights = R_NilValue,
    const bool return_event_information_trace = false
) {
    // initialize stat_mat and numbers
    arma::mat stat_mat = stat_mat_init;
    int n_events = dep_event_mat.n_cols;
    int n_parameters = stat_mat.n_cols;
    // declare auxilliary variables
    arma::rowvec expected_stat_current_event(n_parameters);
    arma::mat fisher_current_event(n_parameters, n_parameters);
    int stat_mat_update_id = 0;
    int stat_mat_broadcast_id = 0;
    // declare return variables
    arma::mat fisher(n_parameters, n_parameters, fill::zeros);
    arma::mat derivative(1, n_parameters, fill::zeros);
    double logLikelihood = 0;
    arma::vec intervalLogL(n_events, fill::zeros);
    // Opt-in per-event score matrix. Each row is the per-event
    // increment already accumulated into `derivative` (observed minus expected
    // statistic); allocated only when requested so the default path pays nothing.
    arma::mat event_scores;
    if (return_event_scores) event_scores.set_size(n_events, n_parameters);
    // Opt-in per-event rank of the observed alternative among the risk set
    // (rank 1 = highest fitted probability); allocated only when requested.
    IntegerVector observed_rank;
    if (return_ranks) observed_rank = IntegerVector(n_events, NA_INTEGER);
    // Opt-in receiver-margin accumulators (per-receiver observed and expected
    // event counts). `expected[r]` sums the fitted conditional-multinomial
    // probability p(r | s_k, t_k) over events, so it totals n exactly at any
    // parameter (each event's probabilities sum to 1); allocated only when
    // requested.
    arma::vec margin_observed;
    arma::vec margin_expected;
    if (return_margins) {
        margin_observed = arma::vec(n_actors_2, fill::zeros);
        margin_expected = arma::vec(n_actors_2, fill::zeros);
    }
    // Opt-in per-event probability vector over the WHOLE receiver set, zero off
    // the risk set: the shared max-shift helper already leaves disallowed
    // entries at 0, so `weights / normalizer` is the actor-indexed vector the
    // contract asks for with no scatter step. Allocated only when requested.
    List event_probabilities(return_probabilities ? n_events : 0);
    // Opt-in per-actor availability. A multinomial family defines no exposure
    // time, so only the opportunity counts are accumulated -- here over the
    // receiver set, where a risk-set position IS a receiver. Allocated only
    // when requested.
    arma::vec availability_opportunities;
    arma::vec availability_seen;
    if (return_availability) {
        availability_opportunities = arma::vec(n_actors_2, fill::zeros);
        availability_seen = arma::vec(n_actors_2, fill::zeros);
    }
    // Opt-in weighted per-interval information: one p x p slice per weight
    // column, and the per-interval trace. Both stay empty when not requested,
    // which is how the shared accumulator reads "not asked for".
    const arma::mat weights_mat =
      as_event_weights(event_weights, (arma::uword) n_events);
    arma::cube weighted_information;
    if (!weights_mat.is_empty()) {
        weighted_information.zeros(
          n_parameters, n_parameters, weights_mat.n_cols
        );
    }
    arma::vec event_information_trace;
    if (return_event_information_trace) {
        event_information_trace.zeros(n_events);
    }
    // Check whether there are composition change and initialize
    // the presence of actor2
    bool has_composition_change = true;
    int active_dyad_update_id = 0;
    if (active_dyad_update.n_elem == 0) {
        has_composition_change = false;
    }
    arma::vec active_dyad = active_dyad_init;
    // `active_dyad` is the folded per-event availability. At the alter
    // encoding it is the length-n2 receiver vector maintained by a (node, replace)
    // buffer. At the point encoding it is a flattened n1 x n2 mask (sender-major:
    // dyad (i, j) at i * n_actors_2 + j) — the folded receiver presence n support
    // n opportunity — maintained by a (node1, node2, replace) buffer; the risk set
    // reads the current sender's row, so no separate support matrix is needed.

    // Go through all events
    for (int id_event = 0; id_event < n_events; id_event++) {
        // update stat_mat
        apply_flat_updates(
          stat_mat, stat_mat_update, stat_mat_update_id,
          stat_mat_update_pointer(id_event), n_actors_2
        );
        apply_broadcast_updates(
          stat_mat, stat_mat_broadcast, stat_mat_broadcast_id,
          stat_mat_broadcast_pointer(id_event), n_actors_1, n_actors_2,
          twomode_or_reflexive
        );

        // impute the missing statistics if necessary
        if (impute) {
            for (int i = 0; i < n_parameters; i++) {
                // Construct a view for the i-th column of the stat_matrix
                // and do the impute
                arma::vec current_col(
                    stat_mat.colptr(i),
                    n_actors_1 * n_actors_2,
                    false
                );
                current_col.elem(find_nonfinite(current_col)).fill(
                    mean(current_col.elem(find_finite(current_col))));
            }
        }

        // update presence / availability (applied before this event's likelihood)
        if (has_composition_change) {
            while (active_dyad_update_id < active_dyad_update_pointer(id_event)) {
                if (active_dyad_is_point) {
                    active_dyad(
                      (active_dyad_update(0, active_dyad_update_id) - 1) *
                        n_actors_2 +
                      (active_dyad_update(1, active_dyad_update_id) - 1)
                    ) = active_dyad_update(2, active_dyad_update_id);
                } else {
                    active_dyad(active_dyad_update(0, active_dyad_update_id) - 1) =
                      active_dyad_update(1, active_dyad_update_id);
                }
                active_dyad_update_id++;
            }
        }



        // We calculate the derivative, logLikelihood,
        //  and hessian matrix of a current event according to the paper.
        // declare the ids of the sender and the receiver,
        const int id_sender = dep_event_mat(0, id_event) - 1;
        const int id_receiver = dep_event_mat(1, id_event) - 1;
        // declare the subviews of th stat mat corresponding to this event
        const arma::mat& current_data_matrix =
          stat_mat.rows(
            id_sender * n_actors_2,
            (id_sender + 1) * n_actors_2 - 1
          );
        int not_allowed_receiver = -1;
        if (!twomode_or_reflexive) not_allowed_receiver = id_sender;
        // At the point encoding the sender's row starts at id_sender * n_actors_2;
        // at the alter encoding the receiver vector is read directly (offset 0).
        const int dyad_offset =
          active_dyad_is_point ? id_sender * n_actors_2 : 0;
        // Staged, numerically stable softmax over the receivers:
        // one GEMV for the linear predictors, the shared max-shift helper for the
        // weights, then a weighted cross-product for the Fisher.
        arma::vec lin_pred = current_data_matrix * parameters;
        arma::vec allowed(n_actors_2, fill::zeros);
        for (int j = 0; j < n_actors_2; j++) {
            if (active_dyad(dyad_offset + j) == 1 &&
                (j != not_allowed_receiver)) {
                allowed(j) = 1;
            }
        }
        arma::vec weights;
        double log_normalizer = log_sum_exp_masked(lin_pred, allowed, weights);
        double normalizer = accu(weights);
        // Opt-in primitives via the shared reductions. Ranks read the shifted
        // weights directly (scale-free); margins take the probability vector
        // with c = 1, which is the multinomial family's only scale.
        arma::vec probabilities;
        if (return_margins || return_probabilities || return_event_scores) {
            probabilities = weights / normalizer;
        }
        if (return_availability) {
            availability_seen.zeros();
            mark_availability(
              n_actors_2, allowed, nullptr, availability_seen
            );
            accumulate_availability(
              availability_seen, 0.0, true, nullptr,
              &availability_opportunities
            );
        }
        if (return_ranks) {
            observed_rank[id_event] =
              rank_of_observed(weights, allowed, id_receiver);
        }
        if (return_margins) {
            std::vector<margin_side> sides;
            sides.push_back(margin_side(&margin_observed, &margin_expected));
            accumulate_margins(
              probabilities, 1.0, allowed, id_receiver, true, sides
            );
        }
        if (return_probabilities) {
            event_probabilities[id_event] =
              NumericVector(probabilities.begin(), probabilities.end());
        }
        expected_stat_current_event = (weights.t() * current_data_matrix) /
          normalizer;
        // derivative
        derivative += current_data_matrix.row(id_receiver);
        derivative -= expected_stat_current_event;
        // The stored per-event score comes from the shared reduction rather
        // than from the before/after difference of the running derivative, so
        // the definition lives in one place instead of once per kernel. The
        // derivative above is deliberately left as it was: it drives the
        // optimizer, and no coefficient may move.
        if (return_event_scores) {
            event_scores.row(id_event) =
              event_score_row(current_data_matrix, probabilities, 1.0, id_receiver, true);
        }
        // fisher matrix: sum_j p_j s_j s_j^T - E E^T
        fisher_current_event =
          (current_data_matrix.each_col() % weights).t() * current_data_matrix /
          normalizer -
          expected_stat_current_event.t() * expected_stat_current_event;
        fisher += fisher_current_event;
        accumulate_event_information(
          fisher_current_event, 1.0, id_event, weights_mat,
          weighted_information, event_information_trace
        );
        // logLikelihood from the shifted predictor (finite under underflow)
        intervalLogL(id_event) = lin_pred(id_receiver) - log_normalizer;
        logLikelihood += intervalLogL(id_event);
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
      Named("availability_n_opportunities") = availability_opportunities,
      Named("event_probabilities") = event_probabilities,
      Named("weighted_information") = weighted_information,
      Named("event_information_trace") = event_information_trace
    );
}


