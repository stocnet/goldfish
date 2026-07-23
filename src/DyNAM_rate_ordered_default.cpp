#include <RcppArmadillo.h>
#include "broadcast_updates.h"
#include "flat_updates.h"
#include "stable_softmax.h"
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;


inline arma::mat reduce_mat_to_vector(
    const arma::mat& stat_mat,
    const int& n_actors_1,
    const int& n_actors_2,
    const bool& twomode_or_reflexive
);

// @inherit estimate_REM params return description

//' Calculation for estimating an DyNAM-rate-ordered model
//' @noRd
// [[Rcpp::export]]
List estimate_DyNAM_rate_ordered(
    const arma::vec& parameters,
    const arma::mat& dep_event_mat,
    const arma::mat& stat_mat_init,
    const arma::mat& stat_mat_update,
    const arma::vec& stat_mat_update_pointer,
    const arma::mat& stat_mat_broadcast,
    const arma::vec& stat_mat_broadcast_pointer,
    const arma::vec& active_sender_init,
    const arma::mat& active_sender_update,
    const arma::vec& active_sender_update_pointer,
    const arma::vec& active_dyad_init,
    const arma::mat& active_dyad_update,
    const arma::vec& active_dyad_update_pointer,
    const int n_actors_1,
    const int n_actors_2,
    const bool twomode_or_reflexive,
    bool impute = true,
    const bool return_event_scores = false,
    const bool return_ranks = false
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
    // Opt-in per-event rank of the observed sender among the risk set
    // (rank 1 = highest fitted probability); allocated only when requested.
    IntegerVector observed_rank;
    if (return_ranks) observed_rank = IntegerVector(n_events, NA_INTEGER);


    // Check whether there are composition change and initialize
    // the presence of actor1 and actor2 
    bool has_composition_change1 = true;
    int active_sender_update_id = 0;
    if (active_sender_update.n_elem == 0) {
        has_composition_change1 = false;
    }
    arma::vec active_sender = active_sender_init;

    bool has_composition_change2 = true;
    int active_dyad_update_id = 0;
    if (active_dyad_update.n_elem == 0) {
        has_composition_change2 = false;
    }
    arma::vec active_dyad = active_dyad_init;


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
                //  and do the impute
                arma::vec current_col(
                    stat_mat.colptr(i),
                    n_actors_1 * n_actors_2,
                    false
                );
                current_col.elem(find_nonfinite(current_col)).fill(
                    mean(current_col.elem(find_finite(current_col))));
            }
        }

        // update presence
        if (has_composition_change1) {
            while (active_sender_update_id < active_sender_update_pointer(id_event)) {
                active_sender(active_sender_update(0, active_sender_update_id) - 1) =
                  active_sender_update(1, active_sender_update_id);
                active_sender_update_id++;
            }
        }
        if (has_composition_change2) {
            while (active_dyad_update_id < active_dyad_update_pointer(id_event)) {
                active_dyad(active_dyad_update(0, active_dyad_update_id) - 1) =
                  active_dyad_update(1, active_dyad_update_id);
                active_dyad_update_id++;
            }
        }

        // We calculate the derivative, log-Likelihood,
        //  and fisher information matrix of a current event
        // declare the ids of the sender and the receiver,
        const int id_sender = dep_event_mat(0, id_event) - 1;
        arma::mat reduced_stat_mat =
          reduce_mat_to_vector(stat_mat, n_actors_1, n_actors_2,
                               twomode_or_reflexive);
        // Staged, numerically stable softmax over the senders:
        // one GEMV for the linear predictors, the shared max-shift helper for the
        // weights, then a weighted cross-product for the Fisher.
        arma::vec lin_pred = reduced_stat_mat * parameters;
        arma::vec allowed(n_actors_1, fill::zeros);
        for (int i = 0; i < n_actors_1; ++i) {
            if (active_sender(i) == 1) {
                allowed(i) = 1;
            }
        }
        arma::vec weights;
        double log_normalizer = stable_softmax_masked(lin_pred, allowed, weights);
        double normalizer = accu(weights);
        if (return_ranks) {
            const double obs_weight = weights(id_sender);
            int rank = 1;
            for (int i = 0; i < n_actors_1; ++i) {
                if (allowed(i) == 1 && weights(i) > obs_weight) rank++;
            }
            observed_rank[id_event] = rank;
        }
        expected_stat_current_event = (weights.t() * reduced_stat_mat) /
          normalizer;
        // derivative
        arma::rowvec score_before;
        if (return_event_scores) score_before = derivative.row(0);
        derivative += reduced_stat_mat.row(id_sender);
        derivative -= expected_stat_current_event;
        if (return_event_scores) {
            event_scores.row(id_event) = derivative.row(0) - score_before;
        }
        // fisher matrix: sum_i p_i s_i s_i^T - E E^T
        fisher_current_event =
          (reduced_stat_mat.each_col() % weights).t() * reduced_stat_mat /
          normalizer -
          expected_stat_current_event.t() * expected_stat_current_event;
        fisher += fisher_current_event;
        // logLikelihood from the shifted predictor (finite under underflow)
        intervalLogL(id_event) = lin_pred(id_sender) - log_normalizer;
        logLikelihood += intervalLogL(id_event);
    }

    return List::create(
      Named("derivative") = derivative,
      Named("fisher") = fisher,
      Named("intervalLogL") = intervalLogL,
      Named("logLikelihood") = logLikelihood,
      Named("event_scores") = event_scores,
      Named("observed_rank") = observed_rank
    );
}


inline arma::mat reduce_mat_to_vector(
    const arma::mat& stat_mat,
    const int& n_actors_1,
    const int& n_actors_2,
    const bool& twomode_or_reflexive
) {
    int n_parameters = stat_mat.n_cols;
    arma::mat reduced_data_mat(n_actors_1, n_parameters);
    arma::rowvec temp(n_parameters);
    for (int i = 0; i < n_actors_1; ++i) {
        temp.zeros();
        int id_start = i * n_actors_2;
        temp = sum(stat_mat.rows(id_start, id_start + n_actors_2 - 1), 0);
        if (!twomode_or_reflexive) {
            temp -= stat_mat.row(id_start + i);
            reduced_data_mat.row(i) = temp / (n_actors_2 - 1);
        } else {
            reduced_data_mat.row(i) = temp / n_actors_2;
        }
    }
    return reduced_data_mat;
}
