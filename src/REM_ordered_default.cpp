#include <RcppArmadillo.h>
#include "broadcast_updates.h"
#include "flat_updates.h"
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// @inherit estimate_REM params return description

//' Calculation for estimating an REM-choice-ordered model
//' @noRd
// [[Rcpp::export]]
List estimate_REM_ordered(
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
    const bool active_dyad_is_point = false
) {
    // initialize stat_mat and numbers
    arma::mat stat_mat = stat_mat_init;
    int n_events = dep_event_mat.n_cols;
    int n_parameters = stat_mat.n_cols;
    // declare auxilliary variables
    double probability_current_receiver;
    arma::rowvec expected_stat_current_event(n_parameters);
    arma::mat fisher_current_event(n_parameters, n_parameters);
    int stat_mat_update_id = 0;
    int stat_mat_broadcast_id = 0;
    // declare return variables
    arma::mat fisher(n_parameters, n_parameters, fill::zeros);
    arma::mat derivative(1, n_parameters, fill::zeros);
    double logLikelihood = 0;
    arma::vec intervalLogL(n_events, fill::zeros);

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
    // `active_dyad` is the folded per-event risk mask (design D7/D11). At the
    // outer encoding it is the length-n2 receiver vector (cell (i, j) available
    // iff active_sender(i) & active_dyad(j)). At the point encoding it is a
    // flattened n1 x n2 mask (sender-major: dyad (i, j) at i * n_actors_2 + j)
    // with both presences n support folded in, maintained by a
    // (node1, node2, replace) buffer and read cell-wise.
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
                if (active_dyad_is_point) {
                    active_dyad(
                      (active_dyad_update(0, active_dyad_update_id) - 1) * n_actors_2 +
                      (active_dyad_update(1, active_dyad_update_id) - 1)
                    ) = active_dyad_update(2, active_dyad_update_id);
                } else {
                    active_dyad(active_dyad_update(0, active_dyad_update_id) - 1) =
                      active_dyad_update(1, active_dyad_update_id);
                }
                active_dyad_update_id++;
            }
        }

        // TO check(gutian): handle ignorant


        // We calculate the derivative, log-Likelihood,
        // and fisher information matrix of a current event
        // Reset auxiliary variables
        expected_stat_current_event.zeros();
        fisher_current_event.zeros();
        double normalizer = 0;
        // declare the ids of the sender and the receiver,
        const int id_sender = dep_event_mat(0, id_event) - 1;
        const int id_receiver = dep_event_mat(1, id_event) - 1;
        // go through all actor1-actor2 paris
        for (int i = 0; i < n_actors_1; ++i) {
          if (active_sender(i) == 1) {
            // declare the subviews of th stat mat corresponding
            // to the first sender
            const arma::mat& current_data_matrix =
              stat_mat.rows(i * n_actors_2, (i + 1) * n_actors_2 - 1);
            // deal with twomode and allow reflexive
            int not_allowed_receiver = -1;
            if (!twomode_or_reflexive) not_allowed_receiver = i;
            // point encoding: sender i's row starts at i * n_actors_2; outer
            // encoding: the receiver vector is read directly (offset 0).
            const int dyad_offset = active_dyad_is_point ? i * n_actors_2 : 0;
            // go through all receiver
            for (int j = 0; j < n_actors_2; j++) {
              if (active_dyad(dyad_offset + j) == 1 &&
                  (j != not_allowed_receiver)) {
                // exp_current_receiver is \exp(\beta^T s)
                double exp_current_receiver =
                  std::exp(dot(current_data_matrix.row(j), parameters));
                normalizer += exp_current_receiver;
                probability_current_receiver = exp_current_receiver;
                expected_stat_current_event +=
                  probability_current_receiver * (current_data_matrix.row(j));
                fisher_current_event +=
                  probability_current_receiver *
                  ((current_data_matrix.row(j).t()) *
                  (current_data_matrix.row(j)));
              }
            }
          }
        }
        // add the quantities of a current event to the variables to be returned
        // derivative
        expected_stat_current_event /= normalizer;
        derivative += stat_mat.row( id_sender * n_actors_2 + id_receiver);
        derivative -= expected_stat_current_event;
        // fisher matrix
        fisher_current_event /= normalizer;
        fisher_current_event -=
          expected_stat_current_event.t() * expected_stat_current_event;
        fisher += fisher_current_event;
        // logLikelihood
        intervalLogL(id_event) =
          log(std::exp(dot(
              stat_mat.row( id_sender * n_actors_2 + id_receiver), parameters
          )) / normalizer);
        logLikelihood += intervalLogL(id_event);
    }

    return List::create(
      Named("derivative") = derivative,
      Named("fisher") = fisher,
      Named("logLikelihood") = logLikelihood,
      Named("intervalLogL") = intervalLogL
    );
}
