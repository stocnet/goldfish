#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;
#include "gather_progress.hpp"

// @inherit estimate_REM params

//' Gathering data for sender receiver model
//'
//' Gathering data for model that for choosing an receiver given a sender,
//' i.e. DyNAM-choice model.
//' Only the useful information are recorded: Consider a actor1-actor2 pair,
//' if the actor1 is not the sender of the current event
//' or the actor2 is not present, then the information is not collected.
//' @param verbose An boolean variable. It it's true, the function print
//' the progress to the screen.
//' @return Return a list with elements as follows. The meaning of the argument
//' can be found in corresponding computation codes,
//' e.g. compute_coordination_selection.cpp.
//' @noRd
// [[Rcpp::export]]
List gather_receiver_model(
    const arma::mat& dep_event_mat,
    const arma::mat& stat_mat_init,
    const arma::mat& stat_mat_update,
    const arma::vec& stat_mat_update_pointer,
    const arma::vec& presence2_init,
    const arma::mat& presence2_update,
    const arma::vec& presence2_update_pointer,
    const int n_actor1,
    const int n_actor2,
    const bool twomode_or_reflexive,
    const bool verbose = false,
    const int impute = true
) {
    // initialize stat_mat and numbers
    arma::mat stat_mat = stat_mat_init;
    int n_events = dep_event_mat.n_cols;
    int n_parameters = stat_mat.n_cols;
    // declare auxilliary variables
    int stat_mat_update_id = 0;
    int n_total = 0;
    // to check(gutian): deal with composition change of actors1
    // composition change
    bool has_composition_change = true;
    int presence2_update_id = 0;
    if (presence2_update.n_elem == 0) {
        has_composition_change = false;
    }
    arma::vec presence2 = presence2_init;
    // declare return variables
    arma::mat stat_all_events(n_events * n_actor2, n_parameters, fill::zeros);
    arma::vec receivers(n_events, fill::zeros);
    arma::vec n_presence2(n_events, fill::zeros);


    // Go through all events
    for (int id_event = 0; id_event < n_events; id_event++) {
        // Print the progress to  consoles if verbose
        if (verbose) {
            if (id_event % 20 == 0) {
                gather_progress_render(id_event, n_events);
            }
        }

        // update stat mat
        int n_present = 0;
        while (stat_mat_update_id < stat_mat_update_pointer(id_event)) {
            stat_mat(stat_mat_update(0, stat_mat_update_id) * n_actor2 +
                     stat_mat_update(1, stat_mat_update_id),
                     stat_mat_update(2, stat_mat_update_id)) =
              stat_mat_update(3, stat_mat_update_id);
            stat_mat_update_id++;
        }

        // impute the missing statistics if necessary
        if (impute) {
            for (int i = 0; i < n_parameters; i++) {
                // Construct a view for the i-th column of
                //  the stat_matrix and do the impute
                arma::vec current_col(
                    stat_mat.colptr(i),
                    n_actor1 * n_actor2,
                    false
                );
                current_col.elem(find_nonfinite(current_col)).fill(
                    mean(current_col.elem(find_finite(current_col))));
            }
        }

        // composition change
        if (has_composition_change) {
            while (presence2_update_id < presence2_update_pointer(id_event)) {
                presence2(presence2_update(0, presence2_update_id) - 1) =
                  presence2_update(1, presence2_update_id);
                presence2_update_id++;
            }
        }

        // In the following part, we take out the rows corresponding to
        // pairs of present senders and receiver in this event.
        // declare the ids of the sender and the receiver, and subviews
        // the stat mat corresponding to this event.
        const int id_sender = dep_event_mat(0, id_event) - 1;
        const int id_receiver = dep_event_mat(1, id_event) - 1;
        const arma::mat& stat_mat_current_event =
          stat_mat.rows(id_sender * n_actor2, (id_sender + 1) * n_actor2 - 1);
        // deal with twomode and allow reflexive
        int not_allowed_receiver = -1;
        if (!twomode_or_reflexive) not_allowed_receiver = id_sender;
        // go through all receiver
        for (int j = 0; j < n_actor2; j++) {
            if (presence2(j) == 1 && (j != not_allowed_receiver)) {
                stat_all_events.row(n_total) = stat_mat_current_event.row(j);
                if (j == id_receiver) {
                    receivers(id_event) = n_present;
                }
                n_total++;
                n_present++;
            }
        }
        n_presence2(id_event) = n_present;
    }
    // clear the screen(console)
    if (verbose) gather_progress_terminate();

    return List::create(
      Named("stat_all_events") = stat_all_events.rows(0, n_total - 1),
      Named("n_candidates") = n_presence2,
      Named("selected") = receivers
    );
}

