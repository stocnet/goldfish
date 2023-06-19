#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;
#include "gather_progress.hpp"

// @inherit estimate_REM params

//' Gathering data for sender receiver model
//'
//' Gathering data for model that considers all present sender-receiver pairs,
//'   i.e. REM, REM-ordered, and DyNAM-coordination models.
//'   Only the useful information are recorded, 
//'   e.g. if a actor1-actor2 pair is not present in an event,
//'   then its information is  not gathered by this function.
//' @param verbose An boolean variable.
//'   It it's true, the function print the progress to the screen.
//' @return Return a list with elements as follows.
//' The meaning of the argument can be found in corresponding computation codes,
//' e.g. compute_coordination_selection.cpp.
//' @noRd
// [[Rcpp::export]]
List gather_sender_receiver_model(
    const arma::mat& dep_event_mat,
    const arma::vec& is_dependent,
    const arma::mat& stat_mat_init,
    const arma::mat& stat_mat_update,
    const arma::vec& stat_mat_update_pointer,
    const arma::mat& stat_mat_rightcensored_update,
    const arma::vec& stat_mat_rightcensored_update_pointer,
    const arma::vec& presence1_init,
    const arma::mat& presence1_update,
    const arma::vec& presence1_update_pointer,
    const arma::vec& presence2_init,
    const arma::mat& presence2_update,
    const arma::vec& presence2_update_pointer,
    const int n_actors_1,
    const int n_actors_2,
    const bool twomode_or_reflexive,
    const bool verbose,
    const bool impute
) {
    // initialize stat_mat and numbers
    arma::mat stat_mat = stat_mat_init;
    int n_events = is_dependent.n_elem;
    int n_parameters = stat_mat.n_cols;
    // declare auxilliary variables
    int stat_mat_update_id = 0;
    int stat_mat_rightcensored_update_id = 0;
    int id_dep_event = 0;
    int n_total = 0;


    // composition change
    bool has_composition_change1 = true;
    int presence1_update_id = 0;
    if (presence1_update.n_elem == 0) {
        has_composition_change1 = false;
    }
    arma::vec presence1 = presence1_init;

    bool has_composition_change2 = true;
    int presence2_update_id = 0;
    if (presence2_update.n_elem == 0) {
        has_composition_change2 = false;
    }
    arma::vec presence2 = presence2_init;
    // declare return variables
    int size_stat_all_events = n_events * n_actors_2 * 4;
    arma::mat stat_all_events(size_stat_all_events, n_parameters, fill::zeros);
    arma::vec selected(n_events, fill::zeros);
    arma::vec selected_actor1(n_events, fill::zeros);
    arma::vec selected_actor2(n_events, fill::zeros);
    arma::vec n_candidates(n_events, fill::zeros);
    arma::vec n_candidates1(n_events, fill::zeros);
    arma::vec n_candidates2(n_events, fill::zeros);

    // Go through all events
    for (int id_event = 0; id_event < n_events; id_event++) {
        // Print the progress to  consoles if verbose
        if (verbose) {
            if (id_event % 20 == 0) {
                gather_progress_render(id_event, n_events);
            }
        }

        // stat_mat_update
        if (is_dependent(id_event)) {
          while (stat_mat_update_id < stat_mat_update_pointer(id_dep_event)) {
            stat_mat(
              stat_mat_update(0, stat_mat_update_id) * n_actors_2 +
                stat_mat_update(1, stat_mat_update_id),
                stat_mat_update(2, stat_mat_update_id)) =
                  stat_mat_update(3, stat_mat_update_id);
            stat_mat_update_id++;
          }
        } else { while (stat_mat_rightcensored_update_id <
          stat_mat_rightcensored_update_pointer(id_event - id_dep_event)) {
          stat_mat(
           stat_mat_rightcensored_update(0, stat_mat_rightcensored_update_id) *
            n_actors_2 +
            stat_mat_rightcensored_update(1, stat_mat_rightcensored_update_id),
           stat_mat_rightcensored_update(2, stat_mat_rightcensored_update_id)) =
           stat_mat_rightcensored_update(3, stat_mat_rightcensored_update_id);
          stat_mat_rightcensored_update_id++;
        }
        }
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

        // composition change
        if (has_composition_change1) {
            while (presence1_update_id < presence1_update_pointer(id_event)) {
                presence1(presence1_update(0, presence1_update_id) - 1) =
                  presence1_update(1, presence1_update_id);
                presence1_update_id++;
            }
        }
        if (has_composition_change2) {
            while (presence2_update_id < presence2_update_pointer(id_event)) {
                presence2(presence2_update(0, presence2_update_id) - 1) =
                  presence2_update(1, presence2_update_id);
                presence2_update_id++;
            }
        }


        // Declare variables for counting the number of present pairs,
        //  actors1, actors2 in this event
        int n_present = 0;
        int n_present_actor1 = 0;
        int n_present_actor2 = 0;

        // Resize the data matrix such that it's big enough to contain the data.
        while (n_total + n_actors_1 * n_actors_2 > size_stat_all_events) {
            size_stat_all_events = size_stat_all_events * 2;
            stat_all_events.resize(size_stat_all_events, n_parameters);
        }


        // We calculate the derivative, log-Likelihood,
        //  and hessian matrix of a current event according to the paper
        //  and add to the variables to be return in the end.
        //  The three quantities are unnormalized until having gone through
        //  all receivers.
        // Reset auxiliary variables
        // declare the ids of the sender and the receiver,
        const int id_sender = dep_event_mat(0, id_event) - 1;
        const int id_receiver = dep_event_mat(1, id_event) - 1;
        const bool is_dependent_current_event = is_dependent(id_event);
        for (int i = 0; i < n_actors_1; ++i) {
            if (presence1(i) == 1) {
                n_present_actor2 = 0;
                // declare the subviews of th stat mat corresponding to
                // the first sender
                const arma::mat& current_data_matrix =
                  stat_mat.rows(i * n_actors_2, (i + 1) * n_actors_2 - 1);
                // deal with twomode and allow reflexive
                int not_allowed_receiver = -1;
                if (!twomode_or_reflexive) not_allowed_receiver = i;
                // go through all receiver
                for (int j = 0; j < n_actors_2; j++) {
                    if (presence2(j) == 1 && (j != not_allowed_receiver)) {
                        // exp_current_receiver is \exp(\beta^T s)
                        stat_all_events.row(n_total) =
                          current_data_matrix.row(j);
                        if ((is_dependent_current_event) &&
                            (i == id_sender) && (j == id_receiver)) {
                            selected(id_event) = n_present;
                            selected_actor1(id_event) = n_present_actor1;
                            selected_actor2(id_event) = n_present_actor2;
                            id_dep_event++;
                        }
                        n_total++;
                        n_present++;
                        n_present_actor2++;
                    }
                }
                n_present_actor1++;
            }
        }
        // Recording the numbers of the present pairs, the present actor1,
        //  and the present actor2 in this event.
        n_candidates(id_event) = n_present;
        n_candidates1(id_event) = n_present_actor1;
        n_candidates2(id_event) = n_present_actor2;
    }
    // clear the screen(console)
    if (verbose) gather_progress_terminate();

    return List::create(
      Named("stat_all_events") = stat_all_events.rows(0, n_total - 1),
      Named("n_candidates") = n_candidates,
      Named("n_candidates1") = n_candidates1,
      Named("n_candidates2") = n_candidates2,
      Named("selected") = selected,
      Named("selected_actor1") = selected_actor1,
      Named("selected_actor2") = selected_actor2
    );
}
