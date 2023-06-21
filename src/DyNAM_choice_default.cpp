#include <RcppArmadillo.h>
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
    const arma::vec& presence2_init,
    const arma::mat& presence2_update,
    const arma::vec& presence2_update_pointer,
    const int n_actors_1,
    const int n_actors_2,
    const bool twomode_or_reflexive,
    bool impute
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
    // declare return variables
    arma::mat fisher(n_parameters, n_parameters, fill::zeros);
    arma::mat derivative(1, n_parameters, fill::zeros);
    double logLikelihood = 0;
    arma::vec intervalLogL(n_events, fill::zeros);
    // Check whether there are composition change and initialize 
    // the presence of actor2
    bool has_composition_change = true;
    int presence2_update_id = 0;
    if (presence2_update.n_elem == 0) {
        has_composition_change = false;
    }
    arma::vec presence2 = presence2_init;


    // Go through all events
    for (int id_event = 0; id_event < n_events; id_event++) {
        // update stat_mat
        while (stat_mat_update_id < stat_mat_update_pointer(id_event)) {
          stat_mat(
            stat_mat_update(0, stat_mat_update_id) * n_actors_2 +
              stat_mat_update(1, stat_mat_update_id),
            stat_mat_update(2, stat_mat_update_id)) =
            stat_mat_update(3, stat_mat_update_id);
          stat_mat_update_id++;
        }

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

        // update presence
        if (has_composition_change) {
            while (presence2_update_id < presence2_update_pointer(id_event)) {
                presence2(presence2_update(0, presence2_update_id) - 1) =
                  presence2_update(1, presence2_update_id);
                presence2_update_id++;
            }
        }



        // We calculate the derivative, logLikelihood,
        //  and hessian matrix of a current event according to the paper.
        // Reset auxilliary variables
        expected_stat_current_event.zeros();
        fisher_current_event.zeros();
        // declare the ids of the sender and the receiver,
        const int id_sender = dep_event_mat(0, id_event) - 1;
        const int id_receiver = dep_event_mat(1, id_event) - 1;
        // declare the subviews of th stat mat corresponding to this event
        const arma::mat& current_data_matrix =
          stat_mat.rows(
            id_sender * n_actors_2,
            (id_sender + 1) * n_actors_2 - 1
          );
        double normalizer = 0;
        int not_allowed_receiver = -1;
        if (!twomode_or_reflexive) not_allowed_receiver = id_sender;
        // go through all actor2
        for (int j = 0; j < n_actors_2; j++) {
            if (presence2(j) == 1 && (j != not_allowed_receiver) ) {
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
        // add the quantities of a current event to the variables to be returned
        // derivative
        expected_stat_current_event /= normalizer;
        derivative += current_data_matrix.row(id_receiver);
        derivative -= expected_stat_current_event;
        // fisher matrix
        fisher_current_event /= normalizer;
        fisher_current_event -=
          expected_stat_current_event.t() * expected_stat_current_event;
        fisher += fisher_current_event;
        // logLikelihood
        intervalLogL(id_event) =
          log(std::exp(dot(current_data_matrix.row(id_receiver), parameters)) /
            normalizer);
        logLikelihood += intervalLogL(id_event);
    }

    return List::create(
      Named("derivative") = derivative,
      Named("fisher") = fisher,
      Named("logLikelihood") = logLikelihood,
      Named("intervalLogL") = intervalLogL
    );
}


