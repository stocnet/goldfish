#include <RcppArmadillo.h>
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
    const arma::vec& presence1_init,
    const arma::mat& presence1_update,
    const arma::vec& presence1_update_pointer,
    const arma::vec& presence2_init,
    const arma::mat& presence2_update,
    const arma::vec& presence2_update_pointer,
    const int n_actors_1,
    const int n_actors_2,
    const bool twomode_or_reflexive,
    bool impute = true
) {
    // initialize stat_mat and numbers
    arma::mat stat_mat = stat_mat_init;
    int n_events = dep_event_mat.n_cols;
    int n_parameters = stat_mat.n_cols;
    // declare auxilliary variables
    arma::rowvec expected_stat_current_event(n_parameters);
    arma::mat fisher_current_event(n_parameters, n_parameters);
    int stat_mat_update_id = 0;
    // declare return variables
    arma::mat fisher(n_parameters, n_parameters, fill::zeros);
    arma::mat derivative(1, n_parameters, fill::zeros);
    double logLikelihood = 0;
    arma::vec intervalLogL(n_events, fill::zeros);


    // Check whether there are composition change and initialize 
    // the presence of actor1 and actor2 
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

        // We calculate the derivative, log-Likelihood,
        //  and fisher information matrix of a current event
        // Reset auxiliary variables
        expected_stat_current_event.zeros();
        fisher_current_event.zeros();
        double normalizer = 0;
        // declare the ids of the sender and the receiver,
        const int id_sender = dep_event_mat(0, id_event) - 1;
        arma::mat reduced_stat_mat =
          reduce_mat_to_vector(stat_mat, n_actors_1, n_actors_2,
                               twomode_or_reflexive);
        // go through all actor1
        for (int i = 0; i < n_actors_1; ++i) {
            if (presence1(i) == 1) {
                // exp_current_sender is \exp(\beta^T s)
                double exp_current_sender =
                  std::exp(dot(reduced_stat_mat.row(i), parameters));
                normalizer += exp_current_sender;
                expected_stat_current_event +=
                  exp_current_sender * (reduced_stat_mat.row(i));
                fisher_current_event +=
                  exp_current_sender *
                  ((reduced_stat_mat.row(i).t()) * (reduced_stat_mat.row(i)));
            }
        }
        // add the quantities of a current event to the variables to be returned
        // derivative
        expected_stat_current_event /= normalizer;
        derivative += reduced_stat_mat.row(id_sender);
        derivative -= expected_stat_current_event;
        // fisher matrix
        fisher_current_event /= normalizer;
        fisher_current_event -=
          expected_stat_current_event.t() * expected_stat_current_event;
        fisher += fisher_current_event;
        // logLikelihood
        intervalLogL(id_event) =
          log(std::exp(dot(reduced_stat_mat.row(id_sender), parameters)) /
            normalizer);
        logLikelihood += intervalLogL(id_event);
    }

    return List::create(
      Named("derivative") = derivative,
      Named("fisher") = fisher,
      Named("intervalLogL") = intervalLogL,
      Named("logLikelihood") = logLikelihood
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
