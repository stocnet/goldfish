#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// @inheritParams estimate_REM

//' Calculation for estimating an DyNAM-coordination model
//'
//' Output the derivative of log-likelihood, the Fisher information matrix,
//'   the log-Likelihood, and the log-likelihood of each event given input data
//'
//' @noRd
// [[Rcpp::export]]
List estimate_DyNAM_MM(
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
    arma::mat derivative_current_event(1, n_parameters);
    arma::mat averaged_derivative_current_event(1, n_parameters);
    arma::mat expected_derivative_pij(1, n_parameters);
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
                // Construct a view for the i-th column of
                // the stat_matrix and do the impute
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

        // TO check(gutian): handle ignorant


        // We calculate the derivative, log-Likelihood,
        //  and fisher information matrix of a current event
        //  and add to the variables to be return in the end.
        // Reset auxiliary variables
        fisher_current_event.zeros();
        // declare the ids of the sender and the receiver,
        const int id_sender = dep_event_mat(0, id_event) - 1;
        const int id_receiver = dep_event_mat(1, id_event) - 1;


        // 1. we calculate p,
        arma::vec exps = arma::exp(stat_mat * parameters);
        // p(j,i) is the probability that i sends an invitation for j
        arma::mat p(exps.memptr(), n_actors_2, n_actors_1, false);
        // We don't consider any self-connected edge
        if (!twomode_or_reflexive) p.diag().zeros();
        for (int i = 0; i < n_actors_1; ++i) {
            for (int j = 0; j < n_actors_2; ++j) {
                if (presence1[i] == false || presence2[j] == false) {
                    p(j, i) = 0;
                }
            }
        }
        // normalize each column such that the sum of each column is 1.
        p = normalise(p, 1, 0);

        // 2. we calculate P according to equation(6) in the paper.
        arma::mat P = p  % (p.t());
        // Norallize
        P = P / (accu(P) / 2);




        // 3. We calculate and store
        //  $\frac{p'_ij}{p_ij} + \frac{p'_ji}{p_ji}$,
        //  which appear in equation(9) in the paper.
        // For j < i, P_3.row(i * n_actors_2 + j) =
        //  \frac{p'_ij}{p_ij} + \frac{p'_ji}{p_ji}
        // For j >= i,  P_3.row(i * n_actors_2 + j) =
        //  \frac{p'_ji}{p_ji} and is not used in the future.
        arma::mat P_3 = stat_mat;
        // Calculate \frac{p'_ji}{p_ji}
        for (int i = 0; i < n_actors_1; ++i) {
            expected_derivative_pij.zeros();
            for (int j = 0; j < n_actors_2; ++j) {
                expected_derivative_pij +=
                  stat_mat.row(i * n_actors_2 + j) * p(j, i);
            }
            for (int j = 0; j < n_actors_2; ++j) {
                P_3.row(i * n_actors_2 + j) -=
                  expected_derivative_pij;
            }
        }
        // Calculate \frac{p'_ij}{p_ij} + \frac{p'_ji}{p_ji} by summation
        for (int i = 0; i < n_actors_1; ++i) {
            for (int j = 0; j < i; ++j) {
                P_3.row(i * n_actors_2 + j) += P_3.row(j * n_actors_2 + i);
            }
        }

        // 4. Calculate the derivative of loglikelihood according to equation(9) in the paper.
        derivative_current_event.zeros();
        averaged_derivative_current_event.zeros();
        // Calculate the last term in equation (9)
        for (int i = 0; i < n_actors_1; ++i) {
            for (int j = 0; j < i; ++j) {
                averaged_derivative_current_event +=
                  P(i, j) * P_3.row(i * n_actors_2 + j);
            }
        }
        // Equation(9)
        derivative_current_event -= averaged_derivative_current_event;
        derivative_current_event +=
          (id_sender > id_receiver) ?
          P_3.row(id_sender * n_actors_2 + id_receiver) :
          P_3.row(id_receiver * n_actors_2 + id_sender);
        // add the derivative of current event to the overall derivative
        derivative += derivative_current_event;


        // 5. Calculate the Fisher information matrix according to equation(11) in the paper
        fisher_current_event.zeros();
        for (int i = 0; i < n_actors_1; ++i) {
            for (int j = 0; j < i; ++j) {
                // temp = P_3.row(i * n_actors_2 + j) - averaged_derivative_current_event;
                fisher_current_event +=
                  P(i, j) * (P_3.row(i * n_actors_2 + j).t() *
                  P_3.row(i * n_actors_2 + j));
                // fisher_current_event = P(i,j) * (temp.t()* temp);
            }
        }
        fisher_current_event -=
          averaged_derivative_current_event.t() *
          averaged_derivative_current_event;
        fisher += fisher_current_event;

        // 6. loglikelihood
        intervalLogL(id_event) = log(P(id_sender, id_receiver));
        logLikelihood += intervalLogL(id_event);
    }

    return List::create(
      Named("derivative") = derivative,
      Named("fisher") = fisher,
      Named("logLikelihood") = logLikelihood,
      Named("intervalLogL") = intervalLogL
    );
}


