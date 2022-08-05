#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// @inherit estimate_REM return

//' Estimate a DyNAM-coordination model with gathered data
//'
//' Given the gathered and distilled data, it outputs the derivative of the loglikelihood, the Fisher information matrix, the logLikelihood,
//'     and the loglikelihood of each event  for DyNAM-coordination models.
//'
//' @param parameters An n_effects by 1 matrix, which is the input parameter
//' @param stat_all_events An matrix with n_effects columns.
//'         Each row represent the values of all effects of a sender-receiver pair in an event. For example, for a model with 2 effects,
//'         stat_all_events might looks like this.
//'         \tabular{rr}{
//'           3.2 \tab 1.9\cr
//'           3.2 \tab 4.5\cr
//'           1.2 \tab 5.2\cr
//'           4.3 \tab 3.1\cr
//'           2.4 \tab 4.7\cr
//'           9.2 \tab 5.6\cr
//'           2.9 \tab 8.9\cr
//'           ... \tab ...\cr
//'         }
//'     The first row means in a event, the values of the two effects of a candidate pair is (3.2,1.9).
//' @param n_candidates An n_events by 1 matrix. It record how many candidate sender-receiver pairs are in each event.
//'         From this, we can know which row in stat_all_events belongs to which event. For example if the first two element of
//'         n_candidates are (2, 4), then the first 2 rows of stat_all_events correspond to  the candidate pairs in the first event,
//'         which is
//'         \tabular{rr}{
//'           3.2 \tab 1.9\cr
//'           3.2 \tab 4.5\cr
//'         }
//'         And the 3rd-6th rows correspond to the candidate pairs in the  second event, which is
//'         \tabular{rr}{
//'           1.2 \tab 5.2\cr
//'           4.3 \tab 3.1\cr
//'           2.4 \tab 4.7\cr
//'           9.2 \tab 5.6\cr
//'         }
//' @param n_candidates1 An n_events by 1 matrix, which is only used for estimating the DyNAM-coordination model.
//'         it record how many candidate sender are in each event. And we have n_candidates1 * n_candidates2 = n_candidates.
//' @param n_candidates2 An n_events by 1 matrix, which is ddonly used for estimating the DyNAM-coordination model.
//'         it record how many candidate receiver are in each event. And we have n_candidates1 * n_candidates2 = n_candidates.
//' @param selected An n_events by 1 matrix.
//'         It records the position of the selected candidate sender-receiver pair in each event.
//'         For example  if the first two element of n_candidates are 2, 4, and the first two elements of selected is (0,2). Then the
//'         value of the effects of the pair selected in the first event is (3.2,1.9), which is the 1st(=0+1) row of
//'         \tabular{rr}{
//'           3.2 \tab 1.9\cr
//'           3.2 \tab 4.5\cr
//'         }
//'         And the value of the effects of the pair selected in the second event is (2.4, 4.7), which is the 3rd(=2+1) row of
//'         \tabular{rr}{
//'           1.2 \tab 5.2\cr
//'           4.3 \tab 3.1\cr
//'           2.4 \tab 4.7\cr
//'           9.2 \tab 5.6\cr
//'         }
//' @param selected_actor1 An n_events by 1 matrix.
//'         It records the index of the selected candidate sender among all candidate sender in each event.
//' @param selected_actor2 An n_events by 1 matrix.
//'         It records the index of the selected candidate receiver among all candidate receiver in each event.
//' @noRd
// [[Rcpp::export]]
List compute_coordination_selection(arma::colvec& parameters,
                                    const arma::mat& stat_all_events,
                                    const arma::uvec& n_candidates,
                                    const arma::uvec& n_candidates1,
                                    const arma::uvec& n_candidates2,
                                    const arma::uvec& selected,
                                    const arma::uvec& selected_actor1,
                                    const arma::uvec& selected_actor2,
                                    const bool twomode_or_reflexive) {
    int n_events = selected.size();
    int n_parameters = parameters.size();
    // declare auxilliary variables
    arma::mat derivative_current_event(1, n_parameters);
    arma::mat averaged_derivative_current_event(1, n_parameters);
    arma::mat expected_derivative_pij(1, n_parameters);
    arma::mat fisher_current_event(n_parameters, n_parameters);
    // declare return variables
    arma::mat fisher(n_parameters, n_parameters, fill::zeros);
    arma::mat derivative(1, n_parameters, fill::zeros);
    double logLikelihood = 0;
    arma::vec intervalLogL(n_events, fill::zeros);

    // Get exp(\beta^T S) for each pair of actors in each events
    arma::vec exps_all = arma::exp(stat_all_events * parameters);
    // start address in stat_all_events of current events
    int id_start = 0;

    // Go through all events
    for (int id_event = 0; id_event < n_events; id_event++) {
        // Initialize data for each event
        int id_end = id_start + n_candidates(id_event);
        // the subviewsof th stat mat
        const arma::mat& stat_mat_current_event = stat_all_events.rows(id_start, id_end - 1);

        // Reset auxilliary variables
        fisher_current_event.zeros();
        // declare the ids of the sender and the receiver,
        const int id_sender = selected_actor1(id_event);
        const int id_receiver = selected_actor2(id_event);
        const int n_actors_1 = n_candidates1(id_event);
        const int n_actors_2 = n_candidates2(id_event);






        // p(j,i) is the probability that i sends an invitation for j.
        // We retrieve it from
        arma::mat p(exps_all.memptr() + id_start, n_actors_2, n_actors_1, false);
        // We don't consider any self-connected edge
        if (!twomode_or_reflexive) p.diag().zeros();
        // normalize each column such that the sum of each column is 1.
        p = normalise(p, 1, 0);

        // 2. we calculate P according to equation(6) in the paper.
        arma::mat P = p  % (p.t());
        // Norallize
        P = P / (accu(P) / 2);




        // 3. We calculate and store $\frac{p'_ij}{p_ij} + \frac{p'_ji}{p_ji}$, which appear in equation(9) in the paper.
        // For j < i, P_3.row(i * n_actors_2 + j) = \frac{p'_ij}{p_ij} + \frac{p'_ji}{p_ji}
        // For j >= i,  P_3.row(i * n_actors_2 + j) = \frac{p'_ji}{p_ji} and is not used in the future.
        arma::mat P_3 = stat_mat_current_event;
        // Calculate \frac{p'_ji}{p_ji}
        for (int i = 0; i < n_actors_1; ++i) {
            expected_derivative_pij.zeros();
            for (int j = 0; j < n_actors_2; ++j) {
                expected_derivative_pij += stat_mat_current_event.row(i * n_actors_2 + j) * p(j, i);
            }
            for (int j = 0; j < n_actors_2; ++j) {
                P_3.row(i * n_actors_2 + j) -= expected_derivative_pij;
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
                averaged_derivative_current_event += P(i, j) * P_3.row(i * n_actors_2 + j);
            }
        }
        // Equation(9)
        derivative_current_event -= averaged_derivative_current_event;
        derivative_current_event +=  (id_sender > id_receiver) ? P_3.row(id_sender * n_actors_2 + id_receiver) : P_3.row(id_receiver * n_actors_2 + id_sender);
        // add the derivative of current event to the overall derivative
        derivative += derivative_current_event;


        // 5. Calculate the Fisher matrix according to equation(11) in the paper
        fisher_current_event.zeros();
        // arma::mat temp(1, n_parameters);
        for (int i = 0; i < n_actors_1; ++i) {
            for (int j = 0; j < i; ++j) {
                // temp = P_3.row(i * n_actors_2 + j) - averaged_derivative_current_event;
                fisher_current_event += P(i, j) * (P_3.row(i * n_actors_2 + j).t() * P_3.row(i * n_actors_2 + j));
                // fisher_current_event = P(i,j) * (temp.t()* temp);
            }
        }
        fisher_current_event -= averaged_derivative_current_event.t() * averaged_derivative_current_event;
        fisher += fisher_current_event;

        // 6. loglikelihood
        intervalLogL(id_event) = log(P(id_sender, id_receiver));
        logLikelihood += intervalLogL(id_event);






        // renew the starting index
        id_start = id_end;
    }

    return List::create(Named("derivative") = derivative,
                        Named("fisher") = fisher,
                        Named("logLikelihood") = logLikelihood,
                        Named("intervalLogL") = intervalLogL);
}
