#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// @inherit compute_coordination_selection params
// @inherit estimate_REM params return

//' Estimate a poisson selection model with gathered data
//'
//' Given the gathered and distilled data, it outputs the derivative of the loglikelihood, the Fisher information matrix, the logLikelihood,
//' and the loglikelihood of each event  for models with poisson selection processes, e.g. DyNAM-rate and REM-choice models.
//' @noRd
// [[Rcpp::export]]
List compute_poisson_selection(arma::colvec& parameters,
                               const arma::mat& stat_all_events,
                               const arma::uvec& n_candidates,
                               const arma::uvec& selected,
                               const arma::vec& timespan,
                               const arma::vec& is_dependent) {
    int n_events = timespan.size();
    int n_parameters = parameters.size();
    // declare auxilliary variables
    arma::mat weighted_sum_current_event(1, n_parameters, fill::zeros);
    arma::mat fisher_current_event(n_parameters, n_parameters, fill::zeros);
    // declare return variables
    arma::mat fisher(n_parameters, n_parameters, fill::zeros);
    arma::mat derivative(1, n_parameters, fill::zeros);
    arma::vec intervalLogL(n_events, fill::zeros);
    double logLikelihood = 0;

    // Get exp(\beta^T S) for each pair of actors in each events
    arma::vec exps = arma::exp(stat_all_events * parameters);
    // start address in stat_all_events of current events
    int id_start = 0;
    int id_dep_event = 0;

    // Go through all events
    for (int id_event = 0; id_event < n_events; id_event++) {
        // Initialize data for each event
        int id_end = id_start + n_candidates(id_event);
        bool is_dependent_current_event = is_dependent(id_event);
        double timespan_current_event = timespan(id_event);
        // the subviewsof th stat mat and exps corresponding to this event
        const arma::colvec& exp_current_event = exps.subvec(id_start, id_end - 1);
        const arma::mat& stat_mat_current_event = stat_all_events.rows(id_start, id_end - 1);
        // reset auxilliary variables
        weighted_sum_current_event.zeros();
        fisher_current_event.zeros();
        // declare the selected and the normalizer (partition function)
        int id_selected = selected(id_event);
        double normalizer = arma::sum(exp_current_event);
        // go through all candidates
        for (unsigned int j = 0; j < n_candidates(id_event); j++) {
            // probability_current_selected = exp_current_event(j) / normalizer;
            weighted_sum_current_event += exp_current_event(j) * (stat_mat_current_event.row(j));
            fisher_current_event += exp_current_event(j) * ((stat_mat_current_event.row(j).t()) * (stat_mat_current_event.row(j)));
        }
        // add the quantities of a current event to the variables to be returned
        // derivative
        // if (id_event == 1) Rcout << timespan_current_event * weighted_sum_current_event << std::endl;
        derivative -= timespan_current_event * weighted_sum_current_event;
        // fisher matrix
        fisher += timespan_current_event * fisher_current_event;
        // logLikelihood
        intervalLogL(id_event) = -timespan_current_event * normalizer;
        // update id_dep_event
        if (is_dependent_current_event) {
            intervalLogL(id_event) += dot(stat_mat_current_event.row(id_selected), parameters);
            derivative += stat_mat_current_event.row(id_selected);
            id_dep_event++;
        }
        // loglikelihood
        logLikelihood += intervalLogL(id_event);


        id_start = id_end;
    }

    return List::create(Named("derivative") = derivative,
                        Named("fisher") = fisher,
                        Named("intervalLogL") = intervalLogL,
                        Named("logLikelihood") = logLikelihood);
}
