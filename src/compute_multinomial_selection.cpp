#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// @inherit compute_coordination_selection params
// @inherit estimate_REM params return

//' Estimate a multinomial selection model with gathered data
//'
//' Given the gathered and distilled data, it outputs the derivative of the loglikelihood, the Fisher information matrix, the logLikelihood,
//' and the loglikelihood of each event  for models with multinomial selection processes, e.g. DyNAM-rate-ordered, DyNAM-choice, and REM-choice models.
//' @noRd
// [[Rcpp::export]]
List compute_multinomial_selection(arma::colvec& parameters,
                                   const arma::mat& stat_all_events,
                                   const arma::uvec& n_candidates,
                                   const arma::uvec& selected) {
    int n_events = selected.size();
    int n_parameters = parameters.size();
    // declare auxilliary variables
    double probability_current_receiver;
    arma::mat expected_stat_current_event(1, n_parameters, fill::zeros);
    arma::mat fisher_current_event(n_parameters, n_parameters, fill::zeros);
    // declare return variables
    arma::mat fisher(n_parameters, n_parameters, fill::zeros);
    arma::mat derivative(1, n_parameters, fill::zeros);
    double logLikelihood = 0;
    arma::vec intervalLogL(n_events, fill::zeros);

    // Get exp(\beta^T S) for each pair of actors in each events
    arma::vec exps = arma::exp(stat_all_events * parameters);
    // start address in stat_all_events of current events
    int id_start = 0;

    // Go through all events
    for (int id_event = 0; id_event < n_events; id_event++) {
        // Initialize data for each event
        int id_end = id_start + n_candidates(id_event);
        // the subviewsof th stat mat and exps corresponding to this event
        const arma::colvec& exp_current_event = exps.subvec(id_start, id_end - 1);
        const arma::mat& currentEffect = stat_all_events.rows(id_start, id_end - 1);
        // reset auxilliary variables
        expected_stat_current_event.zeros();
        fisher_current_event.zeros();
        // declare the receiver and the normalizer (partition function)
        int id_receiver = selected(id_event);
        double normalizer = arma::sum(exp_current_event);
        // go through all candidates
        for (unsigned int j = 0; j < n_candidates(id_event); j++) {
            probability_current_receiver = exp_current_event(j) / normalizer;
            expected_stat_current_event += probability_current_receiver * (currentEffect.row(j));
            fisher_current_event += probability_current_receiver * ((currentEffect.row(j).t()) * (currentEffect.row(j)));
        }
        // derivative
        derivative += currentEffect.row(id_receiver);
        derivative -= expected_stat_current_event;
        // fisher
        fisher_current_event -= expected_stat_current_event.t() * expected_stat_current_event;
        fisher += fisher_current_event;
        // logLikelihood
        intervalLogL(id_event) = log(exp_current_event(id_receiver) / normalizer);
        logLikelihood += intervalLogL(id_event);
        id_start = id_end;
    }

    return List::create(Named("derivative") = derivative,
                        Named("fisher") = fisher,
                        Named("logLikelihood") = logLikelihood,
                        Named("intervalLogL") = intervalLogL);
}
