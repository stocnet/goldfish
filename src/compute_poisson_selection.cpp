#include <RcppArmadillo.h>
#include "log_sum_exp.h"
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// @inherit compute_coordination_selection params
// @inherit estimate_REM params return

//' Estimate a poisson selection model with gathered data
//'
//' Given the gathered and distilled data,
//' it outputs the derivative of the log-likelihood,
//' the Fisher information matrix, the log-likelihood,
//' and the log-likelihood of each event 
//' for models with poisson selection processes,
//' e.g., DyNAM-rate and REM-choice models.
//' @noRd
// [[Rcpp::export]]
List compute_poisson_selection(
    arma::colvec& parameters,
    const arma::mat& stat_all_events,
    const arma::uvec& n_candidates,
    const arma::uvec& selected,
    const arma::vec& timespan,
    const arma::vec& is_dependent,
    const arma::uvec& index_i,
    const arma::uvec& index_j
) {
    // `index_i` / `index_j` are the 0-based per-row actor slots the shared
    // margin reduction scatters into; an empty vector means this shape has no
    // such axis. Threaded here ahead of the accumulators that consume them.
    (void) index_i;
    (void) index_j;
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

    // The linear predictors, hoisted as one GEMV. The exponentiation is per
    // event because the max-shift is; the pass count is unchanged.
    arma::vec lin_pred = stat_all_events * parameters;
    // start address in stat_all_events of current events
    int id_start = 0;
    // Gather slices are pre-masked, so the helper needs no risk-set mask.
    const arma::vec no_mask;
    arma::vec weights;
    // Per-event total rate, and the conditional log-probability of the observed
    // alternative. Both fall out of the log-normalizer, which is why they are
    // computed here rather than reassembled downstream: the algebraic route
    // (intervalLogL - log T + Dt*T) cancels a term against itself and loses
    // digits in proportion to Dt*T, which is exactly the regime a diagnostic
    // evaluated away from the MLE lives in.
    arma::vec total_rate(n_events, fill::zeros);
    arma::vec conditional_logl(n_events, fill::zeros);

    // Go through all events
    for (int id_event = 0; id_event < n_events; id_event++) {
        // Initialize data for each event
        int id_end = id_start + n_candidates(id_event);
        bool is_dependent_current_event = is_dependent(id_event);
        double timespan_current_event = timespan(id_event);
        // the subviews of the stat mat and predictors for this event
        const arma::vec& lin_pred_current_event =
          lin_pred.subvec(id_start, id_end - 1);
        const arma::mat& stat_mat_current_event =
          stat_all_events.rows(id_start, id_end - 1);
        // reset auxilliary variables
        weighted_sum_current_event.zeros();
        fisher_current_event.zeros();
        // declare the selected and the normalizer (partition function)
        int id_selected = selected(id_event);
        // One shifted exp pass yields both scales. The likelihood needs the
        // total rate on the ABSOLUTE scale -- it enters as -Dt * T, not as a
        // ratio, so it is recovered as exp(log_normalizer) and overflows exactly
        // where the raw sum did. Deliberately not stabilized: shifting it would
        // be a different model, and an overflowing step is already rejected by
        // the estimation loop's is.finite() gate.
        double log_normalizer =
          log_sum_exp_masked(lin_pred_current_event, no_mask, weights);
        double shifted_total = arma::sum(weights);
        double normalizer = std::exp(log_normalizer);
        total_rate(id_event) = normalizer;
        conditional_logl(id_event) =
          lin_pred_current_event(id_selected) - log_normalizer;
        // go through all candidates. The reduction runs on the probability
        // scale p = w / sum(w) with the compensator scale Dt * T applied once
        // outside, since Dt * T * p_j == Dt * lambda_j. That keeps the only
        // large factor in a scalar: a rate that overflows inside the vector
        // would meet a mixed-sign statistic and give Inf - Inf = NaN.
        for (unsigned int j = 0; j < n_candidates(id_event); j++) {
            double probability_current_selected = weights(j) / shifted_total;
            weighted_sum_current_event +=
              probability_current_selected * (stat_mat_current_event.row(j));
            fisher_current_event +=
              probability_current_selected *
              ((stat_mat_current_event.row(j).t()) *
              (stat_mat_current_event.row(j)));
        }
        // add the quantities of a current event to the variables to be returned
        const double compensator = timespan_current_event * normalizer;
        // derivative
        derivative -= compensator * weighted_sum_current_event;
        // fisher matrix
        fisher += compensator * fisher_current_event;
        // logLikelihood
        intervalLogL(id_event) = -compensator;
        if (is_dependent_current_event) {
            intervalLogL(id_event) += lin_pred_current_event(id_selected);
            derivative += stat_mat_current_event.row(id_selected);
        }
        // loglikelihood
        logLikelihood += intervalLogL(id_event);

        id_start = id_end;
    }

    return List::create(
      Named("derivative") = derivative,
      Named("fisher") = fisher,
      Named("intervalLogL") = intervalLogL,
      Named("logLikelihood") = logLikelihood,
      Named("total_rate") = total_rate,
      Named("conditional_logl") = conditional_logl
    );
}
