#include <RcppArmadillo.h>
#include "log_sum_exp.h"
#include "event_reductions.h"
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// @inherit compute_coordination_selection params
// @inherit estimate_REM params return

//' Estimate a multinomial selection model with gathered data
//'
//' Given the gathered and distilled data, it outputs the derivative of
//'   the log-likelihood, the Fisher information matrix, the log-Likelihood,
//'   and the log-likelihood of each event for models with
//'   multinomial selection processes, e.g. DyNAM-rate-ordered, DyNAM-choice,
//'   and REM-choice models.
//' @noRd
// [[Rcpp::export]]
List compute_multinomial_selection(
    arma::colvec& parameters,
    const arma::mat& stat_all_events,
    const arma::uvec& n_candidates,
    const arma::uvec& selected,
    const arma::uvec& index_i,
    const arma::uvec& index_j,
    const arma::uword n_actors_1,
    const arma::uword n_actors_2,
    const bool return_event_scores,
    const bool return_ranks,
    const bool return_margins,
    const bool return_probabilities = false,
    const bool return_availability = false,
    const Rcpp::Nullable<Rcpp::NumericMatrix> event_weights = R_NilValue,
    const bool return_event_information_trace = false
) {
    // `index_i` / `index_j` are the 0-based per-row actor slots the shared
    // margin reduction scatters into; an empty vector means this shape has no
    // such axis.
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
    // Opt-in weighted per-interval information: one p x p slice per weight
    // column, and the per-interval trace. Both stay empty when not requested,
    // which is how the shared accumulator reads "not asked for".
    const arma::mat weights_mat =
      as_event_weights(event_weights, (arma::uword) n_events);
    arma::cube weighted_information;
    if (!weights_mat.is_empty()) {
        weighted_information.zeros(
          n_parameters, n_parameters, weights_mat.n_cols
        );
    }
    arma::vec event_information_trace;
    if (return_event_information_trace) {
        event_information_trace.zeros(n_events);
    }

    // The linear predictors, hoisted as one GEMV over every row. The
    // exponentiation is now per event, because the max-shift is per event.
    arma::vec lin_pred = stat_all_events * parameters;
    // start address in stat_all_events of current events
    int id_start = 0;
    // Gather slices are pre-masked -- every row of an event's block is in its
    // risk set -- so the helper's mask is empty.
    const arma::vec no_mask;
    arma::vec weights;

    // Opt-in per-event primitives, allocated only when requested so the default
    // path pays nothing. All three are reductions of the probability vector via
    // the shared helpers, so this kernel carries no reduction arithmetic of its
    // own. Margin sides come from the per-row actor index the gather stack
    // carries; an empty index means this shape has no such axis.
    arma::mat event_scores;
    if (return_event_scores) event_scores.set_size(n_events, n_parameters);
    IntegerVector observed_rank;
    if (return_ranks) observed_rank = IntegerVector(n_events, NA_INTEGER);
    arma::vec margin_observed_i, margin_expected_i;
    arma::vec margin_observed_j, margin_expected_j;
    const bool has_side_i = return_margins && index_i.n_elem > 0;
    const bool has_side_j = return_margins && index_j.n_elem > 0;
    if (has_side_i) {
        margin_observed_i = arma::vec(n_actors_1, fill::zeros);
        margin_expected_i = arma::vec(n_actors_1, fill::zeros);
    }
    if (has_side_j) {
        margin_observed_j = arma::vec(n_actors_2, fill::zeros);
        margin_expected_j = arma::vec(n_actors_2, fill::zeros);
    }
    arma::vec probabilities;
    arma::uvec index_i_event, index_j_event;
    // Opt-in per-event probabilities, actor-indexed over the whole node set.
    // Gather rows are the realized risk set only, so unlike the event-loop
    // engines this one has to scatter through the same per-row actor index the
    // margin reduction uses; positions off the risk set stay 0. Which axes the
    // shape has is read from the indices themselves, NOT from the margin flags,
    // since probabilities can be requested without margins.
    const bool axis_i = index_i.n_elem > 0;
    const bool axis_j = index_j.n_elem > 0;
    List event_probabilities(return_probabilities ? n_events : 0);
    // Opt-in per-actor availability, on the same sides the margins take. A
    // multinomial family defines no exposure time, so only the opportunity
    // counts are accumulated. Gather rows ARE the realized risk set, so
    // membership needs no mask -- but a dyadic family repeats an actor across
    // its dyads within one event, so the shared indicator is what makes the
    // count per actor rather than per row.
    arma::vec availability_opportunities_i, availability_seen_i;
    arma::vec availability_opportunities_j, availability_seen_j;
    const bool avail_side_i = return_availability && axis_i;
    const bool avail_side_j = return_availability && axis_j;
    if (avail_side_i) {
        availability_opportunities_i = arma::vec(n_actors_1, fill::zeros);
        availability_seen_i = arma::vec(n_actors_1, fill::zeros);
    }
    if (avail_side_j) {
        availability_opportunities_j = arma::vec(n_actors_2, fill::zeros);
        availability_seen_j = arma::vec(n_actors_2, fill::zeros);
    }

    // Go through all events
    for (int id_event = 0; id_event < n_events; id_event++) {
        // Initialize data for each event
        int id_end = id_start + n_candidates(id_event);
        // the subviews of the stat mat and predictors for this event
        const arma::vec& lin_pred_current_event =
          lin_pred.subvec(id_start, id_end - 1);
        const arma::mat& currentEffect =
          stat_all_events.rows(id_start, id_end - 1);
        // reset auxilliary variables
        expected_stat_current_event.zeros();
        fisher_current_event.zeros();
        // declare the receiver and the normalizer (partition function)
        int id_receiver = selected(id_event);
        // Max-shifted weights and the log-normalizer. This likelihood is a
        // ratio, so the shift cancels exactly -- and the observed
        // log-probability below becomes a subtraction that stays finite where
        // log(exp_obs / normalizer) underflowed to -inf.
        double log_normalizer =
          log_sum_exp_masked(lin_pred_current_event, no_mask, weights);
        double normalizer = arma::sum(weights);
        // go through all candidates
        for (unsigned int j = 0; j < n_candidates(id_event); j++) {
            probability_current_receiver = weights(j) / normalizer;
            expected_stat_current_event +=
              probability_current_receiver * (currentEffect.row(j));
            fisher_current_event += probability_current_receiver *
              ((currentEffect.row(j).t()) * (currentEffect.row(j)));
        }
        // derivative
        derivative += currentEffect.row(id_receiver);
        derivative -= expected_stat_current_event;
        // fisher
        fisher_current_event -= expected_stat_current_event.t() *
          expected_stat_current_event;
        fisher += fisher_current_event;
        accumulate_event_information(
          fisher_current_event, 1.0, id_event, weights_mat,
          weighted_information, event_information_trace
        );
        // Opt-in primitives, all reductions of the probability vector on the
        // probability scale (c = 1), which is the only scale a multinomial
        // sub-model has.
        if (
          return_event_scores || return_ranks || return_margins ||
          return_probabilities
        ) {
            probabilities = weights / normalizer;
        }
        if (avail_side_i) {
            const arma::uvec slots_i = index_i.subvec(id_start, id_end - 1);
            availability_seen_i.zeros();
            mark_availability(
              slots_i.n_elem, no_mask, &slots_i, availability_seen_i
            );
            accumulate_availability(
              availability_seen_i, 0.0, true, nullptr,
              &availability_opportunities_i
            );
        }
        if (avail_side_j) {
            const arma::uvec slots_j = index_j.subvec(id_start, id_end - 1);
            availability_seen_j.zeros();
            mark_availability(
              slots_j.n_elem, no_mask, &slots_j, availability_seen_j
            );
            accumulate_availability(
              availability_seen_j, 0.0, true, nullptr,
              &availability_opportunities_j
            );
        }
        if (return_ranks) {
            observed_rank[id_event] =
              rank_of_observed(probabilities, no_mask, id_receiver);
        }
        if (return_margins) {
            std::vector<margin_side> sides;
            if (has_side_i) {
                index_i_event = index_i.subvec(id_start, id_end - 1);
                sides.push_back(margin_side(
                  &margin_observed_i, &margin_expected_i, &index_i_event
                ));
            }
            if (has_side_j) {
                index_j_event = index_j.subvec(id_start, id_end - 1);
                sides.push_back(margin_side(
                  &margin_observed_j, &margin_expected_j, &index_j_event
                ));
            }
            accumulate_margins(
              probabilities, 1.0, no_mask, id_receiver, true, sides
            );
        }
        if (return_event_scores) {
            event_scores.row(id_event) = event_score_row(
              currentEffect, probabilities, 1.0, id_receiver, true
            );
        }
        if (return_probabilities) {
            event_probabilities[id_event] = scatter_event_probabilities(
              probabilities,
              axis_i ? index_i.subvec(id_start, id_end - 1) : arma::uvec(),
              axis_j ? index_j.subvec(id_start, id_end - 1) : arma::uvec(),
              n_actors_1,
              n_actors_2
            );
        }
        // logLikelihood
        intervalLogL(id_event) =
          lin_pred_current_event(id_receiver) - log_normalizer;
        logLikelihood += intervalLogL(id_event);
        id_start = id_end;
    }

    const bool two_sided = has_side_i && has_side_j;
    // Availability keeps its own two-sidedness: it can be requested without
    // margins, so it cannot read `two_sided` above.
    const bool avail_two_sided = avail_side_i && avail_side_j;
    const arma::vec empty;
    const arma::vec& one_sided_observed =
      has_side_j ? margin_observed_j : margin_observed_i;
    const arma::vec& one_sided_expected =
      has_side_j ? margin_expected_j : margin_expected_i;

    return List::create(
      Named("derivative") = derivative,
      Named("fisher") = fisher,
      Named("logLikelihood") = logLikelihood,
      Named("intervalLogL") = intervalLogL,
      Named("event_scores") = event_scores,
      Named("observed_rank") = observed_rank,
      // See the Poisson kernel: the named sender/receiver pair ships only for a
      // genuinely two-sided model, so a gather fit has the same margin shape as
      // its cpp counterpart.
      Named("margin_observed") = two_sided ? empty : one_sided_observed,
      Named("margin_expected") = two_sided ? empty : one_sided_expected,
      Named("margin_observed_sender") = two_sided ? margin_observed_i : empty,
      Named("margin_expected_sender") = two_sided ? margin_expected_i : empty,
      Named("margin_observed_receiver") = two_sided ? margin_observed_j : empty,
      Named("margin_expected_receiver") = two_sided ? margin_expected_j : empty,
      Named("availability_n_opportunities") =
        avail_two_sided ? empty
                        : (avail_side_j ? availability_opportunities_j
                                        : availability_opportunities_i),
      Named("availability_n_opportunities_sender") =
        avail_two_sided ? availability_opportunities_i : empty,
      Named("availability_n_opportunities_receiver") =
        avail_two_sided ? availability_opportunities_j : empty,
      Named("event_probabilities") = event_probabilities,
      Named("weighted_information") = weighted_information,
      Named("event_information_trace") = event_information_trace
    );
}
