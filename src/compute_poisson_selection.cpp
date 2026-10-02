#include <RcppArmadillo.h>
#include "log_sum_exp.h"
#include "event_reductions.h"
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
    const arma::uvec& index_j,
    const arma::uword n_actors_1,
    const arma::uword n_actors_2,
    const bool return_event_scores,
    const bool return_ranks,
    const bool return_margins,
    const bool return_probabilities = false,
    const bool return_availability = false,
    const bool return_conditional_scores = false,
    const Rcpp::Nullable<Rcpp::NumericMatrix> event_weights = R_NilValue,
    const bool return_event_information_trace = false
) {
    // `index_i` / `index_j` are the 0-based per-row actor slots the shared
    // margin reduction scatters into; an empty vector means this shape has no
    // such axis.
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
    // Opt-in weighted per-interval information: one p x p slice per weight
    // column, and the per-interval trace. Both stay empty when not requested,
    // which is how the shared accumulator reads "not asked for". The block this
    // family contributes carries the compensator, matching what `fisher`
    // receives.
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

    // The linear predictors, hoisted as one GEMV. The exponentiation is per
    // event because the max-shift is; the pass count is unchanged.
    arma::vec lin_pred = stat_all_events * parameters;
    // start address in stat_all_events of current events
    int id_start = 0;
    // Gather slices are pre-masked, so the helper needs no risk-set mask.
    const arma::vec no_mask;
    // Event-sized work buffers, allocated once at the largest event's size.
    // Each event works on a view of their first `n` rows (Armadillo's
    // auxiliary-memory constructor, strict, so a view never reallocates): the
    // event's block of the stack is copied into one, since the block is not
    // contiguous in the column-major stack, and the weights, probabilities and
    // the Fisher product's weighted copy are written into the others.
    const arma::uword max_candidates =
      n_candidates.n_elem > 0 ? n_candidates.max() : 0;
    arma::mat stat_buffer(max_candidates, n_parameters);
    arma::mat weighted_buffer(max_candidates, n_parameters);
    arma::vec weights_buffer(max_candidates);
    arma::vec probabilities_buffer(max_candidates);
    // Per-event total rate, and the conditional log-probability of the observed
    // alternative. Both fall out of the log-normalizer, which is why they are
    // computed here rather than reassembled downstream: the algebraic route
    // (intervalLogL - log T + Dt*T) cancels a term against itself and loses
    // digits in proportion to Dt*T, which is exactly the regime a diagnostic
    // evaluated away from the MLE lives in.
    arma::vec total_rate(n_events, fill::zeros);
    arma::vec conditional_logl(n_events, fill::zeros);

    // Opt-in per-event primitives, allocated only when requested. Exact-time
    // sub-models carry margins on BOTH scales from the one probability vector:
    // the probability scale totals the event count at any parameter
    // vector, the compensator scale `Dt * T` totals it only at the MLE and its
    // observed-minus-expected is the martingale residual. `observed` is
    // scale-free, so one vector serves both.
    arma::mat event_scores;
    if (return_event_scores) event_scores.set_size(n_events, n_parameters);
    // The conditional (partial-likelihood) score rows: the same shared
    // reduction as `event_scores`, at UNIT scale instead of the compensator's,
    // so the exposure term drops and what is left is the
    // observed-minus-risk-set-mean row -- the Schoenfeld residual. NA on a
    // right-censored interval, which realizes no mover.
    arma::mat conditional_scores;
    if (return_conditional_scores) {
        conditional_scores.set_size(n_events, n_parameters);
    }
    IntegerVector observed_rank;
    if (return_ranks) observed_rank = IntegerVector(n_events, NA_INTEGER);
    arma::vec margin_observed_i, margin_expected_i, margin_prob_i;
    arma::vec margin_observed_j, margin_expected_j, margin_prob_j;
    const bool has_side_i = return_margins && index_i.n_elem > 0;
    const bool has_side_j = return_margins && index_j.n_elem > 0;
    if (has_side_i) {
        margin_observed_i = arma::vec(n_actors_1, fill::zeros);
        margin_expected_i = arma::vec(n_actors_1, fill::zeros);
        margin_prob_i = arma::vec(n_actors_1, fill::zeros);
    }
    if (has_side_j) {
        margin_observed_j = arma::vec(n_actors_2, fill::zeros);
        margin_expected_j = arma::vec(n_actors_2, fill::zeros);
        margin_prob_j = arma::vec(n_actors_2, fill::zeros);
    }

    // Opt-in per-event probabilities, actor-indexed over the whole node set.
    // Gather rows are the realized risk set only, so they scatter through the
    // same per-row actor index the margin reduction uses; positions off the
    // risk set stay 0. Which axes the shape has is read from the indices
    // themselves, NOT from the margin flags, since probabilities can be
    // requested without margins.
    const bool axis_i = index_i.n_elem > 0;
    const bool axis_j = index_j.n_elem > 0;
    List event_probabilities(return_probabilities ? n_events : 0);
    // Opt-in per-actor availability, on the same sides the margins take. Gather
    // rows ARE the realized risk set, so membership needs no mask -- but a
    // dyadic family repeats an actor across its dyads within one event, so the
    // shared indicator is what makes the count per actor rather than per row.
    arma::vec availability_exposure_i, availability_opportunities_i;
    arma::vec availability_seen_i;
    arma::vec availability_exposure_j, availability_opportunities_j;
    arma::vec availability_seen_j;
    const bool avail_side_i = return_availability && axis_i;
    const bool avail_side_j = return_availability && axis_j;
    if (avail_side_i) {
        availability_exposure_i = arma::vec(n_actors_1, fill::zeros);
        availability_opportunities_i = arma::vec(n_actors_1, fill::zeros);
        availability_seen_i = arma::vec(n_actors_1, fill::zeros);
    }
    if (avail_side_j) {
        availability_exposure_j = arma::vec(n_actors_2, fill::zeros);
        availability_opportunities_j = arma::vec(n_actors_2, fill::zeros);
        availability_seen_j = arma::vec(n_actors_2, fill::zeros);
    }

    // Go through all events
    for (int id_event = 0; id_event < n_events; id_event++) {
        // Initialize data for each event
        int id_end = id_start + n_candidates(id_event);
        bool is_dependent_current_event = is_dependent(id_event);
        double timespan_current_event = timespan(id_event);
        // An interval whose risk set is empty (a global constraint switched
        // off, every actor absent) has total rate 0: it adds -Dt * 0 to the
        // log-likelihood and nothing to the score or the information, and it
        // realizes no mover. An empty slice is not a valid subview, so the
        // interval is written here and skipped. A dependent event always has
        // its own dyad at risk, so one with no candidates is a gather defect.
        if (n_candidates(id_event) == 0) {
            if (is_dependent_current_event) {
                Rcpp::stop("A dependent event has an empty risk set.");
            }
            conditional_logl(id_event) = NA_REAL;
            if (return_event_scores) event_scores.row(id_event).zeros();
            if (return_conditional_scores) {
                conditional_scores.row(id_event).fill(NA_REAL);
            }
            // The shape scatter_event_probabilities() gives this kernel's
            // axes, with every position off the (empty) risk set at 0.
            if (return_probabilities) {
                if (axis_i && axis_j) {
                    event_probabilities[id_event] = Rcpp::wrap(
                      arma::mat(n_actors_1, n_actors_2, fill::zeros)
                    );
                } else {
                    event_probabilities[id_event] = Rcpp::NumericVector(
                      axis_j ? n_actors_2 : n_actors_1
                    );
                }
            }
            continue;
        }
        // this event's views: its rows of the stack (copied into the reused
        // buffer), its linear predictors and actor slots (read in place), and
        // the work buffers
        const arma::uword n_event = n_candidates(id_event);
        arma::mat stat_mat_current_event(
          stat_buffer.memptr(), n_event, n_parameters, false, true
        );
        stat_mat_current_event = stat_all_events.rows(id_start, id_end - 1);
        const arma::vec lin_pred_current_event(
          lin_pred.memptr() + id_start, n_event, false, true
        );
        arma::vec weights(weights_buffer.memptr(), n_event, false, true);
        arma::vec probabilities(
          probabilities_buffer.memptr(), n_event, false, true
        );
        arma::mat weighted_stat_mat(
          weighted_buffer.memptr(), n_event, n_parameters, false, true
        );
        const arma::uvec index_i_event(
          axis_i ? const_cast<arma::uword*>(index_i.memptr()) + id_start
                 : nullptr,
          axis_i ? n_event : 0, false, true
        );
        const arma::uvec index_j_event(
          axis_j ? const_cast<arma::uword*>(index_j.memptr()) + id_start
                 : nullptr,
          axis_j ? n_event : 0, false, true
        );
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
        // The Cox partial-likelihood contribution log p_obs = x_obs - lse. A
        // right-censored interval realizes no mover, so `id_selected` is a
        // placeholder (the exogenous event's actor), not an observed
        // alternative, and there is nothing to condition on: the component is
        // NA there by design, matching the `r` backend.
        if (is_dependent_current_event) {
          conditional_logl(id_event) =
            lin_pred_current_event(id_selected) - log_normalizer;
        } else {
          conditional_logl(id_event) = NA_REAL;
        }
        // The reduction runs on the probability scale p = w / sum(w) with the
        // compensator scale Dt * T applied once outside, since
        // Dt * T * p_j == Dt * lambda_j. That keeps the only large factor in a
        // scalar: a rate that overflows inside the vector would meet a
        // mixed-sign statistic and give Inf - Inf = NaN. The probability vector
        // is formed once, and the sums are one product and one weighted
        // cross-product over the event's rows.
        probabilities = weights / shifted_total;
        weighted_sum_current_event = probabilities.t() * stat_mat_current_event;
        weighted_stat_mat = stat_mat_current_event;
        weighted_stat_mat.each_col() %= probabilities;
        fisher_current_event = weighted_stat_mat.t() * stat_mat_current_event;
        // add the quantities of a current event to the variables to be returned
        const double compensator = timespan_current_event * normalizer;
        // derivative
        derivative -= compensator * weighted_sum_current_event;
        // fisher matrix
        fisher += compensator * fisher_current_event;
        accumulate_event_information(
          fisher_current_event, compensator, id_event, weights_mat,
          weighted_information, event_information_trace
        );
        // logLikelihood
        intervalLogL(id_event) = -compensator;
        if (is_dependent_current_event) {
            intervalLogL(id_event) += lin_pred_current_event(id_selected);
            derivative += stat_mat_current_event.row(id_selected);
        }
        // Opt-in primitives. Ranks and observed margins count only dependent
        // events; a right-censored interval contributes its expected mass but
        // has no observed alternative.
        if (return_conditional_scores) {
            if (is_dependent_current_event) {
                conditional_scores.row(id_event) = event_score_row(
                  arma::rowvec(weighted_sum_current_event), 1.0,
                  stat_mat_current_event, id_selected, true
                );
            } else {
                conditional_scores.row(id_event).fill(NA_REAL);
            }
        }
        if (avail_side_i) {
            availability_seen_i.zeros();
            mark_availability(
              n_event, no_mask, &index_i_event, availability_seen_i
            );
            accumulate_availability(
              availability_seen_i, timespan_current_event,
              is_dependent_current_event,
              &availability_exposure_i, &availability_opportunities_i
            );
        }
        if (avail_side_j) {
            availability_seen_j.zeros();
            mark_availability(
              n_event, no_mask, &index_j_event, availability_seen_j
            );
            accumulate_availability(
              availability_seen_j, timespan_current_event,
              is_dependent_current_event,
              &availability_exposure_j, &availability_opportunities_j
            );
        }
        if (return_ranks && is_dependent_current_event) {
            observed_rank[id_event] =
              rank_of_observed(probabilities, no_mask, id_selected);
        }
        if (return_margins) {
            // Compensator scale: c = Dt * T, and the pass that counts observed.
            std::vector<margin_side> sides_compensator;
            if (has_side_i) {
                sides_compensator.push_back(margin_side(
                  &margin_observed_i, &margin_expected_i, &index_i_event
                ));
            }
            if (has_side_j) {
                sides_compensator.push_back(margin_side(
                  &margin_observed_j, &margin_expected_j, &index_j_event
                ));
            }
            accumulate_margins(
              probabilities, compensator, no_mask, id_selected,
              is_dependent_current_event, sides_compensator
            );
            // Probability scale: c = 1, and accumulated over DEPENDENT events
            // only. This is the parallel-to-choice calibration map, so it has to
            // total the same set `observed` counts -- events, not intervals.
            // Including right-censored intervals would total the interval count
            // instead and leave expected systematically above observed by the
            // censored count, which is exactly the comparison the map exists to
            // make. (The compensator scale above does span censored intervals:
            // its identity is the intercept score equation at the MLE, not a
            // per-event one.) `dependent = false` in the call so the shared
            // observed vectors, already counted above, are not double-counted.
            std::vector<margin_side> sides_probability;
            if (has_side_i) {
                sides_probability.push_back(margin_side(
                  &margin_observed_i, &margin_prob_i, &index_i_event
                ));
            }
            if (has_side_j) {
                sides_probability.push_back(margin_side(
                  &margin_observed_j, &margin_prob_j, &index_j_event
                ));
            }
            if (is_dependent_current_event) {
                accumulate_margins(
                  probabilities, 1.0, no_mask, id_selected, false,
                  sides_probability
                );
            }
        }
        if (return_event_scores) {
            // The estimation score, so on the compensator scale: it must sum to
            // the derivative this kernel accumulates.
            event_scores.row(id_event) = event_score_row(
              arma::rowvec(weighted_sum_current_event), compensator,
              stat_mat_current_event, id_selected, is_dependent_current_event
            );
        }
        if (return_probabilities) {
            event_probabilities[id_event] = scatter_event_probabilities(
              probabilities, index_i_event, index_j_event, n_actors_1,
              n_actors_2
            );
        }
        // loglikelihood
        logLikelihood += intervalLogL(id_event);

        id_start = id_end;
    }

    const bool two_sided = has_side_i && has_side_j;
    // Availability keeps its own two-sidedness: it can be requested without
    // margins, so it cannot read `two_sided` above.
    const bool avail_two_sided = avail_side_i && avail_side_j;
    const arma::vec empty;
    const arma::vec& one_sided_observed =
      has_side_i ? margin_observed_i : margin_observed_j;
    const arma::vec& one_sided_expected =
      has_side_i ? margin_expected_i : margin_expected_j;
    const arma::vec& one_sided_probability =
      has_side_i ? margin_prob_i : margin_prob_j;

    return List::create(
      Named("derivative") = derivative,
      Named("fisher") = fisher,
      Named("intervalLogL") = intervalLogL,
      Named("logLikelihood") = logLikelihood,
      Named("total_rate") = total_rate,
      Named("conditional_logl") = conditional_logl,
      Named("event_scores") = event_scores,
      Named("observed_rank") = observed_rank,
      // A model is two-sided only when it marginalises BOTH axes; otherwise the
      // one side ships under the unnamed slot, matching what the single-sided
      // cpp kernels return. Populating both shapes would give a gather fit the
      // two-sided form where its cpp counterpart has the one-sided one.
      Named("margin_observed") = two_sided ? empty : one_sided_observed,
      Named("margin_expected") = two_sided ? empty : one_sided_expected,
      Named("margin_probability") = two_sided ? empty : one_sided_probability,
      Named("margin_observed_sender") = two_sided ? margin_observed_i : empty,
      Named("margin_expected_sender") = two_sided ? margin_expected_i : empty,
      Named("margin_probability_sender") = two_sided ? margin_prob_i : empty,
      Named("margin_observed_receiver") = two_sided ? margin_observed_j : empty,
      Named("margin_expected_receiver") = two_sided ? margin_expected_j : empty,
      Named("margin_probability_receiver") = two_sided ? margin_prob_j : empty,
      Named("conditional_scores") = conditional_scores,
      Named("availability_exposure") =
        avail_two_sided ? empty
                        : (avail_side_i ? availability_exposure_i
                                        : availability_exposure_j),
      Named("availability_n_opportunities") =
        avail_two_sided ? empty
                        : (avail_side_i ? availability_opportunities_i
                                        : availability_opportunities_j),
      Named("availability_exposure_sender") =
        avail_two_sided ? availability_exposure_i : empty,
      Named("availability_n_opportunities_sender") =
        avail_two_sided ? availability_opportunities_i : empty,
      Named("availability_exposure_receiver") =
        avail_two_sided ? availability_exposure_j : empty,
      Named("availability_n_opportunities_receiver") =
        avail_two_sided ? availability_opportunities_j : empty,
      Named("event_probabilities") = event_probabilities,
      Named("weighted_information") = weighted_information,
      Named("event_information_trace") = event_information_trace
    );
}
