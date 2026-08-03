#include <RcppArmadillo.h>
#include "broadcast_updates.h"
#include "flat_updates.h"
#include "event_reductions.h"
#include "log_sum_exp.h"
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;


inline arma::mat reduce_mat_to_vector(
    const arma::mat& stat_mat,
    const int& n_actors_1,
    const int& n_actors_2,
    const bool& twomode_or_reflexive);

// @inherit estimate_REM params return description

//' Calculation for estimating an DyNAM-rate model
//' @noRd
// [[Rcpp::export]]
 List estimate_DyNAM_rate(
     const arma::vec& parameters,
     const arma::mat& dep_event_mat,
     const arma::vec& timespan,
     const arma::vec& is_dependent,
     const arma::mat& stat_mat_init,
     const arma::mat& stat_mat_update,
     const arma::vec& stat_mat_update_pointer,
     const arma::mat& stat_mat_broadcast,
     const arma::vec& stat_mat_broadcast_pointer,
     const arma::vec& active_sender_init,
     const arma::mat& active_sender_update,
     const arma::vec& active_sender_update_pointer,
     const arma::vec& active_dyad_init,
     const arma::mat& active_dyad_update,
     const arma::vec& active_dyad_update_pointer,
     const int n_actors_1,
     const int n_actors_2,
     const bool twomode_or_reflexive,
     bool impute = true,
     const bool return_event_scores = false,
     const bool return_ranks = false,
     const bool return_margins = false,
     const bool return_total_rate = false,
     const bool return_probabilities = false,
     const bool return_availability = false,
     const bool return_conditional_scores = false,
     const Rcpp::Nullable<Rcpp::NumericMatrix> event_weights = R_NilValue,
     const bool return_event_information_trace = false
 ) {
   // initialize stat_mat and numbers
   arma::mat stat_mat = stat_mat_init;
   int n_events = is_dependent.n_elem;
   int n_parameters = stat_mat.n_cols;
   // declare auxilliary variables
   arma::rowvec weighted_sum_current_event(n_parameters);
   arma::mat fisher_current_event(n_parameters, n_parameters);
   int stat_mat_update_id = 0;
   int stat_mat_broadcast_id = 0;

   // declare return variables
   arma::mat fisher(n_parameters, n_parameters, fill::zeros);
   arma::mat derivative(1, n_parameters, fill::zeros);
   double logLikelihood = 0;
   arma::vec intervalLogL(n_events, fill::zeros);
   // Opt-in weighted per-interval information: one p x p slice per weight
   // column, and the per-interval trace. Both stay empty when not requested,
   // which is how the shared accumulator reads "not asked for". The block this
   // family contributes carries the timespan, matching what `fisher` receives.
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
   // Opt-in per-event score matrix. Each row is the per-event
   // increment already accumulated into `derivative` (the timed weighted sum
   // plus the observed statistic on dependent events); allocated only when
   // requested so the default path pays nothing.
   arma::mat event_scores;
   if (return_event_scores) event_scores.set_size(n_events, n_parameters);
   // Opt-in per-event rank of the observed sender among the risk set by fitted
   // rate (rank 1 = highest rate); NA for right-censored events. Allocated only
   // when requested so the default path pays nothing.
   IntegerVector observed_rank;
   if (return_ranks) observed_rank = IntegerVector(n_events, NA_INTEGER);
   // Opt-in sender-margin accumulators (per-sender observed and expected event
   // counts). `expected[s]` sums the interevent-time exposure times the sender's
   // fitted rate over every likelihood interval where s is active, including
   // right-censored intervals (accumulated in the sender loop that runs for
   // every event); at the MLE it totals the number of events via the intercept
   // score equation. Allocated only when requested.
   arma::vec margin_observed;
   arma::vec margin_expected;
   if (return_margins) {
     margin_observed = arma::vec(n_actors_1, fill::zeros);
     margin_expected = arma::vec(n_actors_1, fill::zeros);
   }
   // Opt-in per-event total rate: the summed fitted rate over the realized risk
   // set (the softmax normalizer), one value per event. `total_rate * interevent
   // time` is the Cox-Snell/compensator residual, recovered without an
   // evaluation pass. Allocated only when requested.
   arma::vec total_rate;
   if (return_total_rate) total_rate = arma::vec(n_events, fill::zeros);
   // Opt-in probability-scale margins, beside the compensator-scale ones above.
   // `expected[s]` sums the competing-risks probability that s creates the next
   // event, over DEPENDENT events only, so it totals the event count at ANY
   // parameter vector rather than only at the MLE. Allocated only when requested.
   arma::vec margin_probability;
   if (return_margins) margin_probability = arma::vec(n_actors_1, fill::zeros);
   // Opt-in Cox partial-likelihood contribution log p_obs, the "which" half of
   // the which/when split of the per-event log-likelihood. NA on a
   // right-censored interval, which realizes no mover and so has no observed
   // alternative to condition on. Allocated only when requested.
   arma::vec conditional_logl;
   if (return_total_rate) conditional_logl = arma::vec(n_events, fill::zeros);
   // Opt-in per-event probability vector over the WHOLE sender set, zero off
   // the risk set. Allocated only when requested.
   List event_probabilities(return_probabilities ? n_events : 0);
   // Opt-in conditional (partial-likelihood) score rows: the same shared
   // reduction the stored score rows use, at UNIT scale instead of the
   // compensator's, so the exposure term drops and what is left is the
   // observed-minus-risk-set-mean row -- the Schoenfeld residual. NA on a
   // right-censored interval, which realizes no mover and so has no observed
   // alternative to condition on. Allocated only when requested.
   arma::mat conditional_scores;
   if (return_conditional_scores) {
     conditional_scores.set_size(n_events, n_parameters);
   }
   // Opt-in per-actor availability: the denominators the per-actor margins are
   // read against. On this sender-axis family a risk-set position IS a sender,
   // so the membership indicator is the presence mask itself. Allocated only
   // when requested.
   arma::vec availability_exposure;
   arma::vec availability_opportunities;
   arma::vec availability_seen;
   if (return_availability) {
     availability_exposure = arma::vec(n_actors_1, fill::zeros);
     availability_opportunities = arma::vec(n_actors_1, fill::zeros);
     availability_seen = arma::vec(n_actors_1, fill::zeros);
   }

   // Check whether there are composition change and initialize
   // the presence of actor1 and actor2
   bool has_composition_change1 = true;
   int active_sender_update_id = 0;
   if (active_sender_update.n_elem == 0) {
     has_composition_change1 = false;
   }
   arma::vec active_sender = active_sender_init;
   
   bool has_composition_change2 = true;
   int active_dyad_update_id = 0;
   if (active_dyad_update.n_elem == 0) {
     has_composition_change2 = false;
   }
   arma::vec active_dyad = active_dyad_init;
   
   // Go through all events
   for (int id_event = 0; id_event < n_events; id_event++) {
     // update stat_mat with the combined buffer covering all stored events
     apply_flat_updates(
       stat_mat, stat_mat_update, stat_mat_update_id,
       stat_mat_update_pointer(id_event), n_actors_2
     );
     apply_broadcast_updates(
       stat_mat, stat_mat_broadcast, stat_mat_broadcast_id,
       stat_mat_broadcast_pointer(id_event), n_actors_1, n_actors_2,
       twomode_or_reflexive
     );


     // impute the missing statistics if necessary
     if (impute) {
       for (int i = 0; i < n_parameters; i++) {
         // Construct a view for the i-th column of the stat_matrix and
         // do the impute
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
       while (active_sender_update_id < active_sender_update_pointer(id_event)) {
         active_sender(active_sender_update(0, active_sender_update_id) - 1) =
           active_sender_update(1, active_sender_update_id);
         active_sender_update_id++;
       }
     }
     if (has_composition_change2) {
       while (active_dyad_update_id < active_dyad_update_pointer(id_event)) {
         active_dyad(active_dyad_update(0, active_dyad_update_id) - 1) =
           active_dyad_update(1, active_dyad_update_id);
         active_dyad_update_id++;
       }
     }
     
     // We calculate the derivative, logLikelihood, and 
     // fisher information matrix of a current event according to the paper.
     // Reset auxilliary variables
     weighted_sum_current_event.zeros();
     fisher_current_event.zeros();
     double normalizer = 0;
     double timespan_current_event = timespan(id_event);
     // declare the ids of the sender and the receiver,
     const int id_sender = dep_event_mat(0, id_event) - 1;
     //int sender_corr = 0;
     arma::mat reduce_stat_mat =
       reduce_mat_to_vector(stat_mat, n_actors_1, n_actors_2,
                            twomode_or_reflexive);
     const bool do_rank = return_ranks && (is_dependent(id_event) == 1);
     // The shared reductions read a rate vector, which this loop does not
     // otherwise need, so it stores the very doubles it already computes rather
     // than rebuilding them as one matrix-vector product afterwards. A GEMV
     // sums in a different order from the per-row `dot()` below, so the rebuilt
     // values differ in the last bits.
     //
     // The reason is consistency, NOT coefficient safety, and the difference
     // between those two was measured rather than assumed. `rates` feeds only
     // the reductions -- the margins, the rank, and the stored per-event score;
     // the normalizer and the derivative are accumulated from
     // `exp_current_sender` directly, so rebuilding it CANNOT move a
     // coefficient, and does not: patching in the GEMV leaves every frozen
     // 1e-6 baseline passing and every log-likelihood and aggregate score
     // bitwise unchanged, shifting only stored diagnostics -- 18 of 84 actors'
     // margins on the social-evolution rate model, by at most 2.6e-15 relative.
     //
     // What is preserved is therefore worth stating exactly: a stored
     // diagnostic should report the numbers the likelihood actually used, not a
     // near-copy recomputed by another route. That is cheap to keep here and
     // impossible to verify from R -- 2.6e-15 in a diagnostic is far inside
     // both the 1e-10 cross-backend and 1e-6 baseline tolerances -- so this
     // comment, not a test, is the guard on this line.
     //
     // Zero on inactive senders, as the header requires.
     const bool need_rates = do_rank || return_margins || return_event_scores;
     arma::vec rates;
     if (need_rates) rates = arma::vec(n_actors_1, fill::zeros);
     // go through all actor1
     for (int i = 0; i < n_actors_1; ++i) {
       if (active_sender(i) == 1) {
         // exp_current_sender is \exp(\beta^T s)
         double exp_current_sender =
           std::exp(dot(reduce_stat_mat.row(i), parameters));
         if (need_rates) rates(i) = exp_current_sender;
         normalizer += exp_current_sender;
         weighted_sum_current_event +=
           exp_current_sender * (reduce_stat_mat.row(i));
         fisher_current_event += exp_current_sender *
           ((reduce_stat_mat.row(i).t()) * (reduce_stat_mat.row(i)));
       }// else if (i < id_sender) {
        // sender_corr += 1;
       //}
     }
     //id_sender -= sender_corr;
     // add the quantities of a current event to the variables to be returned
     // derivative
     //Rcpp::Rcout << std::endl << "Event:" << id_event + 1 << std::endl;
     //Rcpp::Rcout << "presence:" << active_sender << std::endl;
     //Rcpp::Rcout << "mat:" << std::endl << reduce_stat_mat << std::endl;
     //Rcpp::Rcout << "timespan:" << timespan_current_event << std::endl;
     //Rcpp::Rcout << "Derivative:" << weighted_sum_current_event << std::endl;
     derivative -= timespan_current_event * weighted_sum_current_event;

     // fisher matrix
     fisher += timespan_current_event * fisher_current_event;
     accumulate_event_information(
       fisher_current_event, timespan_current_event, id_event, weights_mat,
       weighted_information, event_information_trace
     );
     //Rcpp::Rcout << "fisher:" << std::endl << fisher_current_event << std::endl;
     if (return_total_rate) total_rate(id_event) = normalizer;
     // logLikelihood
     intervalLogL(id_event) = - timespan_current_event * normalizer;
     if (is_dependent(id_event)) {
       intervalLogL(id_event) +=
         dot(reduce_stat_mat.row(id_sender), parameters);
       derivative += reduce_stat_mat.row(id_sender);
       //Rcpp::Rcout << "Der +:" << reduce_stat_mat.row(id_sender) << std::endl;
       //Rcpp::Rcout << "sender:" << id_sender << std::endl;
     }
     if (return_margins) {
       // Compensator scale: c = Dt on the raw rates, so the contribution is
       // Dt * lambda_i. A right-censored interval still accumulates exposure on
       // the expected side but has no observed mover to count.
       std::vector<margin_side> sides;
       sides.push_back(margin_side(&margin_observed, &margin_expected));
       accumulate_margins(
         rates, timespan_current_event, active_sender, id_sender,
         is_dependent(id_event) == 1, sides
       );
     }
     if (do_rank) {
       observed_rank[id_event] =
         rank_of_observed(rates, active_sender, id_sender);
     }
     if (return_availability) {
       availability_seen.zeros();
       mark_availability(
         n_actors_1, active_sender, nullptr, availability_seen
       );
       accumulate_availability(
         availability_seen, timespan_current_event,
         is_dependent(id_event) == 1,
         &availability_exposure, &availability_opportunities
       );
     }
     // Quantities that enter as a ratio or as a log of the normalizer, from a
     // max-shifted pass computed BESIDE the raw one above rather than replacing
     // it. The likelihood's total rate must stay on the absolute scale — it
     // enters as -Dt * T, so shifting it would be a different model, not a
     // stabilization — and leaving the raw pass untouched is also what keeps
     // every frozen coefficient exactly where it was. These three are
     // shift-invariant, and are exact where the raw ratio silently returns 1
     // (subnormal underflow) or NaN (overflow).
     if (
       return_probabilities || return_margins || return_total_rate ||
       return_conditional_scores
     ) {
       arma::vec lin_pred = reduce_stat_mat * parameters;
       arma::vec weights;
       double log_normalizer =
         log_sum_exp_masked(lin_pred, active_sender, weights);
       double shifted_total = arma::sum(weights);
       arma::vec probabilities = weights / shifted_total;
       if (return_total_rate) {
         conditional_logl(id_event) = is_dependent(id_event)
           ? lin_pred(id_sender) - log_normalizer
           : NA_REAL;
       }
       if (return_margins && is_dependent(id_event)) {
         // Probability scale, over dependent events only: this is the
         // parallel-to-choice calibration map, so it must total the same set
         // `margin_observed` counts — events, not intervals.
         margin_probability += probabilities;
       }
       if (return_probabilities) {
         event_probabilities[id_event] =
           NumericVector(probabilities.begin(), probabilities.end());
       }
       if (return_conditional_scores) {
         if (is_dependent(id_event)) {
           conditional_scores.row(id_event) = event_score_row(
             reduce_stat_mat, probabilities, 1.0, id_sender, true
           );
         } else {
           conditional_scores.row(id_event).fill(NA_REAL);
         }
       }
     }
     // The stored per-event score comes from the shared reduction over the same
     // `rates` the margin and rank reductions read, rather than from the
     // before/after difference of the running derivative, so the definition
     // lives in one place instead of once per kernel. The derivative above is
     // deliberately untouched: it drives the optimizer, and no coefficient may
     // move.
     if (return_event_scores) {
       event_scores.row(id_event) = event_score_row(
         reduce_stat_mat, rates, timespan_current_event, id_sender,
         is_dependent(id_event) == 1
       );
     }
     // loglikelihood
     logLikelihood += intervalLogL(id_event);
   }
   
   return List::create(
     Named("derivative") = derivative,
     Named("fisher") = fisher,
     Named("intervalLogL") = intervalLogL,
     Named("logLikelihood") = logLikelihood,
     Named("event_scores") = event_scores,
     Named("observed_rank") = observed_rank,
     Named("margin_observed") = margin_observed,
     Named("margin_expected") = margin_expected,
     Named("margin_probability") = margin_probability,
     Named("total_rate") = total_rate,
     Named("conditional_logl") = conditional_logl,
     Named("conditional_scores") = conditional_scores,
     Named("availability_exposure") = availability_exposure,
     Named("availability_n_opportunities") = availability_opportunities,
     Named("event_probabilities") = event_probabilities,
     Named("weighted_information") = weighted_information,
     Named("event_information_trace") = event_information_trace
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
