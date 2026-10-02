#include <RcppArmadillo.h>
#include "availability_updates.h"
#include "broadcast_updates.h"
#include "event_reductions.h"
#include "flat_updates.h"
#include "log_sum_exp.h"
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

//' Calculation for estimating an REM-choice model
//'
//' Given input data, it output the derivative of the loglikelihood,
//'    the Fisher information matrix, the logLikelihood, 
//'    and the loglikelihood of each event
//'
//' @param parameters An n_parameters by 1 matrix, which is the input parameter
//' @param dep_event_mat An 2 by n_events matrix, the (1,n) entry is 
//'    the sender of n-th event, and the (2,n) entry is the 
//'    receiver of the n-th event.
//' @param timespan An n_events by 1 matrix.
//'    The i-th element is the waiting time of the i-th event.
//' @param is_dependent An n_events by 1 matrix with boolean values.
//'    If the i-th event is dependent, then the i-th entry of
//'    is_dependent is TRUE, otherwise it's FALSE.
//' @param stat_mat_init An n_actor1*n_actor2 by n_parameters matrix.
//'    It is the initialization of the statistics matrix.
//'    The initial value of k-th effect for the actor1-actor2 pair (i,j)
//'    is recorded in the ((i-1)*n_actor2 + j,k) entry of stat_mat_init.
//' @param stat_mat_update An matrix with four rows,
//'    which records the updates of the statistics matrix through all
//'    dependent events.
//'    The following is an example.
//'     \tabular{rrrrr}{
//'       0 \tab 2 \tab 0 \tab 9 \tab 9\cr
//'       2 \tab 1 \tab 5 \tab 4 \tab 3\cr
//'       3 \tab 5 \tab 5 \tab  0 \tab 5\cr
//'       1.2 \tab 3.5 \tab 2.5 \tab 9.23 \tab 2.8\cr
//'     }
//'     Each column represents an update.
//'     For example the first column means replacing the value of
//'     the (3+1)-th effect for the actor1-actor2 pair(0+1,2+1) by 1.2.
//'     The +1 is due to the difference between the numberings in R and C.
//' @param stat_mat_update_pointer An n_events by 1 matrix that record
//'     which update belongs to which dependent event.
//'     Suppose that the first three elements are (10,11,15).
//'     Then the first 10 colums of stat_mat_update is the update for 
//'     the first event, the 11th column is  the update for the second event,
//'     and the 12th to 15th columns are the updates for the third event.
//' @param stat_mat_broadcast A matrix with four rows, the updates that write
//'     a whole sender row, receiver column or every dyad at once, in the
//'     layout of `stat_mat_update`; expanded to cells as they are applied.
//' @param stat_mat_broadcast_pointer The per-event pointer into
//'     `stat_mat_broadcast`, with the structure of `stat_mat_update_pointer`.
//' @param active_dyad_sender_init The sender factor of an outer-encoded
//'    `active_dyad`: an n_actor1 by 1 matrix, which records
//'    the initial availability of each actor1.
//'    If the i-th actor1 is not available in the
//'    beginning then the i-th entry is 0, otherwise it's 1.
//'    Zero-length at the point encoding, whose grid already carries it.
//' @param active_dyad_sender_update An matrix with two rows, which record the
//'    updates of the sender factor through all events.
//'    The following is an example.
//'     \tabular{rrrrr}{
//'       0 \tab 3 \tab 4 \tab 9 \tab 20\cr
//'       1 \tab 0 \tab 1 \tab 0 \tab 0\cr
//'     }
//'    Each column represents an update. For example the first column means
//'    the 0+1-th actor1 becomes present.
//'    And the second column means the
//'    the 3+1 th actor1 becomes absent.
//'    The +1 is due to the difference between the numberings in R and C.
//' @param active_dyad_sender_update_pointer An n_events by 1 matrix that
//'    records which update of the sender factor belongs to which
//'    (dependent + right-censored) event.
//'    The structure is similar to stat_mat_update_pointer.
//' @param active_dyad_init At the outer encoding, the receiver factor: an
//'    n_actors2 by 1 matrix, which records the initial availability of each
//'    actor2 (0 unavailable, 1 available). At the point encoding, the whole
//'    n_actors1 * n_actors2 mask, flattened sender-major.
//' @param active_dyad_update An matrix with two rows, which record the updates
//'    of the receiver factor through all events (three rows, node1, node2 and
//'    value, at the point encoding).
//'    The following is an example.
//'     \tabular{rrrrr}{
//'       0 \tab 3 \tab 4 \tab 9 \tab 20\cr
//'       1 \tab 0 \tab 1 \tab 0 \tab 0\cr
//'     }
//'    Each column represents an update. For example the first column means
//'    the 0+1-th actor2 becomes present.
//'    And the second column means the 3+1 th actor2 becomes absent.
//'    The +1 is due to the difference between the numberings in R and C.
//' @param active_dyad_update_pointer An n_events by 1 matrix that record
//'    which update belongs to which (dependent+ rightcensored) event.
//'    The structure is similar to stat_mat_update_pointer.
//' @param n_actors_1 An integer which is the number of actor1
//' @param n_actors_2 An integer which is the number of actor2
//' @param twomode_or_reflexive An boolean variable. If it's true,
//'    then the model is two-model or we consider the reflexive effect
//'    (that's the value in the diagonal entries of the statistics matrix).
//' @param active_dyad_is_point TRUE when `active_dyad` is the point-encoded
//'    mask, FALSE when it is the receiver factor of the outer encoding.
//' @param return_event_scores,return_ranks,return_margins,return_total_rate,return_probabilities,return_availability,return_conditional_scores
//'    Opt-in per-event diagnostics, each computed only when its flag is set.
//'    `return_total_rate` also returns the conditional (partial-likelihood)
//'    component `conditional_logl`.
//' @param event_weights An optional n_events by m matrix; each column weights
//'    the per-event information into one slice of `weighted_information`.
//' @param return_event_information_trace Return the trace of each event's
//'    information contribution.
//'
//' @return Return a list with elements as follows.
//' \describe{
//'   \item{derivative}{An 1 by n_parameters matrix, which is the derivative of
//'    loglikelihood given the input parameter and data.}
//'   \item{fisher}{An n_parameters by n_parameters matrix, which is the fisher
//'    information matrix given the input parameter and data.}
//'   \item{logLikelihood}{An scalar, which is the loglikelihood given
//'   the input parameter and data.}
//'   \item{intervalLogL}{An n_events by 1 matrix,
//'         of which the i-th entry is the loglikelihood of the i-th event
//'         given the input parameter and data.}
//'   \item{event_scores, observed_rank, margin_*, total_rate,
//'         conditional_logl, conditional_scores, availability_*,
//'         event_probabilities, weighted_information,
//'         event_information_trace}{The opt-in components, empty unless
//'         requested.}
//' }
//' @noRd
// [[Rcpp::export]]
List estimate_REM(
    const arma::vec& parameters,
    const arma::mat& dep_event_mat,
    const arma::vec& timespan,
    const arma::vec& is_dependent,
    const arma::mat& stat_mat_init,
    const arma::mat& stat_mat_update,
    const arma::vec& stat_mat_update_pointer,
    const arma::mat& stat_mat_broadcast,
    const arma::vec& stat_mat_broadcast_pointer,
    const arma::vec& active_dyad_sender_init,
    const arma::mat& active_dyad_sender_update,
    const arma::vec& active_dyad_sender_update_pointer,
    const arma::vec& active_dyad_init,
    const arma::mat& active_dyad_update,
    const arma::vec& active_dyad_update_pointer,
    const int n_actors_1,
    const int n_actors_2,
    const bool twomode_or_reflexive,
    const bool active_dyad_is_point,
    const bool return_event_scores = false,
    const bool return_ranks = false,
    const bool return_margins = false,
    const bool return_total_rate = false,
    const bool return_probabilities = false,
    const bool return_availability = false,
    const bool return_conditional_scores = false,
    const Rcpp::Nullable<Rcpp::NumericMatrix> event_weights = R_NilValue,
    const bool return_event_information_trace = false,
    const bool force_shifted_normalizer = false,
    const bool report_normalizer_route = false
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
   // Opt-in per-event rank of the observed dyad among the risk set by fitted
   // rate (rank 1 = highest rate); NA for right-censored events. Allocated
   // only when requested so the default path pays nothing.
   IntegerVector observed_rank;
   if (return_ranks) observed_rank = IntegerVector(n_events, NA_INTEGER);
   // Opt-in both-sided margin accumulators. A tie-oriented model implies both
   // the out-degree (sender) and in-degree (receiver) compensators:
   // `expected_receiver[r]` sums, over every likelihood interval (right-censored
   // included), the interevent time times the summed fitted intensities of dyads
   // incident to r; `expected_sender[s]` is the mirror over senders. Both sides
   // share the same double sum, so their totals are identical and each equals the
   // number of events at the MLE. Allocated only when requested.
   arma::vec margin_observed_sender;
   arma::vec margin_expected_sender;
   arma::vec margin_observed_receiver;
   arma::vec margin_expected_receiver;
   if (return_margins) {
     margin_observed_sender = arma::vec(n_actors_1, fill::zeros);
     margin_expected_sender = arma::vec(n_actors_1, fill::zeros);
     margin_observed_receiver = arma::vec(n_actors_2, fill::zeros);
     margin_expected_receiver = arma::vec(n_actors_2, fill::zeros);
   }
   // Opt-in per-event total rate: the summed fitted intensity over the realized
   // risk set, one value per event. `total_rate * interevent time` is the
   // Cox-Snell/compensator residual, recovered without an evaluation pass.
   // Allocated only when requested.
   arma::vec total_rate;
   if (return_total_rate) total_rate = arma::vec(n_events, fill::zeros);
   // Flat dyad position -> actor id on each axis, for the shared two-sided
   // margin and availability reductions. `e` is flattened sender-major (dyad
   // (i, j) at i * n_actors_2 + j), and the map is the same at every event.
   // Built only when one of those reductions is requested.
   const arma::uword n_dyads =
     static_cast<arma::uword>(n_actors_1) * n_actors_2;
   arma::uvec dyad_sender;
   arma::uvec dyad_receiver;
   if (return_margins || return_availability) {
     dyad_sender.set_size(n_dyads);
     dyad_receiver.set_size(n_dyads);
     for (int i = 0; i < n_actors_1; ++i) {
       for (int j = 0; j < n_actors_2; ++j) {
         dyad_sender(i * n_actors_2 + j) = i;
         dyad_receiver(i * n_actors_2 + j) = j;
       }
     }
   }
   // Opt-in probability-scale margins, beside the compensator-scale ones above.
   // These sum the competing-risks probability that the dyad creates the next
   // event, over DEPENDENT events only, so each side totals the event count at
   // ANY parameter vector rather than only at the MLE. Allocated when requested.
   arma::vec margin_probability_sender;
   arma::vec margin_probability_receiver;
   if (return_margins) {
     margin_probability_sender = arma::vec(n_actors_1, fill::zeros);
     margin_probability_receiver = arma::vec(n_actors_2, fill::zeros);
   }
   // Opt-in Cox partial-likelihood contribution log p_obs, the "which" half of
   // the which/when split of the per-event log-likelihood. NA on a
   // right-censored interval, which realizes no mover and so has no observed
   // alternative to condition on. Allocated only when requested.
   arma::vec conditional_logl;
   if (return_total_rate) conditional_logl = arma::vec(n_events, fill::zeros);
   // Opt-in per-event probability grid over the WHOLE dyad set, zero off the
   // risk set. `probabilities` is flattened sender-major (dyad (i, j) at
   // i * n_actors_2 + j), so cell (i, j) of the n1 x n2 grid is read there.
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
   // Opt-in per-actor availability, per SIDE as the margins are: a dyad at risk
   // makes its sender available on one side and its receiver on the other, and
   // an actor whose outgoing dyads are all masked while its incoming ones are
   // open is available as a receiver only. Marked once per actor per interval
   // through the shared indicator, so an actor at risk in many dyads collects
   // that interval's length once, not once per dyad. Allocated when requested.
   arma::vec availability_exposure_sender;
   arma::vec availability_opportunities_sender;
   arma::vec availability_seen_sender;
   arma::vec availability_exposure_receiver;
   arma::vec availability_opportunities_receiver;
   arma::vec availability_seen_receiver;
   if (return_availability) {
     availability_exposure_sender = arma::vec(n_actors_1, fill::zeros);
     availability_opportunities_sender = arma::vec(n_actors_1, fill::zeros);
     availability_seen_sender = arma::vec(n_actors_1, fill::zeros);
     availability_exposure_receiver = arma::vec(n_actors_2, fill::zeros);
     availability_opportunities_receiver = arma::vec(n_actors_2, fill::zeros);
     availability_seen_receiver = arma::vec(n_actors_2, fill::zeros);
   }


   // Check whether there are composition change and initialize
   // the presence of actor1 and actor2
   bool has_composition_change1 = true;
   int active_dyad_sender_update_id = 0;
   if (active_dyad_sender_update.n_elem == 0) {
     has_composition_change1 = false;
   }
   arma::vec active_dyad_sender = active_dyad_sender_init;
   
   bool has_composition_change2 = true;
   int active_dyad_update_id = 0;
   if (active_dyad_update.n_elem == 0) {
     has_composition_change2 = false;
   }
   // `active_dyad` is the folded per-event risk mask. At the
   // outer encoding it is the length-n2 receiver vector (cell (i, j) available iff
   // active_dyad_sender(i) & active_dyad(j), the sender factor being the
   // object's own second factor). At the point encoding it is a flattened
   // n1 x n2 mask (sender-major: dyad (i, j) at i * n_actors_2 + j) with both
   // presences n support folded in, maintained by a (node1, node2, replace)
   // buffer and read cell-wise; the sender factor is then empty and unread.
   arma::vec active_dyad = active_dyad_init;

   // The risk set over all n1 * n2 dyads (sender-major), one buffer per
   // evaluation. A dyad is at risk when the grid cell (point) or both factors
   // (outer) allow it, and it is not a self-tie unless the model admits one.
   // It is filled once here and then rewritten by the same updates that write
   // the availability, in the cells each flip reaches -- one cell for a point
   // flip, a sender's row or a receiver's column for a factor flip -- so an
   // event pays for what changed rather than for the grid.
   auto at_risk = [&](int i, int j) -> double {
     if (!twomode_or_reflexive && i == j) return 0;
     const bool available = active_dyad_is_point
       ? active_dyad(i * n_actors_2 + j) == 1
       : active_dyad_sender(i) == 1 && active_dyad(j) == 1;
     return available ? 1 : 0;
   };
   arma::vec allowed(n_dyads);
   for (int i = 0; i < n_actors_1; ++i) {
     for (int j = 0; j < n_actors_2; ++j) {
       allowed(i * n_actors_2 + j) = at_risk(i, j);
     }
   }

   // Event-sized work buffers, allocated once and overwritten at every event.
   arma::vec lin_pred(n_dyads);
   arma::vec e(n_dyads);
   arma::mat weighted_stat_mat(n_dyads, n_parameters);
   arma::vec weights;
   arma::vec probabilities;
   std::vector<margin_side> margin_sides;
   if (return_margins) {
     margin_sides.push_back(margin_side(
       &margin_observed_sender, &margin_expected_sender, &dyad_sender
     ));
     margin_sides.push_back(margin_side(
       &margin_observed_receiver, &margin_expected_receiver, &dyad_receiver
     ));
   }

   // Intervals whose diagnostic normalizer came from the shifted pass.
   int n_shifted_normalizer = 0;

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

     // update presence
     if (has_composition_change1) {
       apply_availability_updates(
         active_dyad_sender_update_id,
         active_dyad_sender_update_pointer(id_event),
         [&](int c) {
           const int i = active_dyad_sender_update(0, c) - 1;
           active_dyad_sender(i) = active_dyad_sender_update(1, c);
           for (int j = 0; j < n_actors_2; ++j) {
             allowed(i * n_actors_2 + j) = at_risk(i, j);
           }
         });
     }
     if (has_composition_change2) {
       apply_availability_updates(
         active_dyad_update_id, active_dyad_update_pointer(id_event),
         [&](int c) {
           if (active_dyad_is_point) {
             const int i = active_dyad_update(0, c) - 1;
             const int j = active_dyad_update(1, c) - 1;
             active_dyad(i * n_actors_2 + j) = active_dyad_update(2, c);
             allowed(i * n_actors_2 + j) = at_risk(i, j);
           } else {
             const int j = active_dyad_update(0, c) - 1;
             active_dyad(j) = active_dyad_update(1, c);
             for (int i = 0; i < n_actors_1; ++i) {
               allowed(i * n_actors_2 + j) = at_risk(i, j);
             }
           }
         });
     }

     // We calculate the derivative, log-Likelihood, and fisher matrix of a
     // current event. Staged BLAS form: one GEMV for the linear
     // predictors, a masked exp vector (presence / reflexive / risk-set fold in
     // as zeros), a GEMV weighted sum, and one weighted-crossprod GEMM for the
     // Fisher. The timed hazard keeps PLAIN exp() with no max-shift: its
     // -timespan * sum(exp) enters the likelihood absolutely (Non-Goal), so the
     // scale must not shift.
     double timespan_current_event = timespan(id_event);
     // declare the ids of the sender and the receiver,
     const int id_sender = dep_event_mat(0, id_event) - 1;
     const int id_receiver = dep_event_mat(1, id_event) - 1;
     if (return_availability) {
       const bool dependent_interval = is_dependent(id_event) == 1;
       availability_seen_sender.zeros();
       mark_availability(
         n_actors_1 * n_actors_2, allowed, &dyad_sender,
         availability_seen_sender
       );
       accumulate_availability(
         availability_seen_sender, timespan_current_event, dependent_interval,
         &availability_exposure_sender, &availability_opportunities_sender
       );
       availability_seen_receiver.zeros();
       mark_availability(
         n_actors_1 * n_actors_2, allowed, &dyad_receiver,
         availability_seen_receiver
       );
       accumulate_availability(
         availability_seen_receiver, timespan_current_event, dependent_interval,
         &availability_exposure_receiver, &availability_opportunities_receiver
       );
     }
     lin_pred = stat_mat * parameters;
     // exp() first, then zero the masked dyads (avoids Inf * 0 = NaN when a
     // masked row overflows); an allowed dyad may legitimately overflow — that
     // is the divergence signal the damping handles.
     e = arma::exp(lin_pred);
     for (arma::uword k = 0; k < n_dyads; ++k) {
       if (allowed(k) == 0) e(k) = 0;
     }
     double normalizer = accu(e);
     if (return_total_rate) total_rate(id_event) = normalizer;
     if (return_margins) {
       // Compensator scale: c = Dt applied to the raw intensities, so the
       // contribution is Dt * lambda_ij, credited to both endpoints. The
       // observed side is counted in the dependent-event branch below, which is
       // why `dependent` is false here.
       accumulate_margins(
         e, timespan_current_event, allowed, 0, false, margin_sides
       );
     }
     weighted_sum_current_event = e.t() * stat_mat;
     // The Fisher product's weighted copy is written into its buffer and
     // scaled in place, rather than formed as a fresh n1 * n2 x p temporary.
     weighted_stat_mat = stat_mat;
     weighted_stat_mat.each_col() %= e;
     fisher_current_event = weighted_stat_mat.t() * stat_mat;
     // add the quantities of a current event to the variables to be returned
     // derivative
     derivative -= timespan_current_event * weighted_sum_current_event;
     // fisher matrix
     fisher += timespan_current_event * fisher_current_event;
     accumulate_event_information(
       fisher_current_event, timespan_current_event, id_event, weights_mat,
       weighted_information, event_information_trace
     );
     // logLikelihood
     intervalLogL(id_event) = -timespan_current_event * normalizer;
     if (is_dependent(id_event)) {
       const int id_obs = id_sender * n_actors_2 + id_receiver;
       intervalLogL(id_event) += lin_pred(id_obs);
       derivative += stat_mat.row(id_obs);
       if (return_margins) {
         margin_observed_sender(id_sender) += 1;
         margin_observed_receiver(id_receiver) += 1;
       }
       if (return_ranks) {
         observed_rank[id_event] = rank_of_observed(e, allowed, id_obs);
       }
     }
     // Quantities that enter as a ratio or as a log of the normalizer: the
     // conditional component, the probability vector and what reads it. The
     // likelihood's total rate stays on the absolute scale -- it enters as
     // -Dt * T, so shifting it would be a different model, not a
     // stabilization -- and these are derived from that same raw pass:
     // log T = log(normalizer) and p = e / normalizer, exact whenever the
     // normalizer is a normal positive double. Where it is not -- the raw sum
     // overflowed to Inf, underflowed to 0, or is subnormal, so the ratio
     // would carry only a few significant bits -- the max-shifted log-sum-exp
     // takes over, which is exact in all three. The probability vector is
     // formed only for the primitives that read it.
     const bool need_probabilities =
       return_probabilities || return_margins || return_conditional_scores;
     if (need_probabilities || return_total_rate) {
       const bool raw_usable =
         !force_shifted_normalizer && std::isnormal(normalizer);
       double log_normalizer;
       if (raw_usable) {
         log_normalizer = std::log(normalizer);
         if (need_probabilities) probabilities = e / normalizer;
       } else {
         ++n_shifted_normalizer;
         log_normalizer = log_sum_exp_masked(lin_pred, allowed, weights);
         const double shifted_total = arma::sum(weights);
         // An empty risk set (every candidate masked) leaves no weight to
         // normalize: each probability is 0, not 0 / 0.
         if (need_probabilities) {
           if (shifted_total > 0) {
             probabilities = weights / shifted_total;
           } else {
             probabilities.zeros(weights.n_elem);
           }
         }
       }
       if (return_total_rate) {
         conditional_logl(id_event) = NA_REAL;
       }
       if (return_conditional_scores) {
         conditional_scores.row(id_event).fill(NA_REAL);
       }
       if (is_dependent(id_event)) {
         const int id_obs = id_sender * n_actors_2 + id_receiver;
         if (return_total_rate) {
           conditional_logl(id_event) = lin_pred(id_obs) - log_normalizer;
         }
         if (return_conditional_scores) {
           // On the raw route p' X is the weighted sum the derivative took,
           // over the normalizer; only the fallback forms it afresh.
           conditional_scores.row(id_event) = event_score_row(
             raw_usable
               ? arma::rowvec(weighted_sum_current_event / normalizer)
               : arma::rowvec(probabilities.t() * stat_mat),
             1.0, stat_mat, static_cast<arma::uword>(id_obs), true
           );
         }
         if (return_margins) {
           // Probability scale, over dependent events only: this is the
           // parallel-to-choice calibration map, so it must total the same set
           // the observed sides count — events, not intervals.
           for (int i = 0; i < n_actors_1; ++i) {
             for (int j = 0; j < n_actors_2; j++) {
               const double p = probabilities(i * n_actors_2 + j);
               margin_probability_sender(i) += p;
               margin_probability_receiver(j) += p;
             }
           }
         }
       }
       if (return_probabilities) {
         // Written straight into the R matrix the fit stores, cell by cell,
         // instead of through a reshaped and transposed Armadillo copy.
         NumericMatrix grid(n_actors_1, n_actors_2);
         for (int i = 0; i < n_actors_1; ++i) {
           for (int j = 0; j < n_actors_2; ++j) {
             grid(i, j) = probabilities(i * n_actors_2 + j);
           }
         }
         event_probabilities[id_event] = grid;
       }
     }
     // The stored per-event score comes from the shared reduction over the same
     // raw rate vector `e` the margin reduction reads (its weighted sum is the
     // one the derivative above already took), rather than from the
     // before/after difference of the running derivative, so the definition
     // lives in one place instead of once per kernel. On a right-censored
     // interval there is no observed dyad, so the observed-row index is unused
     // and passed as 0, exactly as the margin call above does. The derivative
     // is deliberately untouched: it drives the optimizer, and no coefficient
     // may move.
     if (return_event_scores) {
       const bool dependent_event = is_dependent(id_event) == 1;
       const arma::uword obs_row = dependent_event
         ? static_cast<arma::uword>(id_sender * n_actors_2 + id_receiver)
         : 0;
       event_scores.row(id_event) = event_score_row(
         weighted_sum_current_event, timespan_current_event, stat_mat,
         obs_row, dependent_event
       );
     }
     // loglikelihood
     logLikelihood += intervalLogL(id_event);
   }
   
   List out = List::create(
     Named("derivative") = derivative,
     Named("fisher") = fisher,
     Named("intervalLogL") = intervalLogL,
     Named("logLikelihood") = logLikelihood,
     Named("event_scores") = event_scores,
     Named("observed_rank") = observed_rank,
     Named("margin_observed_sender") = margin_observed_sender,
     Named("margin_expected_sender") = margin_expected_sender,
     Named("margin_observed_receiver") = margin_observed_receiver,
     Named("margin_expected_receiver") = margin_expected_receiver,
     Named("margin_probability_sender") = margin_probability_sender,
     Named("margin_probability_receiver") = margin_probability_receiver,
     Named("total_rate") = total_rate,
     Named("conditional_logl") = conditional_logl,
     Named("conditional_scores") = conditional_scores,
     Named("availability_exposure_sender") = availability_exposure_sender,
     Named("availability_n_opportunities_sender") =
       availability_opportunities_sender,
     Named("availability_exposure_receiver") = availability_exposure_receiver,
     Named("availability_n_opportunities_receiver") =
       availability_opportunities_receiver,
     Named("event_probabilities") = event_probabilities,
     Named("weighted_information") = weighted_information,
     Named("event_information_trace") = event_information_trace
   );
   if (report_normalizer_route) {
     out.push_back(n_shifted_normalizer, "n_shifted_normalizer");
     out.push_back(
       static_cast<double>(probabilities.n_elem), "probability_buffer_size"
     );
   }
   return out;
 }
