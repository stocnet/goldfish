#include <RcppArmadillo.h>
#include "broadcast_updates.h"
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
//' @param active_sender_init An n_actor1 by 1 matrix, which records 
//'    the initial presence of each actor1.
//'    If the i-th actor1 is not present in the
//'    beginning then the i-th entry of active_sender_init is 0, otherwise it's 1.
//' @param active_sender_update An matrix with two rows, which record the 
//'    updates of the presence of actor1 through all events.
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
//' @param active_dyad_update_pointer An n_events by 1 matrix that record 
//'    which update belongs to which (dependent+ rightcensored) event.
//'    The structure is similar to stat_mat_update_pointer.
//' @param active_dyad_init An n_actors2 by 1 matrix, which records the
//'    initial presence of each actor2.
//'    If the i-th actor2 is not present in the beginning then the i-th entry
//'    of active_dyad_init is 0, otherwise it's 1.
//' @param active_dyad_update An matrix with two rows, which record the updates
//'    of the presence of actor2 through all events.
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
//' @param impute An boolean variable. If it true, the function does
//'    the imputation for missing values.
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
    const arma::vec& active_sender_init,
    const arma::mat& active_sender_update,
    const arma::vec& active_sender_update_pointer,
    const arma::vec& active_dyad_init,
    const arma::mat& active_dyad_update,
    const arma::vec& active_dyad_update_pointer,
    const int n_actors_1,
    const int n_actors_2,
    const bool twomode_or_reflexive,
    bool impute,
    const bool active_dyad_is_point,
    const bool return_event_scores = false,
    const bool return_ranks = false,
    const bool return_margins = false,
    const bool return_total_rate = false,
    const bool return_probabilities = false
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
   // risk set. `weights` is flattened sender-major (dyad (i, j) at
   // i * n_actors_2 + j) while an arma::mat fills column-major, so the n1 x n2
   // grid is recovered by reshaping to n2 x n1 and transposing.
   List event_probabilities(return_probabilities ? n_events : 0);


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
   // `active_dyad` is the folded per-event risk mask. At the
   // outer encoding it is the length-n2 receiver vector (cell (i, j) available iff
   // active_sender(i) & active_dyad(j)). At the point encoding it is a flattened
   // n1 x n2 mask (sender-major: dyad (i, j) at i * n_actors_2 + j) with both
   // presences n support folded in, maintained by a (node1, node2, replace)
   // buffer and read cell-wise.
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
         // Construct a view for the i-th column of the stat_matrix
         //   and do the impute
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
         active_sender(active_sender_update(0, active_sender_update_id) - 1) \
         = active_sender_update(1, active_sender_update_id);
         active_sender_update_id++;
       }
     }
     if (has_composition_change2) {
       while (active_dyad_update_id < active_dyad_update_pointer(id_event)) {
         if (active_dyad_is_point) {
           active_dyad(
             (active_dyad_update(0, active_dyad_update_id) - 1) * n_actors_2 +
             (active_dyad_update(1, active_dyad_update_id) - 1)
           ) = active_dyad_update(2, active_dyad_update_id);
         } else {
           active_dyad(active_dyad_update(0, active_dyad_update_id) - 1) \
           = active_dyad_update(1, active_dyad_update_id);
         }
         active_dyad_update_id++;
       }
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
     // build the risk-set mask over all n1 * n2 dyads (sender-major)
     arma::vec allowed(n_actors_1 * n_actors_2, fill::zeros);
     for (int i = 0; i < n_actors_1; ++i) {
       if (active_sender(i) == 1) {
         int not_allowed_receiver = twomode_or_reflexive ? -1 : i;
         // point encoding: sender i's row starts at i * n_actors_2; outer
         // encoding: the receiver vector is read directly (offset 0).
         int dyad_offset = active_dyad_is_point ? i * n_actors_2 : 0;
         for (int j = 0; j < n_actors_2; j++) {
           if (active_dyad(dyad_offset + j) == 1 && (j != not_allowed_receiver)) {
             allowed(i * n_actors_2 + j) = 1;
           }
         }
       }
     }
     arma::vec lin_pred = stat_mat * parameters;
     // exp() first, then zero the masked dyads (avoids Inf * 0 = NaN when a
     // masked row overflows); an allowed dyad may legitimately overflow — that
     // is the divergence signal the damping handles.
     arma::vec e = arma::exp(lin_pred);
     e.elem(arma::find(allowed < 0.5)).zeros();
     double normalizer = accu(e);
     if (return_total_rate) total_rate(id_event) = normalizer;
     if (return_margins) {
       // `e` is already zero on masked dyads, so the double sum ranges over the
       // realized risk set; sender and receiver totals coincide by construction.
       for (int i = 0; i < n_actors_1; ++i) {
         for (int j = 0; j < n_actors_2; j++) {
           const double contrib = timespan_current_event * e(i * n_actors_2 + j);
           margin_expected_sender(i) += contrib;
           margin_expected_receiver(j) += contrib;
         }
       }
     }
     weighted_sum_current_event = e.t() * stat_mat;
     fisher_current_event = (stat_mat.each_col() % e).t() * stat_mat;
     // add the quantities of a current event to the variables to be returned
     // derivative
     arma::rowvec score_before;
     if (return_event_scores) score_before = derivative.row(0);
     derivative -= timespan_current_event * weighted_sum_current_event;
     // fisher matrix
     fisher += timespan_current_event * fisher_current_event;
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
         const double obs_rate = e(id_obs);
         int rank = 1;
         for (unsigned int d = 0; d < e.n_elem; d++) {
           if (allowed(d) == 1 && e(d) > obs_rate) rank++;
         }
         observed_rank[id_event] = rank;
       }
     }
     // Quantities that enter as a ratio or as a log of the normalizer, from a
     // max-shifted pass computed BESIDE the raw `e` above rather than replacing
     // it. The likelihood's total rate must stay on the absolute scale — it
     // enters as -Dt * T, so shifting it would be a different model, not a
     // stabilization — and leaving the raw pass untouched is also what keeps
     // every frozen coefficient exactly where it was. These three are
     // shift-invariant, and are exact where the raw ratio silently returns 1
     // (subnormal underflow) or NaN (overflow).
     if (return_probabilities || return_margins || return_total_rate) {
       arma::vec weights;
       double log_normalizer = log_sum_exp_masked(lin_pred, allowed, weights);
       double shifted_total = arma::sum(weights);
       arma::vec probabilities = weights / shifted_total;
       if (return_total_rate) {
         conditional_logl(id_event) = NA_REAL;
       }
       if (is_dependent(id_event)) {
         const int id_obs = id_sender * n_actors_2 + id_receiver;
         if (return_total_rate) {
           conditional_logl(id_event) = lin_pred(id_obs) - log_normalizer;
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
         arma::mat grid =
           arma::reshape(probabilities, n_actors_2, n_actors_1).t();
         event_probabilities[id_event] = wrap(grid);
       }
     }
     if (return_event_scores) {
       event_scores.row(id_event) = derivative.row(0) - score_before;
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
     Named("margin_observed_sender") = margin_observed_sender,
     Named("margin_expected_sender") = margin_expected_sender,
     Named("margin_observed_receiver") = margin_observed_receiver,
     Named("margin_expected_receiver") = margin_expected_receiver,
     Named("margin_probability_sender") = margin_probability_sender,
     Named("margin_probability_receiver") = margin_probability_receiver,
     Named("total_rate") = total_rate,
     Named("conditional_logl") = conditional_logl,
     Named("event_probabilities") = event_probabilities
   );
 }
