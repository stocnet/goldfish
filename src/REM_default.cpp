#include <RcppArmadillo.h>
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
//' @param stat_mat_rightcensored_update An matrix with four rows,
//'     which record the updates of the statistics matrix through all 
//'     right-censored events.
//'     the structure is similar to stat_mat_update.
//' @param stat_mat_update_pointer An n_events by 1 matrix that record 
//'    which update belongs to which rightcensored event.
//'    The structure is similar to stat_mat_update_pointer.
//' @param presence1_init An n_actor1 by 1 matrix, which records 
//'    the initial presence of each actor1.
//'    If the i-th actor1 is not present in the
//'    beginning then the i-th entry of presence1_init is 0, otherwise it's 1.
//' @param presence1_update An matrix with two rows, which record the 
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
//' @param presence2_update_pointer An n_events by 1 matrix that record 
//'    which update belongs to which (dependent+ rightcensored) event.
//'    The structure is similar to stat_mat_update_pointer.
//' @param presence2_init An n_actors2 by 1 matrix, which records the
//'    initial presence of each actor2.
//'    If the i-th actor2 is not present in the beginning then the i-th entry
//'    of presence2_init is 0, otherwise it's 1.
//' @param presence2_update An matrix with two rows, which record the updates
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
//' @param presence2_update_pointer An n_events by 1 matrix that record
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
    const arma::mat& stat_mat_rightcensored_update,
    const arma::vec& stat_mat_rightcensored_update_pointer,
    const arma::vec& presence1_init,
    const arma::mat& presence1_update,
    const arma::vec& presence1_update_pointer,
    const arma::vec& presence2_init,
    const arma::mat& presence2_update,
    const arma::vec& presence2_update_pointer,
    const int n_actors_1,
    const int n_actors_2,
    const bool twomode_or_reflexive,
    bool impute
) {
   // initialize stat_mat and numbers
   arma::mat stat_mat = stat_mat_init;
   int n_events = is_dependent.n_elem;
   int n_parameters = stat_mat.n_cols;
   // declare auxilliary variables
   arma::rowvec weighted_sum_current_event(n_parameters);
   arma::mat fisher_current_event(n_parameters, n_parameters);
   int stat_mat_update_id = 0;
   int stat_mat_rightcensored_update_id = 0;
   int id_dep_event = 0;
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
     if (is_dependent(id_event)) {
       while (stat_mat_update_id < stat_mat_update_pointer(id_dep_event)) {
         stat_mat(
           stat_mat_update(0, stat_mat_update_id) * n_actors_2 +
            stat_mat_update(1, stat_mat_update_id),
           stat_mat_update(2, stat_mat_update_id)) =
           stat_mat_update(3, stat_mat_update_id);
         stat_mat_update_id++;
       }
     } else {
       while (stat_mat_rightcensored_update_id < \
         stat_mat_rightcensored_update_pointer(id_event - id_dep_event)) {
         stat_mat(
           stat_mat_rightcensored_update(0, stat_mat_rightcensored_update_id) *
             n_actors_2 +
            stat_mat_rightcensored_update(1, stat_mat_rightcensored_update_id),
           stat_mat_rightcensored_update(2, stat_mat_rightcensored_update_id)) =
           stat_mat_rightcensored_update(3, stat_mat_rightcensored_update_id);
         stat_mat_rightcensored_update_id++;
       }
     }
     
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
       while (presence1_update_id < presence1_update_pointer(id_event)) {
         presence1(presence1_update(0, presence1_update_id) - 1) \
         = presence1_update(1, presence1_update_id);
         presence1_update_id++;
       }
     }
     if (has_composition_change2) {
       while (presence2_update_id < presence2_update_pointer(id_event)) {
         presence2(presence2_update(0, presence2_update_id) - 1) \
         = presence2_update(1, presence2_update_id);
         presence2_update_id++;
       }
     }
     
     
     
     // We calculate the derivative, log-Likelihood,
     //   and fisher matrix of a current event according to the paper.
     // Reset auxilliary variables
     weighted_sum_current_event.zeros();
     fisher_current_event.zeros();
     double normalizer = 0;
     double timespan_current_event = timespan(id_event);
     // declare the ids of the sender and the receiver,
     const int id_sender = dep_event_mat(0, id_event) - 1;
     const int id_receiver = dep_event_mat(1, id_event) - 1;
     // Go through all actor1-actor2 pairs
     for (int i = 0; i < n_actors_1; ++i) {
       if (presence1(i) == 1) {
         // declare the subviews of th stat mat corresponding
         //   to the first sender
         const arma::mat& current_data_matrix \
         = stat_mat.rows(i * n_actors_2, (i + 1) * n_actors_2 - 1);
         // deal with twomode and allow reflexive
         int not_allowed_receiver = -1;
         if (!twomode_or_reflexive) not_allowed_receiver = i;
         // go through all receiver
         for (int j = 0; j < n_actors_2; j++) {
           if (presence2(j) == 1 && (j != not_allowed_receiver)) {
             // exp_current_receiver is \exp(\beta^T s)
             double exp_current_receiver \
             = std::exp(dot(current_data_matrix.row(j), parameters));
             normalizer += exp_current_receiver;
             weighted_sum_current_event \
             += exp_current_receiver * (current_data_matrix.row(j));
             fisher_current_event += exp_current_receiver *
               ((current_data_matrix.row(j).t()) *
               (current_data_matrix.row(j)));
           }
         }
       }
     }
     // add the quantities of a current event to the variables to be returned
     // derivative
     derivative -= timespan_current_event * weighted_sum_current_event;
     // fisher matrix
     fisher += timespan_current_event * fisher_current_event;
     // logLikelihood
     intervalLogL(id_event) = -timespan_current_event * normalizer;
     // update id_dep_event
     if (is_dependent(id_event)) {
       intervalLogL(id_event) \
       += dot(stat_mat.row(id_sender * n_actors_2 + id_receiver), parameters);
       derivative += stat_mat.row(id_sender * n_actors_2 + id_receiver);
       id_dep_event++;
     }
     // loglikelihood
     logLikelihood += intervalLogL(id_event);
   }
   
   return List::create(
     Named("derivative") = derivative,
     Named("fisher") = fisher,
     Named("intervalLogL") = intervalLogL,
     Named("logLikelihood") = logLikelihood
   );
 }
