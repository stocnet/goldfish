#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;
#include "gather_progress.hpp"

inline arma::mat reduce_mat_to_vector(
    const arma::mat& stat_mat,
    const int& n_actors_1,
    const int& n_actors_2,
    const bool& twomode_or_reflexive
);

// @inherit estimate_REM params

//' Gathering data for sender receiver model
//'
//' Gathering data for models for choosing an sender,
//'     i.e. DyNAM-rate and DyNAM-rate-ordered models.
//'     Only the useful information are recorded,
//'     e.g. if a actor1 is not present then its information is not collected.
//' @param verbose An boolean variable. It it's true,
//'     the function prints the progress to the screen.
//' @return Return a list with elements as follows.
//'     The meaning of the argument can be found in corresponding
//'     computation codes, e.g. compute_poisson_selection.cpp.
//' @noRd
// [[Rcpp::export]]
 List gather_sender_model(
     const arma::mat& dep_event_mat,
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
     const bool verbose,
     bool impute = true
 ) {
   // initialize stat_mat and numbers
   arma::mat stat_mat = stat_mat_init;
   int n_events = dep_event_mat.n_cols;
   int n_parameters = stat_mat.n_cols;
   // declare auxilliary variables
   int stat_mat_update_id = 0;
   int stat_mat_rightcensored_update_id = 0;
   int id_dep_event = 0;
   int n_total = 0;
   
   
   // composition change
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
   
   // declare return variables
   int size_stat_all_events = n_actors_1 * n_events;
   arma::mat stat_all_events(size_stat_all_events, n_parameters, fill::zeros);
   arma::vec chosens(n_events, fill::zeros);
   arma::vec n_presence(n_events, fill::zeros);
   
   // Go through all events
   for (int id_event = 0; id_event < n_events; id_event++) {
     // Print the progress to  consoles if verbose
     if (verbose) {
       if (id_event % 20 == 0) {
         gather_progress_render(id_event, n_events);
       }
     }
     
     // stat_mat_update
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
       while (stat_mat_rightcensored_update_id <
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
     // composition change
     if (has_composition_change1) {
       while (presence1_update_id < presence1_update_pointer(id_event)) {
         presence1(presence1_update(0, presence1_update_id) - 1) =
           presence1_update(1, presence1_update_id);
         presence1_update_id++;
       }
     }
     if (has_composition_change2) {
       while (presence2_update_id < presence2_update_pointer(id_event)) {
         presence2(presence2_update(0, presence2_update_id) - 1) =
           presence2_update(1, presence2_update_id);
         presence2_update_id++;
       }
     }
     
     // TO check(gutian): handle ignorant
     
     
     // In the following part, we take out the rows corresponding to pairs
     // of present senders and receiver in this event.
     // reduce the stat mat from n_actors_1 * n_actors_2 * n_parameters to
     // n_actors_1 * n_parameters by taking average.
     arma::mat reduced_stat_mat
     = reduce_mat_to_vector(stat_mat, n_actors_1, n_actors_2,
                            twomode_or_reflexive);
     // declare the ids of the sender and the receiver,and initialize
     // the variable to count the present pairs of actors in this event.
     int n_present = 0;
     const int id_sender = dep_event_mat(0, id_event) - 1;
     const bool is_dependent_current_event = is_dependent(id_event);
     for (int i = 0; i < n_actors_1; ++i) {
       if (presence1(i) == 1) {
         stat_all_events.row(n_total) = reduced_stat_mat.row(i);
         if (i == id_sender && is_dependent_current_event) {
           chosens(id_event) = n_present;
           id_dep_event++;
         }
         n_total++;
         n_present++;
       }
     }
     n_presence(id_event) = n_present;
     if (2 * n_total > size_stat_all_events) {
       size_stat_all_events = size_stat_all_events * 2;
       stat_all_events.resize(size_stat_all_events, n_parameters);
     }
   }
   // clear the screen(console)
   if (verbose) gather_progress_terminate();
   
   return List::create(
     Named("stat_all_events") = stat_all_events.rows(0, n_total - 1),
     Named("n_candidates") = n_presence,
     Named("selected") = chosens
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
