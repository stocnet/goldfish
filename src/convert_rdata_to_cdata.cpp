#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

//' a function to extract the update of composition change from an events and transform the data into a matrix
//' and a vector.
//'
//' @param event an event objects with the information on composition change
//' @param reference_event_time a vector of time stamps that separate updates different time spans.
//' @return A list with two element: changeMat and change_idx. For the structure of these two object see, e.g., the documentation of estimate_DyNAM_choice, in which they are used for stat_mat_update, and stat_mat_update_pointer.
//' @noRd
// [[Rcpp::export]]
List C_convert_composition_change(const DataFrame& event, const arma::vec& reference_event_time) {
  const arma::vec& event_time = event["time"];
  const arma::vec& id = event["node"];
  const arma::vec& presence = event["replace"];
  int n_events = event_time.n_elem;
  int n_reference_events = reference_event_time.n_elem;
  int size = n_events;
  arma::mat presence_update(2, size);
  // n points devide a line into n+1 parts, but we know the total number...
  arma::vec presence_update_pointer(n_reference_events);

  int total = 0;
  for (int i = 0; i < n_reference_events; ++i) {
    while (1) {
      if ((total >= n_events) || (event_time(total) > reference_event_time(i))) {
        presence_update_pointer(i) = total;
        break;
      } else {
        total++;
      }
    }
  }

  for (int i = 0; i < n_events; ++i) {
    presence_update(0, i) = id(i);
    presence_update(1, i) = presence(i);
  }
  return List::create(Named("presenceUpdate") = presence_update,
                      Named("presenceUpdatePointer") = presence_update_pointer);
}



// similar to the the above, but take a event as the second argument
// [[Rcpp::export]]
List convert_composition_change (const DataFrame& event, const DataFrame reference_event) {
  const arma::vec& reference_event_time = reference_event["time"];
  return C_convert_composition_change(event, reference_event_time);
}


//' a function to transform a list of updates into a matrix
//' and a vector.
//' @noRd
// [[Rcpp::export]]
List convert_change(const List& changeList) {
  int n_events = changeList.size();
  const List& temp_list = changeList[0];
  int n_parameters = temp_list.size();


  int size = 4000;
  arma::mat change_mat(4, size);
  arma::vec change_idx(n_events);

  int total = 0;
  for (int i = 0; i < n_events; i++) {
    const List& current_list = changeList[i];
    for (int j = 0; j < n_parameters; j++) {
      if (!Rf_isNull(current_list[j])) {
        const arma::mat& temp_mat = current_list[j];
        while (2 * (total + temp_mat.n_rows) > (unsigned) size) {
          size = size * 2;
          change_mat.resize(4, size);
        }
        for (unsigned int id_change = 0; id_change < temp_mat.n_rows; id_change++) {
          change_mat(0, total) = temp_mat(id_change, 0) - 1;
          change_mat(1, total) = temp_mat(id_change, 1) - 1;
          change_mat(2, total) = j;
          change_mat(3, total) = temp_mat(id_change, 2);
          total += 1;
        }
      }

      change_idx(i) = total;
    }
  }
 
  // not adjust in case there are no changes
  if (total > 0) {
    total -= 1;
  } 

  return List::create(Named("statMatUpdate") = change_mat.cols(0, total),
                      Named("statMatUpdatePointer") = change_idx);
}





