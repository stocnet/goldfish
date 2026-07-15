#include "flat_updates.h"
// [[Rcpp::depends(RcppArmadillo)]]

void apply_flat_updates(
    arma::mat& stat_mat,
    const arma::mat& stat_mat_update,
    int& update_id,
    const int update_end,
    const int n_actors_2
) {
  while (update_id < update_end) {
    stat_mat(
      stat_mat_update(0, update_id) * n_actors_2 +
        stat_mat_update(1, update_id),
      stat_mat_update(2, update_id)) =
      stat_mat_update(3, update_id);
    update_id++;
  }
}
