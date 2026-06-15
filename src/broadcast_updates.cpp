#include "broadcast_updates.h"
// [[Rcpp::depends(RcppArmadillo)]]

void apply_broadcast_updates(
    arma::mat& stat_mat,
    const arma::mat& stat_mat_broadcast,
    int& broadcast_id,
    const int broadcast_end,
    const int n_actors_1,
    const int n_actors_2,
    const bool twomode_or_reflexive
) {
  while (broadcast_id < broadcast_end) {
    const int kind = static_cast<int>(stat_mat_broadcast(0, broadcast_id));
    const int fixed = static_cast<int>(stat_mat_broadcast(1, broadcast_id));
    const int effect = static_cast<int>(stat_mat_broadcast(2, broadcast_id));
    const double value = stat_mat_broadcast(3, broadcast_id);
    if (kind == 1) {
      // broadcast over senders, holding alter `fixed`
      for (int i = 0; i < n_actors_1; ++i) {
        if (twomode_or_reflexive || i != fixed) {
          stat_mat(i * n_actors_2 + fixed, effect) = value;
        }
      }
    } else if (kind == 2) {
      // broadcast over alters, holding ego `fixed`
      for (int j = 0; j < n_actors_2; ++j) {
        if (twomode_or_reflexive || j != fixed) {
          stat_mat(fixed * n_actors_2 + j, effect) = value;
        }
      }
    } else {
      // kind 3: broadcast to all actors
      for (int i = 0; i < n_actors_1; ++i) {
        for (int j = 0; j < n_actors_2; ++j) {
          if (twomode_or_reflexive || i != j) {
            stat_mat(i * n_actors_2 + j, effect) = value;
          }
        }
      }
    }
    broadcast_id++;
  }
}
