#ifndef GOLDFISH_FLAT_UPDATES_H
#define GOLDFISH_FLAT_UPDATES_H

#include <RcppArmadillo.h>

// Apply the cell-specific (point) flat update slice for one event into the
// flattened n_actors_1 * n_actors_2 x nEffects stat matrix, advancing
// update_id to update_end. Each column of stat_mat_update is a coded cell
// update (rows: node1, node2, effect, replace; node1/node2/effect 0-indexed),
// written at row node1 * n_actors_2 + node2, column effect. An empty slice
// (update_end <= update_id) is a no-op. Shared by every default_c estimator so
// the mask/active-set buffers can reuse the same flat-update semantics.
void apply_flat_updates(
    arma::mat& stat_mat,
    const arma::mat& stat_mat_update,
    int& update_id,
    const int update_end,
    const int n_actors_2
);

#endif
