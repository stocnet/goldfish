#ifndef GOLDFISH_BROADCAST_UPDATES_H
#define GOLDFISH_BROADCAST_UPDATES_H

#include <RcppArmadillo.h>

// Decode the broadcast (constant-value fan-out) slice for one event into the
// flattened n_actors_1 * n_actors_2 x nEffects stat matrix, advancing
// broadcast_id to broadcast_end. Each column of stat_mat_broadcast is a coded
// entry (rows: kind, fixed, effect, replace; fixed and effect 0-indexed).
// kind 1 broadcasts over senders holding the alter `fixed`; kind 2 over alters
// holding the ego `fixed`; kind 3 over all actors. The reflexive diagonal cell
// is skipped when !twomode_or_reflexive, matching to_alter()/to_ego()/
// fillChanges(). An empty slice (broadcast_end <= broadcast_id) is a no-op.
void apply_broadcast_updates(
    arma::mat& stat_mat,
    const arma::mat& stat_mat_broadcast,
    int& broadcast_id,
    const int broadcast_end,
    const int n_actors_1,
    const int n_actors_2,
    const bool twomode_or_reflexive
);

#endif
