#ifndef GOLDFISH_AVAILABILITY_UPDATES_H
#define GOLDFISH_AVAILABILITY_UPDATES_H

#include <RcppArmadillo.h>

// Walk one event's slice of a folded availability buffer (active_sender or
// active_dyad), advancing update_id up to update_end and calling `write` with
// each column index in the slice. Every default_c estimator shares this pointer
// walk; the per-column write stays with the caller because its shape differs by
// encoding -- the alter/sender encoding writes one node index, the point
// encoding a flattened (node1, node2) cell. An empty slice
// (update_end <= update_id) calls `write` zero times.
//
// Header-only because it is a template: the per-engine write is a lambda the
// compiler inlines here, so unlike flat_updates it has no companion .cpp.
template <typename WriteColumn>
inline void apply_availability_updates(
    int& update_id,
    const int update_end,
    WriteColumn write
) {
  while (update_id < update_end) {
    write(update_id);
    update_id++;
  }
}

#endif
