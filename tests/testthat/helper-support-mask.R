# The mask timeline a `support_mask` encodes, as one value per stored event at
# the mask's stored kind.
#
# The object carries an initial value plus a flat stream of the entries that
# flipped, keyed by a cumulative per-event pointer, rather than one stored value
# per event. A test that wants the timeline rebuilds it here; a test that is
# ABOUT the representation asserts on `initial` / `update` / `update_pointer`
# directly, and should not come through this helper.
mask_timeline <- function(support_mask) {
  cursor <- mask_cursor(support_mask)
  lapply(seq_along(support_mask$update_pointer), cursor)
}

# The inverse: a `support_mask` encoding a timeline given outright. Lets a test
# state the masks it means, event by event, and hand the producer's own
# representation to a consumer. `mask_timeline()` round-trips it.
mask_from_timeline <- function(masks, stored_kind = 0L) {
  n_stored <- length(masks)
  n_changes <- integer(n_stored)
  entries <- vector("list", n_stored)
  values <- vector("list", n_stored)
  for (e in seq_len(n_stored)[-1L]) {
    crossing <- emit_crossings(masks[[e - 1L]], masks[[e]])
    entries[[e]] <- crossing$entries
    values[[e]] <- as.numeric(crossing$values)
    n_changes[[e]] <- length(crossing$entries)
  }
  at <- unlist(entries, use.names = FALSE)
  list(
    initial = masks[[1L]],
    update = if (length(at) > 0L) {
      rbind(as.numeric(at), unlist(values, use.names = FALSE))
    } else {
      matrix(0, 2L, 0L)
    },
    update_pointer = cumsum(n_changes),
    n_stored = n_stored,
    stored_kind = stored_kind,
    symmetric = FALSE
  )
}
