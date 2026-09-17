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

# The sender gate a rate risk set should have, from a dense grid and without
# any package code: a sender is at risk when some present receiver is
# allowed. On a one-mode layer the self-dyad is not a receiver, whatever the
# grid holds on its diagonal; on a two-mode layer the two indices name
# different node sets and every cell counts.
oracle_sender_gate <- function(grid, present, one_mode) {
  allowed <- grid & matrix(present, nrow(grid), ncol(grid), byrow = TRUE)
  if (one_mode) {
    diag(allowed) <- FALSE
  }
  rowSums(allowed) > 0
}

# A one-mode flavored layer of four actors in which N1 holds a tie to every
# other actor from the start. Before every event up to its own dissolution
# at t = 7, and again after it recreates that tie at t = 9, its creation row
# allows only its own self-dyad, while the other actors create and dissolve
# around it.
self_only_sender_data <- function() {
  history <- data.frame(
    from = c(1L, 1L, 1L),
    to = c(2L, 3L, 4L),
    time = NA_real_,
    weight = 1
  )
  events <- data.frame(
    from = c(2L, 3L, 2L, 4L, 3L, 2L, 1L, 3L, 1L, 4L),
    to = c(3L, 4L, 3L, 2L, 4L, 1L, 2L, 2L, 2L, 2L),
    time = as.numeric(1:10),
    weight = c(1, 1, -1, 1, -1, 1, -1, 1, 1, -1)
  )
  ties <- rbind(history, events)
  ties$layer <- "calls"
  add_flavor(
    list(
      info = list(
        name = "self-only",
        focal = "calls",
        update = c(calls = "increment"),
        directed = c(calls = TRUE),
        observation = c(calls = "event")
      ),
      nodes = data.frame(
        label = paste0("N", 1:4),
        mode = "p",
        stringsAsFactors = FALSE
      ),
      ties = ties
    ),
    layer = "calls",
    values_equivalence = c(creation = 1, dissolution = -1)
  )
}
