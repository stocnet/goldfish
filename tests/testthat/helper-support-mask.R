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

# The folded rate gate, per stored event, for a hand-written mask timeline.
# Builds the minimal preprocessed object `fold_active_sender_support()`
# reads -- the stored times, sender presence as crossings, and the receiver
# presence it counts over -- runs the fold, and decodes the folded
# `active_sender` back into one logical vector per event.
fold_gate_timeline <- function(
  masks,
  stored_kind,
  presence1,
  presence2,
  receiver_crossings = NULL,
  ...
) {
  n_stored <- length(masks)
  out <- list(
    event_time = seq_len(n_stored),
    active_sender_init = presence1,
    active_sender_update = matrix(0, 2L, 0L),
    active_sender_update_pointer = integer(n_stored),
    active_dyad_update = receiver_crossings$update,
    active_dyad_update_pointer = receiver_crossings$pointer,
    avg_active_entity = 0
  )
  folded <- fold_active_sender_support(
    out,
    mask_from_timeline(masks, stored_kind),
    presence2,
    ...
  )
  gate <- folded$active_sender_init
  update <- folded$active_sender_update
  pointer <- folded$active_sender_update_pointer
  seen <- 0L
  lapply(seq_len(n_stored), function(e) {
    hi <- pointer[[e]]
    if (hi > seen) {
      cols <- (seen + 1L):hi
      gate[update[1L, cols]] <<- as.logical(update[2L, cols])
      seen <<- hi
    }
    gate
  })
}

# A mask timeline's dense grids, expanded by hand from the stored kind, so a
# test's expectation never travels through the package's own expansion.
mask_grids <- function(masks, stored_kind, n1, n2) {
  lapply(masks, function(mask) {
    switch(
      as.character(stored_kind),
      "3" = matrix(as.logical(mask), n1, n2),
      "2" = matrix(as.logical(mask), n1, n2),
      "1" = matrix(as.logical(mask), n1, n2, byrow = TRUE),
      "0" = matrix(as.logical(mask), n1, n2)
    )
  })
}
