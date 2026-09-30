# The mask timeline a `support_mask` encodes, as one value per stored event at
# the mask's stored kind.
#
# The object carries an initial value plus a flat stream of the entries that
# flipped, keyed by a cumulative per-event pointer, rather than one stored value
# per event. A test that wants the timeline rebuilds it here; a test that is
# ABOUT the representation asserts on `initial` / `update` / `update_pointer`
# directly, and should not come through this helper.
mask_timeline <- function(support_mask) {
  value <- support_mask$initial
  flips_at <- flip_reader(support_mask$update, support_mask$update_pointer)
  lapply(seq_along(support_mask$update_pointer), function(e) {
    flips <- flips_at(e)
    value[flips$entries] <<- flips$values
    value
  })
}

# The per-event slice of any flat `(entry, replace)` buffer -- a support
# mask's update stream or a presence crossings buffer: entries in the first
# row, replacements in the second, keyed by a cumulative per-event pointer.
# Returns a function of the stored event index, called with ascending
# indices.
flip_reader <- function(update, pointer) {
  seen <- 0L
  function(e) {
    hi <- if (is.null(pointer) || length(pointer) < e) 0L else pointer[[e]]
    if (hi <= seen) {
      return(list(entries = integer(0), values = logical(0)))
    }
    cols <- (seen + 1L):hi
    seen <<- hi
    list(
      entries = as.integer(update[1L, cols]),
      values = as.logical(update[2L, cols])
    )
  }
}

# The observed-event check over a hand-written mask stream, driven the way
# the walk drives it: each event's mask flips and the receiver crossings
# stamped at or before it, then the check at a dependent event, and the
# verdict raised at the end. `active_1` is the sender presence at the start;
# `active_2_update` / `active_2_update_pointer` a receiver crossings buffer
# keyed by the event.
validate_support_stream <- function(
  support_mask,
  event_sender,
  event_receiver,
  is_dependent,
  active_1,
  active_2,
  family,
  drop_diagonal,
  process_label = NULL,
  active_2_update = NULL,
  active_2_update_pointer = NULL
) {
  current <- support_mask$initial
  changes2 <- NULL
  if (!is.null(active_2_update) && ncol(active_2_update) > 0L) {
    changes2 <- list(
      time = rep(
        seq_along(active_2_update_pointer),
        diff(c(0L, active_2_update_pointer))
      ),
      node = as.integer(active_2_update[1L, ]),
      replace = as.logical(active_2_update[2L, ])
    )
  }
  av <- new_availability_effect(
    "gate",
    length(active_1),
    length(active_2),
    active_1,
    active_2,
    changes2 = changes2,
    mask = list(value = function() current),
    stored_kind = support_mask$stored_kind %||% 0L,
    drop_diagonal = drop_diagonal,
    record = FALSE
  )
  availability_enable_check(av, family, label = process_label)
  flips_at <- flip_reader(support_mask$update, support_mask$update_pointer)
  for (e in seq_along(event_sender)) {
    flips <- flips_at(e)
    current[flips$entries] <- flips$values
    availability_push_flips(av, flips)
    availability_step(
      av,
      0L,
      availability_presence_target(av$changes2, av$cursor2, e)
    )
    if (is_dependent[[e]] == 1L) {
      availability_check_event(av, e, event_sender[[e]], event_receiver[[e]])
    }
  }
  validate_support_constraint(
    availability_verdict(av),
    process_label = process_label
  )
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

# The rate gate, per stored event, for a hand-written mask timeline. Drives
# the rate's availability effect the way the walk does -- each event's mask
# flips, then the receiver crossings stamped at or before it -- over a mask
# that follows the timeline, and returns the gate after every event.
# `receiver_crossings` is a crossings buffer (`update`, cumulative
# `pointer`) keyed by the stored event.
fold_gate_timeline <- function(
  masks,
  stored_kind,
  presence1,
  presence2,
  receiver_crossings = NULL,
  drop_diagonal = FALSE
) {
  n_stored <- length(masks)
  stream <- mask_from_timeline(masks, stored_kind)
  current <- stream$initial
  changes2 <- NULL
  if (!is.null(receiver_crossings) && ncol(receiver_crossings$update) > 0L) {
    changes2 <- list(
      time = rep(
        seq_len(n_stored),
        diff(c(0L, receiver_crossings$pointer))
      ),
      node = as.integer(receiver_crossings$update[1L, ]),
      replace = as.logical(receiver_crossings$update[2L, ])
    )
  }
  av <- new_availability_effect(
    "gate",
    length(presence1),
    length(presence2),
    presence1,
    presence2,
    changes2 = changes2,
    mask = list(value = function() current),
    stored_kind = stored_kind,
    drop_diagonal = drop_diagonal,
    record = FALSE
  )
  flips_at <- flip_reader(stream$update, stream$update_pointer)
  lapply(seq_len(n_stored), function(e) {
    flips <- flips_at(e)
    current[flips$entries] <<- flips$values
    availability_push_flips(av, flips)
    availability_step(
      av,
      0L,
      availability_presence_target(av$changes2, av$cursor2, e)
    )
    av$value & TRUE
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
