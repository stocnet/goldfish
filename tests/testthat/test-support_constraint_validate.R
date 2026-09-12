# Preprocessing-time set-size validation. Unit tests over
# validate_support_constraint() with synthetic masks so each case A-E (and the
# rate silent gated-out case) is exercised in isolation, under a pinned cli
# context for reproducible conditions.

# A 4x4 fixture: two dependent events, sender 1 -> receiver 2, sender 2 -> 3.
# The masks are given outright, event by event, and encoded into the flat stream
# the producer emits, so these stay unit tests of the validation and not of the
# representation.
make_validate_inputs <- function(support) {
  list(
    support_mask = mask_from_timeline(support),
    event_sender = c(1L, 2L),
    event_receiver = c(2L, 3L),
    is_dependent = c(1L, 1L),
    active_1 = rep(TRUE, 4L),
    active_2 = rep(TRUE, 4L)
  )
}

all_true <- function() matrix(TRUE, 4L, 4L)

call_validate <- function(support, family) {
  a <- make_validate_inputs(support)
  validate_support_constraint(
    a$support_mask,
    a$event_sender,
    a$event_receiver,
    a$is_dependent,
    a$active_1,
    a$active_2,
    family = family
  )
}

test_that("case A: an observed dyad excluded errors (choice)", {
  s1 <- all_true()
  s1[1, 2] <- FALSE # observed receiver 2 excluded, sender 1 keeps others
  expect_error(
    call_validate(list(s1, all_true()), "choice"),
    "observed dyad is excluded"
  )
})

test_that("case B/D: an empty risk set errors (choice)", {
  s1 <- all_true()
  s1[1, ] <- FALSE # sender 1 has no allowed receiver
  expect_error(
    call_validate(list(s1, all_true()), "choice"),
    "empty risk set"
  )
})

test_that("case C: a single-candidate event warns (forced choice)", {
  s1 <- matrix(FALSE, 4L, 4L)
  s1[1, 2] <- TRUE # sender 1: only the observed receiver 2 allowed
  expect_warning(
    call_validate(list(s1, all_true()), "choice"),
    "forced choice"
  )
})

test_that("case E: a never-allowed receiver warns (choice)", {
  s <- all_true()
  s[, 4] <- FALSE # receiver 4 is never an allowed candidate
  expect_warning(
    call_validate(list(s, s), "choice"),
    "never an allowed candidate"
  )
})

test_that("choice case E: a never-observed gated-out sender warns", {
  # An ego-style gate: sender 3 is allowed no receiver at any event and is
  # never observed. The choice family was silent about this until it gained the
  # sender-side counterpart of the never-a-candidate warning.
  s <- all_true()
  s[3, ] <- FALSE
  expect_warning(
    expect_no_error(call_validate(list(s, s), "choice")),
    "sender.*allowed no receiver at any event"
  )
})

test_that("choice case E sender warning names the gated-out node", {
  withr::local_options(cli.num_colors = 1L)
  local_reproducible_output()
  s <- all_true()
  s[3, ] <- FALSE
  expect_snapshot(call_validate(list(s, s), "choice"))
})

test_that("rate case B: an observed sender gated out errors", {
  s1 <- all_true()
  s1[1, ] <- FALSE # sender 1 (observed at event 1) has no allowed receiver
  expect_error(
    call_validate(list(s1, all_true()), "rate"),
    "gated out"
  )
})

test_that("rate: a non-observed gated-out sender passes silently (warns, no error)", {
  s <- all_true()
  s[3, ] <- FALSE # sender 3 is never observed; gated out is benign (case E warn)
  expect_warning(
    expect_no_error(call_validate(list(s, s), "rate")),
    "never at risk"
  )
})

# The choice branch's four verdicts are all defined over "allowed AND present
# receivers", so freezing the receiver presence at time zero is wrong in both
# directions: a receiver that joins mid-sequence and is never allowed goes
# unnamed by the never-a-candidate warning, and a dependent event whose receiver
# joined after time zero is wrongly reported as an excluded observed dyad.
call_validate_choice_moving <- function(
  support,
  active_1,
  active_2,
  update,
  pointer
) {
  a <- make_validate_inputs(support)
  validate_support_constraint(
    a$support_mask,
    a$event_sender,
    a$event_receiver,
    a$is_dependent,
    active_1,
    active_2,
    family = "choice",
    active_2_update = update,
    active_2_update_pointer = pointer
  )
}

test_that("choice: a late never-allowed receiver is named live, not frozen", {
  s <- matrix(FALSE, 4L, 4L)
  s[1, c(2L, 3L)] <- TRUE
  s[2, c(1L, 3L)] <- TRUE
  a1 <- c(TRUE, TRUE, FALSE, FALSE) # only senders 1, 2 present (and observed)
  a2 <- c(TRUE, TRUE, TRUE, FALSE) # receiver 4 absent at time zero
  # Frozen: receiver 4 is never counted present, so it is not named.
  expect_no_warning(call_validate_choice_moving(list(s, s), a1, a2, NULL, NULL))
  # Live: receiver 4 joins before event 2 and, never an allowed candidate,
  # is named.
  expect_warning(
    call_validate_choice_moving(list(s, s), a1, a2, rbind(4, 1), c(0L, 1L)),
    "never an allowed candidate"
  )
})

test_that("choice: an observed receiver that joined after t0 does not abort", {
  # Everything is allowed; the only reason to abort would be the observed
  # receiver reading absent. Receiver 3 (event 2's observed receiver) is absent
  # at time zero and joins before event 2.
  a2 <- c(TRUE, TRUE, FALSE, TRUE)
  # Frozen presence would report the observed dyad as excluded; live must not.
  expect_no_error(suppressWarnings(call_validate_choice_moving(
    list(all_true(), all_true()),
    rep(TRUE, 4L),
    a2,
    rbind(3, 1),
    c(0L, 1L)
  )))
})

# The rate gate reduces the mask over the receivers PRESENT at the event, and
# the receiver composition moves, so the check has to move with it. Both
# directions are wrong when the receiver presence is frozen at time zero: a
# sender whose only allowed receiver has yet to arrive is not gated out, and
# one whose only allowed receiver has departed is.
call_validate_rate_moving <- function(support, active_2, update, pointer) {
  a <- make_validate_inputs(support)
  validate_support_constraint(
    a$support_mask,
    a$event_sender,
    a$event_receiver,
    a$is_dependent,
    a$active_1,
    active_2,
    family = "rate",
    active_2_update = update,
    active_2_update_pointer = pointer
  )
}

test_that("rate: an arriving receiver un-gates the sender it is allowed for", {
  s <- matrix(FALSE, 4L, 4L)
  s[1, 2] <- TRUE # sender 1, observed at event 1, keeps receiver 2 throughout
  s[2, 3] <- TRUE # sender 2, observed at event 2, is allowed receiver 3 only
  expect_no_error(suppressWarnings(call_validate_rate_moving(
    list(s, s),
    c(TRUE, TRUE, FALSE, TRUE), # receiver 3 absent at event 1
    rbind(3, 1), # and arrives in time for event 2
    c(0L, 1L)
  )))
})

test_that("rate: a departing receiver gates out the sender allowed for it", {
  s <- matrix(FALSE, 4L, 4L)
  s[1, 2] <- TRUE
  s[2, 3] <- TRUE
  expect_error(
    suppressWarnings(call_validate_rate_moving(
      list(s, s),
      rep(TRUE, 4L),
      rbind(3, 0), # receiver 3 leaves before sender 2's own event
      c(0L, 1L)
    )),
    "gated out"
  )
})
