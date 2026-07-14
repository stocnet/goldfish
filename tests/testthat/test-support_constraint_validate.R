# Preprocessing-time set-size validation. Unit tests over
# validate_support_constraint() with synthetic masks so each case A-E (and the
# rate silent gated-out case) is exercised in isolation, under a pinned cli
# context for reproducible conditions.

# A 4x4 fixture: two dependent events, sender 1 -> receiver 2, sender 2 -> 3.
make_validate_inputs <- function(support) {
  list(
    support_mask = list(support = support),
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
