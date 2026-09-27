# choice case E sender warning names the gated-out node

    Code
      call_validate(list(s, s), "choice")
    Condition
      Warning:
      ! `support_constraint`: 1 present sender allowed no receiver at any event.
      i Node: 3.

# every validation verdict keeps its full message

    Code
      call_validate(list(excluded, all_true()), "choice")
    Condition
      Error in `validate_support_constraint()`:
      ! `support_constraint`: the observed dyad is excluded.
      x Event 1: receiver 2 is not allowed for sender 1.

---

    Code
      call_validate(list(empty, all_true()), "choice")
    Condition
      Error in `validate_support_constraint()`:
      ! `support_constraint`: empty risk set at event 1.
      x Sender 1 has no allowed, present receiver.

---

    Code
      call_validate(list(forced, all_true()), "choice")
    Condition
      Warning:
      ! `support_constraint`: 1 event with a single candidate (forced choice; contributes 0 to the log-likelihood).
      i Event: 1.

---

    Code
      call_validate(list(never_receiver, never_receiver), "choice")
    Condition
      Warning:
      ! `support_constraint`: 1 receiver present at some event, never an allowed candidate.
      i Node: 4.

---

    Code
      call_validate(list(empty, all_true()), "rate")
    Condition
      Error in `validate_support_constraint()`:
      ! `support_constraint`: the observed sender is gated out.
      x Event 1: sender 1 has no allowed, present receiver.

---

    Code
      call_validate(list(never_sender, never_sender), "rate")
    Condition
      Warning:
      ! `support_constraint`: 1 present sender never at risk (always gated out).
      i Node: 3.

---

    Code
      call_validate_rate_moving(list(pairs, pairs), c(TRUE, TRUE, FALSE, TRUE), rbind(
        3, 1), c(0L, 1L))
    Condition
      Warning:
      ! `support_constraint`: 2 present senders never at risk (always gated out).
      i Nodes: 3 and 4.
      Warning:
      ! `support_constraint`: 1 event with a single candidate (forced choice; contributes 0 to the log-likelihood).
      i Event: 1.

---

    Code
      call_validate_rate_moving(list(pairs, pairs), rep(TRUE, 4L), rbind(3, 0), c(0L,
        1L))
    Condition
      Error in `validate_support_constraint()`:
      ! `support_constraint`: the observed sender is gated out.
      x Event 2: sender 2 has no allowed, present receiver.

