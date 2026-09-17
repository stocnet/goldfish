# a guard refuses values it cannot use

    Code
      set_simulation_guard(max_events = 0)
    Condition
      Error in `set_simulation_guard()`:
      ! `max_events` must be one positive whole number.
      x You supplied 0.

---

    Code
      set_simulation_guard(max_events = 2.5)
    Condition
      Error in `set_simulation_guard()`:
      ! `max_events` must be one positive whole number.
      x You supplied 2.5.

---

    Code
      set_simulation_guard(max_events = Inf)
    Condition
      Error in `set_simulation_guard()`:
      ! `max_events` must be one positive whole number.
      x You supplied Inf.

---

    Code
      set_simulation_guard(rate_multiple = 0.5)
    Condition
      Error in `set_simulation_guard()`:
      ! `rate_multiple` must be one number of at least 1, or `Inf`.
      x You supplied 0.5.

---

    Code
      set_simulation_guard(wait_collapse = NA)
    Condition
      Error in `set_simulation_guard()`:
      ! `wait_collapse` must be one number of at least 1, or `Inf`.
      x You supplied NA.

---

    Code
      set_simulation_guard(wait_window = 0)
    Condition
      Error in `set_simulation_guard()`:
      ! `wait_window` must be one positive whole number.
      x You supplied 0.

---

    Code
      set_simulation_guard(clock_resolution = -1)
    Condition
      Error in `set_simulation_guard()`:
      ! `clock_resolution` must be `NULL` or one finite number of at least 0.
      x You supplied -1.

---

    Code
      set_simulation_guard(clock_resolution = "0")
    Condition
      Error in `set_simulation_guard()`:
      ! `clock_resolution` must be `NULL` or one finite number of at least 0.
      x You supplied "0".

# a guard prints which stops are on

    Code
      print(set_simulation_guard())
    Message
      -- <goldfishSimGuard> ----------------------------------------------------------
      Stops on: max_events and clock_resolution
      max_events: 10 x observed dependent events
      rate_multiple: off
      wait_collapse: off
      wait_window: 50
      clock_resolution: 0 x window length

---

    Code
      print(set_simulation_guard(max_events = 200, rate_multiple = 1000,
        wait_collapse = 10, wait_window = 20, clock_resolution = NULL))
    Message
      -- <goldfishSimGuard> ----------------------------------------------------------
      Stops on: max_events, rate_multiple, and wait_collapse
      max_events: 200
      rate_multiple: 1000 x total rate at the first event
      wait_collapse: observed mean wait / 10
      wait_window: 20
      clock_resolution: off

