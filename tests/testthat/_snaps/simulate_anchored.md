# an anchored run refuses targets and guards

    Code
      simulate(js, coef = parameters, times = "observed", n_events = 2)
    Message
      Simulating time-anchored, as requested.
      i The specification's default is "generated": it carries a timed rate. The observed event times are held and the marks redrawn at each.
    Condition
      Error in `simulate()`:
      ! `n_events` does not apply to a time-anchored run.
      x The observed event times bound the run, so there is no target to reach and no clock to guard.
      i Drop `n_events`, or run free-running with `times = "generated"`.

---

    Code
      simulate(js, coef = parameters, times = "observed", horizon = 3, control_sim = set_simulation_guard(
        max_events = 10))
    Message
      Simulating time-anchored, as requested.
      i The specification's default is "generated": it carries a timed rate. The observed event times are held and the marks redrawn at each.
    Condition
      Error in `simulate()`:
      ! `horizon` and `control_sim` do not apply to a time-anchored run.
      x The observed event times bound the run, so there is no target to reach and no clock to guard.
      i Drop `horizon` and `control_sim`, or run free-running with `times = "generated"`.

# a coordination specification is refused until it simulates

    Code
      simulate(coordination, coef = 1)
    Condition
      Error in `simulate()`:
      ! Simulating a coordination process is not available yet.
      i Its pair is drawn from the mechanism's mark distribution, which the simulation does not implement yet.

# an anchored run prints its stamps and skips

    Code
      print(out)
    Message
      <goldfishSim>: 80 events
      times: "observed" — from the specification (no rate)
      * calls › creation › choice (modeled)
      * calls › dissolution › choice (modeled)
      Walked 80 observed stamps; 0 skipped.

---

    Code
      print(pool)
    Message
      -- <goldfishSimPool> -----------------------------------------------------------
      3 replicates · times "observed" (from the specification) · 2 processes
      * calls › creation › choice [fid 1] (modeled)
      * calls › dissolution › choice [fid 2] (modeled)
      Events per replicate: min 80 · median 80 · mean 80.0 · max 80
      Observed stamps skipped per replicate (of 80): min 0 · median 0 · max 0
      Stop reasons: observed 3
      i Select replicates by their summary with `filter_simulation()`.

