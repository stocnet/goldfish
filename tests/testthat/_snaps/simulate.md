# a pool prints its aggregate, not its replicates

    Code
      print(pool)
    Message
      -- <goldfishSimPool> -----------------------------------------------------------
      3 replicates · times "generated" (from the specification) · 4 processes
      * calls › rate [fid 1] (modeled)
      * calls › choice [fid 2] (modeled)
      * emails › rate [fid 3] (modeled)
      * emails › choice [fid 4] (modeled)
      Events per replicate: min 14 · median 18 · mean 18.0 · max 22
      Stop reasons: horizon 3
      i Select replicates by their summary with `filter_simulation()`.

---

    Code
      print(pool[[2]])
    Message
      <goldfishSim>: 14 events
      times: "generated" — from the specification (timed rate)
      * calls › rate (modeled)
      * calls › choice (modeled)
      * emails › rate (modeled)
      * emails › choice (modeled)

---

    Code
      print(guarded)
    Message
      -- <goldfishSimPool> -----------------------------------------------------------
      6 replicates · times "generated" (from the specification) · 4 processes
      * calls › rate [fid 1] (modeled)
      * calls › choice [fid 2] (modeled)
      * emails › rate [fid 3] (modeled)
      * emails › choice [fid 4] (modeled)
      Events per replicate: min 14 · median 18 · mean 17.3 · max 20
      Stop reasons: horizon 3 · max_events 3
      ! 3 replicates stopped at a guard.
      i Select replicates by their summary with `filter_simulation()`.

# filter_simulation() refuses a condition outside the summary

    Code
      filter_simulation(pool, stop_reasn == "horizon")
    Condition
      Error in `filter_simulation()`:
      ! Cannot filter on stop_reasn: not a column of the pool's summary.
      i The summary has replicate, n_events, end_time, stop_reason, capped, n_proposals, and acceptance_rate.

---

    Code
      filter_simulation(pool, n_events[1] > 0)
    Condition
      Error in `filter_simulation()`:
      ! `n_events[1] > 0` must give one logical per replicate.
      x It gave a <logical> of length 1 for 2 replicates.

# a filter keeping nothing returns an empty pool that prints

    Code
      print(empty)
    Message
      -- <goldfishSimPool> -----------------------------------------------------------
      0 replicates
      i Select replicates by their summary with `filter_simulation()`.

# anchoring a timed model is announced, then refused for now

    Code
      simulate(js, coef = sim_two_process_parameters(js), times = "observed",
      n_events = 2)
    Message
      Simulating time-anchored, as requested.
      i The specification's default is "generated": it carries a timed rate. The observed event times are held and the marks redrawn at each.
    Condition
      Error in `simulate()`:
      ! Time-anchored simulation is not available yet.
      i Run free-running with `times = "generated"`.

# a model with no estimated clock cannot generate times

    Code
      simulate(ordered, coef = 1, times = "generated", n_events = 2)
    Condition
      Error in `simulate()`:
      ! Cannot generate event times for an ordered rate.
      x An ordered rate models which event comes next, not when, so it estimates no clock to draw waiting times from.
      i Simulate it time-anchored with `times = "observed"`.

---

    Code
      simulate(coordination, coef = 1, times = "generated", n_events = 2)
    Condition
      Error in `simulate()`:
      ! Cannot generate event times for a coordination process.
      x Coordination timing is estimated under Cox, and the rate of the proposals behind its realized events is not identified from them.
      i Simulate it time-anchored with `times = "observed"`.

# an intercept-only rate with no choice is refused at entry

    Code
      simulate(spec, coef = -1, n_events = 2)
    Condition
      Error in `simulate()`:
      ! Cannot simulate a process with no effect to draw from.
      x "calls › rate" is an intercept-only rate with no choice.
      i Completion would add a uniform choice, so every draw would be a constant rate and an equiprobable receiver.
      i Add an effect to the rate, or model a choice.

# a choice-only DyNAM defaults to time-anchored

    Code
      simulate(spec, coef = c(0.2, 0.3), n_events = 2)
    Condition
      Error in `simulate()`:
      ! Time-anchored simulation is not available yet.
      i Run free-running with `times = "generated"`.

# a choice-only DyNAM generates only when asked

    Code
      out <- simulate(spec, seed = 3, coef = c(0.2, 0.3), times = "generated",
      n_events = 6)
    Condition
      Warning in `simulate()`:
      ! Layer "calls" has no rate sub-model; completing it with a pinned intercept-only rate (zero free parameters).
      i Completed because `times = "generated"` was requested; the specification's default is "observed", with no rate.
      i The default adds no free parameter; it is auto-supplied for the simulate generative surface.
      Warning in `simulate()`:
      ! Rate 1 is an intercept-only rate (`~ 1`): it is pinned, not estimated.
      i The pin comes from the observed event count over the relevant period.
      Warning in `simulate()`:
      Simulation flagged 1 of 1 replicate.
      ! 1 ran past the end of the observation window at 5; covariate and composition state is held at its value there.
      i Flagged replicates stay in the result; each one's diagnostics holds its stop reason and total-rate trajectory.

---

    Code
      print(out)
    Message
      <goldfishSim>: 6 events
      times: "generated" — requested; the specification's default is "observed"
      * calls › rate (completed)
      * calls › choice (modeled)

# a clock that cannot advance stops and flags the replicate

    Code
      print(out)
    Message
      <goldfishSim>: 3 events
      times: "generated" — from the specification (timed rate)
      * calls › rate (modeled)
      * calls › choice (modeled)
      * emails › rate (modeled)
      * emails › choice (modeled)
      ! Run stopped at the "clock_resolution" guard at time 1.03;
      flagged as capped.

# simulate refuses a guard that is not a guard object

    Code
      simulate(js, coef = sim_two_process_parameters(js), control_sim = list(
        max_events = 20))
    Condition
      Error in `simulate()`:
      ! `control_sim` must be a <goldfishSimGuard>.
      x A <list> was supplied.
      i Build one with `set_simulation_guard()`.

# simulate refuses arguments it does not take

    Code
      simulate(js, coef = sim_two_process_parameters(js), max_events = 20)
    Condition
      Error in `simulate()`:
      ! `...` must be empty.
      x Problematic argument:
      * max_events = 20

# a call warns once about guard stops and the frozen state

    Code
      invisible(simulate(js, nsim = 3, seed = 4, coef = parameters, n_events = 200,
        control_sim = set_simulation_guard(max_events = 30)))
    Condition
      Warning in `simulate()`:
      Simulation flagged 3 of 3 replicates.
      ! 3 stopped at a guard (3 at "max_events"); each keeps the events drawn before the stop and is flagged capped.
      ! 3 ran past the end of the observation window at 5; covariate and composition state is held at its value there.
      i Flagged replicates stay in the result; each one's diagnostics holds its stop reason and total-rate trajectory.

