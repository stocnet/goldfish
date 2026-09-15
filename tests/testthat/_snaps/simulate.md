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
      Simulating past the last observed exogenous change.
      i Covariate and composition state is held at its value from 4 for the rest of the run.

---

    Code
      print(out)
    Message
      <goldfishSim>: 6 events
      times: "generated" — requested; the specification's default is "observed"
      * calls › rate (completed)
      * calls › choice (modeled)

