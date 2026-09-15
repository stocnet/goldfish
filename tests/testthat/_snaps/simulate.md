# an intercept-only rate with no choice is refused at entry

    Code
      simulate(spec, coef = -1, n_events = 2)
    Condition
      Error in `simulate()`:
      ! Cannot simulate a process with no effect to draw from.
      x "calls › rate" is an intercept-only rate with no choice.
      i Completion would add a uniform choice, so every draw would be a constant rate and an equiprobable receiver.
      i Add an effect to the rate, or model a choice.

