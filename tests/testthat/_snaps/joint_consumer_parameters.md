# simulate's value-gate abort names the unpinned free effect(s)

    Code
      joint_simulation_parameters(p, arg = "coef")
    Condition
      Error:
      ! `coef` leaves 4 free coefficients unpinned.
      x Unpinned: "calls › choice: inertia(calls)", "emails › rate: Intercept", "emails › rate: indeg(emails)", and "emails › choice: inertia(emails)".
      i Pin every free coefficient with `set_init_param()` before simulating.

