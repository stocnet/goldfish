# reaching the ceiling aborts naming the coupled constraint

    Code
      redraw_until_accepted(proposal$propose, accept = function(value) FALSE,
      constraint = "friendship: ~tie(friendship) | outdeg(friendship) < 1", ceiling = 5L)
    Condition
      Error:
      ! No support-legal draw of the interval in 5 attempts.
      x The coupled support constraint "friendship: ~tie(friendship) | outdeg(friendship) < 1" leaves a legal draw vanishingly likely.
      i A support constraint that reads another latent event of the same interval can disable a flip before it is placed; relax the constraint or observe the panel more often.

