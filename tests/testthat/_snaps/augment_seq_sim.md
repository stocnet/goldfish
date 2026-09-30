# an interval no draw can complete aborts at the ceiling

    Code
      draw_sim(built, 1L, c(0.2, 0.2, -1, 0.5))
    Condition
      Error:
      ! No support-legal draw of the interval in 5 attempts.
      x The coupled support constraint "friendship: ~tie(friendship) | recip(friendship)" leaves a legal draw vanishingly likely.
      i A support constraint that reads another latent event of the same interval can disable a flip before it is placed; relax the constraint or observe the panel more often.

