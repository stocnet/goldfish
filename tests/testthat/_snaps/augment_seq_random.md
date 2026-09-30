# a panel no legal ordering reaches aborts at the ceiling

    Code
      random_augmenter(coupled_spec(data))
    Condition
      Error in `random_augmenter()`:
      ! No support-legal draw of the interval in 5 attempts.
      x The coupled support constraint "friendship: ~tie(friendship) | outdeg(friendship, type = \"ego\") < 1" leaves a legal draw vanishingly likely.
      i A support constraint that reads another latent event of the same interval can disable a flip before it is placed; relax the constraint or observe the panel more often.

