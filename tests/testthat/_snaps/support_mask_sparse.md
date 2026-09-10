# a support_mask predating the update stream is refused

    Code
      mask_cursor(stale)
    Condition
      Error in `mask_cursor()`:
      ! This preprocessed object's support_mask predates the mask update stream.
      x It carries a stored mask per event (support) and no update buffer, so its constraint cannot be read over time.
      i Preprocess the specification again with this version of goldfish.

