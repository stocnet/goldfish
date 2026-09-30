# an incomplete wave snapshot aborts before diffing

    Code
      diff_panel_waves(data, "friendship", wave_times = c(0, 5, 10))
    Condition
      Error:
      ! Wave diffing needs complete snapshots.
      x Layer "friendship" has missing (NA) tie values at waves 5 and 10.
      i A missing value is not a tie absence; complete the waves before estimating. Partially observed waves are not supported.

# a state outside the flavor mapping aborts

    Code
      diff_panel_waves(data, "friendship", wave_times = c(0, 5, 10))
    Condition
      Error:
      ! A state panel's snapshots must take the states of its flavor mapping.
      x Layer "friendship" reaches a state outside 0 and 1 at waves 5 and 10.
      i A dichotomous layer holds each tie at 0 or 1.

# the wave grid needs increasing boundaries

    Code
      diff_panel_waves(data, "friendship", wave_times = c(0, 10, 5))
    Condition
      Error:
      ! `wave_times` must be at least two strictly increasing boundaries.
      i Start the grid at the observation start and list every wave after it.

# a flip moved to another interval misses both endpoints

    Code
      validate_endpoint_hitting(retimed(c(6, 4, 7)), diff)
    Condition
      Error:
      ! The sequence does not hit the wave snapshots of layer "friendship".
      x Interval 1 leaves 1 flip unplaced.
      x Interval 2 places an event on "N3" -> "N4" outside its flip set.

