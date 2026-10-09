# the composition warning names each actor, interval and time

    Code
      out <- rewrite_composition(snapped_data(), list(1L, 2L), snap_waves)
    Condition
      Warning:
      Composition snapped to the panel waves.
      ! Actors involved in an interval are made present over all of it; their real join and leave times inside it are ignored.
      * N1 in (0, 2]: real change at 1 ignored
      * N2 in (2, 5]: real change at 3 ignored
      i The model changes: a moved actor's rate enters the compensator over the whole interval.
      i Relational events stamped at a wave can see an actor moved to that wave one interval early.

# a long list of snapped actors is truncated

    Code
      out <- rewrite_composition(data, list(seq_len(n), integer()), c(0, 2, 5))
    Condition
      Warning:
      Composition snapped to the panel waves.
      ! Actors involved in an interval are made present over all of it; their real join and leave times inside it are ignored.
      * N1 in (0, 2]: real change at 1 ignored
      * N2 in (0, 2]: real change at 1 ignored
      * N3 in (0, 2]: real change at 1 ignored
      * N4 in (0, 2]: real change at 1 ignored
      * N5 in (0, 2]: real change at 1 ignored
      * N6 in (0, 2]: real change at 1 ignored
      * N7 in (0, 2]: real change at 1 ignored
      * N8 in (0, 2]: real change at 1 ignored
      * N9 in (0, 2]: real change at 1 ignored
      * N10 in (0, 2]: real change at 1 ignored
      i and 20 more
      i The model changes: a moved actor's rate enters the compensator over the whole interval.
      i Relational events stamped at a wave can see an actor moved to that wave one interval early.

# the composition warning shows times in the data's time class

    Code
      out <- rewrite_composition(posix_data(), list(1L, 2L), posix_day(snap_waves))
    Condition
      Warning:
      Composition snapped to the panel waves.
      ! Actors involved in an interval are made present over all of it; their real join and leave times inside it are ignored.
      * N1 in (2020-01-01, 2020-01-03]: real change at "2020-01-02" ignored
      * N2 in (2020-01-03, 2020-01-06]: real change at "2020-01-04" ignored
      i The model changes: a moved actor's rate enters the compensator over the whole interval.
      i Relational events stamped at a wave can see an actor moved to that wave one interval early.

