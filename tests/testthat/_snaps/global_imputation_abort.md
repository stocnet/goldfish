# a missing initial global value aborts, naming the object

    Code
      assert_globals_defined(state, registry, empty_schedule)
    Condition
      Error in `assert_globals_defined()`:
      ! Global attribute "climate" has a missing initial value.
      x A global attribute has a single value, so there is nothing to summarize it from.
      i Give it an observed initial value, or a first event that sets one, before the observation window.

# a missing global replace event aborts, naming the object and time

    Code
      assert_globals_defined(state, registry, schedule)
    Condition
      Error in `assert_globals_defined()`:
      ! Global attribute "climate" has a missing value at time 1.5.
      x A global attribute has a single value, so a missing update cannot be imputed from other values.
      i Give the event an explicit value.

