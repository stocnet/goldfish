# a panel with a single wave and no start has no interval

    Code
      panel_wave_grid(recipe_count_spec(data))
    Condition
      Error:
      ! The panel data gives no interval to augment.
      x The modeled panel layer "calls" records a single wave and no observation start precedes it.
      i Set a `start_time` before the first wave in `set_preprocessing()`, or pass `wave_times`.

# a non-injective flavor map aborts

    Code
      check_flavor_map_injective(transitions, "friendship", call = NULL)
    Condition
      Error:
      ! The flavor mapping of layer "friendship" is not injective.
      x Two flavors share a state transition, so a wave change could not name its flavor.

