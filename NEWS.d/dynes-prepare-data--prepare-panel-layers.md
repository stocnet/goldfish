* Added `prepare_dynes_data()` to turn survey panel layers into change-lists:
  * Each layer declares a style: `"mutually_exclusive"`, `"count"` or `"none"`.
  * Its `form` says what a wave value means: a state, an amount or a change.
  * Wave states are diffed, with an explicit row for every dropped tie.
  * Amounts since the last wave are summed into running totals.
  * A binary layer leaves flavored, so `add_flavor()` is not needed after it.
  * Every wave keeps its own time; `set_preprocessing()` sets the start.
  * `wave_times` declares each layer's waves; an empty wave dissolves all ties.
  * Repeated dyads, self-ties, invalid values and falling totals abort.
  * A wave that writes no rows warns, since `estimate_dynes()` would merge it.
  * A `wave_times` grid must match the `time` column's class, or it aborts.
  * Preparing a layer twice, or one already flavored, aborts.
* Added a section on `prepare_dynes_data()` to the `dynes-panel` vignette.
