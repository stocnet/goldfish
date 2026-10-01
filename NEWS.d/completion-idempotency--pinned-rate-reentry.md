* Fixed `simulate()` not warning that a rate is pinned when given an
  already-completed specification, such as a simulation result's or a fitted
  model's `spec`. The warning now fires at every entry and says the stored pin
  is kept, not recomputed.
