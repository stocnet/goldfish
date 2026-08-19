# walk_open aborts on an incomplete specification

    Code
      walk_open(js)
    Condition
      Error:
      ! `walk_open()` needs a generatively complete specification.
      x Missing sub-model: "calls › choice".
      i Run the generative-completion transform first: `simulate()` and `estimate_dynes()` complete a half-specified spec (every modeled DyNAM flavor gains both a rate and a choice) at their entry. `walk_open()` only opens an already-complete spec.

