# walk_open aborts on an incomplete specification

    Code
      walk_open(js)
    Condition
      Error:
      ! `walk_open()` needs a generatively complete specification.
      x Missing sub-model: "calls › choice".
      i Run the generative-completion transform first: `simulate()` and `estimate_dynes()` complete a half-specified spec (every modeled DyNAM flavor gains both a rate and a choice) at their entry. `walk_open()` only opens an already-complete spec.

# a constraint on an object no formula reads is refused

    Code
      walk_open(spec)
    Condition
      Error:
      ! The walk handle cannot maintain this support constraint.
      x Constraint atom "tie(emails)" reads a changing object no formula term reads, so the shared walk never steps its events.

