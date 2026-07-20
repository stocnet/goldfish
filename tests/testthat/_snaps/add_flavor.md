# add_flavor aborts on non-dichotomous and non-syntactic mappings

    Code
      add_flavor(x, "friendship", c(a = 1, b = -1, c = 1))
    Condition
      Error in `add_flavor()`:
      ! `values_equivalence` must map exactly two states (a dichotomous layer).
      x Got 3 values for layer "friendship".
      i Only dichotomous state mappings are supported; a multistate encoding would accumulate (increment) or replace (replace) raw values -- consider `weighted = FALSE` in effect terms.

---

    Code
      add_flavor(x, "friendship", c(`tie created` = 1, dissolution = -1))
    Condition
      Error in `add_flavor()`:
      ! Flavor names in `values_equivalence` must be syntactic R names.
      x Non-syntactic name: "tie created".
      i Flavor names key `make_specification()` formula lists.

# add_flavor aborts when values do not match the update encoding

    Code
      add_flavor(x, "friendship", c(creation = 2, dissolution = -3))
    Condition
      Error in `add_flavor()`:
      ! `values_equivalence` values must match the layer's update encoding.
      x Layer "friendship" is "increment"; expected values -1 and 1 but got 2 and -3.
      i Increment layers toggle a state by +1 / -1; weighted updates are not dichotomous states.

# add_flavor aborts on a weighted layer with an uncovered value

    Code
      add_flavor(x, "friendship", c(creation = 1, dissolution = -1))
    Condition
      Error in `add_flavor()`:
      ! Layer "friendship" carries update value outside `values_equivalence`.
      x Uncovered values: 2.
      i Only dichotomous 1 and -1 states are supported; a weighted layer is not a creation/dissolution toggle -- consider `weighted = FALSE` in effect terms.

# add_flavor aborts on a bad style and an unknown layer

    Code
      add_flavor(x, "friendship", c(creation = 1, dissolution = -1), flavor_style = "both")
    Condition
      Error in `add_flavor()`:
      ! `flavor_style` must be one of "mutually_exclusive" or "redundant", not "both".

---

    Code
      add_flavor(x, "nope", c(creation = 1, dissolution = -1))
    Condition
      Error in `add_flavor()`:
      ! `layer` must name a single layer present in ties.
      x "nope" is not among "friendship".

# the validator rejects malformed flavor metadata on info

    Code
      validate_goldfish_data(bad_style)
    Condition
      Error:
      ! `flavor_style` must be one of "mutually_exclusive" and "redundant".
      x Got "wrong".

---

    Code
      validate_goldfish_data(unknown_style_layer)
    Condition
      Error:
      ! info$flavor_style names must be layers present in ties.
      x Unknown layer: "nope".

---

    Code
      validate_goldfish_data(not_a_list)
    Condition
      Error:
      ! info$values_equivalence must be a named list (one mapping per layer).

---

    Code
      validate_goldfish_data(bad_mapping)
    Condition
      Error:
      ! `values_equivalence` must map exactly two states (a dichotomous layer).
      x Got 1 value for layer "friendship".
      i Only dichotomous state mappings are supported; a multistate encoding would accumulate (increment) or replace (replace) raw values -- consider `weighted = FALSE` in effect terms.

# a declared mutually exclusive style is checked against the events

    Code
      invisible(add_flavor(fix, layer = "friendship", values_equivalence = c(
        creation = 1, dissolution = -1)))
    Condition
      Warning in `add_flavor()`:
      ! Layer "friendship" is declared "mutually_exclusive", but an event contradicts it.
      x "creation" at "2" on "A"–"C", whose state is already 1.
      i A specification would put that dyad outside this flavor's risk set and fail when it reaches the observed event.
      i Use `flavor_style = "redundant"` if repeated same-direction events are meaningful here.

