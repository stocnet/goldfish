# layers argument errors are reported before any row is rewritten

    Code
      prepare_dynes_data(x, layers = character())
    Condition
      Error in `prepare_dynes_data()`:
      ! `layers` must name at least one layer.
      i For example `layers = c(friendship = "mutually_exclusive")`.

---

    Code
      prepare_dynes_data(x, layers = "friendship")
    Condition
      Error in `prepare_dynes_data()`:
      ! Every entry of `layers` must be named by its layer.
      x Every layer needs a declared style, one of "mutually_exclusive", "count", and "none".

---

    Code
      prepare_dynes_data(x, layers = c(friendship = "count", friendship = "none"))
    Condition
      Error in `prepare_dynes_data()`:
      ! `layers` names a layer more than once.
      x Repeated layer: "friendship".

---

    Code
      prepare_dynes_data(x, layers = c(friendship = "ordinal"))
    Condition
      Error in `prepare_dynes_data()`:
      ! `layers` must use one of "mutually_exclusive", "count", and "none".
      x Not in the vocabulary: "ordinal".

---

    Code
      prepare_dynes_data(x, layers = c(missing = "count"))
    Condition
      Error in `prepare_dynes_data()`:
      ! `layers` names a layer that is not in ties.
      x Absent layer: "missing".
      i The layers of `x` are "friendship", "calls", "messages", and "advice".

---

    Code
      prepare_dynes_data(x, layers = c(messages = "count"))
    Condition
      Error in `prepare_dynes_data()`:
      ! Only panel layers can be prepared.
      x Layer not observed as "panel": "messages".
      i Event layers keep their times; use `add_flavor()` to flavor one.

# form argument errors are reported before any row is rewritten

    Code
      prepare_dynes_data(x, layers = c(friendship = "count"), form = c(friendship = "sideways"))
    Condition
      Error in `prepare_dynes_data()`:
      ! `form` must use one of "states", "per_wave", and "changes".
      x Not in the vocabulary: "sideways".

---

    Code
      prepare_dynes_data(x, layers = c(friendship = "count"), form = c(calls = "per_wave"))
    Condition
      Error in `prepare_dynes_data()`:
      ! `form` names a layer that is not in `layers`.
      x Layer not in `layers`: "calls".

---

    Code
      prepare_dynes_data(x, layers = c(friendship = "count"), form = c(friendship = "states",
        friendship = "per_wave"))
    Condition
      Error in `prepare_dynes_data()`:
      ! `form` names a layer more than once.
      x Repeated layer: "friendship".

---

    Code
      prepare_dynes_data(x, layers = c(friendship = "mutually_exclusive"), form = c(
        friendship = "per_wave"))
    Condition
      Error in `prepare_dynes_data()`:
      ! A binary tie has no amount per wave.
      x Layer "friendship" is declared "mutually_exclusive" with form "per_wave".
      i Declare the form "states" or "changes", or the style "count".

# flavor_names errors are reported before any row is rewritten

    Code
      prepare_dynes_data(x, layers = me, flavor_names = list(calls = roles))
    Condition
      Error in `prepare_dynes_data()`:
      ! `flavor_names` names a layer that is not in `layers`.
      x Layer not in `layers`: "calls".

---

    Code
      prepare_dynes_data(x, layers = c(friendship = "mutually_exclusive", advice = "none"),
      flavor_names = list(advice = roles))
    Condition
      Error in `prepare_dynes_data()`:
      ! Only a "mutually_exclusive" layer has flavors.
      x Layer "advice" is declared "none".

---

    Code
      prepare_dynes_data(x, layers = me, flavor_names = list(friendship = c(creation = "formed",
        other = "ended")))
    Condition
      Error in `prepare_dynes_data()`:
      ! The `flavor_names` entry for "friendship" must be named by the roles "creation" and "dissolution".
      i For example `c(creation = "formed", dissolution = "ended")`.

---

    Code
      prepare_dynes_data(x, layers = me, flavor_names = list(friendship = c(creation = "not valid",
        dissolution = "ended")))
    Condition
      Error in `prepare_dynes_data()`:
      ! Flavor labels must be syntactic R names.
      x Layer "friendship" has non-syntactic label: "not valid".
      i Flavor labels key `make_specification()` formula lists.

---

    Code
      prepare_dynes_data(x, layers = me, flavor_names = list(friendship = c(creation = "same",
        dissolution = "same")))
    Condition
      Error in `prepare_dynes_data()`:
      ! The two flavor labels must differ.
      x Layer "friendship" labels both flavors "same".

---

    Code
      prepare_dynes_data(x, layers = me, flavor_names = list(friendship = roles,
        friendship = roles))
    Condition
      Error in `prepare_dynes_data()`:
      ! `flavor_names` names a layer more than once.
      x Repeated layer: "friendship".

# a flavor for a count layer aborts with the count guidance

    Code
      prepare_dynes_data(x, layers = c(calls = "count"), flavor_names = list(calls = c(
        creation = "signed", dissolution = "ended")))
    Condition
      Error in `prepare_dynes_data()`:
      ! A count layer needs no flavor.
      x `flavor_names` has an entry for the count layer "calls".
      i A count records repeated events of one process.
      i If "calls" counts two event types together, such as signings and endings, split it into one "count" layer per type.

