# as_goldfish fails early on invalid data

    Code
      as_goldfish(x)
    Condition
      Error in `as_goldfish()`:
      ! info$update is required for every layer.
      x It is missing.
      i Provide a named vector covering layer "calls".

# as_goldfish defers legacy environment conversion

    Code
      as_goldfish(new.env())
    Condition
      Error in `as_goldfish()`:
      ! Converting a legacy <data.goldfish> environment is not yet available.
      i It arrives with the constructor deprecation flip; until then, rebuild the data as a <stocnet> object with `manynet::make_stocnet()`.

# printing a stamped object renders the list shape

    Code
      print(d)
    Message
      
      -- toy -------------------------------------------------------------------------
      1 layer, 3 nodes, 3 ties.
      
      -- Layers 
      * calls (focal): event, directed, increment

# the estimation surface rejects a legacy environment

    Code
      estimate_dynam(y ~ 1, data = env)
    Condition
      Error in `estimate_dynam()`:
      ! `data` must be a <stocnet> object, not a legacy <data.goldfish> environment.
      i Rebuild the object with `make_data()` / `make_groups_interaction()` (both now return a <stocnet>).

---

    Code
      estimate_rem(y ~ 1, data = env)
    Condition
      Error in `estimate_rem()`:
      ! `data` must be a <stocnet> object, not a legacy <data.goldfish> environment.
      i Rebuild the object with `make_data()` / `make_groups_interaction()` (both now return a <stocnet>).

---

    Code
      estimate_dynami(y ~ 1, data = env)
    Condition
      Error in `estimate_dynami()`:
      ! `data` must be a <stocnet> object, not a legacy <data.goldfish> environment.
      i Rebuild the object with `make_data()` / `make_groups_interaction()` (both now return a <stocnet>).

---

    Code
      make_specification(rate = list(y ~ 1), model = "DyNAM", data = env)
    Condition
      Error in `make_specification()`:
      ! `data` must be a <stocnet> object, not a legacy <data.goldfish> environment.
      i Rebuild the object with `make_data()` / `make_groups_interaction()` (both now return a <stocnet>).

