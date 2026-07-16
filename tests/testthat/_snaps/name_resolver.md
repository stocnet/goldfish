# the legacy df$var prefix is dropped with a deprecation warning

    Code
      refs <- resolved_refs(y ~ ego(actors$floor), src)
    Condition
      Warning:
      The data-frame prefix in `actors$floor` was deprecated in goldfish 1.9.0.
      i Please use `floor` instead.
      i Attributes now resolve against the data object's components.

# an unknown name aborts listing the available candidates

    Code
      resolved_refs(y ~ inertia(callz), src)
    Condition
      Error in `resolved_refs()`:
      ! `inertia()` refers to "callz", which is not in the data.
      i Available layer: "calls".
      i Available nodes columns: "label", "floor", and "mode".
      i Available global variable: "gdp".

# a name in both nodes and global aborts as ambiguous

    Code
      resolved_refs(y ~ ego(floor), src)
    Condition
      Error in `resolved_refs()`:
      ! "floor" is both a nodes column and a global variable.
      x Which one `ego()` should read is ambiguous.
      i Rename one of them.

