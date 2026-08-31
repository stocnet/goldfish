# a value supplied for a fixed coefficient warns and is ignored

    Code
      p <- set_init_param(join, `calls › choice` = c(`inertia(calls)` = 0.3,
        `tie(friendship)` = 9))
    Condition
      Warning:
      ! Value supplied for fixed coefficient "calls › choice: tie(friendship)" was ignored.
      i A fixed coefficient (an `offset()` term or an operand-only interaction) keeps the specification's value.

# a key matching no process aborts naming the valid labels

    Code
      set_init_param(join, `calls:rate` = c(0.1, 0.2))
    Condition
      Error in `set_init_param()`:
      ! The label "calls:rate" matches no process.
      i Valid labels: "calls › rate", "calls › choice", "emails › rate", and "emails › choice".

# the same process given twice aborts

    Code
      set_init_param(join, `calls › rate` = c(0.1, 0.2), `calls › rate` = c(0.3, 0.4))
    Condition
      Error in `set_init_param()`:
      ! Each process takes at most one value vector.
      x Process "calls › rate" was given more than once.

# a wrong-length per-fid vector aborts naming the fid

    Code
      set_init_param(join, `calls › rate` = c(0.1, 0.2, 0.3))
    Condition
      Error in `resolve_fid_vector()`:
      ! The values for "calls › rate" must have one entry per coefficient.
      x It has 3 entries but the process has 2 coefficients (`Intercept` and `indeg(calls)`).

# a partly named per-fid vector is rejected

    Code
      set_init_param(join, `calls › rate` = c(Intercept = 0.1, 0.2))
    Condition
      Error in `resolve_fid_vector()`:
      ! The values for "calls › rate" are only partly named.
      x 1 of 2 entries are unnamed.
      i Name every coefficient or none -- a partly named vector cannot be aligned unambiguously.

# a per-fid vector naming an unknown coefficient aborts

    Code
      set_init_param(join, `calls › rate` = c(Intercept = 0.1, wrong = 0.2))
    Condition
      Error in `resolve_fid_vector()`:
      ! The values for "calls › rate" name coefficients the process does not have.
      x Unknown name: "wrong".
      i The coefficients are `Intercept` and `indeg(calls)`.

