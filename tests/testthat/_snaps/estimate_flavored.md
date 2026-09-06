# the container prints a section per flavor

    Code
      print(res)
    Message
      ── <goldfishFlavFit> ───────────────────────────────────────────────────────────
      Model "DyNAM" · layer "calls" · 2 flavors
      
      Flavor "creation"
      Rate
    Output
      Intercept       ideg  
       <num>   <num>  
    Message
      Choice
    Output
        trans  
      <num>  
    Message
      
      Flavor "dissolution"
      Rate
    Output
      Intercept       ideg  
       <num>   <num>  
    Message
      Choice
    Output
        trans  
      <num>  
    Message
      
      Total log-likelihood <num> on 6 parameters

# print reads the fields rather than a class per combination

    Code
      print(gathered)
    Message
      Statistics for 2 processes (stack storage).
      * "creation" (choice)
      * "dissolution" (choice)
      i Index by fid: `x[["1"]]`.

# control-object coefficient values abort on a multi-process spec

    Code
      estimate_dynam(spec, control_algo = set_algorithm_newton(offset_coef = 2))
    Condition
      Error in `estimate_flavored()`:
      ! `offset_coef` does not apply to a multi-process specification.
      i Write the value in the process formula it belongs to: `offset(term, coef = value)`.

# unknown flavor and family keys abort naming the valid ones

    Code
      estimate_dynam(spec, control_algo = set_algorithm_newton(initial_parameters = list(
        creaton = c(ideg = 0.5))))
    Condition
      Warning:
      ! `support_constraint`: 17 events with a single candidate (forced choice; contributes 0 to the log-likelihood).
      i Events: 1, 2, 3, 4, 9, 10, 12, 15, 17, 19, 20, 24, 26, 27, 28, 39, and 40.
      Error in `resolve_flavored_initials()`:
      ! `initial_parameters` names a flavor this specification does not model.
      x Unknown: `creaton`.
      i Modelled flavors: `creation` and `dissolution`.

