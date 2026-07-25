# the container prints a section per flavor

    Code
      print(res)
    Message
      ── <flavored_result.goldfish> ──────────────────────────────────────────────────
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

# flavored db output refuses rather than merging processes

    Code
      compute_statistics(make_specification(choice = list(creation ~ trans,
      dissolution ~ trans), model = "DyNAM", data = data), model = "DyNAM",
      sub_model = "choice", output = "db")
    Condition
      Warning:
      ! `support_constraint`: 17 events with a single candidate (forced choice; contributes 0 to the log-likelihood).
      i Events: 1, 2, 3, 4, 9, 10, 12, 15, 17, 19, 20, 24, 26, 27, 28, 39, and 40.
      Error in `flavored_statistics_output()`:
      ! `output = "db"` is not available for a multi-flavor specification.
      x Its processes would append to one table with nothing to tell their rows apart.
      i Use `output = "gather"`, or preprocess one flavor at a time with its own `db_table`.

