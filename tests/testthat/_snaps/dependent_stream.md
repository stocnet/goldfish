# a dependent name that is not a layer aborts listing the layers

    Code
      ds_check_dependent(src, "nope")
    Condition
      Error:
      ! The dependent process "nope" is not a layer of the data.
      i Available layer: "calls".

