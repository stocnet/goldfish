# specification print is a stable cli overview

    Code
      print(spec)
    Message
      -- <specification.goldfish> ----------------------------------------------------
      Model "DyNAM" · sub-model rate and choice
      
      Dependent
      * Layer: "callsDependent"
      * Events: 120
      * Time span: "1220733470 – 1223140151"
      * Nodes: actors
      * Network: "callNetwork"
      
      Rate: `~1 + indeg + outdeg`
      Choice: `~inertia + recip + trans`
      
      v Specification is valid.

# specification print omits absent sub-model and shows support

    Code
      print(spec)
    Message
      -- <specification.goldfish> ----------------------------------------------------
      Model "DyNAM" · sub-model choice
      
      Dependent
      * Layer: "callsDependent"
      * Events: 120
      * Time span: "1220733470 – 1223140151"
      * Nodes: actors
      * Network: "callNetwork"
      
      Choice: `~inertia + recip`
      Support: `~present`
      
      v Specification is valid.

