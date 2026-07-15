# specification print is a stable cli overview

    Code
      print(spec)
    Message
      -- <specification.goldfish> ----------------------------------------------------
      Model "DyNAM" · sub-model rate and choice
      
      Dependent
      * Layer: "calls_dependent"
      * Events: 120
      * Time span: "1220733470 – 1223140151"
      * Nodes: actors
      * Network: "call_network"
      
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
      * Layer: "calls_dependent"
      * Events: 120
      * Time span: "1220733470 – 1223140151"
      * Nodes: actors
      * Network: "call_network"
      
      Choice: `~inertia + recip`
      Support: `~tie(call_network)`
      
      v Specification is valid.

