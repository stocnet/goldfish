# an unknown layer aborts listing the candidates

    Code
      make_specification(choice = ~inertia, model = "DyNAM", choice_sub_model = "choice",
        layer = "nope", data = make_stocnet_fixture())
    Condition
      Error in `make_specification()`:
      ! The focal layer must name a layer present in ties.
      x "nope" is not among "calls".

# a dependent process with no layer and no focal aborts

    Code
      make_specification(choice = ~inertia, model = "DyNAM", choice_sub_model = "choice",
        data = x)
    Condition
      Error in `make_specification()`:
      ! The dependent process is not identified.
      i Name it with `layer`, or declare info$focal on the data.

# a data frame is not a stocnet

    Code
      make_specification(choice = ~inertia, model = "DyNAM", choice_sub_model = "choice",
        data = data.frame(x = 1))
    Condition
      Error in `make_specification()`:
      ! `data` must be a <stocnet> object.
      i Build it with `manynet::make_stocnet()`, or gate it early with `as_goldfish()`.

# a plain formula on a flavored layer models all rows and says so

    Code
      spec <- make_specification(choice = ~inertia, model = "DyNAM",
        choice_sub_model = "choice", data = flavored_fixture())
    Message
      i Layer "calls" carries the flavor "creation" and "dissolution"; all its events are modeled.
      i Model one with a keyed list, e.g. `rate = list(creation ~ ...)`.

# the spec print nests the modeled and state-only flavors

    Code
      print(spec)
    Message
      -- <specification.goldfish> ----------------------------------------------------
      Model "DyNAM" · sub-model choice
      
      Dependent
      * Layer: "calls"
        Modeled flavor: "creation"
        State-only flavor: "dissolution"
      * Events: 1
      * Time span: "1 – 1"
      * Nodes: nodes
      * Network: "calls"
      
      Choice: `~inertia`
      
      v Specification is valid.

# the spec print lists all flavors when a plain formula models them

    Code
      print(spec)
    Message
      -- <specification.goldfish> ----------------------------------------------------
      Model "DyNAM" · sub-model choice
      
      Dependent
      * Layer: "calls"
        Flavors (all modeled): "creation" and "dissolution"
      * Events: 2
      * Time span: "1 – 2"
      * Nodes: nodes
      * Network: "calls"
      
      Choice: `~inertia`
      
      v Specification is valid.

# several flavor keys abort pointing at the future change

    Code
      make_specification(choice = list(creation ~ inertia, dissolution ~ inertia),
      model = "DyNAM", choice_sub_model = "choice", data = flavored_fixture())
    Condition
      Error in `make_specification()`:
      ! `choice` must key exactly one flavor.
      x 2 formulas were supplied.
      i Estimating several dependent processes jointly is not supported yet; it needs stacked per-flavor likelihoods.

# rate and choice must key the same flavor

    Code
      make_specification(rate = list(creation ~ 1 + indeg), choice = list(
        dissolution ~ inertia), model = "DyNAM", data = flavored_fixture())
    Condition
      Error in `make_specification()`:
      ! `rate` and `choice` must model the same flavor.
      x `rate` keys "creation" but `choice` keys "dissolution".
      i One specification models one dependent process.

# a flavor no focal row carries aborts

    Code
      make_specification(choice = list(nope ~ inertia), model = "DyNAM",
      choice_sub_model = "choice", data = flavored_fixture())
    Condition
      Error in `make_specification()`:
      ! No "calls" row carries the flavor "nope".
      i Flavors on this layer: "creation" and "dissolution".

# a keyed entry must be a formula with the flavor on the left

    Code
      make_specification(choice = list(~inertia), model = "DyNAM", choice_sub_model = "choice",
      data = flavored_fixture())
    Condition
      Error in `make_specification()`:
      ! `choice`'s entry must be a formula whose left-hand side is the flavor.
      i For example `choice = list(creation ~ 1 + indeg())`.

