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

# an unflavored layer infers the mapping and says so

    Code
      spec <- make_specification(choice = list(creation ~ inertia, dissolution ~
        inertia), model = "DyNAM", choice_sub_model = "choice", data = unflavored_increment_fixture())
    Message
      i Layer "calls" is unflavored; assuming "creation" = 1 and "dissolution" = -1.

# a weighted layer aborts inference

    Code
      make_specification(choice = list(creation ~ inertia, dissolution ~ inertia),
      model = "DyNAM", choice_sub_model = "choice", data = x)
    Condition
      Error in `make_specification()`:
      ! Cannot infer a flavor mapping for unflavored layer "calls".
      x Its update values 3, -2, and 5 are not the dichotomous -1 and 1.
      i Stamp flavors explicitly with `add_flavor()`.

# a key matching no flavor value aborts

    Code
      make_specification(choice = list(creation ~ inertia, deletion ~ inertia),
      model = "DyNAM", choice_sub_model = "choice", data = me_flavored_fixture())
    Condition
      Error in `make_specification()`:
      ! Flavor key "deletion" matches no "calls" row.
      i Flavors on this layer: "creation" and "dissolution".

# duplicate flavor keys abort

    Code
      make_specification(choice = list(creation ~ inertia, creation ~ recip), model = "DyNAM",
      choice_sub_model = "choice", data = me_flavored_fixture())
    Condition
      Error in `make_specification()`:
      ! `choice` keys a flavor more than once.
      x Duplicate key: "creation".

# a plain sub-model with a multi-keyed sibling aborts

    Code
      make_specification(rate = ~ 1 + indeg, choice = list(creation ~ inertia,
      dissolution ~ recip), model = "DyNAM", data = me_flavored_fixture())
    Condition
      Error in `make_specification()`:
      ! `rate` must be a flavor-keyed list when modeling several flavors.
      i Key it on the same flavors, e.g. `rate = list(creation ~ ..., dissolution ~ ...)`.

# estimating a multi-flavor specification aborts for now

    Code
      estimate_dynam(spec, sub_model = "choice")
    Condition
      Error in `estimate_from_specification()`:
      ! Estimating a multi-flavor specification is not wired up yet.
      x This specification models 2 flavors ("creation" and "dissolution").
      i For now, estimate one flavor at a time with a single-key specification and its derived `support_constraint`.

# the multi-flavor print nests a section per flavor

    Code
      print(spec)
    Message
      -- <specification.goldfish> ----------------------------------------------------
      Model "DyNAM" · sub-model rate and choice
      
      Dependent
      * Layer: "calls"
        Modeled flavors: "creation" and "dissolution"
      * Events: 3
      * Time span: "1 – 3"
      * Nodes: nodes
      * Network: "calls"
      
      Flavor "creation"
      Rate: `~1 + indeg`
      Choice: `~inertia`
      Constraint: `~!tie(calls)`
      Flavor "dissolution"
      Rate: `~1 + inertia`
      Choice: `~recip`
      Constraint: `~tie(calls)`
      
      v Specification is valid.

# rate and choice must key the same flavor

    Code
      make_specification(rate = list(creation ~ 1 + indeg), choice = list(
        dissolution ~ inertia), model = "DyNAM", data = flavored_fixture())
    Condition
      Error in `make_specification()`:
      ! `rate` and `choice` must key the same flavor set.
      x `rate` keys "creation" but `choice` keys "dissolution".
      i Each flavor is a parallel process modeled by both sub-models.

# a flavor no focal row carries aborts

    Code
      make_specification(choice = list(nope ~ inertia), model = "DyNAM",
      choice_sub_model = "choice", data = flavored_fixture())
    Condition
      Error in `make_specification()`:
      ! Flavor key "nope" matches no "calls" row.
      i Flavors on this layer: "creation" and "dissolution".

# a keyed entry must be a formula with the flavor on the left

    Code
      make_specification(choice = list(~inertia), model = "DyNAM", choice_sub_model = "choice",
      data = flavored_fixture())
    Condition
      Error in `make_specification()`:
      ! `choice`'s entries must be formulas whose left-hand side is the flavor.
      i For example `choice = list(creation ~ 1 + indeg())`.

