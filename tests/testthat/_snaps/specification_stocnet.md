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
      -- <goldfishSpec> --------------------------------------------------------------
      Model "DyNAM" · sub-model choice
      
      Dependent
      * Layer: "calls"
        Modeled flavor: "creation"
        State-only flavor: "dissolution"
      * Events: 1
      * Time span: "1 – 1"
      * Nodes: p
      * Network: "calls"
      
      Choice: `~inertia`
      
      v Specification is valid.

# the spec print lists all flavors when a plain formula models them

    Code
      print(spec)
    Message
      -- <goldfishSpec> --------------------------------------------------------------
      Model "DyNAM" · sub-model choice
      
      Dependent
      * Layer: "calls"
        Flavors (all modeled): "creation" and "dissolution"
      * Events: 2
      * Time span: "1 – 2"
      * Nodes: p
      * Network: "calls"
      
      Choice: `~inertia`
      
      v Specification is valid.

# the two-mode spec print names the real mode pair

    Code
      print(spec)
    Message
      -- <goldfishSpec> --------------------------------------------------------------
      Model "DyNAM" · sub-model choice
      
      Dependent
      * Layer: "membership"
      * Events: 2
      * Time span: "1 – 2"
      * Nodes: p → o
      * Network: "membership"
      
      Choice: `~inertia`
      
      v Specification is valid.

# an unflavored layer infers the mapping and says so

    Code
      spec <- make_specification(choice = list(creation ~ inertia, dissolution ~
        inertia), model = "DyNAM", choice_sub_model = "choice", data = unflavored_increment_fixture())
    Message
      i Layer "calls" is unflavored; assuming "creation" = 1 and "dissolution" = -1.
      i No support constraint is derived from an assumed mapping; use `add_flavor()` with `flavor_style` to declare one.

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

# the multi-flavor print nests a section per flavor

    Code
      print(spec)
    Message
      -- <goldfishSpec> --------------------------------------------------------------
      Model "DyNAM" · sub-model rate and choice
      
      Dependent
      * Layer: "calls"
        Modeled flavors: "creation" and "dissolution"
      * Events: 3
      * Time span: "1 – 3"
      * Nodes: p
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

# estimating a half-specified flavor set re-imposes the abort

    Code
      estimate_dynam(spec)
    Condition
      Error in `estimate_from_specification()`:
      ! `rate` and `choice` must key the same flavor set.
      x Flavors "creation" and "dissolution" are keyed in only one sub-model list.
      i Supply the missing sub-models, or use this specification with a generative consumer (`simulate()` / `estimate_dynes()`) that completes the gap with a zero-parameter default.

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

# a declared style with no mapping aborts instead of doing nothing

    Code
      make_specification(choice = list(creation ~ inertia, dissolution ~ inertia),
      model = "DyNAM", choice_sub_model = "choice", data = x)
    Condition
      Error in `make_specification()`:
      ! Layer "calls" is declared "mutually_exclusive" but carries no values_equivalence.
      x Without the mapping there is no way to tell which flavor creates the tie and which dissolves it, so no support constraint can be derived.
      i Set both with `add_flavor()`.

