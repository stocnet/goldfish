# make_data aborts on an unresolvable node-set name

    Code
      make_data(cd, cn, actors, an)
    Condition
      Warning:
      `make_data()` was deprecated in goldfish 1.9.0.
      i goldfish now consumes a single <stocnet> data object.
      * Assemble the components with `manynet::make_stocnet()` (or merge layers with `manynet::from_ties()`) and pass the result to `data`.
      Error in `make_data()`:
      ! Cannot assemble a <stocnet>.
      x A network or dependent-events layer records a node-set name that matches no node table in the data.
      i This happens when a layer is built from a compound expression: the constructors record the `nodes` argument by deparsing it, so `make_network(nodes = fx$actors)` records the expression, not a plain name.
      i Bind the node set to a plain, resolvable name first, e.g. `actors <- fx$actors; make_network(nodes = actors, ...)`.

# make_data aborts on a bundle with no node set

    Code
      make_data(raw_events)
    Condition
      Warning:
      `make_data()` was deprecated in goldfish 1.9.0.
      i goldfish now consumes a single <stocnet> data object.
      * Assemble the components with `manynet::make_stocnet()` (or merge layers with `manynet::from_ties()`) and pass the result to `data`.
      Error in `make_data()`:
      ! Cannot assemble a <stocnet>: no node set was supplied.
      i Include at least one `make_nodes()` node set (or a `make_network()` that carries one) in the bundle.

