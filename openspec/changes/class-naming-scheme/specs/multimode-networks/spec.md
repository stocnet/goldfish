## MODIFIED Requirements

### Requirement: Legacy make_data assembles two-mode input into a stocnet

`make_data()` (and the legacy constructor wrappers) SHALL assemble two-mode input
into a `stocnet` rather than a legacy `data.goldfish` environment:
`is_stocnet_assemblable()` SHALL accept two-mode bundles, and the assembler SHALL
build the fused `nodes` tibble with a `mode` column, remap each layer's
`from`/`to` into the fused id space, set `info$sender`/`info$receiver` mode sets
per layer, and route composition/attribute events by mode into
`active_mode1`/`active_mode2`. The result SHALL flow through the same stocnet path
as directly-constructed two-mode input.

Throughout this requirement `data.goldfish` names the **legacy environment**
built by `make_data()` and the DyNAMi path, which retains that class as a
deprecated-path name. It is not the `data_goldfish` class that `as_goldfish()`
stamps on a validated `stocnet`; the class-naming capability separates the two,
and nothing here applies to the stamp.

#### Scenario: make_data two-mode returns a stocnet, not an environment

- **WHEN** a model is built with `make_network(m, nodes = firms, nodes2 = unis)`,
  a two-mode `make_dependent_events(...)`, and `make_data(...)`
- **THEN** `make_data()` returns a `stocnet` (a list of tibbles) whose focal layer
  is two-mode under the mode map, and no legacy `data.goldfish` environment is
  produced.

#### Scenario: Two-mode is removed as a legacy-environment producer

- **WHEN** the two-mode assembly is in place
- **THEN** the only remaining producer of a legacy `data.goldfish` environment is
  DyNAMi; the single-object legacy-environment abort remains deferred until DyNAMi
  is also off the environment (this change does not implement the abort).
