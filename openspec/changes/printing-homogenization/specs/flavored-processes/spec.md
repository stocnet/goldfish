## MODIFIED Requirements

### Requirement: Per-flavor estimation returns a sectioned multi-process result

Estimation of a multi-flavor specification SHALL fit each flavor's model separately on
its preprocessed object using the existing engines, and return a container object
holding one result per flavor (and per sub-model for DyNAM). The container's
`process_map` SHALL be the planner's map in its flavor-major fid order —
specification, then flavor, then family — without re-keying, so a process's
rate and choice rows are contiguous and every surface that lists processes
walks the map in fid order. The container's `print()` SHALL render through
the shared multi-process renderer: cli sections per flavor (rate and choice
nested within a flavor for DyNAM), each labeled with the process label and
its fid; `coef()` and `vcov()` SHALL return one flat named vector and one matrix
in fid order, named `f<fid>_<short>`, with `process =` selecting one block
under bare short names, and `logLik()` SHALL return the joint value with
`df` and `nobs` summed over the processes. Estimating flavor g
through the container SHALL produce coefficients identical (within 1e-6) to a
standalone single-flavor specification of flavor g with the equivalent derived
constraint supplied as a user `support_constraint`.

#### Scenario: container equals standalone per-flavor fits
- **WHEN** a two-flavor DyNAM is estimated via the container and each flavor is also
  estimated standalone with the equivalent constraint
- **THEN** all coefficients agree within 1e-6.

#### Scenario: sectioned print
- **WHEN** the container result of a two-flavor DyNAM is printed
- **THEN** the output shows a section per flavor, each with its rate and choice
  estimates labeled by process and fid, rendered with cli semantic elements
  and stable under a pinned cli context.

#### Scenario: the container's fids are the planner's
- **WHEN** a two-flavor DyNAM with rate and choice is preprocessed and
  estimated through the container
- **THEN** its `process_map` lists fids 1 to 4 as creation-rate,
  creation-choice, dissolution-rate, dissolution-choice, and
  `coef(fit)`, `coef_layout(fit)` and `print(fit)` follow that order.

#### Scenario: flat coefficients and a block selector
- **WHEN** `coef(fit)` and `coef(fit, process = "calls › creation › rate")`
  are called on a two-flavor DyNAM container
- **THEN** the first is one named vector whose names start `f1_`, `f2_`,
  `f3_`, `f4_` in that order and `vcov(fit)` is one block-diagonal matrix
  with the same dimnames, and the second is that process's vector under its
  bare short names, equal to the standalone fit's `coef()`.
