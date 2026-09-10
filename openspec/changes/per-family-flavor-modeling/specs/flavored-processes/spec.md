## MODIFIED Requirements

### Requirement: Unmodeled flavors update state only

A flavor present in the layer's data but absent from the formula list SHALL contribute
no dependent events and no likelihood term; its events SHALL update process state and,
on timed rate sub-models, right-censor every modeled flavor's output. `NA`-flavor rows
follow the same state-only convention. The rule SHALL apply **per sub-model
family**: a flavor named in one family's list and absent from the other's is
modeled in the family that names it and unmodeled in the other, where its
events update state and, on a timed rate engine, right-censor the modeled
flavors' outputs. The event-stream estimators SHALL NOT abort on such a
specification; the completion of the absent sub-model remains the generative
consumers' transform only.

#### Scenario: three flavors, two modeled
- **WHEN** a layer carries flavors {creation, dissolution, renewal} and the formula list
  keys only creation and dissolution
- **THEN** renewal events update the network state and right-censor both modeled
  flavors' timed outputs, and no renewal parameters are estimated.

#### Scenario: a flavor modeled in one family estimates there and censors in the other
- **WHEN** `rate = list(creation ~ 1 + indeg, dissolution ~ 1 + outdeg)` and
  `choice = list(creation ~ inertia)` are estimated
- **THEN** the rate family fits both flavors, the choice family fits `creation`
  with dissolution events as state updates only, no abort occurs, and the
  rate coefficients equal those of the same specification with a
  `dissolution` choice added, within 1e-6.

#### Scenario: compute_statistics follows the same rule
- **WHEN** the specification above is passed to `compute_statistics()` for
  the rate family
- **THEN** both flavors' rate statistics are returned and no abort occurs.

### Requirement: Per-flavor estimation returns a sectioned multi-process result

Estimation of a multi-flavor specification SHALL fit each flavor's model separately on
its preprocessed object using the existing engines, and return a container object
holding one result per flavor (and per sub-model for DyNAM). The container's `print()`
SHALL render cli sections per flavor (rate and choice nested within a flavor for
DyNAM); `coef()`, `vcov()`, and `logLik()` SHALL return flavor-named components
(labels rendered from the container's process_map), with
the container's total log-likelihood the sum over flavors. Estimating flavor g through
the container SHALL produce coefficients identical (within 1e-6) to a standalone
single-flavor specification of flavor g with the equivalent derived constraint supplied
as a user `support_constraint`. The container's families MAY model different
flavor sets; the `process_map` carries one row per present `(flavor, family)`,
the print SHALL show each flavor with the families it carries, and the
estimation entry SHALL inform once when the families' flavor sets differ.

#### Scenario: container equals standalone per-flavor fits
- **WHEN** a two-flavor DyNAM is estimated via the container and each flavor is also
  estimated standalone with the equivalent constraint
- **THEN** all coefficients agree within 1e-6.

#### Scenario: sectioned print
- **WHEN** the container result of a two-flavor DyNAM is printed
- **THEN** the output shows a section per flavor, each with its rate and choice
  estimates, rendered with cli semantic elements and stable under a pinned cli context.

#### Scenario: asymmetric families print without assuming symmetry
- **WHEN** a container whose rate family models {creation, dissolution} and
  whose choice family models {creation} is printed
- **THEN** the `dissolution` section shows its rate estimates only, the
  `creation` section shows both, and a single message at estimation stated
  that the families model different flavor sets.
