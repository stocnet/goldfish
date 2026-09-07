## MODIFIED Requirements

### Requirement: Dual input path with one validate-and-stamp boundary
The package SHALL accept a `stocnet` object (list of `info`, `nodes`, `ties`,
`changes`, `global` components per `manynet::make_stocnet()`) as the data input
for DyNAM and REM estimation through two entries sharing one internal
validator: directly via the `data` argument of `estimate_dynam()` /
`estimate_rem()` / `make_specification()` (validated at specification time),
and via an exported `as_goldfish(x, ...)` that validates early and stamps the
object (subclass marker) without restructuring it. Validation SHALL run
unconditionally at specification/estimation time — the stamp SHALL NOT bypass
it (manynet verbs and plain list assignment mutate the object while preserving
the class vector, so a stamp is not evidence of validity); the stamp serves
provenance and print dispatch only. goldfish SHALL read stocnet components
structurally (accepting plain data.frames — tibble not required) while manynet
SHALL be listed in Imports pinned to the version goldfish actually uses, per
the manynet compatibility floor requirement below; manynet functions are
called only where delegation is DRY (wrapper assembly, legacy conversion), via
`@importFrom` or `manynet::` — never `manynet:::`.

#### Scenario: Raw stocnet accepted directly
- **WHEN** `estimate_dynam(formula, data = social_evolution, ...)` is called with a valid
  unstamped stocnet object
- **THEN** the object is validated on the fly and estimation completes.

#### Scenario: as_goldfish validates early and is reused
- **WHEN** `d <- as_goldfish(social_evolution)` succeeds and `d` is passed to two
  `estimate_*()` calls
- **THEN** validation fails early at `as_goldfish()` if the data is invalid; both
  estimations re-validate the stamped object and produce results identical to the
  raw-stocnet path.

## ADDED Requirements

### Requirement: The declared manynet floor names the version goldfish uses

The `manynet` version bound in `DESCRIPTION` SHALL name a version that
provides every manynet behavior goldfish depends on, and SHALL be raised in the
same change as any goldfish code or vignette that comes to need a newer one. A
bound below what the package uses SHALL NOT be retained on the grounds that it
is more permissive: it converts an install-time failure into a runtime one, and
the failure then surfaces wherever the missing behavior is first exercised
rather than where the dependency is declared.

The behavior goldfish relies on SHALL be recorded alongside the bound, so that
a proposed change to the bound can be judged by reading this specification
rather than by bisecting manynet. At minimum:

- `add_info()` on a `stocnet` conforms the legacy field names `nodes` and
  `ties` onto the reserved `modes` and `layers`, so that
  `add_info(ties = c("a", "b"))` sets `info$layers`, and validates the count
  against the network's modes and layers. goldfish's two-mode vignette
  assembles its data this way, and a manynet that stores an unrecognised
  `info$ties` field instead leaves `info$layers` at its one-element
  `as_stocnet()` value, which fails later inside manynet's own
  `reserved_cols()`.
- The construction and manipulation verbs goldfish calls: `make_stocnet()`,
  `as_stocnet()`, `add_info()`, `bind_changes()`, `bind_ties()`,
  `from_ties()`, `join_nodes()`, `rename_nodes()`, and — in the vignettes —
  `mutate_globals()`.

While goldfish is unreleased the bound MAY name a manynet version that is
tagged but not yet on CRAN. Before goldfish 2.0.0 is released the bound SHALL
name a version available on CRAN, and that obligation SHALL be recorded in the
release pre-flight rather than only in a change proposal.

#### Scenario: the bound matches what a vignette build requires

- **WHEN** the vignettes are rebuilt against a manynet at exactly the declared
  bound
- **THEN** the rebuild introduces no error that a newer manynet would not
  produce

#### Scenario: a bound below the used version is not retained

- **WHEN** goldfish is found to depend on a manynet behavior newer than the
  declared bound
- **THEN** the bound is raised in the same change, rather than the dependency
  being coded around to preserve the lower bound

#### Scenario: the pre-release exception is bounded

- **WHEN** the declared bound names a manynet version that is tagged but not
  published on CRAN
- **THEN** goldfish is still unreleased, and the release pre-flight carries the
  obligation to raise the published bound before 2.0.0

### Requirement: A vignette rebuild is the check for the manynet seam

Drift in the manynet interface SHALL be checked by rebuilding the vignettes,
because the unit test suite cannot detect it: the suite constructs `stocnet`
fixtures directly rather than through manynet's verbs, so a change in what
`add_info()` writes is invisible to it. The vignettes are the only artifacts
that drive `as_stocnet()`, `add_info()`, `bind_changes()`, `mutate_globals()`
and `from_ties()` in sequence over real data.

After any change to the declared manynet bound, or to the installed manynet
used for development, the vignettes SHALL be rebuilt and the rebuild SHALL
introduce no new errors relative to the committed documents. A vignette whose
rebuild fails for a reason unrelated to the manynet seam SHALL be diagnosed
before it is either shipped or reverted, so that two unrelated causes are not
recorded as one.

#### Scenario: a manynet bump is checked by rebuilding

- **WHEN** the manynet bound is raised
- **THEN** the vignettes are rebuilt and compared against the committed
  documents, and no new error is introduced

#### Scenario: an unrelated rebuild failure is separated, not absorbed

- **WHEN** a vignette rebuild fails for a cause that is not the manynet seam
- **THEN** the cause is identified and recorded as its own problem, and the
  manynet work does not claim it
