# joint-parameters — change-local NEWS staging

These bullets stage the user-facing changes for this change. They are folded
into the root `NEWS.md` at branch merge, together with the `DESCRIPTION` version
bump — neither the root `NEWS.md` nor the `DESCRIPTION` version is touched on
this branch (see `progress.md`, "Version / NEWS / archival").

## New features

* `make_joint_specification()` now rejects, at build, any joined process whose
  `offset()` term lacks an inline coefficient value. Composing processes closes
  the `set_algorithm_newton(offset_coef = )` route, so a fixed offset value can
  live only in the formula itself; the abort names the offending term and points
  to `offset(term, coef = value)`. A bare `offset(term)` remains legal in a
  standalone `make_specification()`.
* `set_init_param()` builds a self-validating `goldfishParams` over a
  `goldfishJointSpec` — the shared parameter surface both
  `estimate_dynes()` and a joint `simulate()` accept. Each process takes one
  full-length coefficient vector keyed by its rendered `layer › flavor › family`
  label (flavor elided when absent); entries may be named or positional but not
  a mix. A slot the formula fixes (an `offset()` term or an operand-only
  interaction) keeps the specification's value — a value supplied there is warned
  about and ignored — while an unfilled free slot leaves the object incomplete.
* `coef_layout()` is a new generic tabulating the coefficient-space layout of a
  joint parameter surface — one row per coefficient slot, with the process
  label, name, fixed flag, fixed value, and flat-parameter index. It dispatches
  on a `goldfishJointSpec` (the empty authoring layout; completion-
  aware, so a completed spec also renders its autocompleted fids as fixed),
  a `goldfishParams` (the supplied values and free/fixed classification),
  and a joint/DyNES fitted result (θ̂/SE, the projection `summary()`/`print()`
  use to group the flat coefficient vector into per-process blocks).
* `estimate_dynes(initial_parameters =)` and a joint `simulate(coef =)` share one
  acceptance surface that takes only a `goldfishParams`: estimation reads
  its free-parameter projection (a partial object is the normal warm-start
  case), while simulation asserts completeness and reads the full per-fid
  coefficient projection, naming any unpinned free effect. At consumer entry, a
  fid absent from the object because it was autocompleted (a zero-free-parameter
  default) is treated as trivially resolved.
* `set_init_param(spec, result)` accepts a fitted joint/DyNES result in place of
  the per-fid vectors — the fit → re-simulate round-trip — reconstructing values
  from the result's own `coef_layout()` after asserting the result was fit
  against that same specification.
* **Breaking (pre-release):** the two S3 classes this change ships/consumes are
  spelled under the `goldfish<Thing>` camelCase scheme, not the `<noun>.goldfish`
  house convention `class-naming-scheme` retires: `set_init_param()` returns
  `goldfishParams` (was `parameters.goldfish`), and `make_joint_specification()`
  returns `goldfishJointSpec` (was `joint_specification.goldfish`). No code has
  shipped a release under the old spellings, so this is a rename of unreleased
  surface, not a user-facing deprecation.
* **Breaking (pre-release):** `set_parameters()` is renamed to `set_init_param()`
  (and its unexported from-result helper `set_parameters_from_result()` to
  `set_init_param_from_result()`), to read correctly as the starting-point
  builder `estimate_dynes(initial_parameters =)` consumes. No code has shipped
  a release under the old name, so this is a rename of unreleased surface, not
  a user-facing deprecation.

## Internal

* Added flavored-specification test coverage for `set_init_param()` and
  `coef_layout()`: a joint fixture whose flavored `calls` layer carries the same
  effect name (`inertia`) across both flavors and its own `offset()` at two
  distinct values per flavor. The new tests prove same-name free slots resolve
  per fid without leaking, each flavor's fixed offset resolves to its own value
  (`coef_layout()` and the fit → re-simulate round-trip), and the retired
  colon-grammar key still aborts where a naive string-split would have matched.
