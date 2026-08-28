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
* `set_parameters()` builds a self-validating `parameters.goldfish` over a
  `joint_specification.goldfish` — the shared parameter surface both
  `estimate_dynes()` and a joint `simulate()` accept. Each process takes one
  full-length coefficient vector keyed by its rendered `layer › flavor › family`
  label (flavor elided when absent); entries may be named or positional but not
  a mix. A slot the formula fixes (an `offset()` term or an operand-only
  interaction) keeps the specification's value — a value supplied there is warned
  about and ignored — while an unfilled free slot leaves the object incomplete.
