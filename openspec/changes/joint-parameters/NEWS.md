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
