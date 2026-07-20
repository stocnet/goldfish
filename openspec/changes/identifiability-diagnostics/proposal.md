## Why

When a goldfish model is not identified, the user is told:

> Matrix cannot be inverted; probably due to collinearity between parameters.

That message names no effect, and its own "probably" admits it is a guess. The
user is left to bisect their formula by hand. Yet at the moment it is raised the
information matrix is in hand (`R/estimation_core.R:507`,
`R/cpp_interface.R:499`) — everything needed to say *which* coefficients are at
fault is already computed and then discarded.

The `flavored-processes` work made this concrete. Under a mutually exclusive
layer's derived `!tie(L)` mask every allowed alternative has state 0, so
`inertia(L)` is identically 0 and cannot be identified — and `creation ~ inertia`
is exactly what a user would naturally write. Today that returns the bare message
above. The same silence covers the ordinary cases: a constant effect, two
collinear effects, an effect with no variation in the risk set.

A neighbouring failure is worse because it does not error at all. Complete
separation returns coefficients like `-16.3` with matching standard errors and no
warning whatsoever — a model that looks fitted and is not.

## What Changes

- **Name the offending coefficients.** At each inversion-failure site, analyse
  the singular information matrix's null space and report which coefficients are
  implicated: a null vector loading on one coefficient means that effect alone is
  degenerate; loading spread over several means those are collinear as a group.
  Names are rendered through the existing coefficient-naming path, so a
  multi-process fit also names the process the failure belongs to.
- **Explain why, on the failure path only.** When the null space implicates a
  single effect, re-examine that effect's statistics over the risk set to
  distinguish *no variation at all* from *constant within each event's
  alternatives* (which cancels in a choice softmax) — and say which.
- **Warn on separation.** Detect diverging coefficients (large estimate, large
  standard error, log-likelihood at its ceiling) and report probable complete or
  quasi-complete separation, rather than returning the fit silently.
- **Report conditioning on fits that succeed.** `summary()` gains the
  information matrix's condition number and warns above a threshold, so a
  near-singular fit is visible before it is interpreted.

Everything runs **post-hoc or on the failure path**: no engine inner loop is
touched, no statistic is recomputed in the happy path, and no frozen baseline can
move. That is the central scoping constraint, not an afterthought.

## Capabilities

### New Capabilities

- `estimation-diagnostics`: identifiability reporting for fitted and failed
  models — null-space attribution of a singular information matrix to named
  coefficients, the statistics-based explanation of a single degenerate effect,
  separation detection, and condition-number reporting in `summary()`.

## Impact

- **Sequencing**: independent of the 2.0.0 track; consumes `flavored-processes`
  only for rendered process labels in multi-process fits, and degrades to plain
  coefficient names without it. Can land before or after 2.0.0. It directly
  serves the vignette section `flavored-processes` 5.4 writes: that section must
  warn users off `creation ~ inertia`, and this change turns the resulting error
  into an explanation if they try it anyway.
- **R**: the two inversion sites (`R/estimation_core.R`, `R/cpp_interface.R`)
  gain a shared diagnostic helper rather than duplicating one; a new
  `R/diagnostics_identifiability.R`; `summary.result.goldfish()` gains the
  conditioning report.
- **Frozen baselines**: untouched by construction — every addition is on a path
  that currently ends in an error or runs after estimation completes. A
  successful fit's numbers are unchanged.
- **Docs**: a troubleshooting section on non-identified models, cross-referenced
  from the competing-processes vignette.
