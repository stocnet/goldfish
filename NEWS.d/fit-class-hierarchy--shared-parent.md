## New features

* Added `summary()`, `tidy()` and `glance()` for a multi-process fit.
  * `tidy()` and `glance()` row-bind the per-process tables and append
    `flavor` and `family`, as `augment()` and `model_terms()` already did.
  * `summary()` answers with a list named by process label, narrowed by
    `flavor =`, as `coef()` and `vcov()` already did.
  * All three existed only for a single-process fit, so a flavored fit
    previously failed with R's "no applicable method".

## Breaking changes

* Added `goldfishBaseFit`, the shared parent of every fitted-model class.
  * `class(fit)` is now `c("goldfishFit", "goldfishBaseFit")`; a flavored fit
    carries `c("goldfishFlavFit", "goldfishBaseFit")`.
  * `print()`, `summary()`, `tidy()`, `glance()`, `coef()`, `vcov()`,
    `logLik()` and `model_terms()` are registered on the parent, so calling
    one by its full method name (`coef.goldfishFit()`) no longer resolves.
  * Which generics a fit class inherits, overrides or refuses is recorded in
    `inst/fit-class-contract.csv` and enforced by a test.
* `coef_layout()` on a single-process fit now aborts with a reason instead of
  giving R's "no applicable method": it describes a coefficient surface over
  the processes of a joint specification, which one process does not have.

## Bug fixes

* Fixed `tidy()` on a fit with exactly one free coefficient, which returned a
  `term`/`value` pair of four rows instead of one row of four statistics.
