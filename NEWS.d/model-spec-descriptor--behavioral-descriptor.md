## Breaking changes

* Renamed the `right_censored` component of a fit to `is_exact_time`.
  * The same rename applies to every `compute_statistics()` output form.
  * It names the sub-model's own property, that the waiting times between
    events are modeled, rather than the consequence that right-censored rows
    are stored.
  * `has_intercept` is unchanged and still reports the formula's property.
  * **`fit$right_censored` does not error.** It partially matches the
    per-event `right_censored_events` vector and returns it, so old code
    reading a scalar now silently receives a logical vector. Use
    `fit$is_exact_time`, or `fit[["right_censored"]]`, which returns `NULL`.
* Changed `risk_set_axis()` to return `"dyad"` for one-mode coordination.
  * It returned `"dyad_symmetric"`, which is retired.
  * Coordination reads the same dyad grid every dyadic model reads, so it
    names the same axis; that its likelihood sums each unordered pair once is
    carried by the likelihood instead.
* Replaced the statistics-output classes with a single `goldfishStat`.
  * `goldfishStatDB` and `goldfishFlavStat` are retired, with no alias.
  * `attr(x, "storage")` is `"pointer"`, `"stack"` or `"db"`, and
    `attr(x, "scope")` is `"single"` or `"flavored"`.
  * `compute_statistics(output = "gather")` now carries that class, where it
    was returned to users unclassed.
  * `output = "data.frame"` still returns a plain data frame.

## New features

* Added a `flavor` argument to `coef()` and `vcov()` on a flavored fit.
  * Naming one process returns its own vector or matrix, as `fitted()`,
    `predict()` and `residuals()` already did.

## Bug fixes

* Improved the error when a DyNAM-i model reaches the compiled engine.
  * It failed with an internal `object 'res' not found`; it now names the
    model and points at `set_algorithm_newton(backend = "r")`.
  * DyNAM-i still requires the R backend. The compiled path was never wired
    for it and no baseline covers it.

## Internal

* Replaced the nine `goldfishKind<Variant>` classes with six likelihood
  classes, superseding their entry in the class-rename notes above.
  * A spec carries `goldfishLik<Axis><Family>` plus its risk-set axis class;
    the model and sub-model pairing dispatches nothing and stays as
    provenance.
  * DyNAM-i shares its DyNAM counterpart's likelihood class, so the three
    alias methods are deleted rather than re-registered.
  * Preprocessing dispatches once, on a behavioral descriptor built at spec
    construction, rather than once per model variant.
