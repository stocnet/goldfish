## 0. Sequencing gates

- [ ] 0.1 Confirm `preprocess-one-walk` has landed (the merged walk is the
      batch loop) and record in `progress.md` whether ADR-0002's selector
      spelling has been decided; if not, task 2.1 keeps the flavored path
      estimating every family present.
- [ ] 0.2 Branch off `develop`; `NOT_CRAN=true` suite green at the start,
      baselines PASS not SKIP.

## 1. Planners walk per-family flavor subsets

- [ ] 1.1 `plan_flavor_union()` unions the bundles of the flavors that carry
      the family and records the absent modeled flavors on the union; the
      internal abort goes. `process_family_plan()` passes the subset through.
      Symmetric specifications produce byte-identical unions (test).
- [ ] 1.2 Routing of an absent flavor's events in the family's engine: state
      update on a choice engine, right-censoring boundary on a timed rate
      engine (the merged walk's `engine_owning_fid()` path; extend the
      flavored consumer routing only if the recipe loops still exist). Test
      on the fixture: the rate block of the half specification equals the
      rate block of the full specification within 1e-6; the choice block of
      `creation` equals the full specification's within 1e-6.
- [ ] 1.3 Verification: `NOT_CRAN=true` suite green; frozen baselines and C++
      goldens PASS not SKIP.

## 2. Estimation entry and containers

- [ ] 2.1 Remove `abort_on_completion_gaps()` from
      `estimate_from_specification()`; add the one-time `cli_inform` when the
      families' flavor sets differ (r-lib:cli); `compute_statistics()`
      inherits. Keep `completion_gaps` on the specification. Test: the
      vignette's half specification estimates through `estimate_dynam()` and
      `compute_statistics()`; the generative path still completes it with the
      existing warning and `walk_open()` still asserts.
      **Negative guard for the other two estimators.** The abort is a single
      call site shared by all three specification entry points
      (`estimate_dynam()`, `estimate_rem()`, `estimate_dynami()` each route a
      `goldfishSpec` to `estimate_from_specification()`), but a completion gap
      is structurally DyNAM-only: `collect_completion_gaps()` rows come from
      `setdiff(fam, present)`, and `fam` is whichever of `rate` / `choice` was
      supplied. REM refuses `choice` outright, so `fam` is `"rate"` alone and
      the difference is always empty; DyNAMi refuses a flavor-keyed `choice`
      and takes its own construction branch, so it has no flavored processes.
      Assert both, so removing the abort from a shared call site cannot
      silently change a model family it was never reachable for: a flavored REM
      specification carries zero `completion_gaps`, and a DyNAMi specification
      with a flavor-keyed `choice` is still refused at construction.
- [ ] 2.2 `print.goldfishFlavFit` (and the summary printer) group by flavor
      with the families present; snapshot under a pinned cli context for an
      asymmetric container; `coef()`, `vcov()`, `tidy()`, `glance()` and the
      `test_*` fan-outs verified on the asymmetric container.
- [ ] 2.3 Verification: `NOT_CRAN=true` suite green; baselines PASS;
      `devtools::document()` if any roxygen changed.

## 3. Documentation and close

- [ ] 3.1 Vignette `multivariate-specification.Rmd.orig`: the
      `half-specified-estimate` chunk becomes a successful fit with prose
      stating the per-family rule; re-knit per `vignettes/rebuild-all.R`.
- [ ] 3.2 Living-spec deltas re-checked against the folded
      `preprocess-one-walk` wording;
      `bash .plan/opsx-spec-placement-check.sh per-family-flavor-modeling`
      clean; `openspec validate --strict` clean.
- [ ] 3.3 NEWS.d fragment (Improvements: a half-specified flavored
      specification now estimates); no `NEWS.md` or Version edit on the
      branch.
- [ ] 3.4 Final verification: full `NOT_CRAN=true` suite green; baselines
      PASS not SKIP; ready for the trunk merge to fold.
