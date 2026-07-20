> Sequencing: independent of the 2.0.0 track — every addition sits on a path that
> today ends in an error or runs after convergence, so no frozen baseline can
> move and this can land before or after 2.0.0. Consumes `flavored-processes`
> only for rendered process labels; degrades to plain coefficient names without
> it.

## 1. Null-space attribution at the inversion failure

- [ ] 1.1 Shared diagnostic helper: given the singular
      `informationMatrixUnfixed` and the fit's coefficient names, return the null
      space (SVD of the p×p matrix) and classify each basis vector as
      single-coefficient (one dominant loading) or collinear-group (mass spread),
      with the loadings themselves carried for reporting
- [ ] 1.2 Wire it into BOTH inversion sites — `R/estimation_core.R:507`
      (Newton-Raphson) and `R/cpp_interface.R:499` — as one call, not duplicated
      logic; replace the "probably due to collinearity" text with the attributed
      message (cli semantic elements, names via the established coefficient
      naming path). Report an unconcentrated vector as a set, never as a ranked
      guess
- [ ] 1.3 Multi-process fits name the failing process: render its label from the
      process_map when the object carries one, plain coefficient names otherwise
- [ ] 1.4 Tests: a zero-statistic effect names that effect; two perfectly
      collinear effects name both as a set; a multi-flavor failure names its
      process; snapshots under a pinned cli context
- [ ] 1.5 Verification: full `NOT_CRAN=true` run (baselines PASS not SKIP — no
      successful fit's numbers may move); `devtools::document()`; commit

## 2. Explaining a single degenerate effect

- [ ] 2.1 On the failure path only, when exactly one coefficient is implicated,
      examine that effect's statistics over the risk set and classify: no
      variation anywhere, versus constant across each event's alternatives
- [ ] 2.2 Distinct guidance per case — the first cannot enter the model; the
      second cancels in the choice softmax and may belong in the rate sub-model.
      This is the message that teaches, so it carries the sub-model suggestion
- [ ] 2.3 Tests: the flavored `inertia` under a derived `!tie(L)` mask reports no
      variation; an alternative-constant effect reports the softmax cancellation
      and points at the rate sub-model
- [ ] 2.4 Verification: `NOT_CRAN=true` run (PASS not SKIP); commit

## 3. Separation detection

- [ ] 3.1 Post-convergence check combining coefficient magnitude, standard-error
      magnitude, and a log-likelihood at its attainable ceiling; warn naming the
      effects and stating the estimates are not trustworthy and the thresholds
      are heuristic. No penalization, no refit (that is a separate change with
      its own literature)
- [ ] 3.2 Tests: a perfectly-predicting effect warns; an ordinary fit does not
      (guard the false-positive direction explicitly — a warning on every healthy
      fit would be worse than no warning at all)
- [ ] 3.3 Verification: `NOT_CRAN=true` run (PASS not SKIP); commit

## 4. Conditioning report and documentation

- [ ] 4.1 `summary.result.goldfish()` reports the information matrix's condition
      number and warns above a documented threshold; the fit is never refused.
      Settle the design's open question — always shown, or only above threshold —
      against real fits before choosing
- [ ] 4.2 Troubleshooting documentation on non-identified models: what each
      message means and what to do about it, cross-referenced from the
      competing-processes vignette section that warns about the mutually
      exclusive `inertia` trap
- [ ] 4.3 Tests: condition-number reporting and its warning; documentation
      examples run
- [ ] 4.4 Verification: full `NOT_CRAN=true` run (PASS not SKIP); version bump in
      DESCRIPTION + NEWS.md entry; commit
