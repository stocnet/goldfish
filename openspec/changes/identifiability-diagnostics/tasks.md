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

- [ ] 4.1 `summary.goldfishFit()` (post class-naming-scheme name)
  reports the information matrix's condition
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

## 5. Convergence reporting (folded from the parity-followups investigation)

Background, measurements and the cross-package comparison live in
`.plan/convergence_criteria.md`. Read it first: it records why the
likelihood-scaled criterion exists (a 787-actor / 23,065-event fit that never
converged under an absolute gradient tolerance), which is the constraint any
change here must not break.

- [ ] 5.1 Warn when return code 1 coincides with a large `maxAbsUpdate`. The
      live case is in `.plan/residuals_comparison_v01.html`: a fisheries REM
      reporting "gradient close to zero" at `score_rel_norm = 5.67e-07` with
      `max|update| = 1.00e+00`. Warning text via cli, naming both numbers and
      `score_tol` as the lever; the fit is still returned. Settle the threshold
      against real fits rather than picking one
- [ ] 5.2 Report `tconv_max = sqrt(t(g) %*% vcov %*% g)` in `summary()`, beside
      the condition number from 4.1 — the RSiena `tconv.max` analogue (max
      t-ratio over all linear combinations), flagged above **1e-6**.
      **Do not cite RSiena's 0.25**: that is calibrated for Monte Carlo noise in
      a Robbins-Monro estimator, and goldfish's score is deterministic, so its
      floor is several orders smaller. Every cell below passes 0.25, including
      the two that are badly converged.

      The 1e-6 figure is calibrated from what the estimator can actually reach
      (`step_tol = 1e-16`, `max_iterations = 100`, tightening `score_tol`):

      | cell | default | floor | iters (default -> floor) | exit code |
      |---|---|---|---|---|
      | `se_dynam_choice` | 6.8e-04 | 1.3e-12 | 7 -> 9 | 1 |
      | `se_dynam_rate` | 4.7e-05 | 1.6e-10 | 7 -> 8 | 1 |
      | `se_rem` | 4.8e-03 | 4.9e-09 | 12 -> 13 | 1 |
      | `se_dynam_choice_coord` | 2.0e-02 | 1.9e-04 | 16 -> 66 | 2 |
      | `fish_rem` | 3.4e-03 | 4.2e-04 | 12 -> 53 | 2 |

      Healthy and degenerate models separate by 4.6 orders with nothing between;
      1e-6 is the log-midpoint, giving 200x margin on each side. **1e-8 (matching
      `step_tol`) is too tight** — `se_rem` floors at 4.9e-09 and cannot improve,
      so it would clear by only 2x and a slightly larger dataset would false-alarm
      on an ordinary model. Re-derive if the fixture set changes; this rests on
      five cells
- [ ] 5.2b Keep `tconv_max` **reporting-only** — do not make it a stopping rule.
      On the degenerate cells above it costs 53-66 iterations and still exits on
      code 2, which is the "runs to the cap while circling the same region"
      behavior the likelihood-scaled criterion exists to prevent. Record this in
      design.md so it is not re-proposed as an improvement
- [ ] 5.3 Document the scaling in `set_algorithm_newton(score_tol = )`: the rule
      is relative to `|logLik|`, so the absolute gradient it permits grows with
      the data (1.6e-03 on `fish_rem`, ~1e-01 on a 23k-event fit). Include how to
      force a tighter stop, and that `step_tol` is a separate rule that will not
      do it
- [ ] 5.4 Tests: the warning fires on a large-step code-1 fit and is silent on a
      settled one; `tconv_max` is reported and is invariant to data scale on two
      fixtures of different size
- [ ] 5.5 Verification: full `NOT_CRAN=true` run (PASS not SKIP); NEWS entry;
      commit
