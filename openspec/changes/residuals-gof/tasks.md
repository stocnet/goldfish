# Tasks — residuals-gof

Every implementation task: invoke `r-lib:r-package-development` before
package-machinery work, `r-lib:testing-r-packages` before writing tests,
`r-lib:cli` before any console output, `r-lib:lifecycle` for the
deprecations; `cpp-recompile` after any `src/` edit; `not-cran-test`
before each commit (baselines PASS, not SKIP); `devtools::document()`
inline whenever roxygen/exports change; roxygen inheritance
(`@inheritParams`/`@inherit`) instead of duplicated docs. autograph work
happens in `/Users/ualvaro/Documents/repos/autograph` on branch
`feature/goldfish-diag` (create from `develop`).

## 0. Re-ground the 2026-07-17 design (added 2026-07-23)

- [x] 0.1 Re-verify the Context "key code facts" before any edit: the design
      was written 2026-07-17 against a dispatch/estimation surface that has
      since moved — `refactor-single-data-object` archived, `multimode`
      (two-mode fits) landing, and `spec-driven-dispatch` (risk-set
      descriptor onto the model spec + legacy-vocabulary retirement) still
      in flight (16/21). Confirm each Context claim still holds and refresh
      every symbol/line anchor cited in `design.md`/`tasks.md`:
      (a) the six `*_default.cpp` engines still compute `intervalLogL` and
      the `return_event_scores` per-event score rows, and `cpp_interface.R`
      still returns the `pMatrix` "not implemented" fallback (ranks/margins
      really do need in-pass C++); (b) `modelTypeCall` is still the dispatch
      `evaluate_model()` (task 2.1) routes through, and note where it now
      lives post-`spec-driven-dispatch` (`preprocess_writers.R` /
      `preprocess_export.R` / `cpp_interface.R`); (c) the
      `make_specification()`-based fit shape `test_gof()` dispatches on, incl.
      the risk-set descriptor now attached to the spec; (d)
      `preprocessing_only` / `preprocessed` still the replay surface;
      (e) `augment.result.goldfish` shape and the `diagnose_*` /
      `diagnostic.goldfish` classes. Inventory the flavored (per-fid) and
      two-mode (`node_lookup`) fit shapes the fit-shape-compatibility
      paragraph depends on, against the landed multimode surface. Record any
      drift as `design.md`/`tasks.md` deltas BEFORE touching 1.1. Because
      `spec-driven-dispatch` is not yet archived, re-run the dispatch/spec
      portion of this audit against the landed surface once it lands. Write
      findings to `progress.md`.

### What `backend-parity` changed under this change (added 2026-07-26)

`backend-parity` archived on 2026-07-26. It landed work this change was written
before, and settled two questions that belong here. Read this before section 1
so nothing is built twice or against a retired assumption.

- [ ] 0.2 **This change owns the `margins` shape decision — and it is now
      "accessor, not reshape".** Explored and decided 2026-07-26: `margins`
      storage stays as it is (`observed`/`expected` single-sided,
      `*_sender`/`*_receiver` two-sided, coordination single-sided over one
      actor set totalling 2n). Task 1.10 additionally delivers (a) the actor
      labels this change's `margins primitive content` requirement already
      mandates, and (b) a **uniform accessor** presenting one shape to every
      consumer, so `residuals()` / `diagnose_*()` / `predict()` never branch on
      family. Non-breaking, and it keeps the concept in one capability.
      `parity-followups` originally proposed reshaping the storage from its own
      capability and **dropped it** for this reason (its task 0.3); it
      contributes only the exported risk-set axis, which the accessor consumes.
      Fold this into 1.10's scope rather than adding a task.
- [ ] 0.3 **Do not re-implement primitives that now exist on every backend.**
      `backend-parity` extended the in-pass accumulation this change's tasks
      1.4–1.6 started (all marked done) to all three backends, and added what
      they did not cover: per-event `probabilities` on `cpp` and `gather`
      (actor-indexed over the whole node set, zero off the risk set — its D23,
      a BREAKING shape change to the `r` backend's existing output);
      `conditional_logl` on the two exact-time `cpp` kernels, which had never
      computed it; and probability-scale margins beside the compensator scale
      on those kernels. Verify against the code before writing anything in
      sections 1–2 — several design paragraphs here are written in the future
      tense about work that has landed.
- [ ] 0.4 **Assumptions this change was written under that are now false.**
      Corrected in place already, listed so the corrections are not undone:
      the design's key-code-fact "No `default_c` engine returns a probability
      matrix" (they all do now — the conclusion that ranks/margins stay in-pass
      still holds, but because materializing the matrix is $O(n|R|)$, not
      because it is unavailable); and task 1.9's "`scores_explicit` reduces to
      an explicit `diagnostics` request" (it no longer exists — `backend-parity`
      removed it from the control object with the gather+scores abort and the
      silent default-sourced drop it existed to choose between). **Task 1.9 is
      otherwise still real work**: the `return_event_scores` *argument* still
      exists in `set_opt.R` and this change still owns removing it.
- [ ] 0.5 **The evaluator substrate is further along than the design says.**
      D3 names `evaluate_default_c(pars, need_scores)` as the closure to
      generalize into `evaluate_model()`. It now takes six flags
      (`need_scores`, `need_ranks`, `need_margins`, `need_total_rate`,
      `need_probabilities`), and the `(backend, primitive)` support table plus
      `check_diagnostic_support()` already exist in `set_opt.R`. Re-read both
      before designing `evaluate_model()`: most of the threading is done, and
      the capability check it needs is written.

## 1. Diagnostic primitives (phase 1)

- [x] 1.1 `set_algorithm_newton(diagnostics =)`: vocabulary validation
      (`loglik`/`scores`/`ranks`/`margins`/`probabilities`, TRUE/"all"/
      FALSE), default `c("loglik","scores")`, cli errors for unknown
      names; unit tests.
- [x] 1.2 Lifecycle soft-deprecation of `return_interval_loglik`,
      `return_probabilities`, `return_event_scores` with one-to-one
      mapping and conflict error; tests for warning text and mapping.
- [x] 1.3 Probabilities guardrail: pre-run cli warning with estimated
      size (`n_events × |riskset| × 8` bytes, human-readable), once per
      call; snapshot test.
- [x] 1.4 C++ in-pass `observed_rank` accumulation behind a flag in
      `DyNAM_choice_default.cpp`, `DyNAM_rate_default.cpp`,
      `DyNAM_rate_ordered_default.cpp`, `DyNAM_MM_default.cpp`,
      `REM_default.cpp`, `REM_ordered_default.cpp`; wire through
      `cpp_interface.R`; cpp-recompile; correctness test vs R-enumerated
      probabilities on a small fixture; NOT_CRAN baselines green.
- [x] 1.5 C++ in-pass margins accumulation per the corrected margins
      requirement (`.plan/residuals-gof.md` §0.2 table): receiver margins
      for choice; **both sender and receiver margins for REM**
      (exact-time = compensator sums incl. right-censored intervals;
      ordinal = coarsened-softmax sums); sender margins for rate
      (exact-time exposure incl. right-censored intervals / ordinal
      probability sums); per-actor pair margins for MM (each event credits
      both members); accumulation over the realized risk set
      (`twomode_or_reflexive`); wire-through. Consistency tests split
      per flavor: multinomial sums = n_events (2·n_events for MM) at
      machine tolerance and at a non-MLE parameter vector; exact-time
      sums = n_events at the converged MLE only (convergence tolerance,
      intercept present); REM sender total == receiver total identically.
- [x] 1.6 `total_rate` under the `"loglik"` primitive for exact-time
      engines (per-event sum of fitted rates over the realized risk set;
      O(n) vector, on by default); consistency test: `total_rate × Δt`
      equals the Cox–Snell residuals; large-dataset cli storage note per
      spec.
- [x] 1.7 Route `diagnostics` primitives through estimation to the result
      object (all engines); parity test `default` vs `default_c` for
      ranks/margins on a fixture.
- [ ] 1.8 `estimate_*(return_preprocessed =)`: attach `preprocessed.goldfish`
      to the fit with a cli size message; consumer-side `preprocessed =`
      precedence helper + guiding cli error (both routes named); tests.
- [ ] 1.9 Remove `return_event_scores` outright (deprecation-scope audit,
      backend-parity D11: never in a public release — no lifecycle cycle
      owed), partially undoing 1.2: drop the argument, its
      `LEGACY_DIAGNOSTIC_FLAGS` / reconcile branch and deprecation test.
      **`scores_explicit` no longer exists** — `backend-parity` removed it from
      the control object along with the gather+scores abort and the silent
      default-sourced drop it existed to choose between, so there is nothing
      left here to reduce to an explicit `diagnostics` request. The two
      public flags (`return_interval_loglik`, `return_probabilities`) keep
      their 1.2 soft-deprecation unchanged. r-lib:lifecycle before the work;
      snapshot updates; `devtools::document()`.
- [ ] 1.10 Exact-time scale variants — labels and docs (decisions in
      backend-parity design D12/D16/D17 as revised, spec here): margins ship
      labeled — `"probability"` on every family, plus `"expected_count"` on
      exact-time fits. The conditional loglik component is produced **in the
      estimation pass** by backend-parity (its D17 revision: the assembly
      identity is catastrophic cancellation; the kernel computes
      `x_obs − lse` directly, `NA` on right-censored intervals per its D21) —
      this task documents it and labels the margins, it does not compute
      either. Tests: one fit per family; the any-θ probability-margins
      identity vs the MLE-only expected-count identity; the algebraic
      conditional identity asserted **near the MLE only** and over dependent
      events; label/component invariance across backends.
- [ ] 1.11 Milestone: DESCRIPTION bump + NEWS entry for the diagnostics
      surface (the two soft-deprecations, the `return_event_scores`
      removal, and the margins scale marker).

## 2. Evaluator and residual methods (phase 1)

- [ ] 2.1 `evaluate_model()` generic + methods: generalize the existing
      single-pass closure `evaluate_default_c()` (`cpp_interface.R`) —
      dispatch is inside the Rcpp `estimate_()`/`compute_()` entry points
      keyed on `spec` (R-side `modelTypeCall` retired by
      `spec-driven-dispatch`; see task 0.1); single pass at `at`, `return`
      subsetting, engine defaulting to the fit's engine; tests: loglik/
      score at MLE reproduce the fit (1e-10), evaluation at a constrained
      vector returns full-model score/information.
- [ ] 2.2 `residuals.result.goldfish()`: stored-primitive types
      (deviance, schoenfeld, score, dfbeta/dfbetas/cooks — the last three
      from stored scores + information matrix) with zero recompute;
      canonical roxygen page defining all types **including the explicit
      caveats section** (likelihood-vs-history deletion, onset/null-
      benchmark reading, cold-start zero-score property, cross-reference
      to `diagnose_onset()`).
- [ ] 2.3 `residuals()` recompute types: scaled_schoenfeld (corrected
      Grambsch–Therneau scaling `θ̂ + V̄⁻¹ s_k` = `θ̂ + n I⁻¹ s_k`, with
      the diagnosed submodel's own n — REM and DyNAM submodels differ),
      cox_snell, response (conditional multinomial `λ/Σλ` for
      exact-time), martingale (margins; `level = "dyad"` map) via
      `evaluate_model()`; submodel-conditional semantics for DyNAM;
      cox_snell restricted to exact-time rate/REM with cli error
      elsewhere; tests incl. schoenfeld column sums ≈ 0 (multinomial
      submodels only, free parameters, no offsets — exact-time schoenfeld
      rows deliberately do NOT sum to zero) and cox_snell KS-vs-Exp(1) on
      simulated-null fixture.
- [ ] 2.4 `fitted()` (`outcome` from stored loglik; `probabilities` via
      evaluator) and in-sample `predict()` (`probabilities`/`ranks`,
      `events =` subset), documented as non-forecasting; tests: predict
      ranks equal stored `observed_rank`.
- [ ] 2.5 `augment.result.goldfish()` gains `.fitted`/`.resid` broom
      columns (censored rows NA); fix the wiring while touched: register
      `S3method(augment, result.goldfish)` + re-export
      `generics::augment` and drop the bare
      `export(augment.result.goldfish)` (dev-line-only, no stub; NEWS
      note); update `diagnose_*` internals to reuse the columns; tests
      (incl. dispatch through the generic).
- [ ] 2.6 Cross-package validation (NOT_CRAN): scaled Schoenfeld vs
      `survival::cox.zph`-consistent reference on a Cox-expressible REM
      fixture; residual comparison vs `remstimate::diagnostics()` on a
      shared small dataset.
- [ ] 2.7 Milestone: DESCRIPTION bump + NEWS entry for residual/fitted/
      predict/augment methods.

## 3. diagnose_* class alignment (phase 1, goldfish + autograph)

- [ ] 3.1 goldfish: `diagnose_outliers()` → class
      `c("outliers.goldfish","data.frame")`, `diagnose_changepoints()` →
      `c("changepoints.goldfish","data.frame")`; logical `outlier`/`cpt`
      columns; cli print methods dispatching on the new classes; NEWS
      breaking-change entry; tests + snapshots.
- [ ] 3.2 Term-wise `diagnose_*`: `effect =` argument (compact-term-string
      selection, single-series ambiguity error) switching
      `diagnose_changepoints()` to the term's scaled Schoenfeld series and
      `diagnose_outliers()` to dfbeta-based influence ranking; selected
      term recorded on the object for plot labeling; post-selection
      caveat documented; tests + snapshots.
- [ ] 3.3 `diagnose_onset()`: leave-initial-segment-out one-step parameter
      path + information-accrual curve from stored primitives (OPG
      default; exact-information option via the evaluator); plot-ready
      classed object with compact-term-string labels; descriptive cli
      print; cold-start vs warm-started fixture tests (drift-then-
      stabilize vs flat); docs with remedies and the
      why-changepoints-miss-this note.
- [ ] 3.4 autograph (`feature/goldfish-diag` off `develop`): fix
      `plot.outliers.goldfish` to consume logical `outlier` (drop the
      `"YES"` check), verify `plot.changepoints.goldfish` against the new
      objects; refresh the precooked `goldfish_outliers`/
      `goldfish_changepoints` fixtures; autograph tests.

## 4. Diagnostic tests (phase 2)

- [ ] 4.1 `test_gof()` core on `result.goldfish`: standardized cumulative
      score processes from stored `event_scores`, standardization by the
      average per-event information `J_d = I_dd/n` (fallback empirical
      variance of centered contributions), sup statistic, Kolmogorov
      p-value; `clock = c("event", "information")` with the information
      clock from OPG cumulative score sums (zero passes) and the
      proportional-accrual assumption documented on the event clock;
      offset terms excluded with a cli error pointing to
      `test_parameter()`; object stores process paths + clock-labeled
      axis + per-effect table; bridge property test (process ends at 0,
      free effects, both clocks); cold-start clock-comparison coverage
      fixture (NOT_CRAN).
- [ ] 4.2 `test_gof()` on the specification fit: per-block tests,
      per-block and joint Cauchy omnibus; cli print method (grouped by
      block) + snapshot.
- [ ] 4.3 `test_parameter()` score test: constrained fit + candidate
      effects → `evaluate_model()` at the constrained estimate →
      efficient-score LM with chi-square p-value; equivalence test
      LM = t(Δ) I Δ; omitted-reciprocity power fixture; docs pointing to
      `lmtest::lrtest`/`waldtest` for nested fitted pairs.
- [x] 4.4 ~~`test_parameter()` Wald form~~ — DESCOPED 2026-07-19 (user
      decision): trails post-release; `lmtest::waldtest()` covers nested
      pairs meanwhile. No implementation; the diagnostic-tests spec delta
      records the deferral.
- [ ] 4.5 `test_time(method = "trend")`: scaled Schoenfeld vs time
      transform (`identity`/`rank`/`km`), per-effect zero-slope score
      tests + global test, plot-ready residual data on the object; cli
      print + snapshot; null-uniformity fixture test.
- [ ] 4.6 `test_time(method = "periods")`: period-masked partial sums of
      stored `event_scores` (no preprocessing; first period = reference);
      `periods =` forms (integer equal-event default, cut times, grouping
      vector); `information = c("expected", "opg")` — expected via one
      evaluator pass accumulating period-wise Fisher blocks, OPG from
      stored scores; one-step per-period deltas in the output;
      regime-change detection fixture + expected-vs-OPG agreement test +
      no-replay test.
- [ ] 4.7 Effect selection by compact term strings (`effects =` /
      `effect =`): shared matching helper (exact string, family
      expansion in `test_*`, cli ambiguity error with candidates,
      integer fallback) reusing the compact-string builder; tests incl.
      the inertia-variants fixture.
- [ ] 4.8 Coverage/power simulation suite (NOT_CRAN): test_gof null
      uniformity at n ∈ {1000, 5000} and power vs non-linear reciprocity
      DGP, per the Boschi-Wit §4 design; seeds fixed; runtime documented.
- [ ] 4.9 Milestone: DESCRIPTION bump + NEWS entry for the test_* family.

## 5. autograph plot methods (phase 2)

- [ ] 5.1 `plot` method for the test_gof class: per-effect standardized
      process paths with Brownian-bridge reference bands, faceted by
      block, x-axis from the object's clock-labeled process-time axis
      (bands are valid on whichever clock produced the process — the
      plot must not re-derive an event-index axis); precooked fixture
      object; renders without goldfish attached; autograph tests.
- [ ] 5.2 `plot` method for the test_time class: scaled Schoenfeld
      scatter + weighted smooth per effect with zero reference; precooked
      fixture; autograph tests.
- [ ] 5.3 autograph one-call overview: `plot` method on the goldfish fit
      (remstimate-parity) composing via patchwork from **stored
      primitives only** — deviance trace with flagged outliers, scaled
      Schoenfeld smooths (top effects), test_gof bridge processes, and
      the waiting-time Q-Q from `total_rate` (panel drops out when the
      fit lacks the primitive or is ordinal); precooked fixture;
      autograph tests.
- [ ] 5.4 autograph plot method for the `diagnose_onset` class:
      per-coefficient path panel (with stabilization marker) +
      information-accrual panel; precooked fixture; autograph tests.
- [ ] 5.5 autograph NEWS + pkgdown reference entries for the goldfish
      diagnostic family; confirm no goldfish dependency added (dispatch
      on class only, stocnet pattern).

## 6. Documentation and closure

- [ ] 6.0 New long-form vignette `vignettes/diagnostics.Rmd.orig` (D14),
      the canonical prose documentation of the diagnostics layer:
      residual-type map per submodel/flavor (which primitive feeds which
      type; where each identity holds — algebraic vs at-the-MLE);
      **margins as calibration descriptives** (observed-vs-expected actor
      maps, both sides on REM, the ghost-effects heterogeneity
      motivation, explicitly-not-tests caveat, `test_parameter()` as the
      formal route); the `test_*` family walkthrough incl. the clock
      workflow (`diagnose_onset()` accrual curve →
      `clock = "information"`); a **REM-vs-DyNAM comparison section** (D14,
      descriptive only, stored primitives only): per-event conditional
      difference `conditional^REM − (conditional^rate + loglik^choice)` with
      cumulative trace figure, which/when decomposition table with AIC/BIC,
      REM margins vs DyNAM composed probability-scale margins side by side,
      Cox–Snell Q-Q per model, `predict()` who-is-next agreement; closes by
      naming the Vuong statistic (backend-parity appendix item 11) as the
      future formal route, not implemented; literature positioning from
      `.plan/residuals-gof.md` §0.4. autograph-gated chunks; precompile
      rebuild; pkgdown reference entry.
- [ ] 6.1 `vignettes/teaching1.Rmd.orig`: add a **short** model-diagnostics
      section after the estimation walkthrough — deviance residuals and
      surprising events (`residuals()`, `fitted()`), `test_gof()` on the
      fitted DyNAM (per-block + omnibus reading), `test_time()` trend
      plot, closing with a pointer to the diagnostics vignette for depth
      (D14: keep it short and sweet); plots rendered via autograph
      (chunks gated on `requireNamespace("autograph")`; autograph added
      to Suggests); rebuild through the precompile workflow.
- [ ] 6.2 `vignettes/teaching2.Rmd.orig`: expand the existing
      `plot-examine` section — the `diagnose_*` calls gain their new
      classes/plots, plus REM-specific additions kept **short** (D14):
      Cox–Snell waiting-time Q-Q, scaled Schoenfeld/`test_time()` for the
      tie model, `test_parameter()` score-test example (test an effect
      without re-estimating), `lmtest::lrtest()` usage, REM margins shown
      once as a calibration map with the descriptive-not-test caveat and
      a pointer to the diagnostics vignette; same autograph gating and
      precompile rebuild.
- [ ] 6.3 Roxygen inheritance audit: canonical pages
      (`residuals.result.goldfish`, `evaluate_model`, `test_gof`)
      inherited elsewhere; `devtools::document()`; man pages checked for
      resolved inheritance; full `NOT_CRAN=true` suite green.
- [ ] 6.4 Update `.plan/residuals-gof.md` status header (phases 1-2
      implemented; phase 3 pending DyNES); final DESCRIPTION/NEWS pass.

## Rank ties (folded from the parity-followups investigation, 2026-07-26)

**Phasing (user, 2026-07-27, design D15):** R.1–R.5 belong to phase 1 and
run **before section 2** — the tie rule is part of the primitive contract
`evaluate_model()`/`augment()` consume, so it is release-critical and not
subject to the phase-2 cut line. The rule and tolerance are decided
(strict-greater, `tol = 1e-12`; design D15); R.1's survey documents the
choice, R.2 is the design record (done in D15).

Evidence, measured on `Social_Evolution` coordination
(`calls_dependent ~ inertia + trans`) comparing `backend = "r"` against
`backend = "cpp"` at the MLE:

```
  strict `>` (today)          106 of 439 events disagree
  midrank, tolerance 0        159  <- WORSE than the status quo
  midrank, tolerance 1e-15    135
  midrank, tolerance 1e-12      0
  midrank, tolerance 1e-9       0
  midrank, tolerance 1e-6       0
```

The tolerance is what makes the rule reproducible, not the midrank. At zero
tolerance the fractional term amplifies block-membership differences and the
disagreement count goes up. The safe window is wide: the backends agree on
probabilities to ~5.6e-13, and genuinely distinct levels are separated by
factors of ~17 on this fixture, so ~1e-9 sits three orders above the noise and
several below any real structure.

- [ ] R.1 Survey how other packages resolve ties in rank-type diagnostics before
      choosing — `survival` (Efron / Breslow are likelihood corrections, a
      *different* problem from breaking ties in a reported rank; do not conflate
      them), `remstimate`, `relevent`, and base `rank()`'s `ties.method` options.
      Record what each does and why, so the choice is defended rather than
      asserted
- [ ] R.2 Choose the rule and the tolerance, and record both in design.md with
      the numbers above. Midrank is the statistician's default and is
      backend-independent *once a tolerance collapses the blocks identically* —
      state that dependency explicitly, since it is the part that is easy to get
      wrong
- [ ] R.3 Apply the rule on all three backends, and to every rank-sensitive
      primitive (`observed_rank` and any top-k recall), so they cannot disagree
      about which alternatives are tied
- [ ] R.4 Tests: the three scenarios of the "Ranks resolve tied alternatives"
      requirement, including the cross-backend identity that currently fails at
      107 of 439 events on the coordination fixture
- [ ] R.5 Document the rule and tolerance where a user reading `observed_rank`
      will meet them

**Related but not this change:** `coordination-tie-consistency` addresses *why*
the coordination path produces different floating-point values for
mathematically equal dyads in the first place (28 distinct values on `r` versus
19 on `cpp` at one event). A tie rule mitigates the symptom and should be
adopted regardless; it does not remove the need to fix the values, and fixing
the values lets this rule use a tolerance sized for ordinary rounding (~1e-15)
rather than one large enough to absorb an inconsistency (>1e-12).
