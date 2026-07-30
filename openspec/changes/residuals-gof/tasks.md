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

- [x] 0.2 **This change owns the `margins` shape decision — and it is now
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
- [x] 0.3 **Do not re-implement primitives that now exist on every backend.**
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
- [x] 0.4 **Assumptions this change was written under that are now false.**
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
- [x] 0.5 **The evaluator substrate is further along than the design says.**
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
      call; snapshot test. (K.2 extends the same footprint note to
      `"conditional_scores"`, sized like `"scores"` at `n × p × 8`.)
- [x] 1.4 C++ in-pass `observed_rank` accumulation behind a flag in
      `DyNAM_choice_default.cpp`, `DyNAM_rate_default.cpp`,
      `DyNAM_rate_ordered_default.cpp`, `DyNAM_MM_default.cpp`,
      `REM_default.cpp`, `REM_ordered_default.cpp`; wire through
      `cpp_interface.R`; cpp-recompile; correctness test vs R-enumerated
      probabilities on a small fixture; NOT_CRAN baselines green.
- [x] 1.5 C++ in-pass margins accumulation per the corrected margins
      requirement (`.plan/sp/residuals-gof.md` §0.2 table): receiver margins
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
- [x] 1.8 `estimate_*(return_preprocessed =)`: attach `preprocessed.goldfish`
      to the fit with a cli size message; consumer-side `preprocessed =`
      precedence helper + guiding cli error (both routes named); tests.
- [x] 1.9 Remove `return_event_scores` outright (deprecation-scope audit,
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
- [x] 1.10 Exact-time scale variants — labels, actor labels, accessor, docs
      (decisions in backend-parity design D12/D16/D17 as revised, spec here):
      margins ship labeled — `"probability"` on every family, plus
      `"expected_count"` on exact-time fits. Folded in from task 0.2: the
      stored shape stays as it is (single-sided `observed`/`expected`,
      two-sided `*_sender`/`*_receiver`), and this task additionally delivers
      (a) actor labels on every stored vector (the margins requirement already
      mandates them; the assembly in `cpp_interface.R` currently returns
      unnamed numerics), and (b) the **exported `margin_table()` accessor**
      (D16) presenting one tibble schema
      (`actor | role | observed | expected_probability | expected_count`,
      roles `sender`/`receiver`/`endpoint`, both expected columns always
      present with documented not-defined `NA`s) — keyed off the exported
      `risk_set_axis()` — so `residuals()` / `diagnose_*()` / `predict()`
      never branch on family; built via `new_diagnostic_table()` with the
      D18 metadata (`diagnostic`/`context`/`params`/`version`), class
      `c("margin_table", <tibble>)` (D17), with a
      `flavored_result.goldfish` method (rbind per-fid + `flavor`/`family`
      columns from `process_map`) whose print reflects the multiple
      flavors, and a cli print method. Fix while touched: the wrong
      comment at `R/cpp_interface.R:600` listing coordination among the
      two-sided sub-models (only REM/REM_ordered are; MM/coordination
      allocate one pair over `n_actors_1`). The conditional loglik component is produced **in the
      estimation pass** by backend-parity (its D17 revision: the assembly
      identity is catastrophic cancellation; the kernel computes
      `x_obs − lse` directly, `NA` on right-censored intervals per its D21) —
      this task documents it and labels the margins, it does not compute
      either. Tests: one fit per family; the any-θ probability-margins
      identity vs the MLE-only expected-count identity; the algebraic
      conditional identity asserted **near the MLE only** and over dependent
      events; label/component invariance across backends.
- [x] 1.11 Milestone: DESCRIPTION bump + NEWS entry for the diagnostics
      surface (the two soft-deprecations, the `return_event_scores`
      removal, and the margins scale marker).

## 2. Evaluator and residual methods (phase 1)

**Sequencing (user, 2026-07-29, design D22):** the kernel session K.1–K.4
below runs **between 2.2 and 2.3**. It carries task 2.1's open remainder
(`exposure` / `n_opportunities`) and the new `conditional_scores` primitive
(D20), which 2.3's exact-time `schoenfeld` / `scaled_schoenfeld`, 2.6's
`survival::cox.zph` cross-check and 4.5's `test_time(method = "trend")` all
depend on. One flag-plumbing pass, one `cpp-recompile`, one baseline run —
rather than opening the kernels twice and landing 2.3 partial.

- [x] 2.1 `evaluate_model()` generic + methods: generalize the existing
      single-pass closure `evaluate_default_c()` (`cpp_interface.R`) —
      dispatch is inside the Rcpp `estimate_()`/`compute_()` entry points
      keyed on `spec` (R-side `modelTypeCall` retired by
      `spec-driven-dispatch`; see task 0.1); single pass at `at`, `return`
      subsetting incl. the `"exposure"`/`"n_opportunities"`
      quantities (D19 as revised by D23: accumulated inside the same masked
      risk-set loop as the margins — `Δt` per at-risk actor over all
      intervals exact-time; opportunity counts over dependent events on
      every family — so composition changes, state-derived flavor masks,
      and support constraints are honored for free; counted per actor
      membership, and storable as well as on-demand), engine defaulting to
      the fit's engine; tests: loglik/
      score at MLE reproduce the fit (1e-10), evaluation at a constrained
      vector returns full-model score/information. **Consume the
      fixed-coefficient contract, do not re-derive it**
      (`fixed-parameter-contract`, landed 1.9.18): fixedness and seeding
      travel as `fixed_spec` / `initial_spec` (`idx` / `values` / `names`)
      and every estimation path decodes them through
      `resolve_coefficient_mask(fixed_spec, initial_spec, n_params)`, which
      yields the seeded parameter vector, the fixed/unfixed index sets and
      the `likelihood_only` / `intercept_fixed` / `intercept_seeded`
      predicates. The positional NA-vector encoding no longer exists.
      **Landed 2026-07-28 except the two availability quantities**
      (commits `74bd536` refactor + `dc1a6ac` evaluator): the shared
      closure is `make_engine_evaluator()` / `make_r_engine_evaluator()`,
      and `evaluate_model()` serves every other quantity on all three
      backends. `"exposure"` / `"n_opportunities"` moved to **K.1**; close
      this task when K.1 lands.
- [x] 2.2 `residuals.result.goldfish()`: stored-primitive types
      (deviance, schoenfeld, score, dfbeta/dfbetas/cooks — the last three
      from stored scores + information matrix) with zero recompute;
      canonical roxygen page defining all types **including the explicit
      caveats section** (likelihood-vs-history deletion, onset/null-
      benchmark reading, cold-start zero-score property, cross-reference
      to `diagnose_onset()`). Landed 2026-07-28 (`7972065`). Note for
      2.3: the exact-time `schoenfeld` abort this task shipped is the
      *interim* behavior — once K.2 and 2.3 land, that path becomes
      stored-if-present, else recompute, and the abort text (currently
      pointing at `type = "score"`) must be re-pointed at the primitive
      and the replay routes.
- [x] 2.3 `residuals()` recompute types: scaled_schoenfeld (corrected
      Grambsch–Therneau scaling `θ̂ + V̄⁻¹ s_k` = `θ̂ + n I⁻¹ s_k`, with
      the diagnosed submodel's own n — REM and DyNAM submodels differ),
      cox_snell, response (conditional multinomial `λ/Σλ` for
      exact-time), martingale (margins; `level = "dyad"` map) via
      `evaluate_model()`; submodel-conditional semantics for DyNAM;
      cox_snell restricted to exact-time rate/REM with cli error
      elsewhere; tests incl. schoenfeld column sums ≈ 0 (multinomial
      submodels only, free parameters, no offsets — exact-time schoenfeld
      rows deliberately do NOT sum to zero) and cox_snell KS-vs-Exp(1) on
      simulated-null fixture. **Revised 2026-07-29 (D20/D21/D22, and
      2.2's finding):** exact-time `schoenfeld` / `scaled_schoenfeld` read
      the `conditional_scores` primitive K.2 lands — they are NOT derivable
      from a stored score row, whose exposure term cannot be removed
      without the observed alternative's statistic row (one vector
      equation, two unknown vectors). They are the first residual types to
      span both tiers: stored if the fit carries the primitive, otherwise
      recomputed through `evaluate_model()` under the replay rules (this is
      where `residuals()` gains its `preprocessed` argument), aborting only
      when neither route is available — and then naming both. `cox_snell` is NOT a recompute type
      at the fitted estimate: it is `intervals * total_rate`, both stored
      once K.3 lands the interval clock (fall back to the compensator
      identity on a fit without it). `scaled_schoenfeld`'s `n` is the
      diagnosed sub-model's **dependent-event** count,
      `sum(!right_censored_events)`, not the interval count. So the
      genuinely recompute-only types here are `response` and
      `martingale(level = "dyad")`.
- [x] 2.4 `fitted()` (`outcome` from stored loglik; `probabilities` via
      evaluator) and in-sample `predict()` (`probabilities`/`ranks`,
      `events =` subset), documented as non-forecasting; tests: predict
      ranks equal stored `observed_rank`. At the fitted estimate
      `predict(type = "ranks")` SHOULD read the stored ranks rather than
      run a pass — the scenario asks for equality, and reproducing them
      through the evaluator is a pass nobody needs; the evaluator route is
      for `at` ≠ `coef(fit)` and for a fit that stored no ranks. Same
      shortcut for a recall statistic read off stored ranks.
- [x] 2.5 `augment.result.goldfish()` gains `.fitted`/`.resid` broom
      columns (censored rows NA); fix the wiring while touched: register
      `S3method(augment, result.goldfish)` + re-export
      `generics::augment` and drop the bare
      `export(augment.result.goldfish)` (dev-line-only, no stub; NEWS
      note); update `diagnose_*` internals to reuse the columns; tests
      (incl. dispatch through the generic).
- [x] 2.6 Validation against classical equivalent fits (D31, reshaped
      2026-07-30 — no REM package enters Suggests). Harness session
      (one-time, `.plan/residuals_comparison.qmd`): coxph/clogit references
      at a shared parameter vector (`evaluate_model(at =)` on the goldfish
      side, `iter.max = 0` on the survival side) — ordinal REM ↔ `coxph`,
      choice and ordinal rate ↔ `clogit`, coordination ↔ `clogit` only if
      the pair weight proves log-linear; scaled-Schoenfeld matrix plus
      per-transform `cox.zph` statistic/p-value — minting frozen reference
      files with provenance (versions, date, fixture); measure the shared-θ
      agreement and document the tolerance one order above it; add the
      remstimate output-mapping note (`standardized_residuals` ↔
      `scaled_schoenfeld`, `recall`/`top_pct` ↔ ranks-derived recall,
      waiting-time Q-Q ↔ `cox_snell`, margins ↔ none) as documentation
      only. Tests (NOT_CRAN): goldfish vs the frozen references at the
      1e-6 discipline; live `stats::glm` Poisson-offset equivalence on an
      all-Δt>0 exact-time fixture (coefficients; loglik up to the
      Σ log Δt constant; score at shared θ); the two-regime scale-pinning
      invariant (within-regime means of scaled Schoenfeld track the
      per-regime refits).
- [x] 2.7 Milestone: DESCRIPTION bump + NEWS entry for residual/fitted/
      predict/augment methods.

## K. Kernel session (phase 1, runs between 2.2 and 2.3)

One pass over the compiled kernels and their R mirror, so the event loop is
opened once. Every addition is a read-only accumulation behind a
default-off flag; `cpp-recompile` after any `src/` edit, and the frozen
baselines must be PASS (not SKIP) on a `load_all(recompile = TRUE)` build
before each commit.

- [x] K.1 `n_opportunities` and `exposure` (design D19; the open remainder
      of task 2.1). Per-actor counts over the realized risk set: `exposure`
      accumulates the interval length for every at-risk actor over **all**
      intervals (exact-time families only), `n_opportunities` counts
      membership over **dependent events** (every family) — the two mirror
      the two margins scales. Both accumulate in the SAME masked walk the
      margins use, so composition changes, state-derived masks and support
      constraints are honored by construction and no R-side replay of masks
      is attempted. The shared reduction is already drafted at
      `.plan/availability_reduction.patch` (`availability_side` +
      `accumulate_availability()` in `event_reductions.{h,cpp}`); note its
      membership rule, which differs from the weighted reductions:
      availability CANNOT fall back on "w is zero off the risk set",
      because an at-risk actor whose fitted weight underflows is still at
      risk — so it reads the `allowed` mask, treating an empty mask as
      "every position", which is the gather backend's pre-masked slice.
      Wire through the 6 `*_default.cpp` kernels, the 3 gather kernels,
      both dispatchers, the r backend's contribution loops and
      `evaluate_model()`; `"exposure"` requested from the **evaluator** on a
      multinomial-only fit aborts naming `"n_opportunities"`, while on the
      storage side it is simply absent, silently.
      **Per ACTOR, not per risk-set position (D23) — the drafted reduction
      gets this wrong.** `accumulate_availability()` as drafted walks
      positions and scatters through the side index, so on a dyadic family
      an actor at risk in many dyads of one interval would collect that
      interval's length once per dyad: on an 84-actor REM, 166× the window.
      The definition is membership — one interval, one `Δt`; one dependent
      event, one opportunity — so the dyadic kernels (REM, REM_ordered, MM,
      and their gather counterparts) need a per-event actor mask (set a bit
      per at-risk actor during the risk-set walk, then add once per set
      bit); the single-sided families keep the position walk, where
      position *is* actor.
      **Both doors (D23):** add `"availability"` to the `diagnostics`
      vocabulary — one coarse primitive name storing an `availability`
      component — served by the same engine flag as the evaluator route,
      and exempt from the per-event storage footprint note (these are
      `|A|`-sized and θ-independent).
      **Shape = the margins' shape (D23):** same container, same per-side
      suffix rule (unsuffixed on rate / choice / coordination;
      `_sender` / `_receiver` on REM and REM_ordered, because a dyad at
      risk makes its sender available on one side and its receiver on the
      other), same actor labeling — reuse `label_margins()`'s side logic
      rather than writing a second convention. Do NOT extend
      `margin_table()` with availability columns (D16 keeps it strictly
      margins; the join surface is the reserved `node_summary()`).
      Tests: the five model-evaluation-pass scenarios (composition change
      honored; opportunities count dependent events only; the multinomial
      abort; the dyadic per-actor bound; evaluated-equals-stored) plus the
      four availability-primitive scenarios (including the two-sided REM
      shape).
- [x] K.2 `conditional_scores` primitive (design D20). The exact-time
      conditional score rows — the Schoenfeld rows — computed **by the same
      call the multinomial kernels already make**:
      `event_score_row(X, probabilities, 1.0, obs, dependent)`, the shared
      reduction at unit scale, with the normalized `probabilities` vector
      that is already in scope for the probability-scale margins and the
      conditional log-likelihood. Three kernels only —
      `DyNAM_rate_default.cpp`, `REM_default.cpp`,
      `compute_poisson_selection.cpp` — plus the one-line R mirror
      (`event_score_row()` in `estimation_core.R`); the multinomial kernels
      need nothing, since their score rows already are the conditional
      rows. Add `"conditional_scores"` to the `diagnostics` vocabulary
      (opt-in, `"all"` includes it, default unchanged), make it a silent
      no-op off exact-time, and size it like `"scores"` in the storage
      footprint note of task 1.3.
      **Add `"conditional_scores"` to `diagnostic_components_with_na`**
      (`cpp_interface.R`). The rows are `NA` on right-censored intervals —
      where `event_scores` is a perfectly good number — so this is the
      first component whose NAs differ from its sibling's. That list is the
      single place the by-design NAs are declared, and `has_unexpected_na()`
      reads it in two guards: the initial-parameters check and the Newton
      loop's step acceptance. Forget it and a valid step is rejected as a
      numerical failure, surfacing as "Estimation not possible with initial
      parameters" on a model that estimated fine before the flag existed.
      Test the guard directly, not only through a converging fit. **Both consumers, one flag** (user,
      2026-07-29): the same engine flag serves estimation-time storage and
      `evaluate_model(return = "conditional_scores")` over the preprocessed
      statistics, so a fit that stored nothing can still produce the rows
      on demand — never two implementations. Tests: the five scenarios of
      the new primitive requirement, including the ordinal-equivalence
      check (the conditional rows equal the corresponding ordinal
      sub-model's `event_scores` at the same non-intercept coefficients,
      with a zero time-intercept column) and the stored-versus-evaluated
      agreement at the fitted estimate.
- [x] K.3 The fit's interval clock (design D21): carry `intervals`,
      `start_time` and `end_time` on the fitted object, at the shared
      `estimate_wrapper()` assembly so DyNAM, REM and DyNAM-i get them
      together. Do NOT add an `is_dependent` vector — `right_censored_events`
      is that fact already; document the two spellings against each other
      instead. **Do NOT bump `FIT_VERSION`** (user, 2026-07-29, D21): the
      epoch names a *released* layout and moves once per release, and no
      released goldfish has ever written one — the last tag is `v1.7.0`
      (2025-06-23) and the epoch was introduced 2026-07-26 at 1.9.15, so
      epoch 2 is "the 2.0.0 layout" and keeps absorbing additions until
      2.0.0 ships. Instead, make the consumers of the clock check for it
      and abort naming what they need, the way `residual_stored()` names a
      missing primitive — that is what covers a dev-line fit that is
      current in stamp but predates the component. Same rule for
      `prep_version`; record it in `format_version.R`'s epoch comment so
      the next addition does not re-litigate it. Document the components on
      the `estimate` return-value page. Tests: the six scenarios of the
      interval-clock requirement.
- [x] K.4 Milestone: DESCRIPTION bump + NEWS entry for the kernel session
      (the two availability quantities, the conditional score rows and the
      interval clock — no epoch change to announce, per K.3).

## 3. diagnose_* class alignment (phase 1, goldfish + autograph)

- [x] 3.1 goldfish: `diagnose_outliers()` → class
      `c("diagnose_outliers", <tibble>)`, `diagnose_changepoints()` →
      `c("diagnose_changepoints", <tibble>)` (D17: constructor-name
      classes via `new_diagnostic_table()`, replacing both the current
      `diagnostic.goldfish` and the earlier flat `*.goldfish` sketch; no
      alias class — dev-line-only); D18 metadata attached (method/
      threshold/window in `params`); logical `outlier`/`cpt` columns; cli
      print methods dispatching on the new classes (column sniffing in
      `print.diagnostic.goldfish` retired); NEWS breaking-change entry;
      tests + snapshots incl. the class-preservation-under-subsetting
      property (D17 experiment).
      **Also lands the D24 default (added 2026-07-29):** both functions
      analyze the **dependent** intervals only, with an argument admitting
      the right-censored ones, `include_censored = FALSE` (settled
      2026-07-29; `include` is reserved by D19's `node_summary()`,
      `intervals` collides with the fit component K.3 landed, and `series`
      collides with 3.2's `effect =`). Read candidacy off the `NA` pattern of
      `augment()`'s `.resid`, which marks exactly the intervals realizing no
      outcome — do NOT introduce a second definition of that fact. Keep one
      row per interval under either setting (the censored rows stay, they
      are simply never flagged), so `nrow()` is independent of the argument
      and the autograph panels keep the observed time axis. The only real
      work is mapping changepoint positions in the dependent subseries back
      to interval indices. The NEWS entry must say the **numbers move** on
      rate and REM fits, not only that the class was renamed. Tests: the
      four scenarios of the new interval-selection requirement, including
      the windowed-fit one (`calls ~ 1 + indeg(calls, window = "15
      minutes")` on `social_evolution` gives 875 intervals, 436 censored,
      and 427 vs 50 PELT changepoints between the two settings).
- [x] 3.1b `model_terms()` + the shared term matcher (D25, added
      2026-07-29). One internal matcher accepting the compact string, the
      export form, the `coef()` label and an integer position, resolving to a
      coefficient index; every surface that takes a term from a user routes
      through it (`initial_parameters` via `match_coef_labels()`,
      `diagnose_*(effect =)`, `test_*(effects =)`), and every abort renders
      candidates as **compact** strings. Measured motivation: on
      `depNetwork ~ 1 + indeg + outdeg + indeg(networkExog)` the summary shows
      `indeg/networkExog`, `tidy(compact = TRUE)` shows
      `indeg_networkExog`, and `initial_parameters` demands
      `ideg_networ_1` — three vocabularies, and the argument users reach for
      takes the unguessable one. Export `model_terms(fit, pattern = NULL)`
      returning the tibble D25 describes (effect-detail columns + compact
      string + `coef()` label + export form + index; `flavor`/`family`
      appended on a flavored fit), documented with the cases where it earns a
      call. `coef()` labels are unchanged — only what is *accepted* widens.
      **The multi-spelling is correctness, not convenience (D25, measured):**
      the printed summary abbreviates to the console width — two terms
      differing only in a window render `inrt/ca...` and `inrt/ca...`
      identically at width 8 — and a term distinguished only by an anonymous
      function collapses to the shared `[t:fn]` token, while `.compactLegend()`
      offers no map back to the full string. So the `coef()` label and the
      index are the only always-resolving spellings, the ambiguity abort must
      **offer one of them per candidate** (listing colliding compact strings
      resolves nothing), and `model_terms()` must return the FULL strings
      irrespective of console width.
      Note for `effect-term-registry` task 5.1: it repoints the compact-string
      builder at `term_def`, which changes how the string is built, not what
      this returns. Tests: the four scenarios of the term-vocabulary
      requirement.
- [x] 3.2 Term-wise `diagnose_*`: `effect =` argument (compact-term-string
      selection, single-series ambiguity error) switching
      `diagnose_changepoints()` to the term's scaled Schoenfeld series and
      `diagnose_outliers()` to dfbeta-based influence ranking; selected
      term recorded on the object for plot labeling; post-selection
      caveat documented; tests + snapshots. **D24:** 3.1's
      interval-selection argument governs this mode too, so its meaning does
      not depend on which series `effect =` chose. In the exact-time
      `scaled_schoenfeld` case the restriction is already automatic (those
      rows are `NA` on right-censored intervals by construction, D20); the
      dfbeta ranking is where it does real work, `event_scores` being
      defined on every interval. Do not extend the restriction to
      `diagnose_onset()` (3.3), `test_gof()` (4.1) or `test_time()` (4.5):
      a censored interval's score row is a genuine gradient contribution.
- [x] 3.3 `diagnose_onset()`: leave-initial-segment-out one-step parameter
      path + information-accrual curve from stored primitives.
      **Ships `information = "opg"` only (settled 2026-07-29).** The exact
      per-event form needs `tr(I_k)` per interval, which no primitive carries:
      `evaluate_model(return = "information")` and
      `fit$final_information_matrix` are both the TOTAL p x p matrix, and D9
      deliberately declined to store per-event information matrices. It is
      therefore a kernel accumulation, and **task 4.6 already schedules the
      only other one before release** — the period-wise Fisher blocks
      `test_time(information = "expected")` needs. Same masked walk, same
      flag with a caller-supplied grouping; the kernels get opened once for
      both consumers rather than twice for one, so the exact option lands
      there. Two reasons this costs little here: the accrual curve is
      **descriptive** (D13: no p-values), so OPG's calibration weakness — the
      Davidson-MacKinnon over-rejection D9 records — is not a failure mode a
      cumulative share has; and the two `information = "expected"` options
      are one feature under two names, so gating them together leaves no
      half-state if the phase-2 cut line takes section 4.
      **D24: NO `include_censored` here.** The cumulative sums run over ALL
      intervals — a score series restricted to the dependent ones has no null
      (measured: the cumulative `indeg` score ends at -0.0003 over all
      intervals and at 142.3 over the dependent subset, so the bridge stops
      returning to zero), and 49% of the endogenous score mass lives in the
      censored intervals of a windowed fit, because a window's lifetime IS the
      censored interval. Instead **index the reported path by dependent-event
      count**: same numbers, an axis that counts history rather than window
      closures. Document why the argument is absent here while
      `diagnose_outliers()` / `diagnose_changepoints()` have it; plot-ready
      classed object with compact-term-string labels; descriptive cli
      print; cold-start vs warm-started fixture tests (drift-then-
      stabilize vs flat); docs with remedies and the
      why-changepoints-miss-this note.
- [x] 3.4 autograph (`feature/goldfish-diag` off `develop` — **branch exists**;
      audited 2026-07-29, the only diagnostics commit on it is `8a4d665`, a
      two-line `intervalLogL` -> `interval_log_lik` rename in both plot
      methods, so everything below is still open): rebind the
      plot methods to the D17 classes — `plot.diagnose_outliers` /
      `plot.diagnose_changepoints` (replacing the planned
      `plot.*.goldfish` names) reading the D18 metadata instead of
      column-sniffing, consuming logical `outlier` (drop the `"YES"`
      check); add `plot.margin_table` (calibration-ratio vs
      martingale-map chosen from the `context` defined-scales entry);
      refresh the precooked `goldfish_outliers`/`goldfish_changepoints`
      fixtures (D18 `version` attribute stamped); autograph tests.
      **Bigger than a column rename (audited 2026-07-29):**
      `plot.changepoints.goldfish` currently consumes `x$data` and
      `x$cpt_points` — a LIST — while 3.1 makes the object a tibble with a
      logical `cpt` column, so that method is rewritten, not adjusted;
      `plot.outliers.goldfish` still carries the `"YES" %in% x$outlier` string
      check. **Also audit the whole goldfish->autograph surface against the
      plot-data requirement**: every object a plot method consumes must arrive
      as a tibble (or a classed list of tibbles) carrying the D18 metadata, so
      no method reshapes or column-sniffs. Per D27 the flavored tables arrive
      row-bound with `flavor` / `family` columns, which is what the plots facet
      on — no separate flavored plot method.

- [x] 3.5 Make the `diagnose_*` family generics (D29, added 2026-07-29).
      `diagnose_outliers()`, `diagnose_changepoints()` and `diagnose_onset()`
      are plain functions, while `margin_table()`, `model_terms()` and
      `evaluate_model()` already dispatch through `UseMethod()` — the same
      package, the same kind of entry point, two shapes. Convert the three,
      each with a `default` method aborting on what it received (the
      `margin_table.default` wording is the pattern), and move the current
      body to the `result.goldfish` method. `abort_if_not_diagnosable()`'s
      class check becomes redundant on the dispatching path and stays only as
      the primitive check.
      Not cosmetic: D27 requires each of these to map over the processes of a
      flavored fit and row-bind the result, which IS a second method, so the
      generic is owed regardless; and a plain function forecloses relocating
      the generic, which D29 shows is a live question — RSiena already
      publishes the `test_*` generics this change was going to define.
      Tests: dispatch on both classes, and the default method's abort. No
      user-visible call changes.
      **Phase 1, NOT under the phase-2 cut line**, by the same logic that put
      R.1-R.5 there: whether an entry point is a generic is part of the
      released API shape, and retrofitting one after 2.0.0 is a breaking
      change where doing it now is invisible.

- [x] 3.6 Stable-schema printing + defining-column demotion (D30, added
      2026-07-30). `new_diagnostic_table()` gains a defining-columns
      declaration carried in the D18 metadata; one shared pair of `[` /
      `dplyr_reconstruct()` helpers demotes to a plain tibble whenever an
      operation yields a table lacking a defining column (grouped_df
      precedent; covers `diagnose_outliers`/`outlier`,
      `diagnose_changepoints`/`cpt`, `margin_table`/`observed`, `.series`).
      `print_diagnose_table()` lists only the flagged rows, count derived
      from the flag column — zero flagged prints the header alone (today it
      prints the head of the full series, and after a column subset it
      silently reports 0 with flagged rows present). Snapshot tests: the
      two-flagged print, the zero-flagged print, column-subset demotes,
      row-filter keeps class and count.

## 4. Diagnostic tests (phase 2)

- [x] 4.0 Before 4.1: the `test_*` family are S3 generics defined **in
      goldfish**, with methods on `result.goldfish` and on the specification
      fit (D6), and a `default` method that aborts on what it received (D29).
      **Name `test_gof()`'s first argument `object`, and `test_parameter()`'s
      and `test_time()`'s `x`** — RSiena 1.6.6 already publishes all three
      names as generics with those signatures, R CMD check enforces
      generic/method argument consistency, and matching them now is what makes
      a later `Enhances: RSiena` + `S3method(RSiena::test_gof, ...)` a two-line
      change instead of a breaking rename. Two of the three already match
      goldfish's own convention; the cost is one local inconsistency on one
      function. See D29 for the measured hazard being carried in the interim.
- [ ] 4.1 `test_gof()` core on `result.goldfish`: standardized cumulative
      score processes from stored `event_scores`, standardization by the
      empirical per-event variance of the centered contributions (the
      OPG scale of both reference implementations; `I_dd/n` documented
      as the asymptotic equivalent, not used — D32), sup statistic
      **identical under both clocks**; `clock = c("event",
      "information")` selects the reference distribution: analytic
      Kolmogorov p-value on the event clock (proportional-accrual
      assumption documented), Lin–Wei–Ying multiplier simulation on the
      observed OPG grid on the information clock (zero passes, stored
      scores only, replication count exposed, session RNG); offset
      terms excluded with a cli error pointing to `test_parameter()`;
      object stores process paths + clock-labeled axis + per-effect
      table; bridge property test (process ends at 0, free effects,
      both clocks) plus statistic-equality-across-clocks test;
      cold-start clock-comparison coverage fixture with its accrual
      concentration verified (statistics equal, event-clock p-values
      conservative, information-clock uniform; NOT_CRAN).
- [ ] 4.2 `test_gof()` on the specification fit: per-block tests,
      per-block and joint Cauchy omnibus; cli print method (grouped by
      block) + snapshot.
- [ ] 4.3 `test_parameter()` score test. **Rescoped 2026-07-29 (D26): the
      fit's `offset()` terms at their imposed values, NOT candidate effects
      absent from the formula.** That is what RSiena's score test actually
      is (`fix = TRUE, test = TRUE` puts the effect *in* the model held at
      its value), `offset(term, coef = value)` is goldfish's spelling of it,
      and it costs one `evaluate_model()` pass with no preprocessing, where
      the absent-effect form costs a full preprocessing pass over the whole
      sequence. Evaluate at `coef(fit, complete = TRUE)`: the fit's own
      `final_score` has `score[id_fixed] <- 0` applied before the Newton
      step, so the score AT the offset coefficients exists nowhere on the
      fit. (The zeroing itself is correct and must stay: the update indexes
      `score[id_unfixed]` so the zeros are never read, and the convergence
      test needs them — `U_x` is generally non-zero at the constrained
      optimum, so an unzeroed `max|score| < tol` would never be met. One
      vector serving both the convergence test and the report is the whole of
      the problem; see D26.) **Settled 2026-07-29: require the preprocessed
      statistics and the pass; do NOT store the unmasked score on the fit.**
      `test_parameter()` takes `preprocessed =` and routes through
      `resolve_preprocessed()`, aborting with the same guiding error the other
      replay-needing diagnostics raise — one rule for which diagnostics need
      the statistics, and the estimation loop's accept/reject reset stays
      untouched. This also collapses a contract split: under Form A the tested
      term is in the formula, so `preprocessed =` here means the fit's own
      statistics, exactly as on `residuals()`. Efficient-score LM with chi-square p-value; equivalence test
      LM = t(Δ) I Δ; power fixture written as
      `offset(recip, coef = 0)` rather than as an omitted term; docs state
      the restriction, show the `offset(term, coef = 0)` idiom as the way to
      test a candidate today, and name the cost difference; docs pointing to
      `lmtest::lrtest`/`waldtest` for nested fitted pairs. Flavored fits need
      no candidate argument (D26/D27): each process's offsets are already in
      its own formula. The absent-effect form is deferred to phase 3 beside
      the simulation-based GOF, where a full replay is the ambient cost and a
      `compute_statistics(..., add_to = prep)` merge could be justified
      across several consumers — on its own it saves roughly half of
      preprocessing (the other effects' evaluation) and none of the sequence
      walk, so it does not change the order of magnitude.
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
      stored scores. **This task owns the in-pass information accumulator
      for BOTH consumers (added 2026-07-29):** it is the only estimation-kernel
      visit scheduled before 2.0.0, and `diagnose_onset()` (3.3) needs the
      same quantity indexed differently — `tr(I_k)` per interval where this
      needs a p x p block per period. One flag taking a caller-supplied
      grouping serves both (group = period here, the trace variant there), so
      the six `*_default.cpp`, the three gather kernels and the R mirror are
      opened once. Deliver 3.3's `information = "expected"` in the same task:
      it is the same feature under two names, and gating them together means
      cutting section 4 leaves both functions on their documented OPG default
      rather than one of them half-built; one-step per-period deltas in the output;
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
      **Geometry settled 2026-07-29 (D28), every clause measured:**
      (a) the path panel is windowed on the excursion, **per coefficient** —
      `x_max = if (stabilized_at == 0) n_events else min(n_events,
      max(ceiling(1.15 * stabilized_at), 10))`. Full range is mostly bridge
      tail (the onset is 7% of the axis on the cold `calls` fit, 31 of 439
      events, at 4.0x/8.5x the y-range); `1.15` is measured free where `+20`
      absolute is not (2.8x on the warm fit, whose whole window is 5); the
      all-zero case takes full range because a path that never left its band
      is provably within +/- tolerance*SE everywhere; the floor of 10 covers
      every coefficient settling in 1-3 events. A proportional floor
      (`n/3`) was rejected at 4.0x-5.1x.
      (b) **x-axes are free** — between the two panels, and across the path
      facets. A window shared across facets re-creates the squashing (a)
      prevents: on the fisheries REM (`stabilized_at = 4/0/66/2/6`) a shared
      window of 66 draws three of five coefficients in 3-9% of their facet at
      3.0x/4.3x/4.8x their own y-range.
      (c) the accrual panel is **full-range with the onset window shaded**
      (`[0, max(stabilized_at)]`), not truncated to it, and draws the
      proportional diagonal `y = x/n` from `context$n_events` — without which
      a monotone 0-to-1 curve says nothing, the signal being the departure
      (0.0482 accrued at stabilization against 0.0706 proportional).
      (d) the method MAY join `path` with `summary` on the term key to place
      the marker — permitted explicitly by the plot-data requirement, and the
      same permission `test_gof()`'s two tables need.
      (e) `path$fixed` coefficients are **not** drawn, an offset being a flat
      line at its imposed value by construction.
      (f) the x-axis is `dropped_events`, not `dropped_intervals` (D24's
      reasoning).
      (g) **two panels composed with patchwork, not paged** — `plot.ag_conv`
      is the precedent (a faceted panel beside a second one via
      `wrap_plots`), and printing pages would break the return-a-ggplot
      contract that `ggsave()` and every autograph test rely on. A
      `view = c("both", "path", "accrual")` argument (renamed from the
      `panel =` sketch, D28 amendment 2026-07-30: `panel` collides with
      panel data and ggplot's facet panels, `scope` with the D24 print
      vocabulary, `level` with `residuals(level =)`) selects one of the two
      as the escape hatch for the many-coefficient case, composed being the
      default; the name is the convention for any autograph method offering
      several renderings of one object.
- [ ] 5.5 autograph NEWS + pkgdown reference entries for the goldfish
      diagnostic family; confirm no goldfish dependency added (dispatch
      on class only, stocnet pattern).
      **Scope narrowed 2026-07-29:** 3.4 already added the NEWS section and
      bumped autograph to 1.0.4, so what remains is the phase-2 additions.
      **Two items are the maintainer's, not ours — raise, do not fix:**
      (i) `pkgdown/_pkgdown.yml` selects `starts_with("plot.")`, which matches
      no topic whose name uses an underscore, so `plot_adequacy`, `plot_gof`,
      `plot_convergence` and `plot_interp` are all absent from the reference
      index today — pre-existing and wider than this change;
      (ii) **RSiena 1.6.6 already publishes `test_gof`, `test_parameter` and
      `test_time` as S3 generics** with `.sienaFit` methods wrapping
      `sienaGOF`, `score.Test` and `sienaTimeTest` — the same three questions
      this change asks of a different model class — so where the stocnet
      diagnostic generics should live, and how goldfish's methods reach a
      generic it does not own, is a cross-package decision (D29).
      `migraph::test_gof` is a lesser, deprecated collision on the same name.

## 6. Documentation and closure

- [ ] 6.0 New long-form vignette `vignettes/diagnostics.Rmd.orig` (D14),
      the canonical prose documentation of the diagnostics layer:
      residual-type map per submodel/flavor (which primitive feeds which
      type; where each identity holds — algebraic vs at-the-MLE);
      **the D24 measurement** (the windowed-fit pooled-vs-dependent table:
      median -9.04 vs -13.73, IQR 13.63 vs 4.83, 427 vs 50 changepoints) as
      the worked example of why the `diagnose_*` default is dependent-only —
      the roxygen states the mechanism, the vignette shows the numbers;
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
      `.plan/sp/residuals-gof.md` §0.4. Plus the D14 2026-07-28 revision:
      MPP framing of the margins (mark side vs ground side), the narrowed
      novelty claim citing Perry & Wolfe as the dyad-level precedent, the
      martingale-residual terminology box (cumulative score processes vs
      per-unit residuals; `survival` `collapse =` precedent), the amorem
      positioning paragraph, the per-actor aggregation section (D19:
      top-x% recall per actor, deviance sums, aggregated-residual ggplots
      with reading hints, attribution taught not frozen — no function),
      and the published-citation swaps (Boschi–Wit Stat&Comp 36(1):4;
      Lakdawala JCSS 8:92; Juozaitienė–Wit JRSS-A 188(4)).
      autograph-gated chunks; precompile
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
- [ ] 6.4 Update `.plan/sp/residuals-gof.md` status header (phases 1-2
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

- [x] R.1 Survey how other packages resolve ties in rank-type diagnostics before
      choosing — `survival` (Efron / Breslow are likelihood corrections, a
      *different* problem from breaking ties in a reported rank; do not conflate
      them), `remstimate`, `relevent`, and base `rank()`'s `ties.method` options.
      Record what each does and why, so the choice is defended rather than
      asserted
- [x] R.2 Choose the rule and the tolerance, and record both in design.md with
      the numbers above. Midrank is the statistician's default and is
      backend-independent *once a tolerance collapses the blocks identically* —
      state that dependency explicitly, since it is the part that is easy to get
      wrong
- [x] R.3 Apply the rule on all three backends, and to every rank-sensitive
      primitive (`observed_rank` and any top-k recall), so they cannot disagree
      about which alternatives are tied
- [x] R.4 Tests: the three scenarios of the "Ranks resolve tied alternatives"
      requirement, including the cross-backend identity that currently fails at
      107 of 439 events on the coordination fixture
- [x] R.5 Document the rule and tolerance where a user reading `observed_rank`
      will meet them

**Related but not this change:** `coordination-tie-consistency` addresses *why*
the coordination path produces different floating-point values for
mathematically equal dyads in the first place (28 distinct values on `r` versus
19 on `cpp` at one event). A tie rule mitigates the symptom and should be
adopted regardless; it does not remove the need to fix the values, and fixing
the values lets this rule use a tolerance sized for ordinary rounding (~1e-15)
rather than one large enough to absorb an inconsistency (>1e-12).
