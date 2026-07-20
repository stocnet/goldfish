## 1. Gating math note (design D2 — blocks all implementation)

- [x] 1.1 Write the factorized competing-flavor likelihood derivation as
      `.plan/DyNES/flavored_likelihood_note.md`: joint likelihood over K fully observed
      flavored processes factorizes per flavor; other-flavor events as exogenous state
      changes and right-censoring boundaries on timed sub-models; per-flavor rate
      denominators integrating over each flavor's masked active set with boundaries at
      every event and mask flip
- [x] 1.2 Settle the intercept bookkeeping in the note: derive whether
      `avg_active_actors` is per-flavor over the flavor's combined mask (the spec's
      expectation) or coincides across flavors via right-censored entries (the user's A5
      affirmation); verify numerically on a small hand-built two-flavor fixture against
      `log(n_dep_events / total_time / avg_active_actors)`
- [x] 1.3 Fold the note's conclusion back: amend the `flavored-processes` spec
      requirement "Per-flavor intercept bookkeeping" and design D2 if the derivation
      contradicts the per-flavor expectation; record the outcome in progress.md

## 2. Flavor metadata and add_flavor()

- [x] 2.1 Extend the layer-info metadata vocabulary with `flavor_style` and
      `values_equivalence`; validator checks (allowed styles, syntactic flavor names,
      dichotomous mapping against the layer's update semantics)
- [x] 2.2 Implement exported `add_flavor()` (thin: stamp `ties$flavor` from update
      values, record mapping + style; abort paths for non-dichotomous encodings and
      non-syntactic names, cli errors); roxygen with lifecycle experimental badge;
      run `devtools::document()`
- [x] 2.3 Tests for `add_flavor()` and the metadata validator (testthat 3e,
      hand-built stocnet fixtures; snapshot the cli abort messages under a pinned cli
      context)
- [x] 2.4 Verification: full test run `NOT_CRAN=true` (baselines PASS not SKIP);
      commit

## 3. Multi-flavor specification surface

- [x] 3.1 Lift the single-flavor abort in `make_specification()`: multi-key validation
      (distinct keys, keys resolve against layer flavors, rate/choice key-set equality),
      per-flavor parsed-formula storage on the specification object
- [x] 3.2 Default flavor inference on unflavored layers (increment ±1, replace 1/0)
      with the `cli_inform` assumption message; abort paths for ambiguous encodings and
      non-matching keys
- [x] 3.3 Derive per-flavor support constraints for `mutually_exclusive` layers
      (`state != mapped value` as `tie(L)` grammar formulas), AND-compose with any user
      `support_constraint`, compile K masks into `plan$derivations`
- [x] 3.4 Specification print: nest each modeled flavor under the dependent layer with
      its formulas and derived/combined constraint (cli semantic elements; snapshot
      tests under a pinned cli context)
- [x] 3.5 Tests: multi-key validation matrix, inference, derived-constraint
      compilation, print snapshots
- [x] 3.6 Verification: `NOT_CRAN=true` run (PASS not SKIP), `devtools::document()`;
      version bump in DESCRIPTION + NEWS.md entry (specification surface milestone);
      commit

## 4. Single-pass multi-flavor preprocessing

- [x] 4.1 Effect-union planning: deduplicate effects shared across flavors' formulas
      into one computed statistics set with per-flavor effect maps
- [x] 4.2 Per-flavor writer routing in the recipe loop: dependent-vs-right-censored
      assignment by flavor on timed sub-models (DyNAM-rate, REM); state-only routing on
      ordered/choice sub-models; per-flavor mask flips segmenting each flavor's
      right-censored timeline
- [x] 4.3 Per-flavor intercept scalars (`n_dep_events`, `total_time`,
      `avg_active_actors` over the flavor's combined mask) per the task-1 note;
      per-flavor availability C-format buffers
- [x] 4.4 Return shape (design D9): single-call driver `preprocess_flavored(spec)`
      running both family walks; compile each `(layer, flavor)` constraint once into
      `plan$support_constraints` keyed by `constraint_id` (shared by that flavor's
      rate and choice fids); return a fid-indexed list of `preprocessed.goldfish`
      objects carrying a `process_map` attribute (fid, layer, flavor, family,
      stat_block, has_intercept, constraint_id), each element passing the existing
      engine-readiness checks; routing via the `(layer, flavor) → fid` lookup, not
      flavor-name indexing (D10)
- [x] 4.5 Tests: two-flavor fixtures with hand-computed dependent/RC partitions,
      shared-effect single computation, per-flavor scalars against fixture values;
      subset-modeled fixture (fisheries pattern: a present-but-unmodeled flavor's
      events update state AND appear as a rate-integral boundary for every modeled
      timed flavor — assert whether the boundary lands as an RC row or a derived
      mask flip); process_map correctness (fid indexing, constraint_id shared across
      a flavor's rate/choice rows); empty-risk-set abort naming the flavor via a
      label rendered from process_map
- [x] 4.6 Verification: `NOT_CRAN=true` run (frozen baselines PASS not SKIP —
      single-flavor and plain paths unchanged); commit

## 5. Per-flavor estimation and the sectioned result

- [x] 5.1 Estimation loop over the fid-indexed preprocessed list (existing engines
      unchanged); container result class carrying the process_map with per-fid
      results (rate/choice nested per flavor at presentation time)
- [ ] 5.2 Methods: `print()` with cli sections per flavor, `coef()`/`vcov()`/`logLik()`
      returning components whose labels are rendered from the process_map (never
      parsed from keys), with summed total log-likelihood; `devtools::document()`
- [ ] 5.3 Equivalence tests: container fit equals standalone per-flavor fits with the
      equivalent user-supplied constraint (1e-6); print snapshots under a pinned cli
      context. ALSO a `redundant` fixture — currently every flavored test is
      `mutually_exclusive`, yet the redundant branch is what the Fisheries
      flagship runs and it exercises different code: `assign_constraint_ids()`
      returns all-`NA`, consumers carry `NULL` constraints,
      `finalize_consumers()` skips mask realization, and the driver's validation
      loop finds nothing to validate. Its equivalence claim is the DIFFERENT one:
      a redundant flavor's container fit equals a standalone single-flavor fit
      with NO constraint at all. Assert too that `plan$support_constraints` is
      empty and every `constraint_id` is `NA`, so the no-constraint path is
      pinned rather than inferred from the fit agreeing.
- [ ] 5.4 Fisheries flagship example: two-process model in the dataset help page
      and a vignette section on competing processes. `bilatchanges` is
      `flavor_style = "redundant"` — verified in the data: 32 of its 38 dyads
      carry a repeated `+1` with no intervening `-1`, and the tie accumulates to
      a weight of 13 — so NO constraint is derived and `inertia` is the
      substantively interesting effect (do prior treaties beget more?). Do not
      present it as a mutually-exclusive creation/dissolution pair.
      The vignette section MUST also carry the identifiability caveat for the
      mutually-exclusive case, which is where the creation/dissolution framing
      does apply: under a derived `!tie(L)` mask every allowed alternative has
      weight 0, so `inertia(L)` is identically 0 and cannot be identified — this
      holds regardless of `weighted =`. On the `tie(L)` side it is constant (and
      so cancels in the softmax) for a binary layer, but varies and is fine for
      an accumulating one. Without this, `creation ~ inertia` is the obvious
      thing to write and it fails with a bare "matrix cannot be inverted".
- [ ] 5.5 Verification: full `NOT_CRAN=true` run (PASS not SKIP); version bump in
      DESCRIPTION + NEWS.md entry (multi-process estimation milestone); commit.
      Then a SEPARATE `fix:` commit raising `Depends: R (>= 4.4.0)` with its own
      NEWS line: the package already uses base `%||%` in 10+ files and that
      function entered base in 4.4.0, so the declared `>= 4.1.0` is wrong today
      and fails on CRAN's oldrel. It rides this change only because it touches
      the same file — it is an independent bug, so it gets its own commit rather
      than hiding inside a feature milestone where it cannot be found or
      reverted on its own.
