> **TDD.** Implementation tasks below run as red/green pairs — an "(a)" task
> writes a failing test and commits it RED, the paired "(b)" task makes it pass and
> commits GREEN (per `openspec/config.yaml` `rules.tasks`). Integration tests,
> recovery checks, wiring tasks exercised through an already-paired task's tests,
> and documentation/close-out tasks are not paired — there is no new implementation
> for them to drive red-then-green — and stay single tasks.

## 1. Multi-process walk: sender-only stat block

- [ ] 1.1a Write a failing regression fixture with two synthetic sender-only choice
      fids sharing one mode, confirming they land on separate sender-only blocks
      (not merged into a dyad block) and that existing dyad-indexed choice fids are
      unaffected. Run to confirm RED; commit the tests alone.
- [ ] 1.1b Extend `stat_block` keying in the merged single-clock walk (behind
      `make_joint_specification()`/preprocessing) to accept a sender-mode-only block
      kind for a choice/ordered fid that has no receiver mode, alongside the
      existing sender-indexed rate blocks and `(sender, receiver)` dyad blocks.
      Re-run the 1.1a tests to confirm GREEN; commit.
- [ ] 1.2 Closing: full `NOT_CRAN=true` run confirming the frozen single-process and
      multi-process preprocessing baselines still preprocess byte-identically (no
      dyad-indexed fid's block assignment changed); run `devtools::document()` if
      any exported preprocessing helper's signature changed; commit.

## 2. Multivariate specification: behavior-process composition

- [ ] 2.1a Write failing tests for mode-set-identity conformance: whole-shared-mode
      behavior+network composition succeeds; subset/nested mode mismatch still
      aborts. Run to confirm RED; commit the tests alone.
- [ ] 2.1b Extend mode-set-identity conformance in `make_joint_specification()` to
      admit a behavior (nodal, single-mode) process alongside dyadic processes,
      resolving cross-process reads between a behavior layer and a network layer by
      the existing whole-shared-mode rule. Re-run the 2.1a tests to confirm GREEN;
      commit.
- [ ] 2.2a Write a failing test for the focal-layer-uniqueness check: a duplicate
      behavior focal layer aborts. Run to confirm RED; commit the tests alone.
- [ ] 2.2b Extend the focal-layer-uniqueness check to treat a behavior layer
      identically to a network layer (at most one composing specification may be
      focal on it; unlimited exogenous-covariate reads). Re-run the 2.2a tests to
      confirm GREEN; commit.
- [ ] 2.3a Write a failing test for the panel-observed-behavior guard: a
      panel-observed behavior layer aborts. Run to confirm RED; commit the tests
      alone.
- [ ] 2.3b Add the panel-observed-behavior guard: `make_joint_specification()`
      aborts with a cli error when a composed behavior specification's focal layer
      has `observation = "panel"`, naming event-observed behavior layers as the
      current scope. Re-run the 2.3a tests to confirm GREEN; commit.
- [ ] 2.4 Closing: full `NOT_CRAN=true` run (frozen baselines PASS not SKIP); run
      `devtools::document()` for any changed roxygen on `make_joint_specification()`;
      commit.

## 3. Behavior layer declaration and risk-set construction

- [ ] 3.1a Write failing tests for behavior specification construction: valid ±1
      events accepted; non-±1 events rejected. Run to confirm RED; commit the tests
      alone.
- [ ] 3.1b Extend `make_specification()` validation to accept a `layer` naming a
      `nodes.goldfish` attribute, tagging the returned `specification.goldfish` as a
      behavior process; validate that the attribute's recorded change events are all
      ±1 increments, aborting with a cli error naming any offending event otherwise.
      Re-run the 3.1a tests to confirm GREEN; commit.
- [ ] 3.2a Write failing tests for behavior-choice risk-set construction: interior
      vs. boundary risk sets. Run to confirm RED; commit the tests alone.
- [ ] 3.2b Implement behavior-choice risk-set construction: for each eligible actor
      and event, the risk set is `{current − 1, current + 1}` clamped to the
      attribute's declared valid range (one-element risk set at a boundary, never
      zero). Re-run the 3.2a tests to confirm GREEN; commit.
- [ ] 3.3a Write a failing test for standalone behavior-only estimation: on a small
      synthetic dataset, recovers known shape-effect coefficients within tolerance.
      Run to confirm RED; commit the tests alone.
- [ ] 3.3b Wire the behavior process through the existing DyNAM rate/choice
      preprocessing and likelihood path so `estimate_dynam()` can fit a
      single (non-composed) behavior specification standalone. Re-run the 3.3a
      tests to confirm GREEN; commit.
- [ ] 3.4 Closing: full `NOT_CRAN=true` run (frozen baselines PASS not SKIP); run
      `devtools::document()` for new/changed exports; commit.

## 4. Behavior evaluation-function effects

- [ ] 4.1a Write failing tests for `shape_linear` and `shape_quadratic`: statistic
      values on a small hand-computable fixture, confirming the own-state effects
      are independent of alters. Run to confirm RED; commit the tests alone.
- [ ] 4.1b Add `R/functions_effects_behavior.R` with `shape_linear` and
      `shape_quadratic` (own-state-only effects), following the existing
      `init_DyNAM_choice.*`/`update_DyNAM_choice.*` contract. Re-run the 4.1a tests
      to confirm GREEN; commit.
- [ ] 4.2a Write failing tests for `similarity_alters` and `avg_alter`: statistic
      values on a small hand-computable fixture, confirming the network-dependent
      effects track a tie change between behavior-change opportunities (not a
      construction-time snapshot). Run to confirm RED; commit the tests alone.
- [ ] 4.2b Add `similarity_alters` and `avg_alter` effects that read a named
      composed network layer's current tie state and the alters' current behavior
      values, confirming they update live (not a construction-time snapshot).
      Re-run the 4.2a tests to confirm GREEN; commit.
- [ ] 4.3 Register the new effects in the formula-parsing/effect-dispatch tables so
      they are usable in a behavior specification's `rate`/`choice` formulas; add
      roxygen documentation entries alongside the existing DyNAM effect docs and run
      `devtools::document()`. Contract plumbing exercised through the 4.1/4.2 effect
      tests — no bespoke unit test of its own.
- [ ] 4.4 Closing: full `NOT_CRAN=true` run (frozen baselines PASS not SKIP); commit.

## 5. End-to-end network + behavior coevolution

- [ ] 5.1 Integration test: compose an event-observed network specification and an
      event-observed behavior specification via `make_joint_specification()`, with
      the network choice formula reading the behavior layer (selection) and the
      behavior choice formula reading the network layer (influence); confirm the
      specification print marks both fids separable. This composes already-tested
      specifications rather than driving new production code, so it is not a
      red/green pair.
- [ ] 5.2 Confirm `estimate_dynam()` fits each fid's parameters correctly from the
      composed specification's per-fid preprocessed output (no new estimator code
      path required per the separability rule).
- [ ] 5.3 Recovery check: simulate or hand-construct a small dataset with a known
      selection effect and a known influence effect, and confirm both processes'
      estimated coefficients recover the known signs/magnitudes within tolerance.
      This validates the already-implemented pipeline end to end rather than
      driving new production code, so it is not a red/green pair.
- [ ] 5.4 Closing: full `NOT_CRAN=true` run; confirm the frozen coefficient
      baselines are unaffected (PASS, not SKIP); commit.

## 6. Documentation and release bookkeeping

- [ ] 6.1 Add a vignette section (or new vignette) walking through declaring a
      behavior layer, composing it with a network layer, estimating, and interpreting
      both objective functions' coefficients, referencing Snijders & Steglich (2017).
- [ ] 6.2 Document the event-observed-only scope and the panel-observed follow-up
      explicitly in the new effects' and `make_joint_specification()`'s roxygen
      `@details`, so users hitting the panel guard (task 2.3) find the reason.
- [ ] 6.3 On a feature branch: write a `NEWS.d/` fragment describing the new behavior
      layer, effects, and composition surface (per `NEWS.d/README.md`); do not bump
      `DESCRIPTION` Version or edit `NEWS.md` directly (integration-branch-only per
      the project workflow).
- [ ] 6.4 Format all touched R files with `air format` and run `lintr::lint()` on
      them; run `devtools::document()` a final time to confirm man/ pages resolve.
      Verify with not-cran-test, then close the task.
