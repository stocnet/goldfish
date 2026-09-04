## 1. Multi-process walk: sender-only stat block

- [ ] 1.1 Extend `stat_block` keying in the merged single-clock walk (behind
      `make_joint_specification()`/preprocessing) to accept a sender-mode-only block
      kind for a choice/ordered fid that has no receiver mode, alongside the existing
      sender-indexed rate blocks and `(sender, receiver)` dyad blocks.
- [ ] 1.2 Add a regression fixture with two synthetic sender-only choice fids sharing
      one mode, confirming they land on separate sender-only blocks (not merged into a
      dyad block) and that existing dyad-indexed choice fids are unaffected.
- [ ] 1.3 Verify the frozen single-process and multi-process preprocessing baselines
      still preprocess byte-identically (no dyad-indexed fid's block assignment
      changed). Run `devtools::document()` if any exported preprocessing helper's
      signature changed. Verify with not-cran-test, then close the task.

## 2. Multivariate specification: behavior-process composition

- [ ] 2.1 Extend mode-set-identity conformance in `make_joint_specification()` to
      admit a behavior (nodal, single-mode) process alongside dyadic processes,
      resolving cross-process reads between a behavior layer and a network layer by
      the existing whole-shared-mode rule.
- [ ] 2.2 Extend the focal-layer-uniqueness check to treat a behavior layer
      identically to a network layer (at most one composing specification may be
      focal on it; unlimited exogenous-covariate reads).
- [ ] 2.3 Add the panel-observed-behavior guard: `make_joint_specification()` aborts
      with a cli error when a composed behavior specification's focal layer has
      `observation = "panel"`, naming event-observed behavior layers as the current
      scope.
- [ ] 2.4 Unit tests: whole-shared-mode behavior+network composition succeeds;
      subset/nested mode mismatch still aborts; duplicate behavior focal layer
      aborts; panel-observed behavior layer aborts. Run `devtools::document()` for any
      changed roxygen on `make_joint_specification()`. Verify with not-cran-test, then
      close the task.

## 3. Behavior layer declaration and risk-set construction

- [ ] 3.1 Extend `make_specification()` validation to accept a `layer` naming a
      `nodes.goldfish` attribute, tagging the returned `specification.goldfish` as a
      behavior process; validate that the attribute's recorded change events are all
      ±1 increments, aborting with a cli error naming any offending event otherwise.
- [ ] 3.2 Implement behavior-choice risk-set construction: for each eligible actor and
      event, the risk set is `{current − 1, current + 1}` clamped to the attribute's
      declared valid range (one-element risk set at a boundary, never zero).
- [ ] 3.3 Wire the behavior process through the existing DyNAM rate/choice
      preprocessing and likelihood path so `estimate_dynam()` can fit a
      single (non-composed) behavior specification standalone.
- [ ] 3.4 Unit tests: behavior specification construction (valid ±1 events; rejected
      non-±1 events); interior vs. boundary risk sets; standalone behavior-only
      estimation on a small synthetic dataset recovers known shape-effect
      coefficients within tolerance. Run `devtools::document()` for new/changed
      exports. Verify with not-cran-test, then close the task.

## 4. Behavior evaluation-function effects

- [ ] 4.1 Add `R/functions_effects_behavior.R` with `shape_linear` and
      `shape_quadratic` (own-state-only effects), following the existing
      `init_DyNAM_choice.*`/`update_DyNAM_choice.*` contract.
- [ ] 4.2 Add `similarity_alters` and `avg_alter` effects that read a named composed
      network layer's current tie state and the alters' current behavior values,
      confirming they update live (not a construction-time snapshot).
- [ ] 4.3 Register the new effects in the formula-parsing/effect-dispatch tables so
      they are usable in a behavior specification's `rate`/`choice` formulas; add
      roxygen documentation entries alongside the existing DyNAM effect docs and run
      `devtools::document()`.
- [ ] 4.4 Unit tests per effect: statistic values on a small hand-computable fixture
      (own-state effects independent of alters; network-dependent effects tracking a
      tie change between behavior-change opportunities). Verify with not-cran-test,
      then close the task.

## 5. End-to-end network + behavior coevolution

- [ ] 5.1 Integration test: compose an event-observed network specification and an
      event-observed behavior specification via `make_joint_specification()`, with
      the network choice formula reading the behavior layer (selection) and the
      behavior choice formula reading the network layer (influence); confirm the
      specification print marks both fids separable.
- [ ] 5.2 Confirm `estimate_dynam()` fits each fid's parameters correctly from the
      composed specification's per-fid preprocessed output (no new estimator code
      path required per the separability rule).
- [ ] 5.3 Recovery check: simulate or hand-construct a small dataset with a known
      selection effect and a known influence effect, and confirm both processes'
      estimated coefficients recover the known signs/magnitudes within tolerance.
- [ ] 5.4 Verify with not-cran-test; confirm the frozen coefficient baselines are
      unaffected (PASS, not SKIP), then close the task.

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
