## 1. Risk-set descriptor on the model spec (design D1)

- [ ] 1.1 Attach the risk-set descriptor to the spec constructors
      (`R/model_spec.R`: `model_spec_structure()` + the nine `*_spec`
      constructors, incl. `dynami_*`) as ONE `risk_set` named-list field
      (2026-07-19 decision: `list(axis =, fold_target =, encoding =,
      symmetrize =, ...capability entries)`) with small internal accessors;
      roxygen (`@noRd`) documents the field as the single parse-time
      decision point
- [ ] 1.2 Route the existing decision sites through the descriptor:
      `active_dyad_encoding_decide()` (`preprocess_writers.R:552`) and the
      fold-family selection in `fold_active_dyad_support()`
      (`model_preprocess.R:1152`) read descriptor fields instead of
      model-type strings; recipe fold declarations align to the descriptor
      (read, not re-declared)
- [ ] 1.3 Tests: descriptor values per spec class (all nine); preprocessing
      output byte-identical for a fixture grid (encoding decisions
      unchanged); frozen baselines PASS not SKIP

## 2. Complete the outer fold + retire the standalone mask path (design D2)

- [ ] 2.1 Capture the current standalone-mask estimation path for an ego-kind
      (outer) choice constraint as a test-side reference (per-event logL /
      probabilities on a small fixture) BEFORE changing the fold
- [ ] 2.2 Implement the outer fold in `fold_active_dyad_support()`: ego-kind
      choice constraint → dense point `active_dyad` row flips (receiver
      presence ∩ sender-gated support), `active_dyad_folded` TRUE; the
      fail-fast validation (observed-event-excluded, empty risk set) behaves
      identically
- [ ] 2.3 Equivalence tests: outer-constraint fixture — folded path ==
      captured standalone-path reference at 1e-10 per event; == across
      engines where wired (default / gather_compute / default_c) at 1e-8;
      identity-mask == unconstrained
- [ ] 2.4 Delete the standalone estimation-time mask machinery:
      `mask_to_opportunities()` (`model_estimate.R:569`),
      `support_gather`/`supportMask` (`model_estimate.R:1573,1694,1764`;
      `cpp_interface.R:48,61-72`); the R gather's `support =` parameter if
      now unread; remove the captured reference after the tests pass

## 3. Dead channels out, capability map in, block collapsed (design D3/D4)

- [ ] 3.1 Confirm-and-delete `remMask`: add the test that a constrained
      REM/coordination model always arrives folded
      (`prep$active_dyad_folded`), then remove the fallback
      (`model_estimate.R:1720-1726`) and its threading
      (`estimation_core.R:1383-1393` legacy branch, `cpp_interface.R:47,63`)
- [ ] 3.2 Delete `senderGate` everywhere: origin (`model_estimate.R:1571,
      1762`), `estimate_c_int` (`cpp_interface.R:46,63`),
      `compute_iteration_step` + context threading
      (`estimation_core.R:67,268,1258-1273,1439,1504`); the folded
      `active_sender` is the only sender filter
- [ ] 3.3 Implement the engine-capability map (spec/descriptor × engine →
      native | abort) next to the descriptor; generate the guard abort
      message from it; replace the hand-built `native_compiled` boolean and
      the family-enumerating `cli_abort` (`model_estimate.R:1591-1605,
      1674-1685`) and `estimate_c_int`'s string stops
      (`cpp_interface.R:51-72`) with map lookups
- [ ] 3.4 Collapse `model_estimate.R:1550-1733` to validate-then-dispatch:
      family/validation reads from the descriptor, no fold-state branches,
      no side-channel assembly into `argsEstimation`
      (`opportunitiesList` handling for the constraint-free user list stays)
- [ ] 3.5 Tests: guard behavior unchanged — abort/inform messages
      snapshot-tested (pinned cli context, deliberate snapshot update where
      text is now map-generated); every `test-support_constraint_*.R` suite
      passes unchanged; baselines PASS not SKIP

## 4. Legacy vocabulary retirement (design D5)

- [ ] 4.1 Rename `riskMask` → `active_dyad` vocabulary in the contribution
      signatures and threading (`estimation_core.R:687-1702`); pure rename,
      results byte-identical; roxygen updated; `devtools::document()`
- [ ] 4.2 Replace dimension-sniffed rate detection with descriptor reads:
      `is_rate <- length(dim(statsList$initialStats)) == 2L`
      (`estimation_core.R:1443`) and
      `is_rate_model <- modelTypeCall %in% ...` (`cpp_interface.R:77`)
- [ ] 4.3 Dispatch `estimate_c_int` and `gather_` on the spec/descriptor:
      S3 stage-boundary dispatch on the spec class (2026-07-19 decision; the
      descriptor is data read inside the method) replaces every
      `modelTypeCall` string branch (`cpp_interface.R:61,77,199,242,246,349,
      391,591-731`); the C++ function selection and argument shaping key on
      the spec; no compatibility shim — nothing user-facing surfaces the
      values (confirmed 2026-07-19; 4.4's grep-clean check re-verifies)
- [ ] 4.4 Delete `legacy_model_type()` (`model_spec.R:311`) and convert its
      remaining callers (`model_estimate.R:1750`,
      `model_preprocess.R:2006`, `preprocess_writers.R:396,552`); grep-clean
      check per the widened spec requirement
- [ ] 4.5 Tests: full `NOT_CRAN=true devtools::test()` green — coefficient
      baselines + C++ golden PASS not SKIP (byte-identical expectation for
      the pure renames); DyNAMi suites green (monolith path untouched)

## 5. Cleanup + milestone

- [ ] 5.1 Sweep comments referencing the removed channels/paths
      (`senderGate`, `remMask`, standalone mask, downgrade notes) so the
      remaining comments describe the descriptor flow;
      `lintr::lint_package()` clean on touched files
- [ ] 5.2 Milestone: bump `DESCRIPTION` + `NEWS.md` (internal: spec-driven
      risk-set dispatch, ego-kind constraint folds like all kinds, legacy
      model-type vocabulary removed; no user-facing behavior change); spawn
      the spec-conformance agent to cross-check the deltas against the
      implementation
