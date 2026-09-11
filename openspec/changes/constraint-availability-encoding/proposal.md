---
depends-on: support-mask-sparse-updates
---

## Why

**Two things the folded availability object owes the user, both found by a
post-landing conformance review.**

**An ego-kind constraint on DyNAM choice still stores a dense grid.**
`active_dyad_encoding_decide()` returns `"outer"` for that case — a
two-factor-vector encoding, cell `(i, j) = active_sender[i] & active_dyad[j]` —
and `fold_active_dyad_support()` branches only on `"alter"`. Everything else
falls into `build_active_dyad_point()`, which hardcodes the encoding back to
`"point"`. So the `"outer"` return is unreachable on this path, and the comment
above the function describes an encoding the function never produces.

| ego-kind choice availability, 1899 actors | size |
| --- | ---: |
| dense point buffer, as stored today | ~14 MB |
| two factor vectors | ~15 KB |

The living spec is direct about this: a sender-axis-only constraint allocates no
dyad-shaped object for either sub-model. The rate side honours it, because its
gate IS the mask. The choice side does not.

**A choice-family constraint never warns about senders it gates out.** The
current inventory of constraint messages is asymmetric:

| when | family | severity |
| --- | --- | --- |
| empty risk set at a dependent event | choice | error |
| observed dyad excluded | choice | error |
| observed sender gated out | rate | error |
| present receivers never an allowed candidate | choice | warning |
| present senders never at risk, always gated out | rate | warning |

`validate_support_constraint()`'s choice branch accumulates `ever_candidate`
over receivers and warns on the ones never allowed. It accumulates nothing over
senders. So an ego-kind constraint that gates a sender out of every event is
silent unless that sender is observed, in which case it errors late with "empty
risk set" naming one event rather than the pattern.

The two belong together: both are about what the folded availability object
means and what the user is told about it, and both live in the same two
functions.

## What Changes

- **The outer fold is implemented for an ego-kind mask on a choice model**, so
  the encoding the decision function already returns is the encoding the fold
  produces. The estimation side already consumes an outer encoding, because the
  unconstrained REM path produces one.
- **The stale comment goes with it.** It currently describes the two-factor
  encoding as though the function produced it. Either the code matches the
  comment or the comment matches the code; this change picks the first.
- **A choice-family constraint warns about senders allowed no receiver at any
  event**, worded symmetrically to the rate warning that already exists. A
  warning and not an error, because a sender that never appears is legitimate.

## Capabilities

### New Capabilities
<!-- none -->

### Modified Capabilities
- `support-constraint`: the preprocessing-time set-size validation gains its
  sender-side counterpart for the choice family. The requirement lists the
  conditions that must be reported and this one is absent from it, which is why
  the asymmetry was never a test failure.

## Impact

- **Depends on** `support-mask-sparse-updates` for the folded availability
  object this reads and for the post-delta wording.
- **Code**: `R/model_preprocess.R` (`fold_active_dyad_support()`,
  `build_active_dyad_point()`), `R/model_estimate.R`
  (`validate_support_constraint()`).
- **An existing test pins the defect.** `test-support_constraint_ego_fold.R:65`
  asserts `active_dyad_encoding == "point"` for exactly this model. That
  expectation moves with the code, and the captured reference fit in
  `_fixtures/ego_outer_standalone_ref.rds` is what proves the move changed no
  number. A test that has to change is not a reason not to change the code; it
  is a reason to be able to show the numbers held.
- **`r-lib:cli` governs the new warning**: semantic elements, data interpolated
  rather than literal markup, and a pinned reproducible cli context for the
  snapshot.
- **Baselines**: no baseline model carries a constraint, so they are a floor
  here rather than a detector. The reference fit is the detector.
