---
depends-on: preprocess-one-walk
---

# observation-tail-right-censoring

## Why

**A right-censored row past the last real event biases the baseline rate, and
the preprocessing substrate can emit one for a reason the model never asked
for.** The observation-window capability already draws the line: an exact-time
rate or REM fit counts exposure to an explicit `end_time`, a multinomial fit
stores no trailing row, and a boundary row carries no borrowed identity. The
line is correct. What is not audited is whether every path holds it once
*window effects* and *time-varying composition* put rows on the schedule at
times past the last modeled event.

Two mechanisms put such rows there:

- **Window effects.** A windowed term schedules a dissolve (expiry) pseudo-event
  one window length after the event it expires, so the schedule's last row lies
  past the last real event. On the merged walk this was walked as an ordinary
  covariate event and handed every timed rate fid a right-censored row at a time
  no observation reached — an exact-time rate fit on a dataset ending at 6 stored
  events out to 8, with `total_time` 7 against the recipe loop's 5. This was found
  and fixed for the merged walk's windowed rate path during `preprocess-one-walk`
  (tasks 1.6-1.9), by resolving the observation extent over the non-window streams
  and closing the walk there. That fix was one substrate and one family. Whether
  the same defect exists on the recipe loops, the walk handle, REM, a
  coordination sub-model, or a constrained model has not been checked.

- **Time-varying composition (`active_1` / `active_2`).** An actor that joins or
  leaves is a real event on a composition stream, and composition streams count
  toward the resolved end. A composition change after the last dependent event
  therefore extends the window and writes a right-censored boundary — which is
  correct exposure counting only if the process is genuinely still observed
  there, and a spurious tail otherwise. `constraint-availability-encoding`'s
  close recorded two live scope-outs in this exact area: the sender axis
  (`active_1`) is held frozen, so a sender that joins mid-sequence is neither
  counted as ever-at-risk nor reported, and the REM choice-branch validation
  still reads a frozen receiver axis. A frozen presence axis and a mis-timed
  observation tail are the same class of defect read from two directions.

**Support constraints compound both.** A constraint atom can itself be windowed,
so it contributes its own expiry rows; the effective mask conjoins `active_1`,
the support, and `active_2` per event, so a presence axis that is wrong at the
tail makes the risk set wrong exactly where a spurious right-censored row would
be written. The risk-set-size validations (`observed dyad excluded`, `empty risk
set`, `never a candidate`) are defined over *present and allowed* candidates, so
they read the same tail.

This change does not assume a bug beyond the one already fixed. It is a
**review**: establish the invariant precisely, build the detector matrix that
the single-path scenarios do not cover, and find where — if anywhere — a
substrate, family, constraint, or composition combination induces a
right-censored row past the last real event without an explicit `end_time`. Where
the current behavior is correct it is pinned; where it is not it is fixed under
the baseline-diff-with-derivation discipline, because a censoring change moves a
likelihood.

## What Changes

- **The tail invariant is stated for every substrate and family, not one.** The
  observation window, when no explicit `end_time` is given, closes at the last
  **real** event — a modeled dependent event or a genuinely exogenous covariate
  or composition change — and never at a **window-derived** dissolve/expiry row.
  Rows past that bound update state but emit no right-censored observation. This
  holds on the recipe loops, the merged walk, and the walk handle, for
  DyNAM-rate, DyNAM-rate-ordered, REM, REM-ordered, DyNAM-choice and
  choice_coordination, with and without a support constraint, with and without
  composition changes.
- **The detector matrix is built and run against the current tree first**, so
  every claim of conformance is a red-then-green, not an assertion. The existing
  observation-window scenarios cover one path each; this change covers the
  product.
- **The two `constraint-availability-encoding` scope-outs are re-examined here
  as tail defects**: the frozen `active_1` sender axis and the frozen REM
  receiver axis, insofar as either lets a composition change at the tail be
  read wrong. Whether each is fixed here or handed to a dedicated composition
  change is decided by what the detectors show, not in advance.
- **The modeling question is settled (Alvaro, 2026-09-13, ADR-0069): a real
  exogenous event after the last dependent event DOES extend the exact-time
  exposure window.** This is the living spec's current behavior (the end defaults
  to the span of the non-window streams), so the decision confirms it and moves no
  baseline. The rejected alternative (close at the last dependent event) is in
  ADR-0069. The change's remaining work is therefore purely the D1 invariant —
  window-DERIVED rows never extend the tail — audited across every substrate and
  family.

**This change may find nothing to fix beyond pinning.** Design D-stop names the
conditions under which the review concludes "the invariant already holds" and
the deliverable is the detector matrix plus tightened spec scenarios, with no
baseline moved.

## Capabilities

### Modified Capabilities

- `observation-window`: the requirement "The observation window is closed when
  the schedule ends first" is tightened to distinguish a **real** last event
  from a **window-derived** row, and to hold across all three substrates and
  every family rather than the exact-time families on one path. A new scenario
  set covers window-effect expiry rows and composition-change rows at the tail.
- `support-constraint`: the risk-set validation and the effective-mask
  conjunction are stated to read node presence at the tail the same way they read
  it mid-sequence, so a constraint never induces or hides a tail row through a
  frozen presence axis. (Written against `constraint-availability-encoding`'s
  post-delta wording, which already moved the choice branch to live presence;
  this extends it to the observation tail and to the sender axis.)

### New Capabilities
<!-- none — this is an audit that tightens existing capabilities -->

## Impact

- **Depends on** `preprocess-one-walk` (the merged walk's extent logic
  `resolve_walk_window()` / `resolve_walk_extent()` and `window_derived`, which
  tasks 1.6-1.9 introduced and which this change audits and generalizes) and on
  `constraint-availability-encoding` (the live-presence choice fold, whose two
  frozen-axis scope-outs this change re-examines).
- **Code, if the review finds a defect**: `R/preprocess_joint.R` (the merged
  walk and handle extent), `R/model_preprocess.R` (the recipe loops' clip branch
  and the folds), `R/preprocess_builders.R` / the schedule builder (which rows
  count toward the resolved end), and the composition-stream plumbing
  (`active_sender_changes` / `active_dyad_changes`).
- **Baselines are the floor and possibly the finding.** Every step runs
  `NOT_CRAN=true` with the frozen 1e-6 coefficient and C++ golden baselines PASS
  not SKIP. None of them carries a window or a constraint, so for those cases the
  baselines are a floor and the detector matrix is the real check; but a change
  to what a *real exogenous tail* does could move an exact-time rate baseline, in
  which case ADR-0021 governs — the value is derived first, the baseline
  re-frozen beside the derivation, never the other way.
- **Decision record**: relates to ADR-0057 (measure/audit before concluding —
  the discipline this whole change is), ADR-0004 (exact time through the
  evaluator, which is what a censoring row's interval feeds), and the
  observation-window requirement itself. The tail definition the review settles
  warrants its own ADR; its id is claimed in the vault ledger when D2 below is
  answered, not at proposal time.
