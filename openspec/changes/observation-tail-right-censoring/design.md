# Design — observation-tail-right-censoring

## Context

The observation-window capability records the correct behavior for a tail: an
exact-time rate or REM likelihood has a compensator, so exposure between the last
event and an explicit `end_time` is a real contribution and must be counted; a
multinomial likelihood has none, so it stores no trailing row; and a boundary row
carries no borrowed sender or receiver. The resolved `end_time`, when the user
gives none, defaults to the span of the **non-window** event streams.

Two things put rows on the schedule after the last modeled event, and neither is
covered by the capability's single-path scenarios:

1. A **window effect** schedules a dissolve pseudo-event one window length after
   each event it expires. The merged walk read the observation extent off the
   whole schedule, including those rows, and so extended the window past the last
   real event and wrote right-censored rows there. `preprocess-one-walk` tasks
   1.6-1.9 fixed this for the merged walk's windowed rate path by reading the
   extent over non-window streams (`resolve_walk_extent()`) and tagging
   window-derived rows (`window_derived`). The recipe loops, the walk handle,
   REM, coordination, and the constrained path were not part of that fix.

2. A **composition change** (`active_1` / `active_2`) is a real event and counts
   toward the resolved end. A composition change after the last dependent event
   thus extends the window and, for an exact-time family, writes a right-censored
   boundary. `constraint-availability-encoding` left the sender axis frozen and
   the REM receiver axis frozen, so composition at the tail is read inconsistently
   across families.

A support constraint conjoins both presence axes with the support per event, and
the risk-set validations are defined over present-and-allowed candidates, so a
wrong tail presence makes both the mask and the validation wrong exactly at the
tail.

## Goals / Non-Goals

**Goals:**
- State the tail invariant once, precisely, distinguishing a real last event from
  a window-derived row, and hold it across the three substrates and every family.
- Build the detector matrix the single-path scenarios do not cover and run it red
  against the current tree before any fix.
- Re-examine the two frozen-axis scope-outs as tail defects and fix them where the
  detectors show a wrong tail, or hand them to a dedicated composition change with
  the reason recorded.
- Settle whether a real exogenous tail extends the window (D2), and if the answer
  changes today's behavior, move baselines only under ADR-0021.

**Non-Goals:**
- Rewriting composition handling wholesale. A frozen axis is fixed only where it
  produces a wrong observation tail; a full `active_1`-live composition model is a
  separate change if the detectors call for more than the tail.
- Changing the exposure-counting behavior for an **explicit** `end_time`. That is
  correct and pinned; the audit must not break it.
- Touching the C++ estimation kernels. The tail is a preprocessing property; the
  kernels consume the stored rows and intervals unchanged.

## Decisions

### D1 — The invariant is stated over "real" versus "window-derived", not over "dependent" versus "other"

A right-censored row at the tail is correct or spurious by the **source** of the
row that closes the window, not by whether it is a dependent event. A dissolve
pseudo-event from a window effect is never a real observation and never sets the
tail. A genuinely exogenous covariate or composition change is a real event and
(per D2) may. The invariant is therefore phrased: *with no explicit `end_time`,
the resolved window closes at the last non-window-derived event; window-derived
rows past it update state and emit no right-censored observation.*

`window_derived` (on the merged blocks) and the recipe path's `is_window_effect`
mark the derived rows; the extent read must exclude exactly those and no others.

*Rejected:* closing at the last **dependent** event unconditionally. That would
drop legitimate exposure past a real exogenous event on an exact-time family,
which the observation-window capability deliberately counts, and would move
correct baselines.

### D2 — Open, and Alvaro's to settle: does a real exogenous tail extend the window?

Today the resolved end is the span of the non-window streams, so a real covariate
or composition change after the last dependent event extends the exact-time
exposure and writes a right-censored boundary. Two readings:

- **Keep (recommended).** The process was observed until that real event; the
  compensator should integrate to it. This is the current behavior and moves no
  baseline. It also matches the "exposure past the last event is counted"
  scenario, generalized from an explicit `end_time` to a real event.
- **Close at the last dependent event.** The exogenous change carries no
  information about the modeled process's hazard, so integrating past the last
  modeled event adds exposure with no possible event. This is defensible for a
  process whose only observed activity is its dependent events, but it changes the
  likelihood and would move exact-time rate baselines that have a trailing
  exogenous event.

The detectors in group 1 measure the size of the difference on a real fixture
before the question is put, so the decision reads a number. Recorded as an ADR
(id claimed in the vault ledger) when answered, with the rejected side.

### D3 — Detectors are a product matrix, built and run red first

The observation-window scenarios each fix one path. The defect class is
cross-cutting, so the detector is the product:

```
 substrate  { recipe-loop, merged-walk, walk-handle }
 x family   { DyNAM-rate, DyNAM-rate-ordered, REM, REM-ordered,
              DyNAM-choice, choice_coordination }
 x window   { none, a windowed term whose expiry falls past the last event }
 x support  { none, a support_constraint (incl. a windowed atom) }
 x active   { static, a composition change after the last dependent event }
 x end      { resolved (no explicit end_time), explicit end_time past the last }
```

Not every cell is meaningful (a multinomial family stores no trailing row by
design; the walk handle refuses a user constraint today), and the impossible or
already-covered cells are recorded as such rather than faked. Each meaningful
cell asserts, relationally, that the merged/handle output equals the recipe
loop's, and absolutely, that no stored row has an event time after the resolved
end unless an explicit `end_time` licenses it. Every cell that targets the defect
must fail on the current tree before its fix, per the group-0 rule the sibling
changes use.

### D4 — A frozen presence axis is a tail defect only where it changes a stored row

`constraint-availability-encoding` scoped out the frozen `active_1` and the
frozen REM receiver axis. This change fixes them only where a detector shows the
tail row (or the risk set feeding it) is wrong; a frozen axis that changes only a
warning's completeness, not a stored statistic or interval, is handed to a
dedicated composition change with the measurement that says so. The line is: does
the frozen axis move a number the estimator reads, at the tail? If yes, here; if
only a message, not here.

### D-stop — Conditions under which the review concludes "already correct"

The change stops, records the matrix, and tightens the spec scenarios without a
code fix if: every meaningful detector cell already passes once written (the
1.6-1.9 fix and the live-presence fold already cover the field); and D2 is
answered "keep", moving no baseline. A review that cannot conclude "correct" is
as much a result as one that finds a defect; naming the exit keeps the answer
honest.

## Risks / Trade-offs

- **[A tail fix moves a frozen baseline]** → ADR-0021 governs: derive the new
  value first, re-freeze beside the derivation, never refit-to-match. Most
  baselines carry neither a window nor a constraint, so the exposed set is small
  and enumerable before any edit.
- **[The matrix is large and most cells are green]** → that is the permitted
  D-stop outcome; the matrix still becomes the regression floor the single-path
  scenarios were not.
- **[Overlap with a future composition change]** → D4 draws the line at "moves a
  stored number at the tail"; anything short of that is handed over with its
  measurement, not built here.
- **[The walk handle path is exercised by `simulate()`, not estimation]** → its
  tail matters for a simulated sequence's fidelity, not a frozen coefficient, so
  its detector is a parity assertion against the recipe loop rather than a
  baseline.

## Migration Plan

Internal, review-first. One commit per detector group and per fix, tests green at
every commit, `NOT_CRAN=true`, baselines PASS not SKIP. No `NEWS.d/` fragment
unless a user-visible number changes (a moved baseline would be user-visible and
would carry one). No Version or `NEWS.md` edit on the branch (ADR-0040). The
detector matrix and D2's measurement live in `.plan/` and `progress.md`; the
tightened scenarios land in the spec deltas.

## Open Questions

- D2: real exogenous tail — keep or close at the last dependent event? Alvaro,
  after group 1's measurement.
- Does the walk handle need an observation-window bound at all, or is its
  "step everything, write nothing past the last real event" behavior the correct
  one for `simulate()`? (The handle computes no end extent today; the audit says
  whether that is right for the tail or a gap `process-simulation` owns.)
- Are `active_1` and `active_2` the only time-varying presence axes that reach a
  stored tail row, or does a global/nodal covariate stream's tail need the same
  rule? The matrix's `active` axis is composition; a covariate's tail is D1's
  "real exogenous" case and should fall out of D2, but confirm it is not a third
  path.
