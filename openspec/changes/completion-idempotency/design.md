## Context

`complete_generative_spec()` (`R/complete_generative_spec.R`) runs in four
steps: fill each process's missing families, rebuild the joint specification
and recover `process_map$completed` from the bundles' `completed` marks,
then (timed regime only) `mark_pinned_rates()` and `pin_completed_rates()`,
then warn once per filled gap. The `completed` mark rides the synthesized
bundle, so a second pass finds no gap and warns about nothing. That part is
idempotent already, and `test-complete_generative_spec.R` ("completion is
idempotent on an already-complete spec") pins it, but only with the same
`wave_times` passed twice.

Step 3 is where the second pass goes wrong. `pin_completed_rates()` re-pins
every completed timed rate from the `wave_times` of the current call. Its
comment calls that deterministic re-pinning, which holds only if the grid is
the same. With `wave_times = NULL` it falls back to the pinned layer's single
observed window, so a rate pinned on `c(0, 2, 5)` becomes a one-plateau pin.

What a stored pin (`completed_rates[[fid]]`, a `goldfishCteRate` from
`make_intercept_only_rate()`) remembers about its grid depends on its source:

```
  pin source (pin_completed_rates dispatch)   reads the grid?   $wave_times stored
  ------------------------------------------  ----------------  ------------------------
  relational_window_risk_set()  event_count   no                NULL
  panel_wave_risk_set()  hamming_diff, K > 1  yes               the coerced grid
  panel_wave_risk_set()  hamming_diff, K = 1  yes               NULL (grid lost)
```

`$wave_times` is not bookkeeping: `intercept_only_rate_period()` and
`pinned_rate_compensator()` read it to place events and cut intervals into
periods, and `NULL` means "one period, every time belongs to it".

Callers today: `estimate_dynes()` completes the authored spec once at entry;
`simulate()` completes whatever it is given (a DyNES fit passes its
`authored_spec`); `compute_lik_seq()` (on `feature/dynes`) skips completion
when the `completed` column is present, as a workaround for this bug.

## Goals / Non-Goals

**Goals:**

- A completed spec's pins survive any later completion call that does not ask
  for a different grid.
- A request for a different grid fails loudly instead of silently re-pinning.
- `compute_lik_seq()` calls completion unconditionally.

**Non-Goals:**

- Making every DyNES consumer resolve its grid the way `estimate_dynes()`
  does. "No grid" currently has two meanings. `estimate_dynes(wave_times =
  NULL)` resolves `panel_wave_grid()`, the distinct panel-wave times opened by
  `start_time`. `complete_generative_spec(wave_times = NULL)` uses
  `default_window()`, one period over the layer's event-time range. This
  change only makes a *completed* spec keep its pins. A consumer that
  completes an *authored* spec without a grid still pins on the single window,
  and two consumers do that today:
  - `compute_lik_seq(fit$authored_spec, ...)`: once task 2.1 lands, its
    log-likelihood disagrees with a fit pinned per wave (the 4.3b gap, moved
    from completed specs to authored ones);
  - `simulate.goldfishDynesFit()`, which completes `authored_spec` without
    the fit's grid.

  Both are assigned to `dynes-augmentation` task 6.1, which wires the grid
  into `estimate_dynes()`. There each consumer resolves its grid through
  `panel_wave_grid()` and passes it to completion. The alternative, completion
  owning the grid (a NULL `wave_times` on a spec with a modeled panel layer
  meaning `panel_wave_grid()`), is recorded as deferred to v2 in
  `dynes-augmentation`'s design.
- Re-deriving pins at a new grid from a completed spec. The pin's inputs
  (`count_w`, `T_w`, `|R_w|`) are recomputable from the data, but supporting
  it would make a completed spec mutable. The authored spec is the input for
  that.

## Decisions

### D1 — Fix the pin step, not the whole transform

`pin_completed_rates()` pins only completed timed rates that have no stored
pin. A rate that already has a `completed_rates` entry keeps it, after the
grid check in D3. Steps 1 and 2 run as before: on a completed spec they fill
nothing. Step 4 re-announces the stored pinned rates (D5).

**The stored pins come from the input.** Step 2's `rebuild_completed_joint()`
builds a new list (`specifications`, `process_map`, `data`, `modeled_panel`,
`call`) and so drops `completed_rates` and `pinned_rates`. By step 3 the
object `pin_completed_rates()` receives has no stored pins to keep.
`pin_completed_rates()` therefore gains a `stored` argument, which
`complete_generative_spec()` fills with the input's
`joint_spec$completed_rates` (`list()` or `NULL` on a raw spec). For each
completed rate fid, an entry in `stored` is kept as is, after D3's grid check,
and a fid with no entry is pinned. Stored
entries whose fid is no longer a completed rate are not carried over. Fids are
stable across the rebuild because `build_joint_process_map()` assigns them
from the same specifications in the same order. `pinned_rates` is not carried
over: `mark_pinned_rates()` recomputes it from the bundle marks alone, which
gives the same result, so the whole-object `expect_identical()` holds.

**A new pin on re-entry inherits the spec's grid.** A fid with no stored
entry is pinned on `wave_times` when one is supplied, which has already
passed D3's check against every kept pin. Otherwise it is pinned on the grid
the stored grid-carrying pins share, so one specification never holds pins on
two grids. Only `default_window()` is used when no stored pin carries a grid.
If the stored pins disagree among themselves (possible only for a hand-built
or pre-change object), there is no single spec grid to inherit, so completion
aborts with `goldfish_completion_repin` instead of guessing. On a spec this
change completes, the case cannot arise: a completed timed rate always gets
its entry on first completion, and the clock fill (below) happens only on a
spec with no rate. It guards objects built some other way.

*Rejected:* an early `return(joint_spec)` when `process_map$completed` exists.
It is simpler, but it would break `simulate()`. The `completed` column does
not mean "pinned": step 2 adds it to every specification it completes,
all-FALSE included. A composition with no rate anywhere (choice-only, neither
timed nor ordered) is completed without a rate, so it carries the column and
has no clock. For such a specification `simulate(times = "observed")` aborts
today ("Time-anchored simulation is not available yet"), so
`times = "generated"` is its only working path, and that path needs step 1's
`rate_requested` fill to install the constant clock rate. An early exit
would drop the fill. Keeping simulation working would then need a new
simulation requirement ("a completed rate-less specification cannot generate
times; complete the authored one"), a user-visible regression. Keeping D1
needs only a carve-out in this change's own requirement, and a narrow one:
`rate_requested` requires `!timed`, so the clock fill happens only on a
specification with no rate at all, which has no stored pins to protect.

### D2 — A grid-derived pin records its grid in a new field

`pin_completed_rates()` stores the coerced grid a panel-derived pin was
computed on as `$pin_grid` (numeric, always length K + 1, the default window
included). Relational pins store `$pin_grid = NULL`: they never read the grid,
so any `wave_times` leaves them valid and they are always kept.
`make_intercept_only_rate()` gains a `pin_grid = NULL` argument; `$wave_times`
keeps its current meaning.

A *flavored* relational layer records a grid only because of the stopgap
dispatch: until a flavor-aware relational risk set exists, its pin goes
through `panel_wave_risk_set()`, so it reads the grid and is checked like a
panel pin. Once v2 pins it from its own events, it becomes grid-independent
and is always kept. That shift is recorded in `abmcem` (Open Questions,
deferred v2).

*Rejected:* always storing `$wave_times`, even for one period. It would make a
single-period pin carry boundaries, and `intercept_only_rate_period()` would
then place a time outside the default window (another layer's event after the
pinned layer's last one) out of range instead of in the sole period. That is
a behavior change to every single-period pin, for a bookkeeping need.

### D3 — Grid equality is exact boundary equality after coercion; a mismatch aborts

When `wave_times` is supplied and a stored pin has a grid, coerce the
supplied grid with `coerce_time()`, the same coercion `panel_wave_risk_set()`
applies when it records the grid, and compare exactly
(`identical(as.numeric(supplied), as.numeric(pinned))`). No tolerance is
needed. Both sides come out of the same deterministic coercion, and the grids
themselves are wave times (user-supplied, or `sort(unique())` over the data
in `panel_wave_grid()`), not floating-point results. A tolerance would also
do harm. `coerce_time()` maps a `POSIXct` grid to epoch seconds (about
1.7e9), where `all.equal()`'s default relative tolerance of 1.5e-8 accepts
boundaries about 25 seconds apart as equal.

A mismatch aborts with
`cli::cli_abort(class = "goldfish_completion_repin")`. Every kept pin is
checked before the abort fires, and the message has one bullet per
mismatched pin naming its layer (and flavor, when it has one) and its pinned
grid, then the supplied grid with `{.val}`. With several pinned fids, a bare
"the pinned grid" would not tell the caller which layer is in conflict. Two
`i` bullets follow. The first says to complete the authored (uncompleted)
specification on the new grid, which a DyNES fit keeps as
`fit$authored_spec`. The second says that a DyNES fit's `spec` and a
simulation result's `spec` are already completed. Those are the two objects
a caller is likely to be holding when the abort fires, and neither says
which spec is which.

A pin's grid is read as `pin$pin_grid %||% pin$wave_times`. A K > 1 pin
built before this change has no `$pin_grid` but does carry `$wave_times`
(the same coerced boundaries), so it is checked like a new pin. Only a
legacy single-period pin, whose `$wave_times` is `NULL`, ends up
grid-independent. `wave_times = NULL` means "no grid requested": the stored
pins stand, and it aborts only in D1's guard case, where stored pins
disagree among themselves and a new pin has no single grid to inherit.

*Rejected:* ignoring a different grid. The caller passed it on purpose, and
silently keeping the old pins is the same failure this change removes, just
in the other direction. *Rejected:* re-pinning on an explicit grid (today's
behavior). It is the silent plateau replacement this change exists to stop.

### D4 — Where this lands

The `complete_generative_spec()` fix and its tests touch no DyNES code and can
land on `develop` in their own small merge. The `compute_lik_seq()` cleanup
(task 2.1) applies only where `dynes-augmentation` task 4.3b's code exists. On
`feature/dynes` all tasks land together. If the fix goes to `develop` first,
2.1 runs when `feature/dynes` next merges `develop` in. The workaround is
harmless until then.

### D5 — Re-entry re-announces every stored pinned rate

The living `intercept-only-rate` requirement "Each consumer warns that the
rate is pinned, worded for its source" says the pinned-rate warning SHALL fire
at **each consumer entry** and SHALL NOT be suppressed when the same
specification goes through a second consumer. `pinned-rate-warning-source`
keeps that sentence and adds that a completed rate is announced by the
completion's own warning, raised with both condition classes.
`warn_pinned_rates()` therefore skips completed fids. Today, on re-entry,
`warn_completed_defaults()` gets an empty `added`, so a completed spec at a
second consumer gets **no** warning for its pinned rate:

```
 simulate(fit$spec)          fit$spec is already completed
   complete_generative_spec  added = {}            --> no fill warning
   warn_pinned_rates         skips completed fids  --> no pinned warning
                                                   ==> zero warnings
```

That is a live violation of the living spec, not merely something this change
would have made official. Step 4 now announces every **completed pinned
rate**, whether this call filled it or carried it over as a stored pin. A
carried-over rate's warning:

- reads its count source from the stored pin's `$source`, through the same
  `pinned_rate_source_note()`;
- adds the no-SE / excluded line under the estimation consumers only
  (`"estimate_dynes"`, `"compute_lik_seq"`);
- is raised with both `goldfish_pinned_rate_warning` and
  `goldfish_completed_default_warning`, so the one-warning-per-completed-rate
  rule and both handlers still hold;
- replaces the headline "has no rate sub-model; completing it with ..." with
  one saying the rate was completed earlier and its stored pin is kept. Nothing
  is being completed at this entry, and saying otherwise would hide the fact
  that the pin was **not** recomputed on this call.

A completed default that is not a pinned rate (a uniform choice) has no
pinned-rate obligation and fills no gap on re-entry, so it is not announced
again. Idempotence therefore covers the warnings as well as the value: a
re-entry for a consumer raises one warning per pinned rate, as the first
completion for that consumer did, with only the headline differing.

The re-announcement lives in completion, not in `warn_pinned_rates()`.
Completion is where the carried-over/fresh split is known, and moving it
would reopen `pinned-rate-warning-source`'s decision that a completed rate's
announcement is completion's own.

*Rejected:* keeping re-entry silent and amending `intercept-only-rate` so
that "each consumer entry" means "each entry that completes". That needs a
third MODIFIED block on a requirement `pinned-rate-warning-source` is already
rewriting, the same stacking this proposal avoids for the completion
requirement. It would also weaken a user-facing guarantee: a user simulating
from `fit$spec` would never be told the rate is pinned.

### D6 — `compute_lik_seq()` completes as its own consumer

`complete_generative_spec()`'s `consumer` argument gains `"compute_lik_seq"`.
Today `compute_lik_seq()` completes as `"estimate_dynes"`, so on an authored
spec its completion warning names a function the caller never called. Once
task 2.1 calls completion unconditionally, that warning fires on every
authored spec it receives, so the label is fixed in the same task. Anything
that switches on `consumer` (warning wording, and `warn_pinned_rates()` if it
is called) treats `"compute_lik_seq"` like `"estimate_dynes"`: it pins and
evaluates the same likelihood, so only the name in the message differs.

*Rejected:* leaving the label and flagging it. The fix is one argument
value, and the snapshot that would record the wrong name is written in this
change anyway.

## Risks / Trade-offs

- [A caller relied on re-pinning a completed spec with a new grid] → No
  caller does today: `estimate_dynes()` and `simulate()` normally receive
  authored specs, and the test suite is the check. The abort names the fix.
- [A completed spec built before this change has pins without `$pin_grid`]
  → A K > 1 pin is checked against its `$wave_times` (D3). Only a legacy
  single-period pin is treated as grid-independent and kept, which is no
  worse than today's re-pinning. Two objects keep a completed spec: a DyNES
  fit's `spec`, and a simulation result's `spec` (`new_goldfish_sim(...,
  spec = spec)` in `R/simulate_driver.R`). `estimate_dynes()` is unexported
  until `dynes-augmentation` task 6.1, so no released fit carries an old pin.
  A simulation result pins on the single default window, so an old one holds
  only single-period pins, which are kept.
- [A simulation result's completed `spec` is fed back into estimation] →
  Once task 6.1 passes `estimate_dynes()`'s panel-wave grid to completion,
  `estimate_dynes(sim$spec)` meets single-window pins and a multi-wave grid,
  and aborts with `goldfish_completion_repin`. That is intended: silently
  estimating against a one-plateau pin is the failure this change removes.
  The abort's `i` bullets (D3) say the spec is already completed and point to
  the authored one.
- [Exact equality rejects a grid the caller means to be the same] → Only if
  it coerces to different numbers, e.g. a `Date` grid against a pin recorded
  from `POSIXct`. Those really are different axes (days against seconds), and
  the pin itself would be wrong on the other one, so aborting is correct.
