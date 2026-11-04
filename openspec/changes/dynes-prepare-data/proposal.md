depends-on: dynes-augmentation

## Why

Turning survey data into what `estimate_dynes()` reads takes hand-written
preparation, and a mistake in it gives wrong estimates without any error. For the
`social_evolution` vignette (2026-10-29), about 40 lines of data code diffed the
two friendship surveys into a change-list with explicit 0 rows. They also left the
276 wave-1 ties stamped at the first survey's time. `estimate_dynes()` sets
`start_time` to the first wave, and the shared walk scored every timed row at or
after `start_time`. So both cluster fits (21 core-hours) scored those 276 ties as
creation events at t0. The creation rate intercept came out as
log(346 / (51 · T)) instead of log(70 / (51 · T)), and nothing warned. That defect
is fixed in `dynes-augmentation` (6.2i/6.2j, `55d682f5`): a modeled panel layer's
rows at or before the observation start are history, applied at their own times
and never scored.

The preparation burden remains. A survey reports the non-zero ties at each wave.
goldfish stores a panel layer, modeled or covariate, as a list of updates that the
walk applies in time order, and the walk never infers "absent at this wave = 0".
Raw survey rows therefore give the right state only when no tie is ever dropped:
a dropped tie stays in the network after the wave that dropped it. A user should
not have to write the wave diff, with its explicit dissolutions, to get the
network state the surveys report.

Surveys also report counts in two ways: the total so far ("how many calls in
all") and the amount since the last survey ("how many calls since we last
asked"). Each gives the right walk state only under one of the two update
semantics, and the wrong one fails silently: totals read as increments sum to
6 + 9 = 15, and amounts read as replacements set the level to the last
wave's amount. The binary case also needs a second step, `add_flavor()`, and
skipping it is silent too: an unflavored binary panel with no dissolution is
estimated as a count panel (carried into `dynes-augmenter-driver-v2`). The
user knows which case each layer is. One declaration of it is enough to write
the rows correctly and to record the flavor map the estimator reads.

## What Changes

- New exported, experimental function `prepare_dynes_data(x, layers, form =
  NULL, flavor_names = NULL, wave_times = NULL)` that rewrites the named panel layers of a stocnet
  object into the change-list form the walk reads, and flavors the binary
  ones.
- **Two declared facts per layer.** `layers` names each layer's *style*, the
  process it records: `"mutually_exclusive"` (a binary tie that is created and
  dissolved), `"count"` (one monotone process of repeated events), or `"none"`
  (a valued layer with no process, typically a covariate). `form` names what a
  survey value means: `"states"` (the level at the wave: the tie, or the total
  so far; the default), `"per_wave"` (the amount since the previous wave), or
  `"changes"` (already a change-list in the layer's update semantics).
  `c(friendship = "mutually_exclusive", calls = "count")` with
  `form = c(calls = "per_wave")`. `form` defaults to `"states"` (user
  decision 2026-11-04). Any panel-observed layer can be named,
  modeled or covariate. Both facts are declared, never detected:
  survey-recorded interactions repeated at each wave are a legitimate shape
  that a heuristic would misread.
- **Rewrite by form.** `"states"` is diffed wave against wave, `"per_wave"` is
  summed cumulatively, and `"changes"` is passed through. The rows are written
  in the layer's own `update` semantics, one row per changed dyad per wave.
- **Flavoring by style.** A `"mutually_exclusive"` layer gets the creation /
  dissolution flavor map that `add_flavor()` would record, so the user does
  not pipe into `add_flavor()` and cannot skip it. The flavors are named
  `creation` / `dissolution` unless `flavor_names` relabels them
  (`list(friendship = c(creation = "formed", dissolution = "ended"))`). The
  names are labels only: the support masks and the estimator key on the
  mapped values. A `flavor_names` entry for a layer that is not
  `"mutually_exclusive"` aborts. For a count layer, the error says that a
  count needs no flavor, and that two event types counted together
  (signings and endings) should be split into two count layers. A `"count"`
  or `"none"` layer gets no map: the estimator reads an unflavored panel layer as a count
  panel and expands a count rising by Δ into Δ unit events itself
  (`diff_panel_waves()`), so a map would only make it read the layer as a
  binary state and abort.
- **Every wave keeps its time.** Each wave of a `"states"` layer is diffed
  against the state before it (each wave of a `"per_wave"` layer added to it)
  and stamped at its own time. The first wave is
  diffed against the input's `time = NA` rows, or against an empty network.
  Nothing is folded into `time = NA`. The observation start stays in
  `set_preprocessing(start_time = )`, and the walk (6.2j for modeled panel layers,
  the burn-in rule otherwise) applies rows at or before it as history. A fit
  started at a later wave therefore enters with that wave's survey as its state,
  with the earlier changes applied in order.
- **Wave grids per layer.** `wave_times` is one vector for all `"states"` and
  `"per_wave"` layers or a named list per layer. A layer without an entry takes
  its waves from its own row times. A declared wave with no rows is a full
  dissolution on a `"states"` layer and a wave without events on a
  `"per_wave"` layer.
- **Input checks** on `"states"` and `"per_wave"` layers: a dyad repeated at
  one wave aborts, a 0 row is read as absent (no tie, or no events), an `NA`
  tie value aborts, a self-tie on a one-mode layer aborts, and a timed row on
  no wave of the layer's grid aborts. An undirected layer is diffed on
  canonical dyad keys. A tie reported in both orders at one wave warns if the
  values agree and aborts if they differ. The style adds its own checks: a
  `"mutually_exclusive"` layer holds 0/1 levels (or its update encoding, as a
  change-list), and a `"count"` layer holds whole non-negative values whose
  total never falls. The state before the first wave is checked against the
  style too, and a count's wave-1 total may not fall below it; this check
  writes nothing and does not touch the estimation. On an undirected layer, a
  count reported by both respondents with one value counts once, with a
  warning, as a binary tie does. A style/form pair that has no meaning
  (`"mutually_exclusive"` with `"per_wave"`) aborts. A `wave_times` entry for
  a layer not declared, or declared `"changes"`, aborts; a grid is sorted, and
  a duplicated time warns (`goldfish_duplicate_wave_time`) and is kept once.
- **Change-lists are checked too, and arguments strictly.** A
  `"mutually_exclusive"` `"changes"` layer whose event contradicts the style
  aborts (it is not passed through with `add_flavor()`'s warning), and a
  `"count"` `"changes"` layer is checked in its update semantics (whole,
  never falling) and recorded as prepared. Duplicated names, an empty
  `layers`, an unnamed or non-finite grid, an unused grid vector, and a layer
  with no wave abort. Values are compared exactly, with no tolerance.
- **Summary on success.** One `cli` message per call, of class
  `goldfish_prepare_summary`, with totals per layer: events per flavor for a
  binary layer, unit events and changed dyads for a count, changed cells for
  `"none"`, and the first-wave rows on their own line as the starting state.
  It is silenced by class; there is no `quiet` argument.
- **Empty-wave warning.** A wave that writes no rows would drop out of the grid
  `estimate_dynes()` infers from row times and silently merge two intervals.
  The function warns (`goldfish_empty_wave`) and says to pass `wave_times`.
- **Prepared layers are recorded** on the returned object
  (`info$dynes_prepared`, the declared style per layer), and preparing a layer
  a second time aborts. A layer whose flavor map is already recorded aborts
  too: the function owns the flavoring now. When such a layer is declared
  `"count"`, the error adds that a count needs no flavor, and suggests
  splitting event types into separate count layers.
- **Out of scope.** Respondent selection and non-response (user decision
  2026-10-30). Form-mismatch warnings, in this function or in
  `estimate_dynes()` (user decision 2026-11-02). Any change to
  `estimate_dynes()` (user decision 2026-11-04): everything here is written in
  the form the estimator already reads.
- **Deferred to v2.** Each needs the estimator to read more than the wave
  level, so none can be prepared for today's estimator:
  - Ordinal flavored transitions (for example 0 → 1 → 2): the estimator reads
    a flavored panel as a 0/1 state.
  - `"redundant"` panels, two flavors counted gross (6 signings and 2 endings
    in one interval): the estimator reads the net level of 4, so the gross
    counts would be lost.
  - Several flavors in one count layer, and states that change because of
    counts (a treaty in force while signings exceed endings). Until then,
    each event type is its own count layer.

  Also deferred: passing an already-flavored layer through with a warning
  instead of aborting, revisited once the `form` argument has settled (user
  decision 2026-11-04).

  Also deferred: per-layer wave grids feeding per-layer augmentation interval
  boundaries, and the prepared grid recorded on the object for
  `estimate_dynes()` to read, replacing the empty-wave warning. An actor who
  leaves the node set has their ties written as dissolutions in v1 (user
  decision 2026-11-04); whether v2 leaves them to the composition change
  instead is to discuss.

## Capabilities

### New Capabilities
- `dynes-data-preparation`: turning panel layers given as wave states, wave
  amounts or change-lists into the change-list form the walk reads, flavored
  as the estimator reads it. Covers the declared style and form per layer,
  wave grids per layer, the key-based diff with explicit dissolutions, the
  cumulative sum of wave amounts, the flavor map of a binary layer, the input
  checks, the prepared-layers record, and the guarantee that the walk's state
  at every wave equals the survey.

### Modified Capabilities
- None. The time contract (`single-data-object`) and the panel change-list
  contract are used as they are. History at the observation start is settled in
  `dynes-augmentation`'s `sequence-augmentation` delta (6.2j). If an end-to-end
  test of this change exposes an estimator defect, the fix goes to
  `dynes-augmentation`, not here.

## Impact

- New R file and export, man page, NAMESPACE, and a `NEWS.d/` fragment (feature
  branch: no Version bump).
- **No walk or per-event code.** The function runs once on the input data, before
  any preprocessing. It reads each wave once and diffs on dyad keys. It adds no
  preprocessed output and no walk consumer.
- The `dynes-panel` vignette (dynes-augmentation 6.3) can call it in place of its
  hand-written diff and its `add_flavor()` call.
- Reuses `add_flavor()`'s validation and recording (`validate_flavor_mapping()`,
  `warn_style_contradicted()`, the `info$values_equivalence` /
  `info$flavor_style` fields), so the map it writes is the one `add_flavor()`
  writes. `add_flavor()` itself is unchanged and stays the verb for event
  layers.
- `estimate_dynam()` and `estimate_dynes()` are unchanged.
- `dynes-augmenter-driver-v2`'s carried item "an unflavored binary panel is read
  as a count panel without error" is closed: objects built by hand outside
  this function, and binary layers declared with the wrong style, are the
  user's responsibility (user decision 2026-11-04). No estimator guard.
