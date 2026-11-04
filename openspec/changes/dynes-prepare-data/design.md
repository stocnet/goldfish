## Context

`estimate_dynes()` reads a stocnet object in which a panel layer is a
change-list: an initial state, then rows at each wave for the cells that
changed, with explicit value-0 dissolutions. It never reads those rows one by
one. `diff_panel_waves()` materializes the level at each wave with the walk's
own fold (`panel_wave_snapshot()`) and diffs the levels. A flavored layer is
read as a 0/1 state, with one flip per changed dyad, and an unflavored layer as
a count, with Δ unit events per dyad (`dynes-augmentation` D24). The function
in this change therefore has one job: make the level the walk builds at every
wave equal the survey, and record the map exactly when the layer is a binary
state. The same rows also feed every statistic the walk computes between
waves, so the correction has to be in the data and not inside the diff. Survey data does not arrive in that
form. `social_evolution`'s friendship layer, for instance, holds two snapshots:
every nomination reported at each survey, all valued 1, under
`info$update = "replace"`.

The rules a user has to know today:

- `time = NA` rows are history: they initialize the network and are never scored
  (`single-data-object`, "Strict time contract with NA-as-history";
  `R/materialize_state.R`).
- The walk builds a panel layer's state by applying its rows in time order
  (`materialize_network_state()`). `replace` sets a cell and `increment` adds to
  it. Nothing turns "absent at this wave" into 0, for a modeled layer or a
  covariate (`R/data_source.R`: panel layers "enter as change-list covariates
  updated at their wave times").
- In the shared walk, timed rows before `start_time` are history (the burn-in
  rule shared with `estimate_dynam()`). Since `dynes-augmentation` 6.2j, a
  modeled panel layer's rows at or before `start_time` are history too: applied
  at their own times, in order with the burn-in, never scored.
- `estimate_dynes()` builds its wave grid with `panel_wave_grid()`
  (`R/augmentation_recipe.R`): the waves after `set_preprocessing(start_time)`,
  with the start first, or the first wave when no start is set.
  `compile_augmentation_recipe()` reads each boundary's state inclusive at the
  wave.
- A dropped tie needs an explicit value-0 row. This is documented, not detected.

The vignette's data code (dynes-augmentation 6.3) stamped the wave-1 snapshot at
the first wave, and the walk scored it as 276 creation events. That walk defect
is fixed in 6.2j. What remains is the diff itself: deriving the change-list,
with its dissolutions, from the reported states.

## Goals / Non-Goals

**Goals:**
- One exported call that turns panel layers given as wave states into the
  change-list form the walk reads, for modeled and covariate panel layers alike.
- After preparation, the walk's state of a prepared layer at every wave equals
  the survey at that wave.
- Every row keeps a time. A fit started at any wave enters with that wave's
  survey as its state, with the earlier changes applied in order.
- Counts reported as the total so far and as the amount since the last wave
  are both prepared, under either update semantics.
- A binary layer leaves the function flavored, so the estimator can no longer
  read a prepared binary panel as a count panel (user decision 2026-11-04).

**Non-Goals:**
- Non-response or respondent selection (user decision 2026-10-30).
- Partially observed waves (`NA` tie values). The `dynes-augmentation` abort
  stands.
- Ordinal flavored transitions (0 → 1 → 2) and `"redundant"` panels (two
  flavors counted gross). Both need an estimator that reads more than the
  level at each wave, so they go to v2 (user decision 2026-11-04).
- Detecting the input form, or warning on a mismatch, here or in
  `estimate_dynes()` (user decision 2026-11-02).
- Any change to the walk, `estimate_dynam()`, `estimate_dynes()`, or the frozen
  baselines.

## Decisions

**D1. History is carried by timed rows, never by `time = NA`.** Every wave of a
`"states"` layer is written at its own time, the first as its diff against the
input's initial state. The function does not fold any state into `time = NA`
rows and does not need the observation start. The start is set once, in
`set_preprocessing(start_time = )`, and the walk applies every row at or before
it as history: 6.2j for a modeled panel layer, the burn-in rule for covariates
and event layers. With seven waves and a start at wave 3, the entering state is
the wave-1 snapshot plus the wave-2 and wave-3 changes, applied in order, which
is the wave-3 survey. Event rows keep their times as they are: one before the
start is history, one at the start is scored (user decision 2026-10-30).
*Alternative rejected:* folding the state at `t0` into `time = NA` rows (the
first draft). It moves the changes between earlier waves in front of the
events between those waves, so effects that accumulate during the burn-in see
the wrong state (user decision 2026-10-30, carried here 2026-11-02).

**D2. Withdrawn (2026-11-02).** The first draft resolved `t0` in this function
and recorded it on the object. With D1 no row depends on `t0`, so a second place
to set it would only be a second place for the function and the estimator to
disagree. The motivating defect (the data said "wave 1 is an event", the
estimator said "wave 1 is the start") is fixed by 6.2j.

**D3. The diff is computed on dyad keys, never on dense matrices.** Each
snapshot is a set of non-zero `(from, to)` keys with values. Consecutive
snapshots are compared by key match, which gives the changes, the
dissolutions (keys that vanish) and the stable cells (dropped). The cost is
O(rows) per wave. Building an `n x n` adjacency matrix per wave, as the
vignette did, is not needed and is avoided so that large node sets stay cheap.
This is data preparation, run once, not per-event code, so the walk-cost rules
do not apply. It is still kept linear in the number of rows. A cell has
changed when its value differs under exact `!=`, as `minimal_changes()`
compares, with no tolerance (user decision 2026-11-04). On a `"none"` layer
holding doubles, 0.3 against `0.1 + 0.2` therefore writes a row that sets the
second value: the data is reproduced as given, the extra row is harmless, and
a tolerance would be a heuristic that decides two numbers are the same.

**D4. Values are written in the layer's own update semantics.** Under
`replace` the change row carries the new value (0 for a dissolution). Under
`increment` it carries the difference (−1 for a dissolution of a binary tie).
This makes the returned layer valid under the existing change-list contract
with no new semantics. The recovery fixtures use `increment` with −1
dissolutions, and `social_evolution` uses `replace`, so both forms are in use.
On input, the value of a timed row of a `"states"` layer is the tie's level at
that wave, under either `update`: the layer's `update` decides only how the
output rows are written. The diff reads a row's time, sender, receiver and
value. Any other per-row column of a `"states"` layer's rows, `order` in
particular, is ignored and is not written on the rows the diff produces: a
snapshot holds one row per dyad per wave, so a tie-break among rows sharing a
stamp has nothing to order (decided 2026-11-02, stated in the documentation).

The returned ties table is laid out as follows (user decision 2026-11-04).
Rows of layers not named are untouched, in position and in every column.
Within a prepared layer, its `time = NA` rows are kept as given, and its timed
rows are replaced by the written rows, sorted by (time, from, to) and appended
after the other rows, so the result is deterministic. Written rows carry the
grid time converted back to the ties `time` column's class with
`restore_time_class()`, so a `Date` grid on a `POSIXct` column writes `POSIXct`
rows and the column keeps one class. A `flavor` or value column the input
lacks is created, with `NA` flavor and the default weight 1 on the other
layers' rows.

**D5. The function flavors a `"mutually_exclusive"` layer (reinstated
2026-11-04).** Such a layer gets the map `add_flavor()` would record with
`flavor_style = "mutually_exclusive"`: `c(creation = 1, dissolution = -1)`
under `increment`, `c(creation = 1, dissolution = 0)` under `replace`. The
function goes through `add_flavor()`'s own internals (`validate_flavor_mapping()`,
the `info$values_equivalence` / `info$flavor_style` record, and
`warn_style_contradicted()`), so the two paths cannot write different maps.
A `"count"` or `"none"` layer gets no map (D16).

On a `"mutually_exclusive"` `"changes"` layer, an event that contradicts the
style (a creation on a tie already present, a dissolution on an absent one)
**aborts**, naming the layer, the wave and the dyad, before any row is
written (user decision 2026-11-04). `warn_style_contradicted()` only warns,
and `add_flavor()` keeps that warning, but this function's output is meant to
be estimator-ready, and the estimator would fail later on the same event
("observed dyad is excluded"). On a `"states"` layer the diff cannot produce
such an event, so the check only matters for a change-list given by hand.

The flavor names default to `creation` / `dissolution`. The user can relabel
them with `flavor_names`, a list keyed by layer. Each entry is a character
vector named by the two fixed roles, `c(creation = "formed", dissolution =
"ended")`. The roles tie each label to its update value, so the labels cannot
be swapped by accident. The names are labels only, and renaming does not
change the algorithm (checked 2026-11-04):

- the support masks are chosen by mapped value, the higher one getting
  `~ !tie(layer)` (`derive_flavor_constraint()`);
- the estimator reads a flavor's support kind off that derived constraint,
  not off its name (`flavor_support_kind()`);
- `panel_transition_map()` maps each (from, to) transition to whatever name
  is recorded.

The one literal `creation` / `dissolution` in the code is
`make_specification()`'s fallback for a layer with no recorded map, which a
prepared `"mutually_exclusive"` layer never reaches. The labels are checked
as `add_flavor()` checks them (syntactic R names, since they key the
`make_specification()` formula lists). One check is new: the two labels must
differ, which `validate_flavor_mapping()` does not test. The function aborts,
before any row is rewritten, on any of these:

- a `flavor_names` entry for a layer absent from `layers`;
- an entry for a layer whose style is not `"mutually_exclusive"`;
- roles other than `creation` and `dissolution`;
- a non-syntactic label;
- two equal labels.

When the layer is declared `"count"`, the error says that a count records
repeated events of one process and needs no flavor. It also says that when
the layer counts two event types together (signings and endings, say), they
should be split into two `"count"` layers, one per type. Several flavors in
one count layer are deferred to v2.
*Why the 2026-11-02 withdrawal is reversed:* it objected that per-layer
mapping and style lists would grow the argument surface. The declared style
(D7) is that per-layer fact, and the preparation needs it anyway: it decides
the checks (0/1 levels against whole counts) and whether the layer gets a map
at all. Flavoring in the same call also closes the silent case in which a
user skipped the pipe step and the estimator read a creation-only binary
panel as a count panel. Ordinal transitions (0 → 1 → 2) remain deferred to
v2.

**D6. The return value is the input's class and shape, plus a record of the
prepared layers.** It is a stocnet object with a rewritten ties data frame. No
new class is introduced, and every existing consumer (`make_specification()`,
`estimate_dynes()`, the state-at-time helpers) reads it unchanged. The record
is `info$dynes_prepared`, a named character vector keyed by layer whose values
are the declared styles (`c(friendship = "mutually_exclusive", calls =
"count")`), like the other per-layer `info` fields (`update`, `directed`,
`flavor_style`). The declared form is not recorded (user decision 2026-11-04):
the abort below needs only the names, and the style is kept because it is the
declared fact a later reader of the fit is most likely to need. Preparing a
recorded layer again aborts: diffing a change-list
a second time turns every stable tie missing from a later wave into a
dissolution, and this is the one misuse that can be caught from a declared fact
rather than a heuristic.

**D7. Each layer declares a style and a form (revised 2026-11-04).** Two
facts are independent, and a survey layer needs both:

- **Style** (`layers`, a named character vector, every entry named): the
  process the layer records. `"mutually_exclusive"` is a binary tie that is
  created and dissolved. `"count"` is one monotone process of repeated events.
  `"none"` is a valued layer with no process, typically a covariate.
- **Form** (`form`, a named character vector; a layer without an entry is
  `"states"`, user decision 2026-11-04): what a value at a wave means. `"states"` is the level at the
  wave: the tie, or the total so far. `"per_wave"` is the amount since the
  previous wave. `"changes"` means the rows are already a change-list in the
  layer's `update` semantics.

| style \ form | `states` | `per_wave` | `changes` |
|---|---|---|---|
| `mutually_exclusive` | diff, 0/1 levels, map | abort | map only, contradiction aborts |
| `count` | diff, totals never fall, no map | cumulative sum, no map | pass-through, checked (D10) |
| `none` | diff, any value, no map | cumulative sum, no map | pass-through |

A tie that is present or absent has no "amount since the last wave", so
`"mutually_exclusive"` with `"per_wave"` aborts. `"count"` and `"none"`
produce the same rows; they differ in their checks (D9, D10). A modeled count
panel is declared `"count"` and a valued covariate `"none"`. Any
panel-observed layer can be named, modeled or covariate: a covariate stored as
raw survey rows keeps every dropped tie, for the same reason a modeled one
does, so it needs the same diff. The function aborts on an unnamed entry, a
value outside the vocabularies, a `form` entry naming a layer absent from
`layers`, a layer absent from `x`, or a layer whose `info$observation` is not
`"panel"`. It also aborts on an empty `layers` and on a name repeated within
`layers`, `form` or `flavor_names` (user decision 2026-11-04). The vocabularies leave room for v2's `"ordinal"` and `"redundant"`
styles without a new argument.
*Why two arguments:* the form alone does not decide the rewrite. A level of 6
can be a total (diff it) or meaningless on a binary tie (abort). The style
alone does not either: a count can arrive as totals or as amounts.
*Why an unnamed entry aborts:* under the first draft it meant `"states"`.
A default style would silently leave a binary layer unflavored, which is the
failure this revision closes.
*Alternative rejected:* detecting the form or style. Survey-recorded
interactions, repeated at each wave, look like states and are not (user
decision 2026-11-02).

**D8. Waves are per layer, and an empty declared wave is a full dissolution.**
`wave_times` is one vector for every `"states"` and `"per_wave"` layer, or a
named list per layer. On a `"per_wave"` layer a declared wave with no rows
means no events at that wave, so it writes nothing (revised 2026-11-04); what
follows about full dissolution is about `"states"` layers. A layer without an entry takes its waves from the distinct times of its
own timed rows. Grid times and row times are both converted with
`coerce_time()` before they are compared, as `panel_wave_grid()` does, so a
`Date` grid matches `POSIXct` rows at the same instant. A declared wave with no
rows means every tie of that layer dissolves there: the input is read as the
network state at each survey. A
per-layer list is needed because, with one shared vector, a layer surveyed on a
different schedule would get a full dissolution at every wave it was not
surveyed. A timed row of a `"states"` layer that sits on no wave of its grid
aborts. A grid inferred from row times assumes that no wave of the layer is
empty: a wave at which nobody reported a tie has no rows, so it is not in the
inferred grid and its full dissolution is never written. The function cannot
detect this, because the wave was never declared. The documentation of this
decision and of the `wave_times` argument therefore says to declare
`wave_times` for any layer that can have an empty wave (user decision
2026-11-02).

The function checks `wave_times` before any row is rewritten (user decision
2026-11-04). A list entry naming a layer absent from `layers` aborts, and so
does an entry naming a `"changes"` layer, which has no grid to diff on. So do,
for the same reason, a `wave_times` vector when every declared layer is
`"changes"`, an unnamed list entry, an `NA` or non-finite grid time, and a
`"states"` or `"per_wave"` layer left with no wave at all (no timed rows and
no `wave_times` entry) (user decision 2026-11-04). Each
layer's grid is sorted, silently, since the order of a declared grid carries
no meaning. A time listed more than once is kept once, with one cli warning
of class `goldfish_duplicate_wave_time` per call, naming every layer and time
affected: a duplicate is harmless for the diff but may hide a typo for a
different wave.

**D9. A `"states"` snapshot defines exactly one value per dyad.** A dyad
repeated at one wave aborts (the state is ambiguous). A row valued 0 is read as
absent. An `NA` tie value aborts. These checks cover the timed wave rows only.
The `time = NA` rows are history, and the state before the first wave is built
from them the way the walk builds it, with `materialize_network_state()`:
summed per dyad under `increment`, last row wins under `replace`. Repeated
history rows therefore stay legal, as they are for the walk, and the function
and `panel_wave_snapshot()` cannot disagree on the starting state (user
decision 2026-11-02). On an undirected one-mode layer, (i, j) and (j, i) are
the same dyad (D13): reported both ways with the same value they warn and count
once, and with different values they abort as a repeated dyad. On a one-mode
layer a diagonal row aborts (D14). The same checks apply to the rows of a
`"per_wave"` layer, where a 0 row means no events at that wave. The style
adds checks on the timed values. On a `"mutually_exclusive"` `"states"`
layer each value is 0 or 1. On a `"mutually_exclusive"` `"changes"` layer each
value is in the update encoding, as `add_flavor()` requires. On a `"count"`
layer each value is a whole non-negative number. All of these run before any
row is rewritten.

The state before the first wave is checked too, once it is built (user
decision 2026-11-04). Its rows stay free to repeat, but the level they fold to
must suit the style: on a `"mutually_exclusive"` layer every dyad's starting
level is 0 or 1, and on a `"count"` layer it is a whole non-negative number.
Without this, two `increment` history rows (3, 7) = 1 fold to a level of 2,
the wave-1 diff to 1 writes −1, and the flavor map labels a 2 → 1 move a
dissolution. A violation aborts, naming the layer and the dyad.

**D10. A count arrives as totals or as amounts (revised 2026-11-04).** On a
`"count"` `"states"` layer each wave holds the total so far. The diff writes
the changed totals in the layer's own semantics: the new total under
`replace`, the rise under `increment`. A total that falls between waves is a
data error, and the function aborts on it, naming the layer, the wave and the
dyad. The first wave is compared with the state before it (user decision
2026-11-04): a history level of 6 followed by a wave-1 total of 4 aborts,
naming wave 1. That state acts as a "wave 0" for this check only. It is not
added to the layer's grid, writes no row, cannot raise the D12 warning, and is
not seen by `estimate_dynes()`. The entering state and the augmentation are
exactly as D11 states: the `time = NA` rows are history and are never
augmented. The first draft relied on the estimator's later
`goldfish_decreasing_count_panel` abort. Now that the style is declared, the
error is reported against the data it came from. On a `"count"` `"per_wave"`
layer each wave holds the amount since the previous wave. The function sums
the amounts cumulatively per dyad and writes the result in the layer's own
semantics: the amount under `increment`, the running total under `replace`.
Raw rows need no rewrite in two cases: totals stored under `replace`, and
amounts stored under `increment`. In the other two the raw rows are silently
wrong (totals under `increment` sum to 6 + 9 = 15; amounts under `replace`
set the level to the last amount). The declared form is what lets the
function write all four correctly. A `"none"` layer is rewritten the same way,
without the whole-number or monotone checks.

A `"count"` `"changes"` layer is passed through unchanged but checked in its
own update semantics (user decision 2026-11-04): under `increment` every value
is a whole number ≥ 0; under `replace` every value is a whole number and a
dyad's level never falls, the starting state serving as wave 0 as above. A
violation aborts, naming the layer, the wave and the dyad, so the error is
reported against the input rather than as the estimator's later
`goldfish_decreasing_count_panel`. The layer is recorded in
`info$dynes_prepared` like any other (D6). The invariant "the prepared
state at every wave equals the input snapshot" (the running total, for
`"per_wave"`) is tested on both count forms under both update semantics.

**D11. The first-wave diff is augmented only when the user opens an interval
before it.** Under the default start, `estimate_dynes()` sets `t0` to the first
wave. The rows the first wave's diff writes sit at `t0`, so 6.2j makes them
history, no modeled interval ends there, and the augmentation never sees them
as flips. When the user sets `start_time` before the first wave,
`panel_wave_grid()` opens the interval [`t0`, first wave], the input's
`time = NA` rows are its entering state, and the first wave's diff is that
interval's flip set. This is an explicit opt-in (user decision 2026-11-02),
documented: a start before the first survey makes the first survey an interval
endpoint, not the initial state.

**D12. A wave that writes no rows warns (decided 2026-11-02).** When
`wave_times = NULL`, `estimate_dynes()` takes its grid from the distinct times
of the modeled layers' rows (`panel_wave_grid()`, `layer_event_times()`). Raw
survey rows exist at every wave. A prepared change-list has rows at a wave only
where something changed. A wave with no change therefore drops out of the
inferred grid: two intervals merge, `T` doubles, and nothing aborts. The first
wave is affected too: when the input's `time = NA` rows already equal the wave-1
survey, wave 1 writes nothing and the default start moves to wave 2. The
function knows the grid exactly. When at least one wave of a `"states"` or `"per_wave"`
layer writes zero rows, it raises one cli warning of class `goldfish_empty_wave` per
call, however many layers and waves are affected. The warning lists every
(layer, wave) pair that wrote no rows, and says to pass `wave_times` to
`estimate_dynes()` for any of those layers that is modeled. A single warning
keeps a seven-wave, several-layer call from flooding the console (user decision
2026-11-02). It warns rather
than aborts because the function cannot tell a modeled layer from a covariate,
and a covariate's unchanged wave is harmless: covariates do not feed the grid.
This checks a declared fact, the grid, not the input form, so it does not
contradict D7.
*Alternative deferred to v2:* record the grid per layer on the object and have
`panel_wave_grid()` read it when `wave_times = NULL`. That touches the
estimator, so it belongs in `dynes-augmentation` or a later change.
*Alternative rejected:* documentation only, the "documented, not detected"
pattern this change exists to retire.

**D13. An undirected one-mode layer is diffed on canonical dyad keys.** On an
undirected one-mode layer each row's key is ordered `(min(from, to), max(from,
to))`, the upper triangle that `panel_dyad_mask()` uses. A two-mode layer is
never reordered: its `from` and `to` index different node sides, and
`directed` has no meaning on it (`panel_dyad_mask()` folds one-mode layers
only). Without this, a tie reported as (3, 7)
at wave 1 and as (7, 3) at wave 2 would diff into a dissolution plus a
creation. Under `replace` the materialized state would then depend on row
order, because `materialize_network_state()` dedups on the ordered pair before
mirroring. Rows are written in canonical order. Within one wave, (i, j) and
(j, i) with the same value are a double report: the function warns, naming the
layer, the wave and the dyads, and keeps one row. With different values they
are a repeated dyad and abort (D9). The user's decision was to warn and not
abort on the double report, because reporting both directions of a symmetric
tie is a common survey shape.

The same rule holds on a count layer, under either form (user decision
2026-11-04). (3, 7) = 2 and (7, 3) = 2 at one wave are read as the two
respondents reporting the same 2 calls, not 4: the dyad counts once and the
function warns, as for a binary tie. The warning is raised on both binary and
count layers. Two different values (2 and 3) abort as a repeated dyad.

**D14. A diagonal row of a one-mode `"states"` layer aborts.** The augmentation
never places a flip on the diagonal (`panel_dyad_mask()`). A self-tie in a
survey snapshot is a data error, not a state to carry, so the check aborts with
the other D9 checks, before any row is rewritten. Two-mode layers have no
diagonal and are not checked.

**D15. A layer with a recorded flavor map aborts (revised 2026-11-04).** Any
named layer that already has a recorded flavor map
(`info$values_equivalence[[layer]]`, which `add_flavor()` writes) aborts with
a cli error of class `goldfish_layer_already_flavored`, before any row is
rewritten. The error names every such layer. It says that
`prepare_dynes_data()` now writes the flavor map itself, and that the layer
should be passed in unflavored. The check reads the recorded map, a declared
fact, not the `flavor` column, so it does not contradict D7. When such a
layer is declared `"count"`, the error adds the count guidance of D5: a count
needs no flavor, and two event types counted together should be split into
separate `"count"` layers. The likeliest route here is `add_flavor()` run on
a calls layer.
*Why the 2026-11-02 warning is replaced:* the warning passed a flavored layer
through unchanged, because the user was then expected to flavor after
preparing. With D5 the function flavors. A layer flavored beforehand is
either raw survey rows that `add_flavor()` stamped all as creations, which
must not pass through, or a change-list the user flavored by hand, which
should be declared `"changes"` without the map. Neither is helped by a
silent pass-through, and the fix is the same: drop the map. Overwriting the
map silently was also rejected: it would discard a style the user may have
chosen differently (`"redundant"`). The user kept the abort on 2026-11-04,
because the `form` argument is new. The warning pass-through (the layer
untouched, its arguments ignored) is recorded for v2.

**D16. A count layer is written compactly and carries no flavor map
(2026-11-04).** On a `"count"` (or `"none"`) layer the function writes one row
per dyad per wave whose level changed, valued in the layer's update
semantics. It does not write Δ rows of +1. The estimator never reads a panel
layer's rows one by one: `diff_panel_waves()` materializes the level at each
wave (`panel_wave_snapshot()`, the walk's own fold) and turns a level rising
by Δ into Δ unit events itself. One row of +6 and six rows of +1 give the
same snapshot and the same augmentation, so the expansion adds rows and no
information. It would also be wrong under `replace`, where six rows valued 1
leave the level at 1. A flavor map on such a layer would be harmful, not
merely redundant. The estimator decides a panel layer's kind from whether a
map is recorded (`panel_transition_map()`): a map means a 0/1 state, so a
level of 6 aborts with `goldfish_invalid_wave_state`. "No map" is therefore
the correct prepared form of a count layer, and the function does not call
`add_flavor()` on it. This is `dynes-augmentation` D24's decision, which
rejected a count layer as a one-flavor flavored layer, applied to data
preparation.
*Why `"redundant"` is not a style here:* a `"redundant"` layer counts two
flavors gross (6 signings and 2 endings), and a wave level holds only their
net (4). Only rows carry the gross counts, and the estimator reads levels.
Preparing such a layer would lose the information that sets it apart from a
count, so it waits for a v2 estimator that reads gross counts per flavor
(user decision 2026-11-04).

**D17. A successful call reports what it wrote (2026-11-04).** The function
raises one `cli::cli_inform()` per call, of class `goldfish_prepare_summary`,
with one line of totals per prepared layer (user decision): per-wave counts
would flood the console on a seven-wave, several-layer call, for the reason
D12 keeps one warning per call, and they are easy to compute from the returned
rows. Each layer is counted in its style's terms:

- `"mutually_exclusive"`: the events per flavor, under the recorded labels
  (`70 formed, 70 ended`);
- `"count"`: the unit events (the sum of the rises) and the dyads that
  changed;
- `"none"`: the cells that changed;
- `"changes"`: the same counts, read off the rows passed through.

The rows written at the first wave are reported on a line of their own as the
starting state, on every style. Under the default start they are history, not
events (D11), so counting them as creations would repeat the 346 against 70
confusion that motivated this change. The data in the message is interpolated,
not written as literal markup (r-lib:cli). There is no `quiet` argument: the
message class lets `suppressMessages()` or `withCallingHandlers()` target it,
the tests snapshot it under a pinned cli context, and goldfish keeps verbosity
in `set_estimation(verbose = )` rather than in per-function flags.

## Storage and per-event cost

The function adds no preprocessed output and no per-event code. The D5
contradiction check and the D10 change-list check each read a layer's rows
once, in time order, keyed by dyad. The returned
ties data frame holds at most (rows of the input) + (dissolutions) rows (a
`"per_wave"` layer at most its input rows, D16: one row per dyad per wave): on
`social_evolution` among the 51 actors that is 276 first-wave rows + 140 change
rows + 364 calls. Nothing it writes is read per event, beyond the walk's
existing reading of a ties table.

## Risks / Trade-offs

- [Wrong form declared] A change-list declared as `"states"` is diffed and its
  stable ties missing from later waves read as dissolutions; survey data
  declared as `"changes"` keeps its dropped ties. → Mitigation: the form is
  stated in the documentation and the examples. A second preparation of the same
  layer aborts (D6). No heuristic warning, by decision.
- [A non-respondent reads as mass dissolution] An actor absent from a wave
  loses all their ties at that wave. → Mitigation: stated in the docs and in the
  vignette. Respondent selection is the user's step (non-goal).
- [An unchanged wave merges intervals] A modeled layer's wave with no change
  writes no rows and drops out of `estimate_dynes()`'s inferred grid. →
  Mitigation: the D12 warning. The fix (the grid recorded on the object) is in
  v2.
- [Wrong style declared] A binary layer declared `"count"` or `"none"` gets
  no map, and `estimate_dynes()` reads it as a count panel
  (`panel_transition_map()`). With dissolutions it aborts
  (`check_count_snapshots()`, whose abort names `add_flavor()`). With none, a
  creation-only binary panel is estimated as a count panel without any error.
  → Mitigation: D5 removes the skipped-step route, since a layer declared
  `"mutually_exclusive"` always leaves flavored. A wrong declaration is the
  remaining route, and the documentation states which style a binary modeled
  panel takes. A wrong declaration, and an object built by hand without this
  function, are the user's responsibility (user decision 2026-11-04): the
  estimator gets no guard and keeps reading an unflavored panel layer as a
  count panel. The item carried into `dynes-augmenter-driver-v2` is closed on
  the same decision.
- [Counts declared `"states"` that are really amounts] Amounts diffed as
  totals write a negative row wherever an amount drops. → Mitigation: a
  `"count"` `"states"` layer aborts on a falling total (D10), which catches
  this mistake on any dyad whose amount ever drops. A `"none"` layer has no
  such check, by its definition.
- [Estimator gap] The later-wave-start and first-wave-diff tests (group 3)
  exercise the estimator on prepared data. → If one fails, the defect is fixed
  in `dynes-augmentation`, as 6.2j was, not worked around here.

## Migration Plan

The change is purely additive: a new export, and no existing behavior moves.
Rolling back means reverting the change's commits. The vignette switches its
hand-written diff to the function once the function lands.

## Open Questions

Closed on 2026-11-04 (fourth review, grilling):

12. **A contradicted `"mutually_exclusive"` change-list.** Closed: aborts
    (D5); `add_flavor()` keeps its warning.
13. **Checks on a `"count"` change-list.** Closed: checked in its update
    semantics, rows unchanged, recorded as prepared (D10).
14. **Console output on success.** Closed: one classed summary per call,
    totals per layer, no `quiet` argument (D17).
15. **Argument hygiene.** Closed: duplicated names, an empty `layers`, an
    unnamed grid entry, a non-finite grid time, an unused grid vector, and a
    layer with no wave all abort (D7, D8).
16. **What "changed" means on doubles.** Closed: exact `!=` (D3).
17. **Shape of the returned table.** Closed: untouched rows in place, written
    rows sorted and appended, time class restored, missing columns created
    (D4).

Closed on 2026-11-04 (second review):

7. **The record's field name.** Closed: `info$dynes_prepared`, the declared
   style per layer, without the form (D6).
8. **Composition changes.** Closed for v1: a leaving actor's ties are
   dissolutions (D8). Kept as a v2 discussion item.
9. **Checks on the state before wave 1.** Closed: its level is checked
   against the style (D9), and it is wave 0 for the falling-total check, for
   that check only (D10).
10. **An undirected count reported both ways.** Closed: counted once, with a
    warning, as for a binary tie; different values abort (D13).
11. **Malformed `wave_times`.** Closed: an entry for an undeclared or
    `"changes"` layer aborts; a grid is sorted silently; a duplicate time
    warns and is kept once (D8).

Closed on 2026-11-04:

5. **Is `form` required per layer, or does it default to `"states"`?**
   Closed: default `"states"` (user decision), the case of the motivating
   survey and of D7's first draft.
6. **Can the user rename the `"mutually_exclusive"` flavors?** Closed: yes,
   through `flavor_names` (D5). The rule was to allow renaming only if the
   names do not affect the algorithm, and they do not (user decision).

Closed on 2026-11-02:

1. **Inputs and their order.** Closed by D7 and D8: the user names the layers
   and their form, so no specification is needed and the circularity is gone.
   The name is `prepare_dynes_data()`.
2. **A guard, or a fix, in `estimate_dynes()`.** Closed 2026-10-30: a fix, in
   `dynes-augmentation` 6.2i/6.2j (`67379526`, `55d682f5`). No snapshot-form
   detector either (D7).
3. **Count panels.** Closed by D10: a count panel is a valued state.
4. **Does the panel state at `t0` become `time = NA`?** Closed by D1: no.

## v2

- Ordinal flavored transitions (for example 0 → 1 → 2) in data preparation,
  as an `"ordinal"` style. On an ordinal line a jump of two levels has a
  unique minimal path, so the flavor stays observed. The estimator must first
  accept more than two states (`panel_transition_map()`,
  `check_wave_snapshots()`, flavored multi-step chains, ordinal support
  masks).
- `"redundant"` panels: two flavors counted gross per wave (6 signings, 2
  endings), as a `"redundant"` style with two count columns or two count
  layers. The estimator must read gross counts per flavor rather than the net
  level (D16).
- Several flavors in one count layer, and states that change because of
  counts. One layer would count several event types (signings and endings),
  and a state would follow from the counts (a treaty in force while signings
  exceed endings, or a tie that exists once a count passes a threshold). This
  ties the count kind to the state kind: the flavors' gross counts are the
  events, and the state they imply decides the support (where an ending can
  happen). It needs the same estimator reading as `"redundant"` panels, plus
  a rule mapping counts to a state. Until then the function aborts on a
  flavor for a count layer and suggests one `"count"` layer per event type
  (D5, user decision 2026-11-04).
- Passing an already-flavored layer through with a warning, with its
  arguments ignored and the layer untouched, instead of the D15 abort.
  Revisit once the `form` argument has been in use (user decision
  2026-11-04).
- Per-layer wave grids feeding per-layer augmentation interval boundaries in
  `estimate_dynes()`, which today builds one grid for all modeled panel layers.
- The prepared grid recorded on the object, read by `panel_wave_grid()` when
  `wave_times = NULL`. This would replace the D12 warning with a fix.
- Composition and dissolutions, to discuss. v1 keeps an actor who leaves the
  node set as dissolutions: all their ties are written as dissolutions at the
  wave they are missing from, the same as any dropped tie (D8, user decision
  2026-11-04). Whether v2 should instead leave those ties to the composition
  change (`snap_composition_to_waves()`), in the way non-response is left to
  the user, is to discuss.
