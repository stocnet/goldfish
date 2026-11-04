## ADDED Requirements

### Requirement: Each layer declares a style and a form

`prepare_dynes_data()` SHALL take a `layers` argument naming the panel layers to
prepare and the style of each, as a named character vector whose values are
`"mutually_exclusive"`, `"count"` or `"none"`. Every entry SHALL be named. It
SHALL take a `form` argument, a named character vector whose values are
`"states"`, `"per_wave"` or `"changes"`. A layer named in `layers` without an
entry in `form` SHALL take the form `"states"`. Any layer whose
`info$observation` is `"panel"` MAY be named, whether a specification later
models it or uses it as a covariate. The function SHALL rewrite each layer as
its style and form determine:

| style \ form | `states` | `per_wave` | `changes` |
|---|---|---|---|
| `mutually_exclusive` | diff, flavor map | abort | flavor map only |
| `count` | diff | cumulative sum | unchanged, checked |
| `none` | diff | cumulative sum | unchanged |

The function SHALL abort, before any row is rewritten, on an empty `layers`,
an unnamed `layers` entry, a name repeated within `layers`, `form` or
`flavor_names`, a style or form outside these vocabularies, a `form` entry naming a
layer absent from `layers`, the pair `"mutually_exclusive"` with `"per_wave"`,
a layer absent from the object, or a layer whose observation is not
`"panel"`. The function SHALL NOT detect the style or the form, or warn about
either.

#### Scenario: a layer without a form entry is read as states
- **WHEN** the function is called with
  `layers = c(friendship = "mutually_exclusive")` and no `form`
- **THEN** friendship is diffed as a `"states"` layer

#### Scenario: a count or none layer given as changes is passed through
- **WHEN** the function is called with `layers = c(advice = "none")` and
  `form = c(advice = "changes")`
- **THEN** advice's rows in the returned object are identical to the input's

#### Scenario: an unnamed entry aborts
- **WHEN** the function is called with `layers = "friendship"`
- **THEN** the function aborts with a cli error saying that every layer needs a
  declared style, and returns nothing

#### Scenario: a repeated layer name aborts
- **WHEN** the function is called with
  `layers = c(friendship = "count", friendship = "none")`
- **THEN** the function aborts with a cli error naming friendship, and returns
  nothing

#### Scenario: a binary layer has no amount per wave
- **WHEN** the function is called with
  `layers = c(friendship = "mutually_exclusive")` and
  `form = c(friendship = "per_wave")`
- **THEN** the function aborts with a cli error naming the layer and the pair,
  and returns nothing

#### Scenario: an event layer cannot be prepared
- **WHEN** `layers` names `calls`, whose observation is `"event"`
- **THEN** the function aborts with a cli error naming the layer, and returns
  nothing

### Requirement: A mutually exclusive layer leaves the function flavored

A `"mutually_exclusive"` layer SHALL be returned with the flavor map
`add_flavor()` records under `flavor_style = "mutually_exclusive"`:
`c(creation = 1, dissolution = -1)` under `info$update = "increment"` and
`c(creation = 1, dissolution = 0)` under `"replace"`. Its timed rows SHALL
carry the `flavor` column and the object SHALL record the map in
`info$values_equivalence` and the style in `info$flavor_style`, exactly as
`add_flavor()` writes them. The function SHALL validate the map as
`add_flavor()` does. On a `"changes"` layer, an event that contradicts the
style, a creation where the tie is present or a dissolution where it is
absent, SHALL abort with a cli error naming the layer, the wave and the dyad,
before any row is rewritten; the function SHALL NOT pass such a layer through
with a warning. A `"count"` or
`"none"` layer SHALL NOT be given a flavor map or a `flavor` value: the
estimator reads an unflavored panel layer as a count panel and expands a level
rising by Δ into Δ unit events itself.

The function SHALL take a `flavor_names` argument, a list keyed by layer.
Each entry SHALL be a character vector named by the roles `creation` and
`dissolution`, whose values relabel the two flavors. A `"mutually_exclusive"`
layer without an entry SHALL use the labels `creation` and `dissolution`. The
labels SHALL key the recorded map and the `flavor` column, and the map's
values SHALL be the same whatever the labels. The function SHALL abort,
before any row is rewritten, on any of these:

- a `flavor_names` entry for a layer absent from `layers`;
- an entry for a layer whose style is not `"mutually_exclusive"`;
- an entry whose names are not exactly the two roles;
- a label that is not a syntactic R name;
- two equal labels.

When the entry's layer is declared `"count"`, the error SHALL state that a
count layer needs no flavor, and SHALL suggest splitting a layer that counts
several event types into one `"count"` layer per type.

#### Scenario: a prepared binary layer equals one prepared and then flavored
- **WHEN** a binary friendship panel is prepared with
  `layers = c(friendship = "mutually_exclusive")`
- **THEN** its rows, `flavor` column, `info$values_equivalence` and
  `info$flavor_style` equal those obtained by diffing the same states and then
  calling `add_flavor()` with the creation / dissolution map and
  `flavor_style = "mutually_exclusive"`

#### Scenario: a binary change-list is flavored without a diff
- **WHEN** a binary layer already in change-list form is prepared with
  `form = c(friendship = "changes")` and style `"mutually_exclusive"`
- **THEN** its rows keep their times and values, and gain the flavor column
  and the recorded map

#### Scenario: a contradicted binary change-list aborts
- **WHEN** a `"mutually_exclusive"` `"changes"` layer under
  `update = "increment"` holds (3, 7) = 1 at wave 1 and (3, 7) = 1 at wave 2
- **THEN** the function aborts with a cli error naming the layer, wave 2 and
  the dyad, and returns nothing

#### Scenario: a count layer carries no map
- **WHEN** a `calls` panel is prepared with `layers = c(calls = "count")`
- **THEN** the returned object records no `info$values_equivalence` entry for
  calls, and its rows carry no `flavor` value

#### Scenario: flavors are relabeled
- **WHEN** friendship is prepared as `"mutually_exclusive"` with
  `flavor_names = list(friendship = c(creation = "formed", dissolution =
  "ended"))`, under `update = "replace"`
- **THEN** the recorded map is `c(formed = 1, ended = 0)`, and the `flavor`
  column holds `"formed"` and `"ended"`

#### Scenario: a flavor for a count layer aborts with guidance
- **WHEN** `flavor_names` has an entry for `calls`, declared `"count"`
- **THEN** the function aborts with a cli error stating that a count layer
  needs no flavor and suggesting one `"count"` layer per event type, and
  returns nothing

#### Scenario: invalid flavor labels abort
- **WHEN** a `flavor_names` entry uses equal labels, a non-syntactic label, or
  a role other than `creation` and `dissolution`
- **THEN** the function aborts with a cli error naming the layer, and returns
  nothing

### Requirement: Every wave is written at its own time as a change-list

For each `"states"` or `"per_wave"` layer, every wave SHALL be written as the
cells whose level differs from the level before it, stamped at that wave's
time. On a `"states"` layer the value of a timed input row SHALL be read as
the tie's level at that wave, under either `update` semantics. On a
`"per_wave"` layer it SHALL be read as the amount since the previous wave, and
the level at a wave SHALL be the level before the first wave plus the amounts
up to and including that wave. The level before the first wave SHALL be the
state the walk builds from the input's `time = NA` rows for that layer
(summed per dyad under `increment`, the last row winning under `replace`), or
the empty network when there are none. Of these layers' rows the function
SHALL read only the time, sender, receiver and value. Any other per-row
column, `order` in particular, SHALL be ignored and SHALL NOT be written on
the rows the function produces. A changed cell SHALL get one row in the
layer's own semantics: the new level under `info$update = "replace"`, the
difference under `"increment"`. The function SHALL NOT write one row per unit
of a count. A cell SHALL count as changed when its level differs under exact
comparison, with no tolerance. Cells that do not change SHALL NOT be listed
again. No row SHALL be moved to `time = NA`, and the input's `time = NA` rows
SHALL be kept as they are. The diff SHALL be computed on dyad keys, not on one
dense matrix per wave. The returned layer SHALL keep the input's stocnet shape
and SHALL NOT be a new class.

Rows of layers not named SHALL keep their position and every column value.
The written rows of a prepared layer SHALL replace its timed rows, sorted by
time, then sender, then receiver, after the other rows. Their times SHALL be
in the class of the input's `time` column, whatever the class of the grid. A
`flavor` or value column the input lacks SHALL be created, holding `NA` flavor
and the default weight 1 on the rows of other layers.

#### Scenario: the first survey is written at its own time
- **WHEN** a `"states"` layer has no `time = NA` rows and its first wave reports
  276 ties
- **THEN** the returned layer holds those 276 rows at the first wave's time and
  no row with `time = NA`

#### Scenario: a dropped tie gets a zero row
- **WHEN** dyad (3, 7) is reported at wave 1 and not at wave 2 of a `"states"`
  layer, under `update = "replace"`
- **THEN** the returned layer holds a row (3, 7) at wave 2 with value 0

#### Scenario: an increment layer gets the difference
- **WHEN** the same dyad drops under `update = "increment"`
- **THEN** the row at wave 2 carries value −1

#### Scenario: a stable tie is not repeated
- **WHEN** dyad (1, 2) is reported at both waves of a `"states"` layer
- **THEN** the returned layer holds its wave-1 row and no row at wave 2

#### Scenario: totals are written as rises under increment
- **WHEN** a `"count"` `"states"` layer under `update = "increment"` reports
  dyad (3, 7) at 6 at wave 1 and at 9 at wave 2
- **THEN** the returned layer holds (3, 7) = 6 at wave 1 and (3, 7) = 3 at
  wave 2

#### Scenario: amounts are written as running totals under replace
- **WHEN** a `"count"` `"per_wave"` layer under `update = "replace"` reports
  dyad (3, 7) at 6 at wave 1 and at 3 at wave 2
- **THEN** the returned layer holds (3, 7) = 6 at wave 1 and (3, 7) = 9 at
  wave 2

#### Scenario: amounts are kept under increment
- **WHEN** the same amounts are given on a `"per_wave"` layer under
  `update = "increment"`
- **THEN** the returned layer holds (3, 7) = 6 at wave 1 and (3, 7) = 3 at
  wave 2, one row per wave, not one row per unit

#### Scenario: a dyad without events at a per-wave wave writes nothing
- **WHEN** a `"per_wave"` layer reports (3, 7) at wave 1 and not at wave 2
- **THEN** the returned layer holds no row for (3, 7) at wave 2, and its level
  at wave 2 equals its level at wave 1

#### Scenario: repeated history rows on an increment layer sum
- **WHEN** an `increment` `"states"` layer holds two `time = NA` rows (3, 7)
  valued 1 and reports (3, 7) at level 2 at wave 1
- **THEN** the function does not abort, the state before wave 1 holds (3, 7)
  at 2, and the returned layer holds no row for (3, 7) at wave 1

#### Scenario: an inexactly equal double writes a row
- **WHEN** a `"none"` `"states"` layer reports (3, 7) = 0.3 at wave 1 and
  (3, 7) = 0.1 + 0.2 at wave 2
- **THEN** the returned layer holds a row for (3, 7) at wave 2 carrying the
  wave-2 value

#### Scenario: written rows take the time column's class
- **WHEN** the ties `time` column is `POSIXct` and `wave_times` declares an
  empty wave as a `Date`
- **THEN** the dissolution rows at that wave carry `POSIXct` times at the same
  instant

#### Scenario: an order column is not carried
- **WHEN** a `"states"` layer's rows carry an `order` column
- **THEN** the function does not abort, and the rows written for that layer
  carry no `order` value

### Requirement: The prepared state at every wave equals the survey

For every `"states"` or `"per_wave"` layer and every wave of its grid, the
network state obtained by applying the returned layer's rows at or before
that wave in time order SHALL equal the survey's level at that wave, every
dyad not reported being 0. On a `"states"` layer that level is the input's
snapshot at the wave. On a `"per_wave"` layer it is the level before the
first wave plus the amounts up to that wave. This SHALL hold under both
`update` semantics, for binary states, valued states and counts.

#### Scenario: a binary state panel round-trips
- **WHEN** a `"mutually_exclusive"` `"states"` layer with three waves, ties
  formed and dropped between each, is prepared
- **THEN** the state materialized at each wave equals that wave's input
  snapshot

#### Scenario: a count panel round-trips in both forms
- **WHEN** the same calls are given at three waves as totals (`"states"`) and
  as amounts (`"per_wave"`), each under `increment` and under `replace`
- **THEN** all four prepared layers materialize the same state at each wave,
  equal to the totals

### Requirement: Waves are given per layer, and an empty declared wave dissolves every tie

`wave_times` SHALL be either one time vector applied to every `"states"` and
`"per_wave"` layer, or a list naming a vector per layer. Such a layer without
an entry SHALL take its waves from the distinct times of its own timed rows,
and SHALL be assumed to have no empty wave. Grid times and row times SHALL be
compared after conversion to one numeric time scale, so that a grid and rows
given in different time classes for the same instants match. The
documentation SHALL state that `wave_times` must be declared for a layer that
can have an empty wave. A declared wave at which a `"states"` layer has no
rows SHALL be read as the empty network, so every tie of that layer present
before it SHALL get a dissolution row at that wave. A declared wave at which
a `"per_wave"` layer has no rows SHALL be read as no events, and SHALL write
no row. A timed row of either form whose time is not a wave of the layer's
grid SHALL abort.

Before any row is rewritten, a `wave_times` list entry naming a layer absent
from `layers`, or a layer whose form is `"changes"`, SHALL abort with a cli
error naming the layer. So SHALL an unnamed list entry, an `NA` or
non-finite grid time, a `wave_times` vector when every declared layer has form
`"changes"`, and a `"states"` or `"per_wave"` layer with no wave (no timed
rows and no `wave_times` entry). Each layer's grid SHALL be sorted without a
message.
A time listed more than once in a grid SHALL be kept once, and the function
SHALL raise one cli warning of class `goldfish_duplicate_wave_time` per call,
naming every layer and time affected.

#### Scenario: an empty declared wave is a full dissolution
- **WHEN** `wave_times` declares waves at t1, t2 and t3 for friendship, a
  `"states"` layer, and friendship has rows at t1 and t3 only
- **THEN** every tie present after t1 gets a dissolution row at t2

#### Scenario: an empty declared per-wave wave writes nothing
- **WHEN** `wave_times` declares waves at t1, t2 and t3 for calls, a
  `"per_wave"` layer, and calls has rows at t1 and t3 only
- **THEN** calls gets no row at t2, and its level at t2 equals its level at t1

#### Scenario: an inferred grid assumes no empty wave
- **WHEN** friendship was surveyed at t1, t2 and t3, has rows at t1 and t3
  only, and `wave_times` has no entry for it
- **THEN** its grid is t1 and t3, and no dissolution row is written at t2

#### Scenario: layers on different schedules
- **WHEN** friendship is surveyed at t1 and t2, advice at t1 and t3, and
  `wave_times = list(friendship = c(t1, t2), advice = c(t1, t3))`
- **THEN** advice gets no row at t2, and its changes are written at t3

#### Scenario: a row off the grid aborts
- **WHEN** `wave_times` for friendship is `c(t1, t2)` and a friendship row is
  stamped between them
- **THEN** the function aborts with a cli error naming the layer and the time

#### Scenario: a Date grid matches POSIXct rows
- **WHEN** `wave_times` for friendship is given as `Date` values and the
  friendship rows carry `POSIXct` times at the same instants
- **THEN** no row is reported off the grid, and each wave is diffed at its
  own time

#### Scenario: a grid for an undeclared or changes layer aborts
- **WHEN** `wave_times` is a list with an entry for `advice`, and `advice` is
  either absent from `layers` or declared with form `"changes"`
- **THEN** the function aborts with a cli error naming advice, and returns
  nothing

#### Scenario: a malformed grid aborts
- **WHEN** `wave_times` holds an `NA` time, or is a list with an unnamed
  entry, or is a vector while every declared layer is `"changes"`
- **THEN** the function aborts with a cli error naming `wave_times`, and
  returns nothing

#### Scenario: a layer with no wave aborts
- **WHEN** a `"states"` layer has only `time = NA` rows and no `wave_times`
  entry
- **THEN** the function aborts with a cli error naming the layer, and returns
  nothing

#### Scenario: an unsorted grid is sorted
- **WHEN** `wave_times` for friendship is `c(t2, t1, t3)`
- **THEN** the result equals the one obtained with `c(t1, t2, t3)`, and no
  message is raised

#### Scenario: a duplicated grid time warns once
- **WHEN** `wave_times` for friendship is `c(t1, t2, t2, t3)`
- **THEN** the function raises one warning of class
  `goldfish_duplicate_wave_time` naming friendship and t2, and the result
  equals the one obtained with `c(t1, t2, t3)`

### Requirement: A wave defines one value per dyad, valid for the layer's style

Within one wave of a `"states"` or `"per_wave"` layer, a dyad appearing in
more than one timed row SHALL abort. A row valued 0 SHALL be read as the dyad
being absent on a `"states"` layer and as no events on a `"per_wave"` layer.
An `NA` tie value SHALL abort. These checks SHALL apply to the timed wave
rows only; the `time = NA` rows SHALL be read as the walk reads them. On a
one-mode layer, a row whose sender equals its receiver SHALL abort. On an
undirected one-mode layer, (i, j) and (j, i) SHALL be the same dyad, keyed
and written in canonical order (the smaller index first). A two-mode layer's
rows SHALL NOT be reordered. Reported both ways at one wave with the same
value, they SHALL raise a cli warning naming the layer, the wave and the
dyad, and SHALL count as one row. With different values they SHALL abort as a
repeated dyad. This SHALL hold on binary and count layers alike, under the
`"states"` and `"per_wave"` forms: two respondents reporting the same count
are read as one report of that count, not as its double.

The style SHALL add checks on the timed values. On a `"mutually_exclusive"`
`"states"` layer every value SHALL be 0 or 1. On a `"mutually_exclusive"`
`"changes"` layer every value SHALL be in the layer's update encoding, as
`add_flavor()` requires. On a `"count"` layer every value SHALL be a whole
non-negative number, and on a `"count"` `"states"` layer a dyad's total SHALL
NOT fall from one wave to the next. A `"none"` layer SHALL have no value
check beyond `NA`. A `"count"` `"changes"` layer SHALL be checked in its
update semantics and returned unchanged: under `increment` every value SHALL
be a whole number of at least 0, and under `replace` every value SHALL be a
whole number and a dyad's level SHALL NOT fall, the starting state counting as
the level before the first row. Every violation SHALL abort with a cli error naming the
layer, the wave and the dyad. All these checks SHALL run before any row is
rewritten.

The state before the first wave, built from the `time = NA` rows, SHALL be
checked against the style: on a `"mutually_exclusive"` layer every dyad's
starting level SHALL be 0 or 1, and on a `"count"` layer a whole
non-negative number. On a `"count"` `"states"` layer, a wave-1 total below
the dyad's starting level SHALL abort as a falling total, naming wave 1. The
starting state SHALL serve these checks only: it SHALL NOT be added to any
grid, SHALL NOT write a row, SHALL NOT raise the empty-wave warning, and SHALL
NOT change the entering state or the augmentation of a fit.

#### Scenario: a repeated dyad at one wave aborts
- **WHEN** dyad (3, 7) appears twice at wave 2 of a `"states"` layer
- **THEN** the function aborts with a cli error naming the layer, the wave and
  the dyad

#### Scenario: a zero row is absence
- **WHEN** dyad (3, 7) is reported at wave 1 and reported with value 0 at
  wave 2, under `update = "replace"`
- **THEN** the result equals the one obtained when (3, 7) is simply not
  reported at wave 2

#### Scenario: a self-tie aborts
- **WHEN** a one-mode `"states"` layer holds a row (4, 4) at wave 1
- **THEN** the function aborts with a cli error naming the layer, the wave and
  the node, and returns nothing

#### Scenario: a binary layer reporting a level of 2 aborts
- **WHEN** a `"mutually_exclusive"` `"states"` layer reports (3, 7) = 2 at
  wave 1
- **THEN** the function aborts with a cli error naming the layer, the wave and
  the dyad

#### Scenario: a falling total aborts
- **WHEN** a `"count"` `"states"` layer reports (3, 7) = 6 at wave 1 and
  (3, 7) = 4 at wave 2
- **THEN** the function aborts with a cli error naming the layer, wave 2 and
  the dyad, and returns nothing

#### Scenario: a count change-list that falls aborts
- **WHEN** a `"count"` `"changes"` layer under `update = "increment"` holds
  (3, 7) = −2 at wave 2, or under `update = "replace"` holds (3, 7) = 6 at
  wave 1 and (3, 7) = 4 at wave 2
- **THEN** the function aborts with a cli error naming the layer, wave 2 and
  the dyad, and returns nothing

#### Scenario: a binary starting level of 2 aborts
- **WHEN** an `increment` `"mutually_exclusive"` layer holds two `time = NA`
  rows (3, 7) valued 1
- **THEN** the function aborts with a cli error naming the layer and the
  dyad, and returns nothing

#### Scenario: a total below its starting level aborts
- **WHEN** a `"count"` `"states"` layer holds a `time = NA` row (3, 7) = 6 and
  reports (3, 7) = 4 at wave 1
- **THEN** the function aborts with a cli error naming the layer, wave 1 and
  the dyad, and returns nothing

#### Scenario: the starting-state check writes nothing
- **WHEN** a `"count"` `"states"` layer's `time = NA` rows pass the check
- **THEN** the returned layer's grid, rows and `time = NA` rows are the same
  as they would be without the check, and no wave before the first is added

#### Scenario: an undirected count reported in both orders counts once
- **WHEN** an undirected `"count"` `"per_wave"` layer reports (3, 7) = 2 and
  (7, 3) = 2 at wave 1
- **THEN** the function warns, naming the layer, the wave and the dyad, and
  the level of (3, 7) at wave 1 is 2

#### Scenario: a fractional or negative count aborts
- **WHEN** a `"count"` `"per_wave"` layer reports (3, 7) = 1.5 or (3, 7) = −1
- **THEN** the function aborts with a cli error naming the layer, the wave and
  the dyad

#### Scenario: an undirected tie reported in both orders warns once
- **WHEN** an undirected `"states"` layer reports (3, 7) and (7, 3), both with
  value 1, at wave 1
- **THEN** the function warns, naming the layer, the wave and the dyad, and the
  returned layer holds one row (3, 7) at wave 1

#### Scenario: an undirected tie reported in both orders with different values aborts
- **WHEN** an undirected `"states"` layer reports (3, 7) = 1 and (7, 3) = 2 at
  wave 1
- **THEN** the function aborts as for a repeated dyad

#### Scenario: an undirected tie that switches orientation is stable
- **WHEN** an undirected `"states"` layer reports (3, 7) at wave 1 and (7, 3) at
  wave 2
- **THEN** the returned layer holds no row for the dyad at wave 2

#### Scenario: a two-mode undirected layer is not reordered
- **WHEN** a two-mode `"states"` layer declared undirected reports a row whose
  sender index is larger than its receiver index
- **THEN** the returned row keeps that sender and receiver

### Requirement: A successful call reports what it wrote

On success the function SHALL raise exactly one cli message of class
`goldfish_prepare_summary`, holding one line of totals per prepared layer and
no per-wave breakdown. A `"mutually_exclusive"` layer SHALL be reported as its
event count per flavor, under the recorded labels; a `"count"` layer as its
unit events and changed dyads; a `"none"` layer as its changed cells; a
`"changes"` layer by the same counts read off its rows. The rows written at
the first wave SHALL be reported on a separate line as the starting state,
and SHALL NOT be counted as events. The function SHALL take no argument to
silence the message.

#### Scenario: a binary layer reports its flavors
- **WHEN** the `social_evolution` friendship panel among the 51 actors is
  prepared as `"mutually_exclusive"` with `flavor_names = list(friendship =
  c(creation = "formed", dissolution = "ended"))`
- **THEN** the function raises one message of class
  `goldfish_prepare_summary` reporting 276 starting-state rows, 70 formed and
  70 ended for friendship

#### Scenario: the summary can be silenced by class
- **WHEN** the call is wrapped in `suppressMessages(classes =
  "goldfish_prepare_summary")`
- **THEN** nothing is printed and the returned object is unchanged

### Requirement: A wave that writes no rows warns

For each `"states"` or `"per_wave"` layer and each wave of its grid at which
the function writes no row, including a first wave whose state equals the
input's `time = NA` rows, the function SHALL raise a cli warning of class
`goldfish_empty_wave`. It SHALL raise exactly one such warning per call,
listing every (layer, wave) pair that wrote no row. The warning SHALL say
that `wave_times` must be passed to `estimate_dynes()` for any listed layer
that is modeled, since that function infers its grid from row times. The
function SHALL NOT abort on an empty wave.

#### Scenario: an unchanged middle wave warns
- **WHEN** a `"states"` layer reports the same ties at waves 1 and 2 and
  different ties at wave 3
- **THEN** the function warns with class `goldfish_empty_wave`, naming the layer
  and wave 2, and returns the prepared layer with no row at wave 2

#### Scenario: a first wave equal to the initial state warns
- **WHEN** a `"states"` layer's `time = NA` rows equal its wave-1 survey
- **THEN** the function warns with class `goldfish_empty_wave`, naming wave 1

#### Scenario: empty waves on several layers give one warning
- **WHEN** friendship writes no row at wave 2 and calls, a `"per_wave"`
  layer, writes no row at wave 3
- **THEN** the function raises exactly one `goldfish_empty_wave` warning, which
  names friendship at wave 2 and calls at wave 3

### Requirement: A layer that is already flavored aborts

A named layer whose flavor map is recorded on the object
(`info$values_equivalence`) SHALL abort with a cli error of class
`goldfish_layer_already_flavored`, before any row is rewritten, whatever its
declared style and form. The error SHALL name every such layer, state that
`prepare_dynes_data()` writes the flavor map itself, and state that the layer
should be passed in unflavored. For a layer declared `"count"`, the error
SHALL also state that a count layer needs no flavor, and SHALL suggest one
`"count"` layer per event type. The check SHALL read the recorded flavor map,
not the `flavor` column.

#### Scenario: a layer flavored by hand aborts
- **WHEN** `prepare_dynes_data()` is called with
  `layers = c(friendship = "mutually_exclusive")` on an object where
  `add_flavor()` has already flavored friendship
- **THEN** the function aborts with class `goldfish_layer_already_flavored`,
  naming friendship, and returns nothing

#### Scenario: a flavored count layer gets the count guidance
- **WHEN** `add_flavor()` has flavored `calls` and the function is called with
  `layers = c(calls = "count")`
- **THEN** the function aborts with class `goldfish_layer_already_flavored`,
  and the error states that a count layer needs no flavor and suggests one
  `"count"` layer per event type

### Requirement: A prepared layer cannot be prepared again

The returned object SHALL record the layers the function rewrote, flavored or
passed through in `info$dynes_prepared`, a character vector named by layer
whose values are the declared styles. The declared form SHALL NOT be recorded.
Naming a recorded layer in a later call SHALL
abort, since a change-list diffed again turns every stable tie missing from a
later wave into a dissolution, and amounts summed again double-count.

#### Scenario: a second preparation aborts
- **WHEN** the result of
  `prepare_dynes_data(x, c(friendship = "mutually_exclusive"))` is passed to
  `prepare_dynes_data()` again naming friendship
- **THEN** the function aborts with a cli error saying that friendship is
  already prepared

#### Scenario: a passed-through count change-list is recorded
- **WHEN** the function is called with `layers = c(calls = "count")` and
  `form = c(calls = "changes")`
- **THEN** the returned object's `info$dynes_prepared` holds
  `calls = "count"`, and naming calls in a later call aborts

### Requirement: Event rows keep their times

The function SHALL NOT change any row of a layer it is not asked to prepare. In
particular, event-layer rows SHALL keep their recorded times: the walk treats a
row before the observation start as history (applied, not scored) and scores a
row at or after it, as under `estimate_dynam()`. No event row SHALL be set to
`time = NA`, since that would discard the timing that time-dependent effects
(windows, recency) read.

#### Scenario: a call before the first survey keeps its time
- **WHEN** a call is recorded on 2008-09-06 and the first friendship wave is on
  2008-09-09
- **THEN** the call stays in the returned calls layer at 2008-09-06

### Requirement: Prepared data enters the fit at the survey of the observation start

On data prepared by this function, a fit with `estimate_dynes()` SHALL enter
its first modeled interval with the prepared layer's state at the observation
start, which by the round-trip requirement is the survey at that wave, with
no further flavoring step. Rows at or before the start SHALL be applied in
time order and SHALL NOT be scored. The same SHALL hold when the same states
are given as an equivalent `"changes"` layer.

#### Scenario: the scored creation count is the change count
- **WHEN** the `social_evolution` friendship panel (51 actors, 276 ties at the
  first survey, 70 formed and 70 dissolved between the surveys) is prepared as
  `"mutually_exclusive"` and fitted, without `add_flavor()`, with an
  intercept-only creation rate and the default start
- **THEN** the creation rate intercept equals log(70 / (51 · T)) up to
  Monte-Carlo error, where T is the length of the interval, not
  log(346 / (51 · T))

#### Scenario: a prepared count panel is estimated as a count panel
- **WHEN** a calls panel is prepared as `"count"` from amounts (`"per_wave"`)
  and passed to `estimate_dynes()` as the modeled panel layer
- **THEN** the wave diff reads it as a count panel, and each interval's event
  count equals the sum of that wave's amounts

#### Scenario: a fit started at a later wave enters with that wave's survey
- **WHEN** a `"mutually_exclusive"` panel layer with at least four waves is
  prepared from states and fitted with `set_preprocessing(start_time = )` at
  the third wave
- **THEN** the augmentation's entering state equals the third wave's survey,
  the changes at the first and second waves are applied and not scored, and the
  same holds for the equivalent `"changes"` input

### Requirement: The first-wave diff is augmented only when an interval opens before it

Under the default start, the rows written for the first wave SHALL NOT be
augmented: the start is the first wave, those rows are history, and no
modeled interval ends there. When the user sets the observation start before
the first wave, the input's `time = NA` rows SHALL be the entering state of the
interval from the start to the first wave, and the first wave's rows SHALL be
that interval's flip set. The documentation SHALL state that a start before
the first survey makes the first survey an interval endpoint, not the initial
state.

#### Scenario: the default start never augments the first-wave diff
- **WHEN** a `"states"` layer with `time = NA` rows and two waves is prepared
  and fitted with the default start
- **THEN** no interval of the augmentation recipe holds a flip for the first
  wave's diff, and the entering state equals the first wave's survey

#### Scenario: history rows are never augmented
- **WHEN** a `"count"` `"states"` layer with `time = NA` rows and three waves is
  prepared and fitted with the default start
- **THEN** the augmentation recipe's intervals are those between the waves
  only, no interval ends at or before the first wave, and no flip of the
  recipe comes from the `time = NA` rows

#### Scenario: an explicit earlier start opens an interval before the first survey
- **WHEN** the friendship waves are at 2008-09-09 and 2008-10-19 and
  `start_time` is 2008-09-01
- **THEN** 2008-09-09 is the right endpoint of the first modeled interval, the
  entering state is the input's `time = NA` rows, and the first wave's diff is
  that interval's flip set

### Requirement: The respondent set is the user's

The function SHALL NOT drop, impute or flag actors for non-response. It SHALL
prepare the node set and rows it is given. Restricting to the actors whose
answers can be compared across waves is the user's step, before the call.

#### Scenario: a non-respondent is kept as given
- **WHEN** an actor sends no nomination at wave 2 of a `"states"` layer and the
  user has not removed them
- **THEN** their wave-1 ties are written as dissolutions at wave 2, exactly as
  for any other dropped tie
