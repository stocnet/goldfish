## ADDED Requirements

### Requirement: A fit's recorded dependent events are the events it modeled
A fitted model SHALL record, as its dependent events, exactly the events inside
the resolved observation window, so that every per-interval quantity the fit
carries pairs with them row for row. The recorded table SHALL NOT contain events
the likelihood excluded, and the filter SHALL be applied where the table is
built rather than by each consumer, so no surface can be reached with the
unfiltered table.

The window applied SHALL be the **resolved** one the preprocessing computed, not
the caller's raw argument: `start_time` and `end_time` default to the span of
the non-window event streams when unset, and those resolved bounds are the ones
the fit stores.

#### Scenario: a windowed fit records only its modeled events
- **WHEN** a model is fitted with a `start_time` that excludes some events of the
  focal layer
- **THEN** the fit's dependent-events table has one row per likelihood interval,
  and its event times equal the fit's own dependent event times in order

#### Scenario: the augmented table is reachable on a windowed fit
- **WHEN** `augment()` is called on a fit whose `start_time` excluded events
- **THEN** it returns one row per interval, and the describers built on it
  (`diagnose_outliers()`, `diagnose_changepoints()`) run without error

#### Scenario: an unwindowed fit is unaffected
- **WHEN** a model is fitted with neither `start_time` nor `end_time`
- **THEN** its dependent-events table is unchanged from the layer's own modeled
  rows

### Requirement: Per-event ordering state is continuous across the burn-in
The per-event order counter that order-dependent effects read SHALL advance by
the same amount per event before and after `start_time`, so that an effect whose
statistic depends on adjacency in the event stream computes the same value
whether a preceding event fell inside or outside the observation window.

Events before `start_time` are replayed to build the initial statistics, so they
are part of the stream the effect reasons about. An effect SHALL NOT need to know
whether the observation window has opened.

#### Scenario: consecutive closure accumulates across the burn-in
- **WHEN** a model with `trans(history = "consecutive")` is fitted with a
  `start_time` that places some events in the burn-in
- **THEN** the effect's statistic at the first modeled event is the value it
  would take had those events been observed, and is not identically zero

#### Scenario: the window boundary does not change the statistic
- **WHEN** the same event stream is fitted once with `start_time = t` and once on
  data truncated so that `t` is the first event, with the earlier events supplied
  as history
- **THEN** the order-dependent statistics agree wherever the two histories
  coincide

### Requirement: The walk stops at end_time on every preprocessing path
Preprocessing SHALL stop traversing the event schedule once `end_time` is
reached, on both the recipe-loop path and the legacy monolith path, since events
beyond the window are not stored and traversing them only costs time. The two
paths SHALL NOT disagree about what the end of the observation window means.

The documentation of `end_time` SHALL describe this behavior.

#### Scenario: traversal stops rather than draining
- **WHEN** a model is preprocessed with an `end_time` well before the last event
  of the schedule, on either preprocessing path
- **THEN** no stored row has an event time after `end_time`, and the events past
  it are not visited

#### Scenario: the documented behavior matches the code
- **WHEN** the `end_time` argument documentation is read
- **THEN** it states that preprocessing stops at that time

### Requirement: The observation window is closed when the schedule ends first
Preprocessing SHALL close the observation window at `end_time` even when the
event schedule is exhausted before `end_time` is reached, on the sub-models whose
likelihood defines a compensator — the exact-time rate and REM families. The
elapsed time between the last event and `end_time` is exposure during which no
event occurred, which is a likelihood contribution, and dropping it biases the
baseline rate upward.

A sub-model whose likelihood has no compensator — the multinomial families —
SHALL store no such row, because it would contribute zero while changing the
interval count.

A right-censored row written at the window boundary SHALL NOT carry the
sender and receiver of an out-of-window event.

#### Scenario: exposure past the last event is counted
- **WHEN** an exact-time rate model is fitted with an `end_time` some days after
  its last event
- **THEN** the accumulated intervals reach `end_time`, and the fitted baseline
  rate is lower than the same model fitted with no `end_time`

#### Scenario: a later end_time counts more exposure
- **WHEN** the same exact-time rate model is fitted at two `end_time` values,
  both past the last event
- **THEN** the two fits differ, the later `end_time` giving the lower baseline
  rate

#### Scenario: a multinomial sub-model stores no trailing row
- **WHEN** a choice model is fitted with an `end_time` past its last event
- **THEN** its stored intervals end at the last dependent event, and its
  log-likelihood is unchanged from the same fit with no `end_time`

#### Scenario: the boundary row carries no borrowed identity
- **WHEN** a right-censored row is written at the window boundary
- **THEN** it does not report the sender or receiver of an event outside the
  window
