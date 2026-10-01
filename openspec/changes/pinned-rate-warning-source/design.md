## Context

`complete_generative_spec()` runs once at each generative consumer's entry
(`estimate_dynes()`, `simulate()`). For a timed composition it fills a missing
rate with an intercept-only default, marks it pinned (`mark_pinned_rates()`), and
pins its log-hazard from `(count_w, T_w, |R_w|)` (`pin_completed_rates()`). That
triple comes from one of two helpers, chosen by how the layer is observed:

```
  completed layer                          helper                        count_w
  ---------------------------------------  ----------------------------  -----------------------
  unflavored, event-observed, not panel    relational_window_risk_set()  observed event count
  modeled panel layer                      panel_wave_risk_set()         net wave Hamming diff
  flavored relational layer (stopgap)      panel_wave_risk_set()         net wave Hamming diff
```

Two warnings then fire at that entry, for the same fid:

```
  complete_generative_spec()
    +-- warn_completed_defaults()   goldfish_completed_default_warning
  consumer entry
    +-- warn_pinned_rates()         goldfish_pinned_rate_warning
          wording picked by `consumer`:
            estimate_dynes -> "wave Hamming diff ... no SE ... excluded"
            simulate       -> "observed event count"
```

The second warning picks its provenance from `consumer`, which the helper
dispatch ignores. The requirement it implements (`intercept-only-rate`, "Each
consumer warns that the rate is pinned, worded for its source") assumed each
consumer had one source. `pin_completed_rates()` was later made observation-keyed
(the `|R_w|` helpers comment says so explicitly) and the warning was not revisited.

## Goals / Non-Goals

**Goals:**
- The provenance line always names the count source the pin actually used.
- One warning per completed rate per consumer entry.
- The process is named by its `layer [› flavor] › rate` label.
- Existing class-based handlers and tests keep matching.

**Non-Goals:**
- Pinning an authored `rate = ~ 1` in the joint context (the known gap in the
  proposal). The standalone warning is made correct for that case, not reachable.
- Changing any pinned value. The helper dispatch, and therefore every pinned
  intercept, is untouched.
- Revisiting the flavored-relational stopgap routing to the panel path. The
  warning reports that routing faithfully ("Hamming diff") while it stands.

## Decisions

### D1 — The pin records its source; the warning reads it

`pin_completed_rates()` already knows which helper it called (`relational`). It
records that on the pinned object as `source = "event_count"` or
`source = "hamming_diff"`, via a new argument to `make_intercept_only_rate()`.
Every warning reads the provenance from there.

*Alternative — re-derive the source in the warning from the process map and
`modeled_panel`.* Rejected: it duplicates the dispatch rule in a second place,
and the two would drift the next time the stopgap routing changes. The object
that holds the pinned number should also hold where the number came from.

*Alternative — make the source follow the consumer (keep the requirement,
change the pin).* Rejected: the same completed specification would then pin to
different values depending on who reads it. That breaks the one-completion,
every-consumer-reads-the-same-object design `estimate_dynes()` depends on (a
pinned rate must not move between proposals or between fitting and simulating).

### D2 — A completed rate is announced once, by the completed-default warning

For a completed `rate` row, `warn_completed_defaults()` appends:

- the source line, from the pin's recorded `source` (D1);
- for `consumer = "estimate_dynes"` only, the line that the rate carries no
  standard error and is excluded from estimation. `simulate()` gets no
  standard-error language, since a standard error is not a simulation concept.

`warn_pinned_rates()` then skips every fid whose bundle is marked `completed`.
`warn_completed_defaults()` already runs after the pins are built (step 4 follows
step 3 in `complete_generative_spec()`), so it only needs the pinned objects
passed in; the step order is unchanged.

*Alternative — keep two warnings and fix only the wording (option A in the
review).* Rejected by the user: two conditions for one fact at one entry is
noise, and the second repeated the first's "pinned intercept-only rate".

### D3 — The merged warning carries both condition classes

The single warning is raised with
`class = c("goldfish_pinned_rate_warning", "goldfish_completed_default_warning")`.
Condition classes are matched, not dispatched on, so a handler or
`expect_warning(class = )` on either name still catches it. That keeps
`test-simulate.R`'s and `test-intercept_only_rate.R`'s `goldfish_pinned_rate_warning`
expectations, and the helper `pinned_rate_warnings()`, meaningful without
rewriting them to a new class. A completed **choice** keeps only
`goldfish_completed_default_warning`: nothing about it is pinned.

*Alternative — a new class for the merged warning.* Rejected: it breaks every
existing handler for no information gain.

### D4 — The standalone warning is named by label and worded by source

`warn_pinned_rate()` takes the process label (built the way
`assert_joint_offsets_valued()` builds `proc_label`) instead of the fid, and
takes the pin's `source` in place of the consumer-keyed provenance. The consumer
still decides whether the standard-error line appears. An authored pinned rate
has no pin object until the known gap is fixed, so the source is passed as
`NULL` and the provenance line is omitted rather than guessed.

### D5 — The completed rate's fid is found by an NA-safe join, not carried on `added`

`added` (the completed-default rows `complete_generative_spec()` collects in its
step 1) is built before `rebuild_completed_joint()` assigns fids in step 2, so it
cannot carry one at construction. `warn_completed_defaults()` resolves each
`family == "rate"` row's fid by matching `(layer, flavor)` against the rebuilt
`process_map` (which already carries `fid`, `layer`, `flavor`, `family`) — the
same key `pin_completed_rates()` reads off `map$fid[...]`. `flavor` is `NA` for
an unflavored layer, so the match uses an NA-safe comparison (e.g. `identical()`
per row), not `==`, which returns `NA` — and therefore drops — every unflavored
row. Given the fid, the pin's `source` comes from
`completed_rates[[as.character(fid)]]$source` (D1); the call site passes
`completed$completed_rates` and `completed$process_map` into
`warn_completed_defaults()` alongside `added`.

*Alternative — thread the fid onto `added` at creation.* Rejected: no fid exists
until after the rebuild (step 2), so building one early would duplicate
`build_joint_process_map()`'s ordering rule in a second place for no gain, since
the join already reads the real `process_map` at step 4.

*Alternative — reorder `complete_generative_spec()` so default resolution runs
after the rebuild.* Rejected: a bigger structural change than this fix needs; it
touches step ordering that other reads of `added` and `rebuild_completed_joint()`
depend on, out of scope for a warning-wording fix.

## Risks / Trade-offs

- [Standalone warning has no live caller] → Recorded as the known gap in the
  proposal. It stays unit-tested through `warn_pinned_rates()` on a hand-marked
  specification (the existing `mark_pinned_rates()` fixtures), so it cannot rot
  silently.
- [Re-recorded snapshots hide an unrelated wording drift] → The re-record task
  reviews the snapshot diff line by line: only the provenance line, the label,
  and the dropped second warning may change.
- [An older pinned object without `source`] → Objects are built only inside
  `complete_generative_spec()` at each consumer entry, never persisted across
  versions, so no migration is needed. A missing `source` is treated as unknown
  (line omitted), matching D4.
