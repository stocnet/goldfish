# active-availability-stat (delta)

Wording-only. The `default_c`/`gather_compute` *rejection* named in the
requirement body is an implementation gate being lifted, so it keeps its
implementation naming; the agreement scenario enumerates the backends a user
selects, so it moves to the `backend` vocabulary.

## MODIFIED Requirements

### Requirement: Opportunity list absorbed in the main loop and applied in order
The opportunity list SHALL be consumed during the main preprocessing loop and
merged into the `active_dyad` point buffer (forcing the point encoding), NOT
re-derived per estimation iteration. For each dependent event `e` with sender
`i = sender(e)`, preprocessing SHALL emit point updates
`(node1 = i, node2 = j, replace = 1)` for every `j` allowed by
`opportunities_list[[e]]` and the folded receiver availability. The per-event
update slice SHALL be applied before that event's likelihood, and the first
dependent event's opportunity SHALL be in `active_dyad_init`. The
`updateopportunities` per-iteration recompute and the `mask_to_opportunities`
adapter SHALL be removed, and the opportunity rejection on the `cpp` and
`gather` backends SHALL be lifted.

#### Scenario: opportunity produces per-event point updates
- **WHEN** a choice model is estimated with an `opportunities_list`
- **THEN** preprocessing emits, per dependent event `e`, the `(sender(e), j)` point
  updates for the allowed-and-available receivers into `active_dyad`, and no
  per-iteration `opportunities <- seq_len(n2) %in% opportunitiesList[[e]]` recompute
  runs at estimation time.

#### Scenario: first event's opportunity is in the init (ordering)
- **WHEN** `active_dyad` is consumed by the likelihood
- **THEN** for every dependent event `e` the availability the likelihood sees equals
  the from-scratch intersection including `opportunity[e]` — the first event's value
  taken from `active_dyad_init` and each later event's from its pre-likelihood update
  slice — with no off-by-one on event 1.

#### Scenario: opportunity reproduces the pre-change coefficients
- **WHEN** the same `opportunities_list` model is estimated before and after this
  change
- **THEN** the estimated coefficients agree to within 1e-6 on every backend
  (`r`, `gather`, `cpp`).
