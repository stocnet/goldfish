> **Track (user, 2026-07-27): post-release.** This change lands **after
> 2.0.0**; `residuals-gof` ships its rank tie rule first (strict-greater,
> tolerance 1e-12 — its design D15), accepting that the tolerance masks
> the float-split at the rank layer until this change fixes the values.
> When it lands it may tighten that tolerance toward ~1e-15. Because it
> can move the `se_dynam_choice_coord` / `fish_dynam_choice_coord` frozen
> baseline cells, it must NOT run concurrently with any release-train
> change that relies on the 1e-6 floor.

## Why

On the social-evolution coordination fixture at the MLE, **107 of 439 events
(24%) get a different `observed_rank` from `backend = "r"` than from
`backend = "cpp"`**. Seventy-three of them differ by exactly one rank, and the
median observed rank among the disagreeing events is 4 — the very top of the
ranking, which is the part a user reads.

The obvious explanation is wrong, and ruling it out is what makes this its own
change. It is **not** that coordination has more ties:

| family | candidates | distinct values | tied with observed | cross-backend prob. diff | rank disagreements |
|---|---|---|---|---|---|
| `se_rem` | 6972 | 13 | 8 | 1.8e-13 | **0** |
| `se_dynam_choice_coord` | 6972 | **293** | 4 | 5.6e-13 | **107** |
| `fish_dynam_choice_coord` | 22052 | 7868 | 2 | 1.1e-13 | 0 |
| `se_dynam_choice` | 83 | 3 | 1 | 2.0e-14 | 0 |

`se_rem` has *twice* the ties and comparable cross-backend noise, and agrees
perfectly. The distinguishing column is **distinct values**: on one event the
`r` backend produces 28 distinct probabilities where `cpp` produces 19, from
6972 candidates that the model makes mathematically equal in large blocks. So
the coordination path **splits its own mathematically-equal dyads into different
floating-point values, and splits them differently on each backend**. Ranking
with a strict `>` then resolves those splits arbitrarily, and differently per
backend.

A separate question sits underneath. The coordination model is defined over
unordered pairs, and it is applied here to `Social_Evolution` calls — directed
data. If an undirected process is the intent, the tie should be symmetrized in
the data so statistics update in both directions, rather than being symmetrized
by the estimator over a directed event stream. Whether
`se_dynam_choice_coord` is a meaningful specification at all is open, and it
matters beyond this change: it is one of the twelve frozen coefficient baseline
cells.

## What Changes

- **Establish where the coordination path loses tie consistency.** Mathematically
  equal dyads must produce equal floating-point values within a backend before
  any question about ranking them can be answered.
- **Make the three backends agree on the coordination probability vector** to the
  same standard the other families already meet, so equal alternatives are equal
  everywhere.
- **Settle whether coordination over a directed event stream is a supported
  specification**, and if not, what the user is told — this decides whether
  `se_dynam_choice_coord` remains a defensible baseline cell.

## Capabilities

### Modified Capabilities

- `backend-primitive-parity`: the existing numeric-agreement requirement is
  sharpened for alternatives the model makes equal — agreement within a tolerance
  is not sufficient when a downstream primitive compares values exactly.

## Impact

- **Code:** the coordination kernels (`src/DyNAM_MM_default.cpp`,
  `src/compute_coordination_selection.cpp`) and the R mirror's coordination
  path; possibly the symmetrization step shared with the undirected risk set.
- **Baselines:** if the investigation changes coordination probabilities, the
  `se_dynam_choice_coord` and `fish_dynam_choice_coord` frozen cells move. That
  is a deliberate, documented regeneration, not a silent one.
- **Adjacent, deliberately not here:** what rule `observed_rank` should use for
  ties, and with what tolerance, belongs to `residuals-gof`, which owns ranks and
  `recall@k`. This change is about the values being ranked; that one is about how
  to rank them. Both are needed — a tie rule alone would paper over an
  inconsistency, and consistency alone still leaves exact ties to break.

## Open Questions

- Is the split introduced by the mutual-product likelihood, by the symmetrization
  of the dyad matrix, or by the order in which the triangle is accumulated?
- Should coordination over a directed event stream abort, warn, or be documented
  as the user's responsibility to symmetrize beforehand?
