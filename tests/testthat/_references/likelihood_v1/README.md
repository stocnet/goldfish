# Per-event likelihood pins

`test-likelihood_pins.R` reads `likelihood_references.rds`. Like the
coefficient baselines in `_baselines/`, these are goldfish's own numbers,
frozen; unlike them, they are taken *before* the optimizer, at a fixed
parameter vector, so they see every per-event output a refactor of the
likelihood site could move, not only the optimum it reaches.

## What is pinned

For each entry of `baselines_model_grid()` (the six likelihood families on
Social Evolution and on Fisheries), one engine pass on each backend (`cpp`,
`r`, `gather`) at the converged `cpp` coefficients, with every opt-in
component requested: log-likelihood, score, information, interval
log-likelihoods, event scores, ranks, margins, availability, total rate and
the conditional component and scores on the exact-time families, and the
event information trace.

Each backend is pinned against **itself**. Cross-backend agreement is
`test-backend_parity.R`'s job; these exist so that a rewrite of one backend's
loop can be shown to change none of its own numbers.

Every component is flattened to a numeric vector under the engine's own name
for it. The per-event probability element is a vector over one axis or a
whole `n1 x n2` grid, and the grids alone would be about 25 MB per backend,
so each interval's element is reduced to three projections -- its total and
two fixed oscillating weightings of its entries (`likelihood_pin_project()`
in `helper-likelihood-pins.R`). Any single entry moves at least one of them.
Shape is not pinned here; it is asserted where it is specified.

## Tolerance

1e-10, the living spec's floor for per-event outputs. Byte-identity is not
claimed: a BLAS-reordered sum is an admissible difference.

The aggregate score is the exception to "relative to itself". The pins are
taken at the optimum, where the score is a near-zero sum of large per-event
terms, so a reordered sum moves it by rounding that is tiny against those
terms (measured at most 4e-14 of them) yet can be 1e-9 of the near-zero
total. It is held to 1e-10 of the summed absolute per-event score of each
parameter instead.

## Regenerating

`generate_likelihood_references.R`, run from the package root. The commit it
was generated at is the `commit` attribute of the stored list. A reference
moves only when the new value was derived before the run, and the commit that
moves it says how.
