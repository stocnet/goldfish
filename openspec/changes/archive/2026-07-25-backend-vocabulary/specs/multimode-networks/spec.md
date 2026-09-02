# multimode-networks (delta)

Wording-only: both compute backends named by their `backend` value.

## MODIFIED Requirements

### Requirement: Two-mode coefficient equivalence with the legacy path

A two-mode model built as a mode-map stocnet SHALL produce the same coefficients
(to within 1e-6, on both compute backends) as the same model built through the
legacy two node-set constructors (now assembling to stocnet), and a mixed
one/two-mode-layer object SHALL estimate consistently. Frozen one-mode DyNAM/REM
baselines SHALL remain PASS (not SKIP) under `NOT_CRAN=true`.

#### Scenario: Stocnet and legacy two-mode paths agree

- **WHEN** the same two-mode model is estimated via the mode-map stocnet path and
  via the legacy two node-set constructors
- **THEN** the coefficients agree to within 1e-6 for both the `r` and
  `cpp` backends.
