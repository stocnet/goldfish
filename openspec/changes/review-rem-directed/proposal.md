## Why

The `support-constraint-risk-set` change settles the mask for **undirected** REM and DyNAM
`choice_coordination` by requiring a **symmetric** mask (D11): a mutual / undirected tie cannot
form if only one direction is allowed. It deliberately does **not** settle the **directed** REM
case, where symmetry is not obviously right and several semantics are undefined. Rather than bake
an assumption into the support-constraint engine, this change reviews and pins down REM-on-directed
semantics so the mask (and the statistics) have a defined meaning before any engine work depends on
them.

This is a **review / definition** change: its output is agreed decisions (and, if needed, a
follow-up implementation change), not immediately shipped code.

## What Changes

- **Define the directed-REM likelihood** precisely for the risk-set / support-constraint setting:
  what the per-event denominator ranges over when ties are directed, and how right-censored
  intervals are scored.
- **Decide whether the support mask may be asymmetric** for directed REM (`mask[i, j]` independent
  of `mask[j, i]`), or whether the same symmetric rule as undirected must hold — and why.
- **Decide whether directed statistics are asymmetric** in this setting (e.g. sender- vs
  receiver-role effects) and how that interacts with an asymmetric mask.
- **Cross-check against DyNAM-choice / REM current behaviour** so the definition is a
  clarification, not a silent change to existing directed REM fits.

## Capabilities

### Modified Capabilities
- `support-constraint`: extend/clarify the mask semantics for **directed REM** (the
  `support-constraint-risk-set` change covers undirected + coordination via a symmetric mask; this
  change defines the directed case). *(deltas authored once the review decisions are made.)*

## Impact

- Primarily a **definition / review** — no immediate code. If it concludes that directed REM needs
  distinct mask/likelihood handling, a follow-up implementation change is opened against
  `R/estimation_core.R` / the C++ gathers.
- **Blocks nothing** in `support-constraint-risk-set` v1 (which ships undirected + coordination
  with the symmetric mask); this is the directed-REM follow-up.

## Open Questions
- Is an **asymmetric mask** meaningful for directed REM, or does the risk set still require
  symmetry? What does an asymmetric restriction *mean* for a directed relational event?
- How does the **directed likelihood** integrate the mask over right-censored intervals (does the
  D12 interval-recompute carry over unchanged)?
- Do **directed statistics** (sender/receiver asymmetry) need to align with mask asymmetry, or are
  they independent concerns?
- Is there an existing `goldfish` / `goldfish.latent` directed-REM use case that already implies
  an answer (check before defining)?
