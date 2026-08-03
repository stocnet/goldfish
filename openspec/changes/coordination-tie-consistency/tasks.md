# Tasks — coordination-tie-consistency

Disciplines (openspec/config.yaml): one focused conventional commit per task,
tests green at every commit, `air format` the touched R files before `lintr`,
`NOT_CRAN=true` with the frozen baselines PASS (not SKIP) before each commit.
Every task touching `src/*.cpp` / `src/*.h` invokes `cpp-recompile` BEFORE
testing — the coefficient and C++ golden baselines pass silently on a stale
`.o`/`.so`, which would make a kernel change look like a no-op.

## 0. Locate the split before changing anything

- [ ] 0.1 Find where mathematically equal dyads stop being equal. Instrument the
      coordination path at one known-bad event (social evolution, event 27:
      6972 candidates, 28 distinct values on `r`, 19 on `cpp`) and follow the
      value from the linear predictor through the mutual product,
      symmetrization, and triangle accumulation. The deliverable is the line, not
      a hypothesis
- [ ] 0.2 Answer the design's second open question: are `margins` and
      `probabilities` affected too? Both are compared with tolerances today, so a
      block split would pass every existing test. Check the block structure
      directly rather than the aggregate
- [ ] 0.3 Answer the third: is `fish_dynam_choice_coord` genuinely consistent, or
      only masked by covariates that differentiate its dyads? Construct a
      structural-only formula on that fixture and re-measure
- [ ] 0.4 Decide the directed-stream question (design D3): is coordination over a
      directed event stream supported, and if not, is the answer an abort, a
      warning, or documentation? Record the decision and whether it needs a guard
      here or belongs with estimation validation

## 1. Restore consistency

- [ ] 1.1 Fix whatever 0.1 found, on every backend that has it. `cpp-recompile`
      before testing
- [ ] 1.2 Test the three `backend-primitive-parity` scenarios: an equal block is
      bit-identical within a backend; the backends agree on the number of
      distinct values; coordination `observed_rank` vectors are identical across
      backends. The last one is the user-visible statement and currently fails at
      107 of 439 events
- [ ] 1.3 Re-measure the disagreement count and record it. Expect zero; anything
      else means 0.1 found a contributing cause rather than the cause

## 2. Baselines and closure

- [ ] 2.1 If coordination values moved, regenerate the affected cells through the
      `regen-baselines` skill with the justification recorded — the two
      coordination cells are the ones at risk and the other ten are the control
      that they are the only ones. **Do not** regenerate to make a test pass
      before 1.3 has explained the movement
- [ ] 2.2 NEWS entry: coordination ranks no longer depend on the backend, with
      the before/after disagreement count. Note the values change if they did.
      DESCRIPTION version bump
- [ ] 2.3 Full `NOT_CRAN=true` suite green with the frozen baselines PASS (not
      SKIP), `openspec validate` green, and
      `bash .plan/opsx-spec-placement-check.sh coordination-tie-consistency`
      green
