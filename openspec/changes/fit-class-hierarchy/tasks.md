# Tasks — fit-class-hierarchy

Disciplines (`openspec/config.yaml`): one focused conventional commit per task,
full `NOT_CRAN=true` suite green at every commit with the frozen 1e-6 baselines
PASS not SKIP, `air format` then `lintr` on touched files,
`devtools::document()` inline when roxygen changes. No `src/` edit. Implements
ADR-0038. **Runs after `class-naming-scheme`, and after
`model-spec-descriptor` D10 where practical — see design D6.**

## 1. Inventory and the table's shape

- [x] 1.1 Confirm the predecessors' state: `class-naming-scheme` has landed
      (so the table names final classes) and whether `model-spec-descriptor`
      D10 has collapsed the six mechanical fan-outs (so those rows never enter
      the table). Record which in `progress.md`.
- [x] 1.2 Enumerate every generic dispatching on a fit class and every concrete
      fit class. Today that is 16 on both classes, 4 on the single class only
      (`summary`, `tidy`, `glance`, `print.summary`) and 1 on the flavored only
      (`coef_layout`) — re-derive rather than trusting these numbers.
- [x] 1.3 Choose the table's on-disk form (design D5): machine-readable, and
      the same artifact the test reads and a human edits.

## 2. The logLik row first

- [x] 2.1 Decide and record the `logLik` verdict for a Monte-Carlo fit
      (design D3) together with the consequence for `AIC()`/`BIC()`. This row
      is written before the others because it is why the table exists; review
      it on its own rather than as part of a filled grid.
- [x] 2.2 Write the refusal message if the verdict is `refuse`: name the class,
      say the quantity is a Monte-Carlo estimate, and point at what to use
      instead. Render via cli semantic elements per **r-lib:cli**; snapshot
      under a pinned cli context.

## 3. Parent and table together

- [x] 3.1 Introduce the parent class on the fit constructors. It lands in the
      same commit as the table (design D1) — never before it.
- [x] 3.2 Fill the remaining cells. Every `override` records its reason; every
      `refuse` records the reason its message states.
- [x] 3.3 Add the completeness test (design D4): read the table, enumerate
      generics and fit classes from the package, fail on any cell without a
      verdict, naming it.
- [x] 3.4 Verification: `air format` → `lintr` → `devtools::document()` →
      **not-cran-test**. The NAMESPACE diff is read: a method removed in favour
      of inheritance must not silently drop dispatch.

## 4. Close the leak

- [x] 4.1 Give `summary`, `tidy`, `glance` and `print.summary` their verdicts,
      so flavored fits stop silently lacking them.
- [x] 4.2 Decide `coef_layout`, currently flavored-only: `inherit` for the
      single class, or a deliberate `refuse` (design, Open Questions).
- [x] 4.3 Tests for each newly reaching generic on a flavored fit; snapshots
      for every `refuse` message, reviewed individually.
- [x] 4.4 Verification: `air format` → `lintr` → **not-cran-test**.

## 5. Close

- [x] 5.1 The completeness test passes with no undecided cell, and adding a
      throwaway generic makes it fail — verify the test can actually fail.
- [x] 5.2 `devtools::check()` clean; `openspec validate fit-class-hierarchy
      --strict`; `bash .plan/opsx-spec-placement-check.sh fit-class-hierarchy`.
- [x] 5.3 `NEWS.d/` fragment: flavored fits gain `summary`/`tidy`/`glance`;
      any generic now refusing is named. **Do not** bump `DESCRIPTION` or edit
      `NEWS.md` — merge-time folds (ADR-0040).
- [ ] 5.4 Final **not-cran-test**; frozen 1e-6 and C++ goldens PASS not SKIP.
- [x] 5.5 Update ADR-0038's `spec:` field to this change, and note in
      `abmcem` that the DyNES fit class must arrive with its column filled.
