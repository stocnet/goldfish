> **Lands on `feature/dynes`** after `abmcem` and `process-simulation`, whose
> code holds the two call sites (`R/estimate_dynes.R`, `R/simulate_methods.R`).
> The delta targets only the living `intercept-only-rate` spec, so no
> `depends-on` is declared.

> **TDD.** Implementation tasks run as red/green pairs: an "(a)" task writes a
> failing test and commits it RED, the paired "(b)" task makes it pass and
> commits GREEN (per `openspec/config.yaml` `rules.tasks`).

> **Changelog.** Warning text is user-facing, so group 3 writes one `NEWS.d/`
> fragment. On this feature branch there is no `DESCRIPTION` bump, root
> `NEWS.md` edit or archive (deferred to the merge, `rules.merge`).

## 1. The pin records its source (design D1)

- [x] 1.1a Write failing tests: a completed rate on an unflavored,
      event-observed relational layer is pinned with `source = "event_count"`;
      one on a modeled panel layer, and one on a flavored relational layer, with
      `source = "hamming_diff"`. Read the source off
      `complete_generative_spec(...)$completed_rates`. Also assert that the
      pinned intercepts are identical to the pre-change values (the source is
      metadata, not a new computation). Run to confirm RED; commit the tests
      alone.
- [x] 1.1b Add a `source` argument to `make_intercept_only_rate()` (validated
      against the two values, stored on the object) and pass it from
      `pin_completed_rates()` off its existing `relational` flag. `air format`
      and `lintr` the touched files; re-run to GREEN; commit.

## 2. One warning per completed rate, worded by source (design D2–D4)

- [x] 2.1a Write failing tests (`r-lib:testing-r-packages`, pinned cli context):
      at `estimate_dynes()` entry, a completed rate on the relational fixture
      (`pinned_entry_join()`) raises exactly **one** warning that names
      `emails › rate`, states the observed-event-count source, carries the
      no-SE / excluded line, and contains no "Hamming"; the same spec through
      `simulate()` raises one warning with the same source line and no
      standard-error language; a panel-layer fixture under `estimate_dynes()`
      states the Hamming-diff source. Assert that one condition is caught by a
      handler on either `goldfish_pinned_rate_warning` or
      `goldfish_completed_default_warning`, and that a completed **choice**
      carries only the latter. Run to confirm RED; commit the tests alone.
- [x] 2.1b Implement per `r-lib:cli`: `complete_generative_spec()` passes
      `completed$completed_rates` and `completed$process_map` into
      `warn_completed_defaults()` alongside `added` and the consumer. For each
      `added` row with `family == "rate"`, resolve its fid with an NA-safe
      `(layer, flavor)` match against `process_map` (design D5 — not `==`,
      which drops unflavored rows on the `NA` flavor compare), then read that
      fid's `source` off `completed_rates`. Append the source line and, for
      `estimate_dynes`, the no-SE / excluded line to the rate row, and raise it
      with both classes. `warn_pinned_rates()` skips fids whose bundle is
      marked `completed`. Interpolate data into cli markup, never paste literal
      markup. `air format` and `lintr`; re-run to GREEN; commit.
- [x] 2.2a Write failing tests for the standalone warning on a hand-marked
      specification (the existing `mark_pinned_rates()` fixtures, with the
      bundle's `completed` mark cleared): it names the process by its
      `layer [› flavor] › rate` label, not `Rate <fid>`; a known source adds the
      matching provenance line, an unknown one (`NULL`) omits it; the no-SE line
      appears only for `estimate_dynes`. Run to confirm RED; commit the tests
      alone.
- [x] 2.2b Change `warn_pinned_rate()` / `warn_pinned_rates()` to take the
      label and the source, keeping the consumer only for the estimation line.
      Update the section comment above them so it describes the
      observation-keyed provenance (it currently says the wording follows the
      consumer). `air format` and `lintr`; re-run to GREEN; commit.

## 3. Snapshots, changelog, and closing

- [ ] 3.1 Re-record the `estimate_dynes()` / `simulate()` entry snapshots in
      `tests/testthat/_snaps/intercept_only_rate.md` (and any other snapshot the
      suite reports as changed). Review the diff line by line: only the
      provenance line, the label, and the dropped second warning may change.
      Commit the snapshots alone.
- [ ] 3.2 Write one `NEWS.d/` fragment per `NEWS.d/README.md`: a completed
      pinned rate is announced once, names its process by label, and states the
      count source it was pinned from. Commit.
- [ ] 3.3 Closing: `not-cran-test` full suite (NOT_CRAN=true), frozen baselines
      PASS not SKIP; `bash .plan/opsx-spec-placement-check.sh
      pinned-rate-warning-source` exits 0; add `status: landed (feature/dynes,
      awaiting fold)` to `proposal.md`; commit.
