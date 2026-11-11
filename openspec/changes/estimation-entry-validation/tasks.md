Every implementation unit is an (a)/(b) pair. (a) writes the failing test,
confirms RED, and commits the test alone. (b) writes the minimal code, confirms
GREEN, and commits after `air format` + `lintr` on the touched files,
`devtools::document()` when roxygen changed, and the not-cran-test skill. Tests
follow r-lib:testing-r-packages; the new warning follows r-lib:lifecycle and
r-lib:cli, and its snapshots pin a cli context. No task touches per-event
code: the validator runs once per call, before preprocessing (design,
"Storage and per-event cost"), so no group opens with a cost guard. The
frozen baselines must stay PASS; every suite fixture validates (design,
Context). Feature-branch fold: a `NEWS.d/` fragment, no DESCRIPTION bump, no
`NEWS.md` edit, no archive. Code comments carry the reasoning, never a
decision ID.

## 1. The formula path validates and warns

- [ ] 1.1(a) In a new `tests/testthat/test-estimation_entry_validation.R`,
      build a valid hand-made one-mode stocnet fixture and an invalid copy with
      its `info$directed` entry removed. Test (spec "Post-stamp mutation is
      caught on the formula path", "An invalid object warns on every call",
      "Raw stocnet accepted directly"):
      - `estimate_rem()`, `estimate_dynam()` and `compute_statistics()`, each
        given a formula and the invalid object, raise
        `lifecycle::expect_deprecated()`, and one `expect_snapshot()` of the
        warning text;
      - `estimate_dynami()` with an invalid two-mode actors × groups object
        warns before the stocnet-to-environment bridge runs;
      - two calls in a row each warn (not silenced by lifecycle's default
        frequency);
      - the valid fixture raises no condition from the check.
      Confirm RED (no warning today). Commit the test alone.
- [ ] 1.1(b) Add the internal `validate_or_warn_goldfish_data()` to
      `R/validate_goldfish.R` (design D3: `validate_goldfish_data()` under
      `tryCatch()`, on error `lifecycle::deprecate_warn(when = "2.0.0", what =
      I(...), details = <the validator's message via cli>, always = TRUE)`).
      Add `data_validated = FALSE` to `estimate_wrapper()` and call the helper
      right after `check_estimation_data()` when `data` is a stocnet list and
      `data_validated` is `FALSE`, with `focal = NULL` (D1, D2, D4). GREEN.
- [ ] 1.2 Pin "the result equals the one without the check" (spec scenario
      "Post-stamp mutation is caught on the formula path", D6). For the
      invalid fixture, compare `estimate_rem()` coefficients and
      `compute_statistics()` output with the helper active against the same
      calls with the helper mocked to a no-op (`local_mocked_bindings()`);
      `expect_identical()`. GREEN by design after 1.1(b); it guards against a
      later change that alters the data on the warn path. Commit the test
      alone.
- [ ] 1.3 Close group 1: NOT_CRAN=true full suite, baselines PASS (not SKIP),
      no failure; grep the suite log for the warning's `what` text and record
      the count in progress.md (expected 0, design Context). Commit.

## 2. A specification validates once, and substitute data is validated

- [ ] 2.1(a) Test (spec "A specification estimated against its own data
      validates once", "A specification estimated against substitute data
      validates that data"). Count calls to `validate_or_warn_goldfish_data()`
      through a mocked binding that records and delegates: a specification
      built on a valid object and estimated with no `data` makes 0 calls, and
      with `data =` the same object 0 calls; estimated with `data =` an
      invalid different object, 1 call and the deprecation warning. Also: a
      flavored specification estimated through `estimate_flavored` makes 0
      calls. Confirm RED (the default validates every time, so the first
      counts are 1). Commit the test alone.
- [ ] 2.1(b) Pass `data_validated` from the three internal callers (D2):
      `estimate_from_specification()` passes `is.null(data) ||
      identical(data, spec$data)`; `estimate_flavored` (both calls,
      `R/estimate_flavored.R:123`, `:189`) and the DyNES warm start
      (`R/estimate_dynes.R:616`) pass `TRUE`. Add a one-line comment at each
      saying why the data is already validated. GREEN.
- [ ] 2.2 Close group 2: NOT_CRAN=true full suite, baselines PASS (not SKIP).
      Commit.

## 3. Documentation, the old test, and the milestone

- [ ] 3.1 Replace `test-as_goldfish.R:20` ("post-stamp mutation is caught on
      re-validation"), which calls `validate_goldfish_data()` by hand, with two
      tests through the entries: a stamped object edited into invalidity
      aborts in `make_specification()` (spec "Post-stamp mutation is caught at
      specification") and warns in `estimate_rem()` with a formula. Its
      comment goes with it, since the functionality it explains moves to the
      new tests. Run the file; commit.
- [ ] 3.2 Reword the header comment at `R/as_goldfish.R:4` to what the code
      does (D7): `make_specification()` validates and aborts; the formula path
      of `estimate_dynam()`/`estimate_rem()` validates and, during the
      deprecation period, warns. The rest of the comment is kept. Commit.
- [ ] 3.3 Document the `data` argument once. `R/model_estimate.R:141` and
      `:866` both still read "a `data.goldfish` object created with
      [make_data()]". Replace the first with the stocnet wording and the
      validation behavior (validated on every call; an object failing
      validation warns now and will abort in a future release, see
      [as_goldfish()]); make the second inherit it (`@inheritParams`). Run
      `devtools::document()`, confirm the rendered `man/` pages, commit.
- [ ] 3.4 Write `NEWS.d/estimation-entry-validation--formula-path.md` per
      `NEWS.d/README.md`: the formula path of `estimate_dynam()`,
      `estimate_rem()`, `estimate_dynami()` and `compute_statistics()` now
      validates a stocnet `data` object; an object that fails validation
      raises a deprecation warning and will abort in a later release; declare
      what the message asks for, or gate with `as_goldfish()`. Each line at
      most 80 characters. Commit.
- [ ] 3.5 Close group 3: NOT_CRAN=true full suite, baselines PASS (not SKIP),
      `openspec validate --strict`, placement check (1 placement, exit 0).
      Add `status: landed (<branch>, awaiting fold)` to proposal.md. Then run
      the conformance-inspector skill by a fresh agent before the change is
      trusted or folded. Commit.
