## 1. The reader and the writer (D2, D3)

- [x] 1.1 Add `specify_dynam()` in `R/specify_dynam.R`: read the model a
      formula was written for, and rewrite each term operand by operand.
- [x] 1.2 With `.data`, drop an unidentified main effect with the reason of
      `validate_effects()`, and build the specification with
      `make_specification()`; add `record` and `input` to it.
- [x] 1.3 Without `.data`, return a `"goldfishFormula"` with `input` and
      `record` attributes (D5).
- [x] 1.4 Add `tidy.goldfishSpec()`.
- [x] 1.5 Verification: `devtools::document()`; tests in
      `tests/testthat/test-specify_dynam.R` pass.

## 2. The table of every effect (D4)

- [x] 2.1 Write out each effect of `model` and `sub_model` for the network's
      layers and node attributes, from the first formal of its `update_*`
      function.
- [x] 2.2 Keep a row only where `make_specification()` of that effect alone
      fits it, with the columns `term`, `effect`, `layer`, and `attribute`.
- [x] 2.3 Add `print()`, `tidy()`, and `formula()` for `"goldfishTerms"`.
- [x] 2.4 Verification: tests pass; `NOT_CRAN=true` suite with the frozen
      baselines reporting PASS.
- [x] 2.5 Keep one of `diff(x)` and `sim(x)` in `formula()`, and print a
      `"goldfishFormula"` with a line only for each term that is not `"fits"`.

## 3. Documentation

- [x] 3.1 Help page with an experimental lifecycle badge, and the topic in
      `_pkgdown.yml`.
- [x] 3.2 Fragment `NEWS.d/specify-dynam--specify-dynam.md`; no Version bump
      and no `NEWS.md` edit on the branch.

## 4. At the fold (integration branch)

- [ ] 4.1 Decide D1: whether `make_specification()` is superseded by
      `specify_dynam()`, and whether DyNAM-i and flavor-keyed lists move first.
- [ ] 4.2 Fold the fragment into `NEWS.md`, bump the Version, and archive this
      change.
