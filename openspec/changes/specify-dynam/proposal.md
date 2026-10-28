status: landed (feature/specify-dynam, awaiting fold)

## Why

`make_specification()` takes the rate and choice formulas as one-sided
formulas, with the dependent layer in `layer =`. That shape is goldfish's
alone: a formula written for an MRQAP in `{infernet}`, or for `{ergm}`, cannot
reach it, and a goldfish formula cannot move the other way. infernet 0.6.0
already reads one formula for three models, with one function for each model:
`specify_mrqap()`, `specify_dynam()`, and `specify_ergm()`. Its
`specify_dynam()` writes a formula for goldfish, but it lives in infernet, so
goldfish users do not have it, and infernet must follow goldfish's effects by
hand.

This change moves the goldfish writer into goldfish, as `specify_dynam()`, and
proposes it as the successor of `make_specification()`. It also gives goldfish
a table of every effect it can build for a network, as `RSiena::getEffects()`
does for a Siena data object, so that a user chooses effects from a table
rather than from the help pages.

## What Changes

- **New** `specify_dynam(formula, .data, input, model, sub_model,
  support_constraint)`, exported and marked experimental:
  - reads a two-sided formula, or a list of them named by sub-model; the
    left-hand side names the dependent layer, as in `estimate_dynam()`;
  - reads the model the formula was written for from `input`, the formula's
    class (`"ergm_formula"`, `"mrqap_formula"`, `"dynam_formula"`), or the
    names in it;
  - rewrites each term with a goldfish effect as that effect, keeps and names
    each term that goldfish defines differently, and drops and names each
    term that goldfish cannot fit;
  - with `.data`, drops a main effect that the sub-model cannot identify, with
    the reason of `validate_effects()`, and returns the `goldfishSpec` of
    `make_specification()`, with a `record` of each term;
  - without `.data`, returns the formula, of class `"goldfishFormula"`, with
    the record as an attribute.
- **New** table of every effect: `specify_dynam(.data = net)`, or
  `specify_dynam(calls ~ ., net)`, gives a tibble of class `"goldfishTerms"`,
  with one row for each effect written out for the network's layers and node
  attributes, and its status from `make_specification()` of that effect alone.
  `formula()` of chosen rows gives their specification; `tidy()` gives a plain
  tibble; `print()` uses cli.
- **New** `tidy()` for a `goldfishSpec`: one row for each term and its status,
  from the record, or `"fits"` for a specification of `make_specification()`.
- `make_specification()` is not changed, and not deprecated. Whether
  `specify_dynam()` replaces it, and on what schedule, is the decision this
  change asks for (design D1).

## Capabilities

### New Capabilities

- `formula-specification`: reading a formula written for any stocnet model
  into a goldfish specification, with a record of each term, and the table of
  every effect for a network.

### Modified Capabilities

- None. `make_specification()` and the formula parser are unchanged.

## Impact

- Code: `R/specify_dynam.R` (new), `NAMESPACE`, `man/specify_dynam.Rd`,
  `_pkgdown.yml`. No C++ change, and no change to estimation: a specification
  of `specify_dynam()` estimates to the same coefficients as the one of
  `make_specification()`, and the frozen 1e-6 baselines do not move.
- Tests: `tests/testthat/test-specify_dynam.R` (new).
- Related: infernet's `specify_dynam()` and its table of every term
  (`specify_mrqap(.data = net)`), whose conventions this change follows; the
  roadmap's decisions D16 (a shared specification object) and D17 (where the
  shared formula classes live). `effect-term-registry` would give the table its
  effects and kinds from the registry, rather than from the `update_*`
  functions, once it lands.
