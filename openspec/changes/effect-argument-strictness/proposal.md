## Why

Tracked as stocnet/goldfish#121.

The formula parser discards an unrecognized effect argument in silence. Two
independent paths do it, and neither counts nor reports the drop:

```r
# R/formula_parser.R — signature binding
.args_replace <- pmatch(names(parms_to_set), .args_names)
.signature[na.omit(.args_replace)] <- parms_to_set[!is.na(.args_replace)]
#                ^^^^^^^ NA for an unknown name; na.omit discards it

# R/utils.R — object resolution
ids <- isReservedElementName(names(objNames)) | names(objNames) == ""
objNames <- objNames[ids]
```

So `inertia(calls, transformFun = sqrt)` estimates exactly as though the argument
had never been written. `transformFun` is the pre-1.7.0 name of `transformer_fn`,
one of five effect arguments the camelCase retirement renamed with no
deprecation shim and no `NEWS.md` entry.

An AST sweep of the repo's own corpus found **21 sites, 72 occurrences**, all
`subType` (now `sub_type`) — including nine terms in the DyNAM-i vignette and the
frozen DyNAM-i rate baseline. That baseline asks for `"centered"` age, never
binds the argument, and was fit on **uncentered** age.

Eight of the nine affected effects request their own default, so correcting them
changes nothing. `ego` is the exception, and it is why this is not cosmetic.

## What Changes

- **Effect argument names are matched exactly** and an unmatched name is an
  error. `pmatch`'s prefix acceptance goes with it (`transformer = sqrt` binds
  today; it will not).
- **A retired-name map**, as data rather than prose: the five 1.7.0 renames
  produce an error naming the *replacement*, not a generic unknown-argument
  message.
- **The 21 corpus sites migrate** to `sub_type`, and the DyNAM-i vignette is
  re-knit.
- **The DyNAM-i M1 rate baseline is corrected and re-frozen** — two intercepts,
  derived before the run and recorded in `tests/testthat/_baselines/README.md`
  (ADR-0021).
- **`NEWS.md` gains the announcement** that 1.7.0 skipped, naming both spellings
  for all five renames.

**BREAKING**: a formula using a retired or misspelled effect argument stops
estimating. Those formulas were already producing models their authors did not
specify, so the break surfaces a defect rather than creating one.

## Capabilities

- `effect-argument-validation` (new) — exact name matching, retired-name
  reporting, and the rule that an argument goldfish cannot honor is an error.

## Impact

- Code: `R/formula_parser.R` (signature binding), `R/utils.R`
  (`get_data_objects`, `isReservedElementName`).
- Tests: `test-dynami_baselines.R` (two inline intercepts re-frozen),
  `test-compute_statistics.R`, `test-dynami_bridge.R`, `test-dynami_surface.R`,
  `test-residuals_recompute.R`.
- Vignettes: `dynami-example.Rmd.orig`, re-knit.
- Docs: `NEWS.md`, retrospective entry for the 1.7.0 renames.
- **Not** `_baselines/*.rds` or `global_v1/` — the DyNAM-i baselines are inline
  literals in the test file. The `regen-baselines` skill governs the `.rds` floor
  and is not the route here.
- Related: extracted from `effect-term-registry` D25 so it can land before
  2.0.0; the registry later absorbs this into the `term_def` argument schema
  without a second migration.
