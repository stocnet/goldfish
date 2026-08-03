# Tasks — tied-event-times

Disciplines (openspec/config.yaml): one focused conventional commit per task,
tests green at every commit, `devtools::document()` inline when roxygen /
exports / signatures change, `air format` the touched R files before `lintr`,
`NOT_CRAN=true` with the frozen baselines PASS (not SKIP) before each commit,
r-lib skills (r-package-development, testing-r-packages, cli, lifecycle) invoked
before the work they cover.

**Framing:** goldfish supplies the mechanism, the researcher supplies the
analysis. Nothing here performs multiple imputation or pools fits — see design
D3 and D4 for why that is deliberate rather than deferred.

## 0. Ground the design before building

- [ ] 0.1 Reproduce the measurement on a second dataset. The fisheries figure
      (12 of 69 dependent events tied, 13 of 241 intervals with `dt == 0`) is
      from one fixture; confirm the shape of the problem elsewhere and record
      whether ties cluster (a coarse clock) or are isolated (true simultaneity),
      because D1's "warn, do not refuse" rests on both being plausible
- [ ] 0.2 **Settle D3's mechanism against the event-stream code before writing
      any.** Read `R/event_streams.R` and `R/data_source.R` and decide among a
      secondary sort key, a documented stable sort with an exposed pre-sort hook,
      and an explicit order column — the three differ in how much of the stocnet
      contract they touch. Record which, and what it costs, in design.md. If the
      cheapest option cannot satisfy "a supplied order survives to estimation",
      say so rather than weakening the requirement
- [ ] 0.3 Answer the design's first open question: does a tied right-censored
      interval need the same treatment as a tied dependent event? There is one in
      the fisheries fixture and it contributes to neither likelihood term

## 1. Report the problem

- [ ] 1.1 Count tied dependent events (`dt == 0`) where the event schedule is
      built, and record it on the fitted object for every family. Roxygen
      `@return` entry; `devtools::document()` inline
- [ ] 1.2 Warn on the exact-time families only, via cli, naming the count and
      what it means for the rate scale. Read
      `identifiability-diagnostics` task 5.1's warning first and make the two
      read as siblings — both answer "should I trust this fit?" and a user may
      see both at once
- [ ] 1.3 Tests: the warning fires on tied exact-time data and is silent both on
      untied data and on every multinomial family; the recorded count is zero
      rather than absent on untied data; snapshot the warning with a pinned cli
      context

## 2. Make an explicit order possible

- [ ] 2.1 Implement whatever 0.2 settled, so an order among tied events is
      representable in the data object and readable from it before fitting
- [ ] 2.2 Preserve that order through preprocessing and estimation, with no
      silent re-sort at any hop. The test that matters is end-to-end: two data
      objects differing only in the order of one tied group produce fits that
      differ exactly where that order changes what a later event observes
- [ ] 2.3 Tests: the three scenarios of the `single-data-object` delta, including
      that processing stays deterministic when no order is supplied

## 3. Document the researcher's workflow

- [ ] 3.1 Vignette or article section: why tied times break the exact-time
      assumption, how to read the warning, and a worked recipe for treating the
      order as missing data — generate orderings, refit, combine — using only
      exported surface. State plainly that goldfish deliberately does not pool
      for you, and why (design D4)
- [ ] 3.2 Cross-reference from `set_algorithm_newton()` / the estimator docs so a
      user meeting the warning can find the recipe

## 4. Closure

- [ ] 4.1 NEWS entry: the warning and the recorded count as additive; the
      ordering mechanism as whatever 0.2 made it. DESCRIPTION version bump
- [ ] 4.2 Full `NOT_CRAN=true` suite green with the frozen baselines PASS (not
      SKIP), `openspec validate` green, and the spec-delta placement check green
      (`bash .plan/opsx-spec-placement-check.sh tied-event-times`)
