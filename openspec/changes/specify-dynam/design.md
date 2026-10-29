## Context

infernet 0.6.0 reads one formula for an MRQAP, a goldfish model, and an ERGM.
Each `specify_<model>()` function reads the formula into infernet's terms,
which are goldfish's where the two share them, and writes those terms for its
model: one reader and one writer for each model, not one translation for each
pair. Each records what its model makes of each term, in one vocabulary:
`fits`, `adapted`, `interpreted`, `warns`, `drops`, and `refuses`. This change
puts the goldfish writer in goldfish.

## Decisions

### D1. `specify_dynam()` is proposed as the successor of `make_specification()`, not yet as its replacement

`specify_dynam()` returns the same `goldfishSpec`, so `estimate_dynam()` takes
either. It differs in three ways: the left-hand side names the layer, as in
`estimate_dynam(calls ~ ...)`, rather than `layer =`; one formula, or a list
named by sub-model, rather than `rate =` and `choice =`; and a formula written
for another model is read rather than refused.

Not decided here, and asked of the maintainers: whether `make_specification()`
becomes a superseded wrapper of `specify_dynam()` (lifecycle "superseded", no
warning), and whether DyNAM-i and flavor-keyed lists move to `specify_dynam()`
before that. Until then both stay, and `specify_dynam()` builds the
specification by calling `make_specification()`, so the two cannot disagree.

### D2. A term goldfish cannot fit is dropped and named, not refused

A formula written for another model should still run in goldfish without the
terms goldfish cannot fit, so that one term does not stop a multimodel
investigation. They are dropped with one `cli` warning that names each term and
its reason. A formula aborts only where no term is left. A name that no model
knows is left for `make_specification()`, which aborts, since it is probably a
typing error.

With `.data`, a main effect that the sub-model cannot identify, such as
`ego()` in a choice model, is dropped with the reason that
`validate_effects()` gives, where `make_specification()` aborts.

### D3. What each model's terms become

| Written for | Term | goldfish |
|---|---|---|
| ergm | `edges`, `density` | `1` in a rate model; dropped in a choice model |
| ergm | `mutual` | `recip` |
| ergm | `nodeocov("x")`, `nodeofactor("x")`, `b1cov("x")` | `ego(x)` |
| ergm | `nodeicov("x")`, `nodeifactor("x")`, `b2cov("x")` | `alter(x)` |
| ergm | `nodecov("x")` | `ego(x) + alter(x)` |
| ergm | `nodematch("x")` | `same(x)`; dropped with `diff = TRUE` or `levels =` |
| ergm | `absdiff("x")`, `diff("x")` | `diff(x)`, `diff(x, transformer_fn = identity)` |
| ergm | `edgecov("x")` | `tie(x)` |
| ergm | `ttriple`, `ctriple`, `cycle(4)`, `triangle` | `trans`, `cycle`, `four`, `triangle` |
| ergm | `gwesp()`, `sender`, ... | dropped |
| MRQAP | `exchange(x)`, `dist(x)` | `recip(x)`, `diff(x)` |
| MRQAP | a layer named on its own | `tie(layer, weighted = TRUE)` |
| MRQAP | `outdeg(x)`, `indeg(x, node = "ego")` | `outdeg(x, type = "ego")`, `indeg(x, type = "ego")` |
| MRQAP | `sim()`, `tertius()`, `tertius_diff()` | kept, and named as defined differently |
| MRQAP | `mix()`, `same(x, diff = TRUE)`, `levels =`, `reporter*()`, `(1 \| g)`, `\| g` | dropped |

infernet's `outdeg()` reads the sender by default, and goldfish's the
receiver, so an MRQAP `outdeg(x)` is written with `type = "ego"`.

### D4. The table of every effect is built from goldfish's effect functions

goldfish has no effect registry yet (`effect-term-registry` is open), so the
table reads the `update_<model>_<sub_model>_<effect>` functions. The first
formal says what an effect reads: `network`, `attribute`, or both, as
`tertius()`. Each is written out for the network: a network effect on its own
and with each other layer, and with `type = 'ego'` where it has a `type`; an
attribute effect for each node attribute, `diff()` and `sim()` for each
numeric one; `tertius()` for each layer and numeric attribute; `global()` for
each global variable. A label, a name, `active`, or a categorical attribute
with a different value for every node gets no effect. The closures through two
layers (`mixed_*()`) and `ego_alter_interaction()` are not listed, since they
need a pair of objects.

A row is kept where `make_specification()` of that effect alone fits it, so
the table and a written formula cannot disagree. An effect that the sub-model
cannot identify, such as `ego()` in a choice model, has no row. The table has
only the columns `term`, `effect`, `layer`, and `attribute`, as infernet's
does: it says which effects a user can choose, and a written formula gives the
reason for one that goldfish drops. A string argument is in single quotes, so
that the printed term has no escaped quotes.

`diff(x)` and `sim(x)` measure the same thing, since `sim()` is a negative
difference, so `formula()` of rows that hold both keeps the first and names the
other in a message. A formula without `.data` prints as a formula, with a line
only for each term that goldfish adapts, interprets, or drops.

### D5. The classes stay goldfish's own until the shared ones are decided

Every class goldfish attaches is `goldfish<Thing>` (`test-class_naming.R`),
so the table is a `goldfishTerms`, and without `.data` `specify_dynam()`
returns a formula of class `c("goldfishFormula", "formula")`, with `input` and
`record` attributes. infernet removes its own `specify_dynam()` and
`"dynam_formula"` once this lands, so no package masks the other's methods. A
formula moves between them by its names, which both read, and infernet's
reader also takes the class `"goldfishFormula"`. One shared class, and where it lives, is the roadmap's D17.

## Risks / Trade-offs

- The table makes one `make_specification()` call for each row: about 0.2
  seconds for the 46 effects of a choice model on `social_evolution`.
- A term that is rewritten changes its name in the fit: `nodematch("dept")`
  is `same(dept)` in `coef()`. The record keeps both.
