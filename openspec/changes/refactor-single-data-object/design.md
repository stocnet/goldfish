## Context

goldfish data today: `make_data()` returns an **environment** (`data.goldfish`) holding
`nodes.goldfish` / `network.goldfish` / `dependent.goldfish` objects whose event streams
hang off `attr(obj, "events")`; preprocessing builders resolve everything through
`get(name, envir = prepEnvir)`. Two-mode models use two *separate* node-set data frames
with per-set local indices (`preprocess_builders.R:109`, n1×n2 matrices), and the formula
parser auto-sets `is_two_mode` on effects (`formula_parser.R:552-575`).

manynet's `stocnet` class (`manynet/R/class_stocnet.R`) is a list of tibbles
(`nodes`, `ties`, `changes`, `global`) plus an `info` metadata list. Its validator
(`class_validate.R`) checks *shape only*: `ties` requires just `from`/`to`
(integer-indexed into the single nodes tibble); `time`, `layer`, `weight` on ties and
`update`, `directed`, `focal`, `observation`, `sender`, `receiver` in `info` are
**reserved, not required**, and `time` may even be `character` or `mdate`. Coercions
arrange ties by `from`/`to`, losing event order. Construction workflow examples live in
`~/Documents/sp2026/goldfish_asta/code/plan/single_object_examples.R`.

Sequencing: `refactor-formula-parsing` (archived 2026-07-02) provides
`make_specification(..., data)` and the layer-name resolution seam;
`support-constraint-risk-set` (archived 2026-07-07), `support-constraint-as-stat`
(archived 2026-07-10), and `refactor-likelihood-compute` (archived 2026-07-12) have all
landed on `refactor/rate_prep`: the `active_1`/`active_2` mask factors, the shared
effect/constraint name resolver, and the builder layer (`build_state_container`,
`build_update_plan`) this change wires into all exist on the branch. manynet is already
in Suggests (DESCRIPTION). Core decisions were made with the user on 2026-07-03;
D9–D12 were added in the 2026-07-12 review session — there are no open questions.

## Goals / Non-Goals

**Goals:**
- One data input: a stocnet object, consumed directly or via `as_goldfish()`, replacing the
  environment-of-objects representation for DyNAM and REM.
- A goldfish-side validator that narrows stocnet's reserved-but-optional contract into
  required, class-checked, order-deterministic input, failing early with cli errors.
- Conversion exclusively at the builder layer: recipe loops, writers, estimation, and the
  C++ core untouched; 1e-6 coefficient equivalence with the legacy path.
- Legacy constructors alive for one deprecation cycle as stocnet-assembling wrappers whose
  warnings show replacement code.

**Non-Goals:**
- DyNAMi / `make_groups_interaction()` (deferred to the DyNAMi engine change).
- Snapshot-diff panel semantics and between-wave augmentation (DyNES change; only the
  per-layer flag seam is reserved).
- Multivariate / per-layer formula designation beyond `info$focal` (multivariate change).
- Data *manipulation* verbs in goldfish (filtering, joining, binding stay in manynet).
- The opportunity list — it is `set_preprocessing_opt(opportunities_list =)`, a
  preprocessing option deprecated by `support-constraint-risk-set`, never data.

## Decisions

### D1 — Dual input path; validate-and-stamp; manynet in Imports (amended 2026-07-13)
`estimate_dynam()` / `estimate_rem()` / `make_specification()` accept a raw stocnet in
`data`, validating on the fly; `as_goldfish(x)` is the optional early gate. Both call the
**same internal validator**, which *stamps* the object (subclass marker) rather than
restructuring it — downstream builders see one input either way. **The stamp never skips
re-validation** (amendment; the original decision let it): manynet verbs mutate in place
and preserve the class vector (`bind_ties.stocnet` does `out <- .data`,
`manip_ties.R:101`), as does plain list assignment, so a stamped object is not trustably
"already checked". Validation runs unconditionally at specification/estimation time — it
is vectorized column/class/pool checks plus one duplicated-key pass, milliseconds against
a preprocessing loop that visits every event. The stamp's roles are provenance and
`print.data.goldfish()` dispatch; `as_goldfish()`'s roles are early-failure UX and
legacy-environment conversion (D9). manynet moves to **Imports, pinned `>= 2.1.0`**
(2026-07-13 amendment, reversing the earlier Suggests decision): the D6 wrappers must
work unconditionally and delegate to `manynet::make_stocnet()`/`from_ties()` rather than
re-implementing `index_ties()`-style assembly, and the deprecation messages tell users to
run manynet code — recommending a possibly-absent package is broken UX. Usage rule:
`@importFrom` when a function is used across functions or in hot paths; `manynet::fn()`
for occasional calls; **never `manynet:::`** (cross-package `:::` on unexported objects is
flagged by R CMD check and effectively CRAN-forbidden — manynet's `snet_abort()` helpers
are internals, so goldfish keeps its own cli calls, mimicking manynet's phrasing style
only). The same `::`/`@importFrom` rule applies to other dependencies (tibble, dplyr
verbs where measurably faster than base). The validator still reads plain list components
structurally (tibbles accepted as data.frames) and the hand-built fixtures remain the
API-drift guard. *Rejected:* a converting `as_goldfish()`
that mints a restructured goldfish class — a second representation to maintain, while the
heavy per-model work (state, schedule) happens in preprocessing anyway; staying in
Suggests — the wrapper path can't reuse manynet code and its deprecation messages would
recommend an uninstalled package; stamp-skips-validation with a
documented don't-mutate warning — users will pipe stamped objects through manynet verbs,
and a fingerprint check adds machinery for no gain over just validating.

### D2 — Deterministic ordering; reserved `order` column; abort on true ambiguity
stocnet coercions lose row order (`arrange` by `from`/`to`), so conversion sorts events by
a documented key: `time`, then dependent-before-exogenous, then component order
(ties → changes → global), then layer, then `from`/`to`. When ties/changes carry an integer
**`order`** column it is the final tie-break instead of `from`/`to`. One unrecoverable
case aborts: multiple `replace` events targeting the same cell/node-var at the same time
with no `order` column — silent arbitrary resolution would be wrong data; increments
commute and stay silent. Proposed upstream (non-blocking): manynet reserves `order` and
preserves it through coercions.

### D3 — Strict time contract; `time = NA` is history
`time` is required on every event layer and on `changes`/`global`; classes limited to
numeric / POSIXct / Date (converted to numeric internally). `character` and `mdate` abort
with conversion guidance. Integer wave times are allowed only when **all** layers live on
that axis — mixed incomparable time axes (POSIXct focal + integer-wave panel) abort,
because "state of the most recent wave at each event time" is undefined across axes.
`time = NA` tie rows are pre-observation history written into the initial state matrices
(the convention the construction examples already use; goldfish documents that it blesses
it, since manynet does not validate it). Rows timestamped before `start_time` fold into
initial state through the existing startTime logic.

### D4 — Observation window from arguments, defaulting to the focal span
Explicit `start_time` / `end_time` arguments (specification/estimation surface) define the
observation window; absent those, the focal layer's dependent-event span. No new info
metadata convention. *Rejected:* reserved `info$begin`/`info$end` — a convention manynet
doesn't validate, adding a third precedence level for little gain.

### D5 — Panel layers: change-list semantics for DyNAM/REM; flag reserved for DyNES
A `observation = "panel"` layer enters as an exogenous dyadic covariate whose rows are
**updates applied at their wave time** (per `info$update`); dissolutions must be explicit
value-0 rows (documented — not statically detectable). Wave updates emit right-censored
statistic changes like any exogenous event. A panel layer cannot be `focal` (the error
points to SAOM/RSiena/DyNES for panel-dependent processes); `window` on a panel-layer effect
aborts (expiry would reset all panel ties). `observation` values other than
`event`/`panel` abort as unmodeled. A **per-layer panel-semantics flag is reserved, not
implemented**: the future DyNES model owns the snapshot interpretation, diffing waves and
augmenting with latent formation/dissolution events between waves via MCMC/EM (its own
OpenSpec change). Same seam pattern as the interaction-terms reservation: define the slot,
build no machinery. Snapshot→change-list diffing as a plain data transform is proposed
upstream as a manynet/netrics verb.

### D6 — Legacy constructors become stocnet-assembling wrappers; warnings show code
For one deprecation cycle the legacy constructors keep their signatures and behave
equivalently, but internally assemble the same stocnet representation the direct path
consumes:

```
make_nodes(df)                 → nodes tibble (label/active validated)
make_network(mat, nodes)       → single-layer stocnet; matrix entries → history ties (time = NA)
link_events(x, events, …)      → bind_ties()/bind_changes()-equivalent rows
make_global_attributes(df)     → global tibble rows
make_dependent_events(ev, net) → ties rows on a layer + info$focal
make_data(...)                 → from_ties()-style merge into one stocnet
```

Each emits a lifecycle deprecation (`deprecate_warn`, badge, NEWS) whose message body is a
**cli code block showing the equivalent replacement code with the user's actual object
names** (e.g. `make_network(m, nodes = actors)` suggests
`as_stocnet(m) |> join_nodes(actors)`). The exported `make_data_goldfish()` alias
(`make_data.R:866`) follows `make_data()` automatically — deprecated but working, and
like every wrapper it **returns the new stocnet structure**, so a fresh session can never
mint a legacy environment. Named exceptions: `make_groups_interaction()`
(DyNAMi path, untouched until its engine change) and the 1.7.0 camelCase aliases in
`goldfish-defunct.R` (already-deprecated renames chain to the new warnings — no second
wrapper layer).

### D7 — Two-mode: declared `info$sender`/`receiver` mode *sets*; mode map to local indices (amended 2026-07-13)
The engine keeps its two local index spaces (n1×n2 matrices, per-mode composition,
`is_two_mode` auto-set) — re-indexing to stocnet's global node ids would touch the C++
gathers for no semantic gain. Conversion owns a **mode map** (global node id ⇄ (side,
local id)). `info$sender`/`info$receiver` are **sets of `nodes$mode` values** (manynet's
`validate_info()` pools them against mode names with no length constraint, so
multi-valued declarations pass upstream); a side is the union of nodes whose mode is in
its set:

- **Identical sets** (`sender` == `receiver` as sets) → **one-mode over that subset** of
  nodes (e.g. friendship among `c("employees", "supervisors")` while other modes exist in
  `nodes`). The former single-value "declare `sender = receiver = "person"` to restrict"
  case is just a size-one set.
- **Disjoint sets** → **two-mode**: `from`/`to` remap to (sender-side local,
  receiver-side local) in n1×n2; goldfish validates **side purity**
  (manynet's validator does not) and errors naming the layer and offending nodes.
  The focal layer's side pair defines the model's `nodes`/`nodes2`; other layers may carry
  other pairs (existing mixed-effect validation applies to the remapped objects).
- **Partially overlapping sets abort** with a cli error: nodes indexed in both local
  spaces make effect semantics (reciprocity-style mixing) and mask symmetrization
  ill-defined. Express such designs as an identical-set one-mode layer plus a
  `support_constraint`.
- A layer **without** the declaration is **one-mode over all nodes** (global = local,
  `nodes ≡ nodes2` for that layer) — ties are never inspected to infer modes; the
  `nodes$mode` column alone is an ordinary attribute. Documented callout: in a multimodal
  object an undeclared layer spans *all* modes; declare identical sets to restrict.
- One nodes tibble serves both sides' attributes: `ego(size)` reads the sender-side slice,
  `alter(size)` the receiver-side slice of the same column; NAs flow into the existing
  missing-data imputation.
- **Original identity is never lost**: the mode map (global node id + `label` ⇄ (side,
  local id)) is carried onto the preprocessed/estimation result and the gather/db exports
  (which already surface `index_i`/`index_j` local indices), so postestimation
  (residuals, event scores, exported long tables) can always recover `nodes` ids/labels.
- Composition splits by the node's mode into `active_mode1`/`active_mode2` — exactly the
  `active_1`/`active_2` factors the support-constraint mask assembly (its design D10)
  consumes.
- `directed` is vacuous on a two-mode layer (no symmetry concept in n1×n2); validation
  notes and ignores it, and mask symmetrization (support-constraint D11) never applies.

### D8 — Dev-plan reconciliation (added 2026-07-03)
The guiding interface proposal (`goldfish_asta/code/plan/goldfish_dev_plan.md`) predates
the manynet stocnet implementation; where they diverge, **the implemented manynet names
supersede the plan**: `info$focal` (not `dependent`), `info$observation` (not
`layer_type`), `info$receiver` (not `recipient`), `nodes$active`. These names are owned by
manynet and were **never released to goldfish users, so no lifecycle treatment applies** —
the plan is simply superseded. Further reconciliations:

- **`as_goldfish()` supersedes the plan's `make_data_goldfish()`** as the boundary: the
  constructor role the plan's examples sketch is fully covered by
  `make_stocnet()`/`as_stocnet()` (manynet) plus the D1 validate-and-stamp boundary — a
  goldfish-owned constructor would duplicate `make_stocnet()` with the validator bolted
  on. The already-exported `make_data_goldfish()` alias is NOT removed: it rides the D6
  wrapper treatment (deprecated, returns the new structure).
- **Both-sides-initiate two-mode layers** (plan §nodes note: e.g. firms and universities
  both initiating collaborations): *(superseded 2026-07-13 by the D7 amendment)* —
  multi-valued mode **sets** on `sender`/`receiver` now express this natively: declare
  identical sets `sender = receiver = c("firm", "university")` for a one-mode layer over
  that union (restricting to cross-mode dyads still uses a `support_constraint`). Nodes
  still carry exactly one `nodes$mode` value each; it is the *declaration* that is
  multi-valued, which manynet's `validate_info()` already admits.

### D9 — Legacy environment input: error at estimation, convert in `as_goldfish()` (added 2026-07-12)
After the flip, a legacy `data.goldfish` **environment** (only obtainable from a saved
`.rds`/`.RData`, since the D6 wrappers no longer produce one) passed to
`make_specification()`/`estimate_*()` **aborts** with a cli error pointing to
`as_goldfish()` as the one-line migration. `as_goldfish(legacy_env)` **converts**: it walks
the environment's `nodes.goldfish`/`network.goldfish`/`dependent.goldfish`/global objects
and their `attr(x, "events")` streams and assembles the equivalent stocnet, then validates
and stamps as usual. Detection is `is.environment(data)` — the stamp reuses the class name
`data.goldfish` prepended to the stocnet class vector (`c("data.goldfish", "stocnet", …)`),
keeping the exported class-name continuity while `is.environment()` cleanly separates the
legacy shape; `print.data.goldfish()` is rewritten for the list shape. *Rejected:* erroring
everywhere (strands users' saved objects for no implementation savings — the conversion
reuses the same assembly the D6 wrappers need anyway).

### D10 — Prebuilt stocnet datasets; Rd examples load them; human-readable times (added 2026-07-12)
The shipped datasets gain **prebuilt stocnet objects** — `social_evolution` and
`fisheries_treaties` — built by `data-raw/` scripts following the recipes in
`goldfish_asta/code/plan/single_object_examples.R` (`from_ties()` + `join_nodes()` +
`add_info()`; the shipped object is plain list/tibbles). Rd examples across the package
**load the prebuilt objects** instead of constructing data, so examples avoid the
deprecated constructors (no lifecycle noise in checks); the *construction* workflow is
shown once in the dataset help pages and as a dedicated vignette section (no
`requireNamespace` guard needed — manynet is in Imports per amended D1).
The raw `Social_Evolution` data frames keep their names and columns but `calls$time` /
`friendship$time` convert from numeric Unix epochs to **POSIXct with `tz = "GMT"`**
(2008 data, human-readable). This is coefficient-neutral — `as.numeric(POSIXct)`
round-trips to the identical epoch seconds — so the frozen 1e-6 baselines remain valid
and MUST still PASS, not be regenerated.

### D11 — Vignettes stay precompiled; teaching sources rewritten (added 2026-07-12)
`vignettes/teaching1.Rmd.orig` / `teaching2.Rmd.orig` are rewritten to the stocnet
workflow (prebuilt datasets + `make_stocnet()`/`as_stocnet()` construction where
didactically useful) and re-knit through the existing `vignettes/precompile.R` flow to
regenerate the `.Rmd`/`.R` outputs. **Precompilation is retained for this change**: the
teaching vignettes fit ~10 models each, and dropping the `.orig` flow would couple a
build-infrastructure decision to an already large refactor while CRAN rebuilds
non-precompiled vignettes on every check. Whether the post-`refactor-likelihood-compute`
speedups make live vignettes viable is measured and decided at release-prep, not here.

### D12 — Dedicated cleanup session precedes implementation (added 2026-07-12; amended 2026-07-13)
The codebase carries ~262 code comments referencing OpenSpec artifacts (design IDs,
task numbers) across ~30 files — dangling pointers once changes are archived. Session 0
of this change sweeps them all, **inlining the actual rationale** (assumption, edge case,
equation) in place of the reference, plus a DRY pass over the files this change touches.
The DRY pass includes one named extraction (2026-07-13 amendment): the
verbatim-duplicated recipe-loop setup opening `run_sender_recipe_loop()` and
`run_dyad_recipe_loop()` (`model_preprocess.R:361-447` / `1379-1447` — spec unpacking,
`realize_derivations()`, `fetch_events()`, start/end-time resolution,
`imputeMissingData()`, `initializeCacheStat()`) moves into a **shared context
constructor** both drivers call, behavior identical. Beyond de-duplication, this is the
seam the downstream changes build on: `flavored-processes` generalizes the walk to
multiple formula-plan consumers over one shared state (its D3), and
`dynes-augmentation` adds the generative source port (its D9) — both start from the
extracted context rather than re-touching duplicated setup. Commits gated by a clean
`grep -rn "design D[0-9]\|(task [0-9]" R/ src/ tests/testthat/` and a green
`NOT_CRAN=true` run with the frozen baselines PASS.

### D13 — Validator treats manynet as untrusted; syntactic names required (added 2026-07-12)
manynet's `validate_stocnet()` follows a reserved-not-required pattern (`reserved_cols()`:
"if present, check class/pool") — beyond `ties$from`/`to` almost nothing goldfish needs is
guaranteed: `info$update`/`observation` carry no names/length check against the layers
actually present, `info$directed` is not validated at all, `nodes$label` uniqueness is
unchecked, `time` admits character/mdate, `changes$value`/`global$value` are unchecked
list-columns, and mode purity of ties is never tested. The goldfish validator therefore
checks **everything it consumes on every entry path**, assuming nothing from upstream:
named per-layer coverage of `update`/`observation`/`directed` against the distinct
`ties$layer` values, label uniqueness, the D3 time contract, per-side mode purity,
list-column value unwrapping with per-`var` type consistency, and the D2 ordering
ambiguity abort. Additionally, layer and attribute-variable names MUST be syntactically
valid R names (`make.names(x) == x`), aborting with a rename suggestion — a validated
object is always expressible in effect/`support_constraint` formulas without backticks.
*Rejected:* warn-only on non-syntactic names (leaves validated-but-unformulatable
objects) and parser-side backtick support (more work, uglier formulas).

The validator is built by **generalizing the existing `R/class_checks.R` machinery**, not
from scratch: `check_classes()` (`methods::is()`-based, S4-aware — preferred over bare
`inherits()`), `check_columns()` (mandatory/optional names + per-column class lists),
the label rules from `check_nodes()`, the per-`var` value-type matching from
`check_events.nodes.goldfish()` (replace class vs attribute `typeof`), sortedness/NA
rules from `check_events.network.goldfish()`, and `check_presence()` composition
consistency all map onto stocnet components. Components are accepted as plain
`data.frame`s (tibbles pass, being data.frames) — no pillar dependency is needed (it
arrives transitively via tibble, already in Imports) and hand-built fixtures stay
dependency-free.

### D14 — No bridge era: the `envir` seam becomes the data object (added 2026-07-13)
The transition is a clean cut, not a coexistence: there are **no version-bridging helper
functions**. `build_spec_map()` is already metadata-pure — object keys, derivations
(window, `ignore_rep`), the fetch plan, and the support-mask compilation all resolve
*names* through one seam, the `envir` argument consumed by `get(name, envir)` at state
creation. That seam is **replaced, not paralleled**: `build_object_keys()`,
`build_derivations()`, `compile_support_constraint()`, and the fetch plan take the
stamped data object and resolve layer/attribute names against
`data$ties`/`data$nodes`/`data$global`; the `envir` parameters are deleted. Promises
(derived inputs) keep their laziness — realized at state creation, now from data
components. Because the D6 wrappers assemble stocnet from day one, no legacy shape exists
in-tree to resolve; the model-specification "bridge era" `dependent.goldfish` lookup is
removed rather than maintained. Consequence for sequencing: the whole test suite builds
data through the legacy constructors, so the **wrappers must land in the same milestone
as the builder/surface flip** — the flip is one commit series (wrappers + builders +
acceptance) gated by the frozen baselines, which are the legacy-coefficients reference
(no in-tree old path is needed for equivalence).

### D15 — Event schedule: split stocnet components into the existing per-stream walk (added 2026-07-13)
Neither raw component walking nor a monolithic copied event stack: the recipe loop
already merges **multiple per-object event streams** by pointer (the `fetch_plan`
materializes them inside state creation). Conversion splits `ties`/`changes`/`global`
into exactly those per-layer / per-variable streams — local indices from the D7 mode
map, numeric time per D3, sorted once per D2 — and hands them to the unchanged
multi-stream walk. `ties`/`changes` cannot be walked directly (they need remapping and
deterministic re-ordering anyway), and a single stacked copy would duplicate what the
fetch plan already organizes.

### D16 — Update methods become the initial-state materializer; state-at-t keeps a public face (added 2026-07-13)
The vectorized update engine in `R/methods_update.R` (dedup-last `replace`, `tapply`
increment aggregation — no event loop) is **kept and promoted**: it becomes the internal
initial-state materializer that, given component streams and a `[start_time, t)` range,
produces state matrices / attribute vectors in one vectorized pass. It **replaces the
current per-event `startTime` fold** in preprocessing and applies equally to derived
promises (window, `ignore_rep`) — the premise being that updating effect inputs and
re-running effect init is cheaper than replaying the event stream, because the update is
vectorized. The same mechanism is the documented **seam for future parallel chunked
preprocessing** (split the stream at time points, materialize each chunk's initial state)
— seam only, not built here. Surface: the legacy S3 faces
(`as.matrix.network.goldfish`, `as.data.frame.nodes.goldfish`) deprecate with the
constructors; goldfish exports **new state-at-t helpers on the stocnet/stamped object**
(network state and node attributes evaluated at time `t`, honoring update semantics and
`time = NA` history) for vignettes and users. manynet's `to_time()` generic exists but
has no stocnet method (its `tbl_graph` method is wave-based; the dynamic case is
unimplemented), so a `to_time.stocnet` upstream contribution is **explored as one of the
last tasks** — the internal materializer variant stays regardless (inside preprocessing
it works on ready state objects + streams, a different entry point than the user-facing
helper).

### D17 — Formula attribute syntax: `ego(var)` resolves against components; `df$var` deprecated (added 2026-07-13)
With one data object, the `ego(actors$floor)` syntax loses its purpose: nodal attributes
resolve against `nodes` columns and global attributes against `global$var` values through
the shared resolver (candidates listed on error). The `df$` prefix follows D6's
keep-working promise: during the deprecation cycle the parser **accepts `anything$var`,
drops the prefix, resolves `var`, and deprecation-warns once** (pointing to `ego(var)`);
after the cycle it aborts. New validator: an attribute name that matches both a `nodes`
column and a `global` variable aborts as ambiguous (rename one); effect functions
reading global attributes resolve from `global` only.

### D18 — Strict snake_case naming policy (added 2026-07-13)
All functions, arguments, and R objects use **snake_case** (tidyverse style) — confirmed
as the strict policy; the 1.7.0 renames already retired camelCase from the exported API
and are never reversed. Internals still in camelCase (`prepEnvir`, `linkEnvir`,
`GetDetailPrint`, …) migrate **whenever a file is touched** (the session-0 DRY pass and
every subsequent task apply it to files they modify). Enforcement: `.lintr` gains
`object_name_linter("snake_case")` (session 0), the policy is recorded in the project
`CLAUDE.md`, and a memory entry enforces it across sessions. New user-facing names in
this change (`as_goldfish`, `start_time`, `end_time`, state-at-t helpers) are snake_case
by construction.

### D19 — Minimal flavor support: reserved `flavor` column, one modeled flavor (added 2026-07-13)
Deprecating `make_dependent_events()` breaks the Fisheries pattern (creation events
modeled, dissolution events state-only: legacy filters
`bilatchanges[increment == 1, ]` into the dependent object while the full change list
updates the network), and "focal-layer rows → dependent events" cannot express it — the
change would fail its own equivalence gate. The dev plan's flavor mechanism lands
**minimally**:

- **Reserved `ties$flavor` column** (character; optional; values must be syntactic R
  names per D13 since they appear as formula-list keys — **American spelling**, see the
  CLAUDE.md language policy; proposed upstream, non-blocking, that manynet reserve
  `flavor` alongside `order`).
- **`make_specification()` accepts a flavor-keyed list** for `rate`/`choice` (e.g.
  `rate = list(creation ~ 1 + indeg())`): closes the spec/implementation gap left by
  `refactor-formula-parsing`, whose landed spec text promises the list syntax that
  `build_specification_bundle()` never implemented. **Exactly one modeled flavor** in
  this change — multiple keys abort pointing to the future DyNES/multivariate change
  (stacked per-flavor likelihoods are new engine work). When both `rate` and `choice`
  are lists they must key the same flavor. Focal rows whose flavor is not the modeled
  key (including `NA` flavor) **update state only**; all focal rows keep updating state
  per `info$update` regardless.
- **Plain formula on a flavored focal layer models ALL rows** with a `cli_inform` noting
  the layer carries flavors and every row is modeled — supports category-style flavors
  modeled together without ceremony, while making the forgot-the-list case visible.
- **The `make_dependent_events()` wrapper marks flavor**: it joins the supplied events
  to the default network's layer rows (time, sender, receiver, update value) and stamps
  matches with `flavor = <the dependent object's name>`; the legacy call surface
  (dependent-object name in formulas/`layer`) resolves to (focal layer, that flavor
  key) internally — no inform fires, and legacy estimation is row-for-row equivalent.
  Dependent rows with no matching layer row: on increment layers they are added as
  flavored increment-0 rows (no-op state change); on replace layers the wrapper aborts
  with guidance (the legacy semantics — dependent events that never touch state — has
  no replace-safe encoding).
- **Deferred**: `add_flavor()` (value→flavor mapping sugar) and auto-derived
  mutually-exclusive support constraints — their own change; the prebuilt
  `fisheries_treaties` dataset ships with `flavor` set from `increment` (+1 = creation,
  −1 = dissolution) in `data-raw/`, so no helper is needed for the flagship example.

### D20 — American English for the user interface (added 2026-07-13)
User-facing names — functions, arguments, reserved column values, and documentation
prose — use **American English** (`flavor`, `color`, `-ize`): recorded in the project
`CLAUDE.md` and the naming memory. Note the coordination seam: manynet uses British
spellings in places; goldfish-owned surface is American regardless, and the upstream
`flavor` reservation proposal carries the spelling explicitly.

## Risks / Trade-offs

- **Order loss upstream is permanent** → D2's abort catches the harmful case
  (same-target same-time replaces); everything else is deterministically re-sorted, and the
  reserved `order` column gives users an escape hatch. Upstream proposal to manynet filed
  as a nice-to-have, not a dependency.
- **Dual path drift** (raw stocnet vs `as_goldfish()`) → one shared validator that runs
  unconditionally on both entries (the stamp never bypasses it, per amended D1); tests run
  every conversion scenario through both entries, including post-stamp mutation.
- **Wrapper equivalence regressions** → the legacy constructors' wrappers are gated by the
  same 1e-6 coefficient-equivalence tests as the direct path (Social_Evolution, Fisheries),
  with `NOT_CRAN=true` baselines PASS not SKIP.
- **Silent semantics on undeclared multimodal layers** (one-mode over all nodes may
  surprise) → documented callout + the mode-purity error whenever a declaration exists;
  no inference means no silently-wrong structure.
- **manynet API drift** (now Imports) → version pinned `>= 2.1.0`; goldfish still
  validates the structure it reads rather than trusting manynet internals, and the
  hand-built fixture stocnets keep the unit tests independent of manynet's release
  cadence. Trade-off accepted: manynet's transitive tree (igraph, network, tidygraph,
  dplyr, pillar) becomes a hard dependency — the price of DRY wrappers and honest
  deprecation messages.
- **Flip milestone is large** (D14: wrappers + builders + acceptance land together) →
  the commit series inside the milestone stays per-task with tests green at each step;
  the frozen baselines gate the whole series, and rollback is reverting the series.
- **Panel dissolution rows are unverifiable** → change-list semantics documented
  prominently with the panel examples; the DyNES seam is where inference will eventually
  live.

## Migration Plan

1. Boundary first (validator, stamp, `as_goldfish()`, conversion module incl. mode map,
   ordering, streams, initial-state materializer), behind tests on hand-built fixtures —
   the legacy pipeline untouched and green throughout this stage.
2. **The flip (one milestone, D14)**: legacy constructors become stocnet-assembling
   wrappers, builders swap the `envir` seam for the data object, and
   `estimate_*()`/`make_specification()` acceptance flips — a single commit series gated
   by the frozen baselines (the legacy-coefficients reference). No bridge code exists at
   any commit.
3. Datasets, examples, vignettes, docs on the new surface.
4. Rollback = revert the flip series; before the flip, the legacy path is untouched.

## Open Questions

*(none — core decisions resolved 2026-07-03; D9–D13 in the 2026-07-12 review sessions;
the D1/D7 amendments and D14–D20 in the 2026-07-13 sessions)*
