## Context

`estimate_dynes()` (`abmcem`) and `simulate()` (`process-simulation`) both need
to supply parameter values against a `joint_specification.goldfish`, and neither
change defined the shape. Rather than each inventing its own `initial_parameters`
/ `coef` layout, one shared surface lives with the spec that owns the fid
vocabulary — the `multivariate-specification` capability (from the archived
`make-multivariate-spec`). Archived changes are immutable history; this is a
**new** change whose delta ADDs to the living `multivariate-specification` spec.

The reuse substrate already exists:

- The `process_map` makes the integer **`fid` the canonical consumer identity**
  and renders **human-readable labels** from its `layer` / `flavor` / `sub_model`
  columns (living `multivariate-specification`, "The process_map extends across
  processes"). `build_joint_process_map()` is the single source of fid ordering.
- Each joined spec models a **distinct focal layer** (`s$focal`, unique by
  construction), so the focal layer name is a ready-made unique **process id** —
  no user-supplied process names are needed.
- `formula_parser` emits `offset_coef_parameter` per effect — the per-effect
  **fixed (offset) mask** — and `make_specification()` already carries offset
  terms inside each formula.
- The result-display machinery already exists to reuse: `GetFixed()` and
  `stats::.vcov.aliased()` NA-pad and toggle fixed positions today
  (`methods_postestimate.R`).

## Goals / Non-Goals

**Goals:**
- One shared, self-validating parameter object (`parameters.goldfish`) both
  consumers accept, authored against readable labels, robust to block
  mis-alignment.
- A single, spec-derived rule for what `NA` means, so users never hand-classify
  fixed vs free.
- A `coef_layout()` generic serving authoring, the parameter object, and result
  rendering.

**Non-Goals:**
- Estimation, simulation, or the EM loop themselves (their changes).
- Parameter uncertainty draws (θ ~ N(θ̂, vcov)) — a `gof-dynes` extension.
- Accepting a bare list / flat vector on the **joint** consumer surfaces — v1
  accepts only `parameters.goldfish` there (a coercion convenience is recorded
  future work).
- Touching the single-process surfaces. `estimate_dynam()` / `estimate_rem()`
  and single-spec `simulate()` are **unchanged** — they keep numeric
  `initial_parameters` / `coef` / `offset_coef`. This change adds surface for
  joint specs only; it renames nothing and forces the object on no existing API.
- Subset/nested cross-process coupling label grammar (follows the spec's own
  deferral).

## Decisions

### D1 — `set_parameters(spec, ...)` returns a self-validating `parameters.goldfish`

`set_parameters(spec, ...)` takes a `joint_specification.goldfish` and one
**full-length per-fid vector** per process sub-model. The returned
`parameters.goldfish` object (class name per the `<noun>.goldfish` house
convention, matching `specification.goldfish` / `result.goldfish`) carries the
validated per-fid values, the flat free-parameter θ projection, and the layout
metadata; it validates **once** at construction so both consumers trust it
without re-validating. *Rejected:* a bare named flat vector (no self-validation,
fragile downstream); a nested per-process list mirroring the composition (harder
to project to `coef()` names).

**Scope — the object is required only where the specification is joint.**
`set_parameters()` takes a `joint_specification.goldfish`, and the ambiguity it
resolves (the same effect name recurring across fids, unlabeled block
concatenation) exists **only** for multivariate/flavored compositions. So the
object is required exactly at the two joint-spec surfaces:
`estimate_dynes(initial_parameters=)` (always joint) and `simulate(coef=)`
**when its input is a `joint_specification.goldfish`**. It is **not** forced
anywhere else and changes no existing signature: the single-process estimators
(`estimate_dynam()` / `estimate_rem()`) keep their plain **numeric**
`initial_parameters` and formula/`offset_coef` handling, and `simulate()` on a
single `specification.goldfish` (or a fitted single-process result's θ̂) keeps a
plain **numeric** `coef`. A single-process spec has one fid, so its parameter
vector is already unambiguous — wrapping it in `parameters.goldfish` would add
ceremony with no disambiguation to earn it.

### D2 — Readable composite label grammar `layer[:flavor]:sub_model` *(superseded by D7)*

Each `...` argument is keyed by a **readable composite label** projected from the
`process_map`: the **focal `layer`** is the process id (unique by construction),
`:flavor` is included **only** when a layer carries multiple flavors (elided for
single-flavor processes), and `:sub_model` names the fid within the process. So a
plain process reads `friendship:rate` / `friendship:choice`; a flavored one reads
`calls:outgoing:rate`. Labels resolve to fids through the `process_map`; an
unresolvable or ambiguous label aborts naming the valid labels. *Rejected:*
integer-fid keys (opaque, unstable to authors); positional order (fragile).

### D3 — Per-fid vector is full-length in formula order; names optional

Each per-fid vector SHALL have **one entry per effect of that fid, in formula
order** — full length, including slots for offset (fixed) effects. Its length
must equal the fid's effect count (a mismatch aborts, naming the fid and the
expected length). Per-effect **names are optional but all-or-nothing**: a
vector is either fully positional (no names) or fully named. When named, every
name is validated against the fid's effect labels (disagreement aborts); a
**partially named** vector — some slots named, some bare (`c(inertia = 0.5,
0.2)`) — is **rejected**, because guessing whether the bare slots are positional
or name-defaulted is exactly the silent mis-alignment this change exists to
remove. Full-length-with-slots is self-checking and lets `NA` be
classified positionally against the offset mask (D4). *Rejected:* free-effects-only
vectors (cannot be length-checked against the fid; force the user to pre-classify
fixed vs free — exactly what D4 removes).

### D4 — `NA` disambiguation: the offset always prevails

The one semantic rule, resolved entirely from the spec's per-effect offset mask
(`offset_coef_parameter`), so the user never hand-classifies:

| slot in the per-fid vector        | offset in formula? | outcome                                   |
|-----------------------------------|--------------------|-------------------------------------------|
| `NA`                              | yes                | **fixed** at the spec's offset value       |
| non-`NA` value                    | yes                | **warn**, value ignored, **offset wins**   |
| `NA`                              | no                 | **free** (estimate) / **incomplete** (sim) |
| non-`NA` value                    | no                 | that value pins the free slot              |

**Offset always prevails**: a slot the formula marks `offset()` is fixed at the
spec's value regardless of what `set_parameters()` receives; a non-`NA` value
there warns once and is dropped. A `NA` at a **non-offset** slot is **free** — the
parameter `estimate_dynes()` estimates, and the slot `simulate()` treats as
incomplete (D5). This is why `abmcem` drops `fixed_parameters` from
`set_algorithm_em()`: the fixed set lives in the spec's formula offsets, not on the
EM control.

### D5 — Complete-vs-partial contract

`parameters.goldfish` tracks a **complete** flag: complete iff every **non-offset**
slot carries a non-`NA` value (offset slots are always "resolved" by the spec).
The two consumers place different obligations on the same object:

- `estimate_dynes()` reads the free (non-offset `NA`) slots as the parameters to
  estimate — partial is the normal case; a fully complete object is a valid fully
  pinned start.
- `simulate()` **asserts completeness** and aborts on any remaining free `NA`,
  naming the free effects — the parallel to `walk_open()` asserting generative
  completeness (living `multivariate-specification`, "The walk handle asserts
  completeness").

### D6 — `coef_layout()` generic over spec, parameters, and result

`coef_layout()` is a small S3 generic returning **one row per effect** with
columns: `fid`, process **label** (D2), `sub_model`, `flavor`, effect **name**,
`fixed` (logical, from the offset mask), the **offset value** for fixed rows
(`NA` for free rows), and `index` — the position of free effects in the flat θ
(`NA` for fixed rows, which are not in θ). Methods:

- **`joint_specification.goldfish`** — the empty layout (all values `NA`), so
  users can discover labels/names/order to author `set_parameters()` before they
  have values. **Completion-aware (D16a):** on a **raw** spec it spans the
  authored fids only; on a **completed** spec (`complete_generative_spec(spec)`)
  it also renders the autocompleted fids' rows as `fixed = TRUE` with their frozen
  values — the full pre-fit walked layout.
- **`parameters.goldfish`** — layout plus the supplied values and the
  complete/free classification.
- **a joint/DyNES fitted result** — layout plus θ̂ and SE, the projection
  `summary()`/`print()` use to group the flat coefficient vector back into
  per-process blocks. Single-process results are already single-block and need
  no such method.

`coef()` / `vcov()` on results stay **flat** (base-generic contract: named
vector / matrix), names being the D2 labels; the multivariate structure is a
**rendering** concern `coef_layout()` supplies, not a change to the generics.

### D7 — Label resolution is membership against rendered labels, never string-parsing

The composite label is an *identity the user types* into `set_parameters(...)`,
which collides head-on with an invariant the codebase states twice: labels are
**rendered for reading, never parsed back as data**, because layer/flavor are
arbitrary user strings that may carry the separator or collide
(`methods_postestimate.R` "labels for reading, never identity"; `preprocess_flavored.R`
"a pasted key … cannot be parsed back … never read back as data"). So this design
resolves keys **safely**: `set_parameters()` renders every fid's label once and
matches each `...` key by **set membership** against that rendered set — it SHALL
NOT split the key on the separator to recover `(layer, flavor, sub_model)`. A key
that matches no rendered label, or matches more than one (a name carrying the
separator, or a collision), lands in the existing **ambiguous → abort naming the
valid labels** path rather than mis-resolving silently. *Rejected:* string-parsing
the key back into components (the exact anti-pattern the two comments forbid).

**Reuse the canonical rendered label, do not mint a second grammar.** The existing
`render_process_label()` already emits `layer › flavor › family` and is already what
`coef()` / `vcov()` / `print()` name flavored components. D2's `layer[:flavor]:sub_model`
would be a *second* grammar for the same objects, so a user reads
`friendship › creation › rate` from `coef(fit)` but must key
`friendship:create:rate` into `set_parameters()` — different strings for one fid.
`set_parameters()` SHALL therefore key on the **same rendered label** the result
surfaces use (via `render_process_label()`), which (a) makes membership resolution
above safe by construction and (b) unlocks the **fit → re-simulate round-trip**:
`do.call(set_parameters, c(list(spec), coef(fit)))` aligns because both sides speak
one label vocabulary. This supersedes D2's separate `:`-grammar and its flavor
elision; the D2 spec delta (`specs/multivariate-specification/spec.md`, the
"per-fid vectors keyed by readable label" requirement and the flavor-elision
scenario) SHALL be reconciled to the rendered-label vocabulary before implementation.
Note the rendered label uses `family`
(`rate`/`choice`), not the finer `sub_model` (`choice_coordination`/`rate_ordered`),
which is redundant for identity and diverges from every existing label; `sub_model`
survives as a **descriptive `coef_layout()` column**, not part of the key.

### D8 — A joint spec enforces an inline `coef=` on every offset at build (resolves OQ1)

D4 ("offset prevails — the object carries the offset value") and D6 ("offset value
for fixed rows") both read the fixed value from the spec. **Code-confirmed premise:**
for a multi-process spec that value can come from *only one* place — the inline
`offset(term, coef = v)` captured in `offset_coef_parameter` — because the
algorithm-control route is closed (`estimate_flavored.R:61` rejects `offset_coef`
for a multi-process specification, and `:68` rejects `fixed_parameters`). The
"every offset needs a value" abort lives in `assemble_fixed_parameters()`
(`formula_validate.R:580`), whose **sole caller is `model_estimate.R:2475`** — i.e.
it fires only at **estimate time**. Neither `make_specification()` nor
`make_joint_specification()` calls it, so a bare `offset(term)` sails through build
with `offset_coef_parameter = NA` (`formula_parser.R:208`), leaving
`coef_layout(joint_spec)` with no offset value to show and `set_parameters()`'s
warn-and-ignore nothing to carry.

**Decision:** `make_joint_specification()` SHALL, at **build**, reject any joined
process whose offset term lacks an inline `coef=` (assert `offset_coef_parameter`
non-`NA` at every `offset_parameter` position of every fid). This is the correct
layer because joining is the exact moment the deferral route closes: a bare
`offset(term)` is legal in a standalone `make_specification()` (defer to
`offset_coef`), but the instant it is joined that escape hatch is gone, so failing
early is honest. **Message refinement (do not reuse the estimate-time abort):**
`assemble_fixed_parameters()`'s message offers
`set_algorithm_newton(offset_coef = ...)` as an alternative — the very route that
is closed for joint specs. The build-time `cli_abort` SHALL point **only** to
`offset(term, coef = value)`, mirroring `estimate_flavored.R:64`, or it contradicts
itself. *Rejected:* deferring to estimate time (leaves D4/D6 unable to rely on the
value at spec/parameters construction).

### D9 — The canonical per-fid effect list is the coefficient layout, not the raw parser mask (resolves OQ2, correcting it)

D3's full-length per-fid vector, its length check, and the D4 NA classifier must
all index one per-effect list. **Code finding:** there are genuinely **two**
indexings, bridged by `intercept_shift`:

1. **rhs-space** — the parser's per-effect masks (`offset_parameter`,
   `offset_coef_parameter`, `estimate_parameter = is_main | is_offset`) are
   `as.list(...)` over `rhs_names`: **no intercept prepended, operand-only
   interaction terms included** (`formula_parser.R:204-216`).
2. **coefficient-space** — `coefficient_term_labels()` = `[intercept?,
   rhs effects…, interactions…]`, length `n_params = length(rhs_names) +
   length(interactions) + intercept_shift` (`formula_validate.R:509-513`). This is
   the list `coef()` names and the estimation loop indexes; `assemble_fixed_parameters()`
   projects the offset mask into it via `offset_positions <- which(is_offset) +
   intercept_shift` (`:515`).

The OQ2 text proposed anchoring to `offset_coef_parameter`'s indexing (rhs-space).
**That is the wrong anchor** — it would misalign every rate fid by the intercept
and drop the interaction columns, breaking both authoring against `coef()` names and
the D7 fit → re-simulate round-trip. **Decision:** the per-fid vector, its
length check, and the D4 classifier SHALL anchor to the **coefficient layout**
(`coefficient_term_labels()`, length `n_params`) — the same list `coef()` surfaces.
The fixed/offset mask the classifier reads SHALL be the mask **projected into
coefficient space** (reuse the `intercept_shift` projection
`assemble_fixed_parameters()` already computes; do not re-derive from the raw
rhs-aligned parser slots). Consequence for D4: "fixed" is broader than `offset` —
an **operand-only** interaction term is a coefficient-space slot fixed at `0`
(`values[which(!estimate)+intercept_shift] <- 0`, `formula_validate.R:604`), so it
joins the same "fixed / offset-prevails" bucket (NA accepted, non-NA warns-and-ignored),
and `coef_layout()` marks it `fixed = TRUE` with offset value `0`. The **Intercept**
(rate) is a free coefficient-space slot the user may pin for `simulate()`.

### D10 — One `NA`-eliding label renderer is canonical (resolves OQ3)

D7 keys `set_parameters()` on the label `coef()`/`print()` surface. **Code finding:**
the tree has two renderers that disagree on **both** the `NA`-flavor case and the
present-flavor format:

- `render_process_label()` (`preprocess_flavored.R:722`) —
  `paste(layer, flavor, family, sep = " › ")`, **no `NA` guard**, so a non-flavored
  process renders `friendship › NA › rate`. This is what `coef()`/`vcov()`/`print()`
  emit today (`methods_postestimate.R:156` → `flavored_component_labels`).
- `walk_handle.R:89` — `paste0(layer, (flavor), " › ", family)`, eliding `NA` flavor
  and **parenthesizing** a present one: `friendship › rate` / `calls (creation) › rate`.

So even with a present flavor the strings differ (`layer › flavor › family` vs
`layer (flavor) › family`), and the reconciled D7 spec ("flavor elided when the
process carries none") matches walk_handle, not the renderer `coef()` actually uses.
**Decision:** `render_process_label()` becomes the **single** canonical renderer and
is taught `NA`-flavor elision (drop the flavor segment when `NA`), keeping its
`›`-segment form (the vocabulary `coef()` already surfaces and the round-trip needs).
`walk_handle.R`'s parenthesized ad-hoc format is retired — its completeness message
routes through `render_process_label()`. This shifts `coef()`/`vcov()`/`print()`
names for a non-flavored joint component from `friendship › NA › rate` to
`friendship › rate` (a visible improvement, not a regression). An implementation
detail with a user-visible label consequence; it lands with task 1.2.

### D11 — The parameter object has two products; one canonical order; autocompleted defaults join the fixed bucket (resolves the free-vs-full gap)

`parameters.goldfish` carries **two** projections because the two consumers need
different vectors:

- **Free-parameter θ** — the concatenated free slots, in the canonical order
  below. `estimate_dynes()` estimates these (a partial object's `NA` free slots)
  or warm-starts from them (a pinned free slot). Offsets are **not** here — they
  live in the spec (D4/D8), which is exactly why `estimate_dynes()` needs no full
  vector: the fixed values are read from the joint spec, not supplied on the
  parameter object.
- **Full per-fid coefficient vectors** — every coefficient-space slot resolved to
  a value: the user's pins for free slots plus the spec-resolved fixed values.
  `simulate()` consumes these to drive the walk and **asserts the full vector is
  complete**, aborting on any remaining free `NA` naming the effect. An offset has
  no generative meaning in a forward walk, so `simulate()` reads each fixed value
  but requires every *free* slot to be pinned — a full coef vector is the
  simulation's initial parameters, or it errors.

**Canonical order (the round-trip spine).** Both projections and `coef()` on a
fit concatenate in one order: **`process_map` fid order, then coefficient order
within each fid** (`coefficient_term_labels()` = `[Intercept?, effects,
interactions]`), with fixed slots skipped for the free-θ projection. This one
order is what makes `coef(fit)` names, `set_parameters()`'s slots, and the θ̂
vector align — the substrate of the D7 fit → re-simulate round-trip.

**Autocompleted sub-models join the fixed bucket.** `complete_generative_spec()`
fills a missing sub-model with a **zero-free-parameter** default — a pinned
intercept-only rate, or a uniform choice / coordination / ordered draw
(`complete_generative_spec.R:678-684`, "zero free parameters"). Such a fid has
**no free slots**; its value is frozen by the spec exactly as an offset's is. So
it broadens the D9 fixed bucket a third time (offset ∪ operand-only-at-`0` ∪
**autocompleted default**): `set_parameters()` needs no vector for it (an omitted
key is legal — see D13), `coef_layout()` marks its rows `fixed = TRUE` with the
pinned value, and `simulate()`'s completeness passes over it. This is what "the
parameters are already described by the intercept-only and multivariate specs"
means concretely — they are not user-suppliable and not in θ.

### D12 — `coef_layout()` rows are coefficient-space; intercept and interaction naming follow `coef()` (resolves the row-granularity gap)

Because D9 anchored the per-fid vector to coefficient space, `coef_layout()`
returns **one row per coefficient-space slot** (length `n_params`), **not** "one
row per effect": the **Intercept** row (for a rate sub-model that carries one),
the effect rows, and the interaction rows. The `name` column mirrors exactly what
`estimate_dynam()`'s `coef()` already surfaces via `coefficient_term_labels()`
(`formula_validate.R:451`): the rate intercept is named **`"Intercept"`**, an
interaction is named by its `label`. If `estimate_dynam()` does **not** surface an
intercept for a sub-model, `coef_layout()` does not invent one. Where an
autocompleted-default slot has no `coef()`-surfaced name, the placeholder
**`"1"`** is used for now.

### D13 — An omitted process key means all-free

`set_parameters(spec, ...)` does not require a vector for every fid. A process
whose key is **omitted** is read as **all-`NA` → all-free** — every non-fixed slot
of that fid is a free parameter. This is coherent with the complete-vs-partial
contract (D5): for `estimate_dynes()` a partial object with omitted processes is
the normal warm-start case; for `simulate()` the completeness assertion (D11) then
aborts naming those unfilled free effects. A fid that is entirely fixed (all
offset / operand-0 / autocompleted default) is trivially complete when omitted.

### D14 — The fit → re-simulate round-trip is a from-result method, not a flat splice (resolves NQ2)

D7's aspirational `do.call(set_parameters, c(list(spec), coef(fit)))` cannot be the
literal mechanism: `coef(fit)` is a **flat, free-only** named vector, whereas
`set_parameters()`'s primary input is **full-length per-fid vectors** keyed by the
process label — different shapes — and, worse, the flat vector's per-parameter
names collide across fids (`inertia` recurs in three processes), which is the exact
ambiguity this whole change exists to remove. **Decision:** the round-trip is a
**from-result method with the signature `set_parameters(spec, result)`** — the
spec is supplied explicitly (keeping the `set_parameters(spec, ...)` first-argument
contract uniform) and the fitted joint/DyNES **result** is passed in place of the
per-fid vectors. `set_parameters()` reconstructs the per-fid vectors from the
result's own `coef_layout()` (which already carries `fid`, slot, and the canonical
order); it SHALL first assert the result was fit **against that same spec** — the
result's fid vocabulary/`coef_layout()` must match `spec`'s — and abort otherwise,
so a result spliced onto a mismatched spec cannot silently mis-align. No name
disambiguation is needed because the layout keeps the fid grouping the flat
`coef()` vector throws away. The flat-splice form is **not** supported in v1.
*Rejected:* a bare `set_parameters(result)` / `as_parameters(result)` with no
explicit spec (breaks the uniform first-argument contract and removes the
spec/result consistency check). *Rejected:* teaching the flat splice to regroup by fid
(re-derives the grouping `coef_layout()` already has, and still cannot resolve a
name that legitimately collides across fids).

### D15 — `simulate()`'s value gate and `walk_open()`'s structural gates are complementary, at different layers (resolves NQ1)

`walk_open()` runs two **structural** assertions over the *spec's shape*, never a
parameter value (`walk_handle.R`): `assert_generatively_complete()` (every modeled
flavor carries both a rate and a choice — a skipped `complete_generative_spec()`)
and `assert_walkable_submodels()` (no effect-free sub-model — the auto-supplied
defaults the generative consumers walk themselves). The D11 **parameter-value**
gate asks a different question — is every *free coefficient* pinned — over the
*parameters.goldfish*, never the spec shape. They are **complementary, not
redundant**: neither can fire for the other's reason, so there is no double-abort.

**Order inside `simulate()`:** `complete_generative_spec(spec)` →
**parameter-value gate (D11)** → `walk_open(completed_spec)`. Because completion
runs first, `walk_open()`'s generative-completeness assert is a **backstop that
passes** on `simulate()`'s own path (it bites only a direct `walk_open()` caller),
so the value gate is the one that yields the actionable error ("free effect X
unpinned") and correctly runs first. **This change ships only the value gate** (the
task-2.2 acceptance helper); `walk_open()`'s structural asserts already exist at
the spec/walk layer and are **not** duplicated here. The exact interleaving of the
helper call and `walk_open()` inside `simulate()` is `process-simulation`'s wiring;
this decision fixes only the contract (two distinct gates, value-first, no overlap).

### D16 — The parameter object is defined over the *authored* fid set; autocompleted fids resolve at consumer entry (resolves the completion-sequencing gap)

**Code-confirmed premise:** `make_joint_specification()` does **not** call
`complete_generative_spec()` — completion is a **consumer-entry** step
(`walk_handle.R:46` "runs once at the consumer entry"; also inside
`model_estimate.R`). So the raw `joint_specification.goldfish` a user passes to
`set_parameters()` contains only the **authored** fids, not the sub-models
`complete_generative_spec()` will later synthesize, and its `process_map` has no
`completed` column yet (that column is set *by* completion,
`complete_generative_spec.R:127`). Two facts forbid folding autocompleted defaults
into the parameter object at authoring time: (a) `coef_layout(joint_spec)` and the
object's two projections (D11) are built from the raw `process_map`, which has **no
rows** for fids that do not yet exist; (b) for the **timed** regime the
autocompleted intercept-only rate's pinned value is computed from
`wave_times`/event counts at completion (`pin_completed_rates()`,
`complete_generative_spec.R:136`), so it is **not knowable** at authoring time.

**Decision:** `set_parameters()` and `coef_layout(joint_specification)` operate on
the **raw authored** spec and SHALL NOT call `complete_generative_spec()` (so the
user eats no completion warning at authoring time, and none is double-emitted
against the copy `simulate()`/`estimate_dynes()` complete internally). The
`parameters.goldfish` object — both projections (D11) and the complete flag (D5) —
is defined over the **authored** fid set only. Autocompleted-default fids are a
**consumer-time** concern: at consumer entry the spec is completed, and the
completeness/estimation logic treats any fid **not present in the parameter
object** — an autocompleted default the user never keyed, carrying zero free
parameters — as **trivially resolved**: it needs no user value and does not make
the object incomplete.

**Scoping consequence for D9/D11/D12/D13.** D9's/D11's "an autocompleted default
joins the fixed bucket" and D12's "`fixed = TRUE` autocompleted rows with the
frozen value" apply on the surfaces that see a **completed** `process_map`: a
fitted result, and — per D16a below — a **completed** joint spec. They do **not**
apply to `set_parameters()` or to the `parameters.goldfish` projections, which are
built from the **raw** spec and never see the autocompleted fids. D13's "omitted
key = all-free" governs an **authored** fid the user leaves unkeyed; an
autocompleted fid is not "omitted" — it is simply absent from the authored spec,
and resolved later. *Rejected:* completing inside `set_parameters()` (double-warns,
and cannot pin timed-rate values without wave data); requiring the user to key
autocompleted fids (zero free parameters, not user-suppliable).

### D16a — `coef_layout()` on a joint spec is completion-aware (pre-fit full-layout preview)

A completed spec is **still class `joint_specification.goldfish`**
(`complete_generative_spec.R:285-293` — `rebuild_completed_joint()` reassigns that
same class; a completed fid is "indistinguishable in kind from an authored one"),
distinguished only by content: its `process_map` carries the extra autocompleted
fids and a populated **`completed`** logical column
(`complete_generative_spec.R:127`), plus the frozen values (`$completed_rates` for
timed rates, the bundle defaults for uniform choice/coordination/ordered). So the
completed-spec layout is **not a new S3 method** — `coef_layout.joint_specification`
is made **completion-aware** off that content:

- **raw (authored) spec** — no `completed` column / no autocompleted fids: one row
  per authored coefficient slot, autocompleted rows **absent** (the authoring
  layout of D6).
- **completed spec** (e.g. `coef_layout(complete_generative_spec(spec))`) — reads
  the `completed` column to mark each autocompleted fid's rows `fixed = TRUE` with
  its frozen value, giving the **full walked layout before fitting**. This closes
  the pre-fit blind spot: a user can preview exactly what `simulate()` will walk —
  authored slots to pin plus the frozen autocompleted rows — without first running
  an estimation.

`set_parameters()` and the `parameters.goldfish` projections are **unaffected** —
they take the **raw** spec by contract (D16) and stay over the authored fid set;
only `coef_layout()` gains the completion-aware reading. *Rejected:* a separate
class / S3 method for a completed spec (there is no separate class to dispatch on,
and the `completed` column is the honest discriminant); auto-completing inside
`coef_layout()` (the raw-spec authoring layout is the intended default — a caller
who wants the full layout completes explicitly).

### D17 — Both classes this change ships/consumes rename to the goldfish\<Thing\> camelCase scheme before archive

New (2026-08-28). All twelve tasks are checked and `openspec list` reports
this change `complete`, but it has not archived. Since task 1.1 was written,
the ecosystem naming rule changed: `class-naming-scheme` (proposed
2026-08-19, not yet folded — 0 of its tasks are checked) retires the
`<noun>.goldfish` house convention task 1.1 explicitly invoked
("`parameters.goldfish` S3 class (house `<noun>.goldfish` convention)")
in favor of `goldfish<Thing>` camelCase, following the stocnet ecosystem
rule (autograph CONTRIBUTING; RSiena's `sienaFit`/`sienaGOF` precedent).
Two classes are affected:

- **`parameters.goldfish`** (`R/joint_parameters.R:83`) — this change's own
  class, built by `set_parameters()`. Renames to **`goldfishParams`**.
- **`joint_specification.goldfish`** (`R/make_joint_specification.R:184`) —
  *not* this change's class; it belongs to the already-archived
  `make-multivariate-spec` change and is folded into the living spec
  (`openspec/specs/multivariate-specification/spec.md:7`). This change
  does not own the file that defines it, but is the last active change
  holding it before it archives — `abmcem`, `dynes-augmentation`, and
  `process-simulation` all reference it read-only via `parameters.goldfish`
  or `coef_layout()`. Renames to **`goldfishJointSpec`**.

Both names are recorded in `class-naming-scheme`'s rename table (design D16
there) as the authoritative spelling. Coordination with that change is
recorded there too (design D8b): `class-naming-scheme`'s own docs-only sweep
task (9.3) covers this change's **delta spec text** only if this change is
still unarchived when that task runs; it explicitly does **not** duplicate
the R-code rename, because this change is the one actively holding those
files. **Decision:** this change executes both renames itself, as new
tasks (§3) before archiving, rather than waiting on `class-naming-scheme`
to fold first — `class-naming-scheme` hasn't started (0 of its own tasks
checked), there is no reason to leave shipped code on a
retired convention in the interim, and archiving with the current
spelling would let it harden into `openspec/specs/` under the old name,
creating exactly the kind of stale living-spec text `class-naming-scheme`
design D8/D8a exists to avoid.

**Scope of the `joint_specification.goldfish` half:** since that class's
producer (`make_joint_specification()`) is not part of this change's own
delta, the rename is recorded as a `## RENAMED` block against the
`multivariate-specification` capability in this change's own
`specs/multivariate-specification/spec.md` (the same file this change
already deltas), not as a new capability — mirroring how
`class-naming-scheme`'s own spec uses `## RENAMED Requirements` blocks for
this exact pattern (e.g. `make_specification constructs a
specification.goldfish object` → `... a goldfishSpec object`). *Rejected:*
leaving `joint_specification.goldfish` for `class-naming-scheme` to sweep
later (it would sit under the retired name in `R/` for an unbounded
number of intervening commits, and any of `abmcem`/`dynes-augmentation`/
`process-simulation` landing in the meantime would need updating twice);
renaming only `parameters.goldfish` and leaving `joint_specification.goldfish`
alone (the whole point of the ecosystem rule is that every live class
carries it — a half-renamed pair sitting next to each other in the same
file is a worse inconsistency than the one being fixed).

## Open Questions

_All resolved._ OQ1 → **D8**, OQ2 → **D9** (with correction), OQ3 → **D10** — all
confirmed in code. The 2026-08-28 design-review clarity items folded into **D11**
(free vs full projection, canonical order, autocomplete-as-fixed), **D12**
(coef_layout rows + intercept/interaction naming), **D13** (omitted key =
all-free), **D14** (round-trip = from-result method `set_parameters(spec, result)`
with a spec/result-match assertion, NQ2), **D15** (value gate vs `walk_open()`'s
structural gates are complementary, value-first, NQ1), plus the spec-text
`n_params` reconciliation. The 2026-08-28 explore pass closed a fourth batch:
**D16** (completion sequencing — the object is over the authored fid set;
autocompleted fids resolve trivially at consumer entry; scopes D9/D11/D12/D13's
autocompleted-default rows to the completed-spec/result `coef_layout()` methods),
plus **D3** (partial/mixed naming rejected — all-or-nothing) and **D14**'s
signature fix (`set_parameters(spec, result)`, not a bare `as_parameters(result)`).
The same pass added **D17** (post-completion class-naming migration —
`parameters.goldfish` → `goldfishParams`, `joint_specification.goldfish` →
`goldfishJointSpec`, per `class-naming-scheme` design D16/D8b), tracked as
new tasks in §3.
