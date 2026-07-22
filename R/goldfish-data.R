#' Building the goldfish data object
#'
#' `r lifecycle::badge("experimental")`
#'
#' goldfish consumes a **single data object**: a `stocnet` (from the
#' [`manynet`](https://stocnet.github.io/manynet/) package) that bundles the
#' node set, the network layers, and any attribute changes together. This page
#' is the reference for assembling that object with manynet's construction
#' verbs; the prebuilt [social_evolution] and [fisheries_treaties] datasets are
#' built exactly this way, and their help pages show the recipes end to end.
#' Once assembled, the object is passed straight to [estimate_dynam()],
#' [estimate_rem()], or [make_specification()] as `data`, or gated early with
#' [as_goldfish()].
#'
#' @details
#' # The components
#'
#' A `stocnet` is a plain list of tibbles:
#'
#' - `info` -- metadata: the layer `name`s, the `focal` (dependent) layer, and
#'   per-layer `update` (`"increment"` / `"replace"`), `directed`, and
#'   `observation` (`"event"` / `"panel"`) declarations;
#' - `nodes` -- one row per node with a unique `label` and any attribute columns
#'   (a reserved `active` column marks presence, a reserved `mode` column names
#'   the node's mode);
#' - `ties` -- the relational events across all layers (`from`, `to`, `time`,
#'   `layer`, and optionally `weight`, `flavor`, `order`); `from`/`to` are
#'   integer indices into `nodes`;
#' - `changes` -- nodal attribute updates (`time`, `node`, `var`, `value`);
#' - `global` -- global attribute updates (`time`, `var`, `value`).
#'
#' # The construction verbs
#'
#' Assemble the object one layer at a time and then merge:
#'
#' - `manynet::as_stocnet()` turns an edge list or an adjacency matrix into a
#'   single-layer `stocnet`;
#' - `manynet::from_ties()` merges several single-layer objects over one shared
#'   node set (`layer_names` labels them);
#' - `manynet::join_nodes()` / `manynet::rename_nodes()` attach the node
#'   attributes and relabel the node ids;
#' - `manynet::bind_ties()` appends timed relational events to a layer;
#' - `manynet::bind_changes()` attaches a nodal attribute change stream;
#' - `manynet::add_info()` records the `focal`, `directed`, and `observation`
#'   metadata goldfish requires (manynet derives `update` from the change
#'   column but not `directed`, so declare it).
#'
#' Alternatively `manynet::make_stocnet()` builds the whole object from the
#' `info`, `nodes`, `ties`, `changes`, and `global` pieces in one call.
#'
#' # Observation types: event and panel layers
#'
#' goldfish models two `observation` types. An **`"event"`** layer is a stream
#' of time-stamped relational events. A **`"panel"`** layer is a covariate
#' observed at survey waves whose rows are applied at their wave times; a panel
#' layer cannot be `focal`, and a panel layer only knows what its rows say -- to
#' *remove* a tie at a wave you must include an **explicit value-`0` row**,
#' since goldfish cannot infer a dissolution from the absence of a row.
#'
#' # History with `time = NA`
#'
#' Ties that hold *before* the observation window -- an initial adjacency
#' matrix, say -- enter with `time = NA`. They seed the starting network state
#' instead of being modeled as events. `as_stocnet()` of a matrix produces such
#' history ties automatically; `bind_ties()` then appends the timed events on
#' top.
#'
#' # Node modes: one-mode, two-mode, and the undeclared default
#'
#' A layer's sides are declared per layer through `info$sender` /
#' `info$receiver` as sets of `nodes$mode` values (a repeated-name character
#' vector, e.g. `c(survey = "employee", survey = "supervisor",
#' report = "employee")`). Identical sets make a layer one-mode over that subset
#' of nodes; disjoint sets make it two-mode. A layer **without** the declaration
#' is one-mode over **all** nodes -- in a multimodal node set an undeclared
#' layer therefore spans every mode, so declare identical sets to restrict it.
#'
#' # The reserved `flavor` and `order` columns
#'
#' Two optional `ties` columns tune goldfish's reading of a layer:
#'
#' - **`flavor`** (character) marks sub-processes of one layer -- for the treaty
#'   layer of [fisheries_treaties], `"signing"` versus `"ending"`. A
#'   flavor-keyed `rate`/`choice` list then models one flavor while every row
#'   still updates the network state (see [make_specification()]). Stamp it with
#'   [add_flavor()] rather than by hand: the column and the layer-info metadata
#'   that goes with it are then written together and cannot drift apart.
#' - **`order`** (integer) is the final tie-break for events sharing a
#'   timestamp. Same-time, same-target `replace` events are otherwise genuinely
#'   ambiguous and abort; supplying `order` pins their sequence. See
#'   [as_goldfish()] for the full event-ordering contract.
#'
#' @section Missing data:
#'
#' goldfish imputes a missing value during preprocessing, before any effect is
#' evaluated, by a single rule that depends on the object's *shape* and on
#' *when* the value is missing. The complete contract, by shape:
#'
#' - **dyad (network):** a missing entry is *no tie* (zero) -- both at the start
#'   of the window and for a missing `increment` or `replace` during the walk.
#' - **global:** aborts at schedule construction (see below) -- both at the
#'   start of the window and during the walk.
#' - **node, numeric:** the mean of the node's mode category, the node itself
#'   excluded -- at the start of the window over the initial values, and during
#'   the walk over the state at the event's time.
#' - **node, categorical:** the most common value in the node's mode category,
#'   the node itself excluded -- by the same start-of-window and event-time
#'   rule.
#'
#' A missing tie is treated as the **absence of a tie**: this is the *meaning*
#' of a missing entry, a definition rather than a summary over other values. For
#' a one-mode node set (or nodes carrying no `mode` column) the "mode category"
#' is every other node -- one implicit category.
#'
#' A **global** attribute holds a single value, so the pool it would be
#' summarized from is empty by construction. A missing global value -- in its
#' initial value or in any of its event streams -- therefore aborts at schedule
#' construction, naming the object (and, for an event, its time), rather than
#' being replaced by an arbitrary zero.
#'
#' ## Imputation feeds back into later imputations
#'
#' An imputed value joins the process state exactly as an observed one does. A
#' nodal value missing *after* the start of the window is therefore summarized
#' from a pool that may already contain **earlier imputed values**, not only
#' observed ones. Single imputation treats every imputed value as if it had been
#' observed, so it understates the uncertainty the missingness carries.
#'
#' @name goldfish_data
#' @seealso [as_goldfish()] for the validate-and-stamp boundary and the
#'   event-ordering contract; [social_evolution] and [fisheries_treaties] for
#'   worked construction recipes; [estimate_dynam()], [estimate_rem()], and
#'   [make_specification()] for consuming the object.
#'
#' @examplesIf rlang::is_installed("manynet")
#' # Assemble Social Evolution: a panel friendship layer and an event calls
#' # layer over one node set (reproduces the shipped `social_evolution` object).
#' data("Social_Evolution")
#' se <- manynet::from_ties(
#'   manynet::as_stocnet(friendship),
#'   manynet::as_stocnet(calls),
#'   layer_names = c("friendship", "calls")
#' )
#' se <- manynet::join_nodes(se, actors)
#' se <- manynet::add_info(
#'   se,
#'   name = "Social Evolution MIT",
#'   focal = "calls",
#'   directed = c(friendship = TRUE, calls = TRUE),
#'   observation = c(friendship = "panel", calls = "event")
#' )
#'
#' estimate_dynam(
#'   calls ~ inertia + recip,
#'   sub_model = "choice",
#'   data = se
#' )
NULL
