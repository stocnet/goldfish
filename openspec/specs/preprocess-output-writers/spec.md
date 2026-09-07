# preprocess-output-writers Specification

## Purpose
Define the writer-strategy contract for preprocessing output: preprocessing recipes emit
their per-event statistics exclusively through a writer object (`init` / `write_event` /
`finalize`) selected by `compute_stats(..., output = c("default", "gather", "db"))`. Covers
the default flat-buffer writer (estimation-ready `goldfishStat`), the native
gather-stack writer (one row per event × alternative, constraint-aware), and the streaming
DBI writer, plus documented-but-unimplemented extension points (alternatives sampling,
parallel chunking, per-event simulation hook).
## Requirements
### Requirement: Writer strategy contract
Preprocessing recipes SHALL emit their output exclusively through a writer object exposing three hooks: `init(spec, dims)` (called once before the event loop with the model spec and problem dimensions), `write_event(event_updates, event_info)` (called once per stored event), and `finalize()` (called once after the loop, returning the writer's output). Writer constructors SHALL live in `R/preprocess_writers.R` and SHALL NOT be exported. `compute_stats(formula, data, model, sub_model, output = c("default", "gather", "db"), ...)` SHALL select the writer; `output` defaults to `"default"`. No recipe method SHALL branch on the output format inside its event loop beyond calling the writer hooks.

#### Scenario: Default output selected implicitly
- **WHEN** `compute_stats(formula, data, model = "DyNAM", sub_model = "rate")` is called without `output`
- **THEN** the returned object is a `goldfishStat` produced by the default flat-buffer writer

#### Scenario: Invalid output value rejected
- **WHEN** `compute_stats(..., output = "parquet")` is called
- **THEN** an informative error lists the valid output values

### Requirement: Default writer produces the flat-buffer preprocessing object
The default writer SHALL produce the `goldfishStat` object specified by the flat-preprocess-output capability (`stat_mat_update`, `stat_mat_pointer`, unified event fields, engine-native `initialStats`, intercept scalars, presence C-format). Both estimation engines (R `default`, C++ `default_c`) SHALL consume this output without restructuring.

#### Scenario: Default writer output is estimation-ready
- **WHEN** the result of `compute_stats(..., output = "default")` is passed to `estimate_dynam()` as precomputed preprocessing
- **THEN** estimation completes and coefficients match the non-precomputed path to within 1e-6

### Requirement: Gather writer produces the gather stack format natively
The gather writer SHALL build, during the event loop, the gather stack format: a statistics matrix with one row per event × alternative (`stat_all_events`, columns = effect statistics) together with `selected` (position of the chosen alternative or sender), `n_candidates`, and `timespan` (inter-event time; NA where not applicable). Every row SHALL carry explicit identity in the shared sanitized-index vocabulary: dyad-indexed models (REM, DyNAM-coordination) emit per-row `index_i` and `index_j`, sender-set rows emit `index_i`, receiver-set rows emit `index_j` — the same 1-based sanitized ids used by the internal event representation, decodable to node labels at the export boundary. When a `support_constraint` is active, `stat_all_events` SHALL contain rows only for mask-allowed candidates, with `n_candidates` and `selected` reflecting the constrained set; constraint-role atoms SHALL contribute no statistic columns. One-mode coordination SHALL NOT emit diagonal (self-tie) rows. For models without a constraint, the output SHALL be value-identical to the pre-refactor `gather_model_data()` result for the same model, except the coordination row set (diagonal rows removed) and the added index metadata, both documented in NEWS. The canonical user surface for this output SHALL be `compute_statistics(..., output = "gather")`; `gather_model_data()` SHALL remain a lifecycle soft-deprecated wrapper over it and the `compute_stats` name SHALL be deleted — not exported, no stub (model-data-export capability). The post-hoc `gather_()` C++ conversion SHALL be removed, and `backend = "gather"` SHALL consume the native gather output.

#### Scenario: Gather writer matches legacy gather output
- **WHEN** `compute_statistics(..., output = "gather")` runs on a model previously processed by the pre-refactor `gather_model_data()`
- **THEN** `stat_all_events`, `selected`, `n_candidates`, and `timespan` are identical to the pre-refactor values, except that one-mode coordination omits the diagonal rows

#### Scenario: the gather backend uses native output
- **WHEN** `estimate_dynam(..., backend = "gather")` is called after the refactor
- **THEN** estimation completes without invoking `gather_()` and coefficients match the `cpp` backend to within 1e-6

#### Scenario: Constrained gather emits only allowed candidates
- **WHEN** `compute_statistics(..., output = "gather")` runs on a choice model with a `support_constraint` allowing 4 of 10 present receivers at an event
- **THEN** that event contributes exactly 4 rows to `stat_all_events`, `n_candidates` equals 4, and `selected` is the chosen receiver's position among the 4

#### Scenario: Rows are identifiable after filtering
- **WHEN** any gather output is produced for a dyad-indexed model with rows filtered (mask or diagonal)
- **THEN** each remaining row's `index_i`/`index_j` identify its dyad exactly, and decoding them yields the correct node labels — no positional convention is needed to interpret the stack

### Requirement: DB writer streams gather rows to a DBI table during preprocessing
The db writer SHALL append gather-format rows to the connection configured by `set_preprocessing(db = <DBIConnection>, db_table = <name>)`, into the per-process table `<db_table>_<fid>`, in batches during the event loop, holding at most one batch in memory at any time. The long table SHALL carry, alongside `event_id` and `is_selected`, the row-identity columns of the gather writer (`index_i`/`index_j` as applicable to the model family), so rows are identifiable relationally without positional decoding, followed by one column per effect statistic **named by that effect's short name** (`names_effects`, bounded by `max_length`, the identifier-safe default) rather than by position. The reserved identity column names SHALL participate in the uniqueness resolution, so an effect name colliding with one is disambiguated instead of overwriting it. The rows written SHALL be the rendered gather rows, so an active `support_constraint` excludes its candidates before they reach the database. After `finalize()`, the table SHALL contain the same rows as the in-memory gather writer would have produced. `compute_statistics(..., output = "db")` SHALL error informatively when no `db` connection is configured, and user-facing messages that point to the db route SHALL name `compute_statistics(output = "db")`. A write failure mid-loop SHALL produce an error naming the last successfully written event index, and the process whose table failed when several are written.

#### Scenario: statistic columns identify their effects
- **WHEN** any db export completes
- **THEN** the statistic columns are named by the effect short names, matching `names_effects` of the equivalent `output = "gather"` result, and no `stat_<i>` positional column remains

#### Scenario: a constraint excludes rows before they are written
- **WHEN** `compute_statistics(..., output = "db")` runs on a model whose `support_constraint` allows 4 of 10 present receivers at an event
- **THEN** that event contributes exactly 4 rows to the written table, the same rows `output = "gather"` would hold in memory

#### Scenario: Streamed table matches in-memory gather
- **WHEN** `compute_statistics(..., output = "db")` runs with an RSQLite in-memory connection configured
- **THEN** reading back the process's table yields the same rows — statistics and `index_i`/`index_j` identity columns — as `compute_statistics(..., output = "gather")` on the same model

#### Scenario: Missing connection errors clearly
- **WHEN** `compute_statistics(..., output = "db")` is called and `set_preprocessing()` has `db = NULL`
- **THEN** an informative error states that a DBI connection is required for `output = "db"`

### Requirement: Extension points documented, not implemented
The writer contract documentation SHALL describe, without implementing them: an alternatives-sampling gather writer (keeps rows for a sample of alternatives; requires estimation adaptation), parallel chunk preprocessing (chunk the event sequence by time points, warm-start each chunk, run recipe + writer per chunk in parallel, merge in `finalize()`), and the per-event simulation hook on the recipe loop (after the stats update for event i, before advancing — visible state and event-stream append semantics documented) reserved for a future `simulate()` method for GoF.

#### Scenario: Contracts are documented
- **WHEN** the writer contract documentation (roxygen for `R/preprocess_writers.R` and the recipe loop) is inspected after this change
- **THEN** it describes the sampling writer, parallel chunking, and the per-event simulation hook as future extension points with their interface obligations

### Requirement: Products are rendered after the constraint is realized
A writer SHALL separate assembling the per-event buffer from rendering its
product: `finalize()` SHALL return the assembled object in the shared
preprocessed shape, and a `render()` stage SHALL produce the writer's product
from it. The preprocessing loop SHALL realize and fold each output's
`support_constraint` into availability on the assembled object, and SHALL call
`render()` only afterwards. `render()` SHALL be the identity for the default
writer, the gather-stack construction for the gather writer, and inherited from
the gather writer by the db writer (persistence stays a later step, once the
effect names resolve). Rendering before the fold SHALL NOT occur, since the
expansion enumerates candidates from availability and would otherwise emit rows
the mask excludes.

#### Scenario: constrained gather output succeeds and excludes masked candidates
- **WHEN** `compute_statistics(..., output = "gather")` runs on a model with a
  `support_constraint` that allows 4 of 10 present receivers at an event
- **THEN** it returns a stack in which that event contributes exactly 4 rows,
  rather than failing while realizing the mask

#### Scenario: the default product is unchanged by the split
- **WHEN** the same unconstrained model is preprocessed for
  `output = "preprocessed"` and for `output = "gather"`
- **THEN** the preprocessed object and the gather stack are each identical to
  what the single-stage writer produced, so estimation consuming the default
  product is unaffected

### Requirement: The gather expansion honors the availability encoding
The gather expansion SHALL read the `active_dyad_encoding` of the object it
expands and interpret availability accordingly — the `point` encoding as a
sender-by-receiver mask, the `alter` encoding as a receiver vector — matching
what the estimation path does with the same object. It SHALL NOT assume the
`alter` encoding. Folding a `support_constraint` upgrades availability to the
`point` encoding, so any constrained object reaching the expansion carries it.

#### Scenario: a point-encoded object expands correctly
- **WHEN** a stored `goldfishStat` whose availability is point-encoded
  (because a constraint folded into it) is rendered to a gather stack
- **THEN** the rows enumerate that event's allowed candidates, rather than
  indexing past the statistics matrix

### Requirement: A db export is self-describing without the producing session
A db export SHALL write, alongside the statistics table(s), a node table named
`<db_table>_nodes` carrying `(side, local, global, label)` — the same lookup the
returned descriptor holds — so `index_i`/`index_j` resolve to the original
`nodes` rows and labels in SQL alone. It is written once per export and is
shared by every process of a flavored specification, whose processes read one
layer's node set.

Every export SHALL write **one table per process**, named `<db_table>_<fid>` by
the process's integer fid, plus a map table named `<db_table>_map` holding the
`process_map` columns and each fid's table name. The schema SHALL be the same
whether or not the specification is flavored: a single-process export writes
`<db_table>_1` and a one-row map, so a consumer reads the map and then the
tables it names without knowing which case produced them. The map SHALL be the
authority for which tables belong to the export; tables left by an earlier
export with more processes SHALL NOT be dropped automatically. The returned
descriptor SHALL name the tables written, so the R side reports where the rows
went.

#### Scenario: flavored export is keyed by fid and identified by the map
- **WHEN** a two-flavor specification is exported with `output = "db"` and the
  default `db_table = "stats"`
- **THEN** the connection holds `stats_1` and `stats_2` with each process's own
  effect columns, `stats_map` naming which fid, layer, flavor and family each
  table is, and `stats_nodes` resolving the index columns

#### Scenario: a single-process export has the same shape
- **WHEN** a specification with no flavors is exported with the default
  `db_table = "stats"`
- **THEN** the connection holds `stats_1`, `stats_map` with one row naming that
  process's layer and family, and `stats_nodes` — the same tables a flavored
  export writes, so the same reader handles both

#### Scenario: indices join back to the original nodes
- **WHEN** a two-mode or node-subset model is exported and a written row's
  `index_i`/`index_j` are joined to `<db_table>_nodes` on `(side, local)`
- **THEN** they resolve to the original node labels and `nodes`-row ids, which
  the raw indices alone cannot do

#### Scenario: a failure names the process
- **WHEN** writing the table of one process of a flavored export fails midway
- **THEN** the error names that process's fid together with the last
  successfully written event index

