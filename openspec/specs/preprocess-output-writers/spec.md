# preprocess-output-writers Specification

## Purpose
Define the writer-strategy contract for preprocessing output: preprocessing recipes emit
their per-event statistics exclusively through a writer object (`init` / `write_event` /
`finalize`) selected by `compute_stats(..., output = c("default", "gather", "db"))`. Covers
the default flat-buffer writer (estimation-ready `preprocessed.goldfish`), the native
gather-stack writer (one row per event × alternative, constraint-aware), and the streaming
DBI writer, plus documented-but-unimplemented extension points (alternatives sampling,
parallel chunking, per-event simulation hook).
## Requirements
### Requirement: Writer strategy contract
Preprocessing recipes SHALL emit their output exclusively through a writer object exposing three hooks: `init(spec, dims)` (called once before the event loop with the model spec and problem dimensions), `write_event(event_updates, event_info)` (called once per stored event), and `finalize()` (called once after the loop, returning the writer's output). Writer constructors SHALL live in `R/preprocess_writers.R` and SHALL NOT be exported. `compute_stats(formula, data, model, sub_model, output = c("default", "gather", "db"), ...)` SHALL select the writer; `output` defaults to `"default"`. No recipe method SHALL branch on the output format inside its event loop beyond calling the writer hooks.

#### Scenario: Default output selected implicitly
- **WHEN** `compute_stats(formula, data, model = "DyNAM", sub_model = "rate")` is called without `output`
- **THEN** the returned object is a `preprocessed.goldfish` produced by the default flat-buffer writer

#### Scenario: Invalid output value rejected
- **WHEN** `compute_stats(..., output = "parquet")` is called
- **THEN** an informative error lists the valid output values

### Requirement: Default writer produces the flat-buffer preprocessing object
The default writer SHALL produce the `preprocessed.goldfish` object specified by the flat-preprocess-output capability (`stat_mat_update`, `stat_mat_pointer`, unified event fields, engine-native `initialStats`, intercept scalars, presence C-format). Both estimation engines (R `default`, C++ `default_c`) SHALL consume this output without restructuring.

#### Scenario: Default writer output is estimation-ready
- **WHEN** the result of `compute_stats(..., output = "default")` is passed to `estimate_dynam()` as precomputed preprocessing
- **THEN** estimation completes and coefficients match the non-precomputed path to within 1e-6

### Requirement: Gather writer produces the gather stack format natively
The gather writer SHALL build, during the event loop, the gather stack format: a statistics matrix with one row per event × alternative (`stat_all_events`, columns = effect statistics) together with `selected` (position of the chosen alternative or sender), `n_candidates`, and `timespan` (inter-event time; NA where not applicable). Every row SHALL carry explicit identity in the shared sanitized-index vocabulary: dyad-indexed models (REM, DyNAM-coordination) emit per-row `index_i` and `index_j`, sender-set rows emit `index_i`, receiver-set rows emit `index_j` — the same 1-based sanitized ids used by the internal event representation, decodable to node labels at the export boundary. When a `support_constraint` is active, `stat_all_events` SHALL contain rows only for mask-allowed candidates, with `n_candidates` and `selected` reflecting the constrained set; constraint-role atoms SHALL contribute no statistic columns. One-mode coordination SHALL NOT emit diagonal (self-tie) rows. For models without a constraint, the output SHALL be value-identical to the pre-refactor `gather_model_data()` result for the same model, except the coordination row set (diagonal rows removed) and the added index metadata, both documented in NEWS. `gather_model_data()` SHALL be reimplemented as a wrapper over `compute_stats(..., output = "gather")`. The post-hoc `gather_()` C++ conversion SHALL be removed, and `engine = "gather_compute"` SHALL consume the native gather output.

#### Scenario: Gather writer matches legacy gather output
- **WHEN** `compute_stats(..., output = "gather")` runs on a model previously processed by the pre-refactor `gather_model_data()`
- **THEN** `stat_all_events`, `selected`, `n_candidates`, and `timespan` are identical to the pre-refactor values, except that one-mode coordination omits the diagonal rows

#### Scenario: gather_compute engine uses native output
- **WHEN** `estimate_dynam(..., engine = "gather_compute")` is called after the refactor
- **THEN** estimation completes without invoking `gather_()` and coefficients match the `default_c` engine to within 1e-6

#### Scenario: Constrained gather emits only allowed candidates
- **WHEN** `compute_stats(..., output = "gather")` runs on a choice model with a `support_constraint` allowing 4 of 10 present receivers at an event
- **THEN** that event contributes exactly 4 rows to `stat_all_events`, `n_candidates` equals 4, and `selected` is the chosen receiver's position among the 4

#### Scenario: Rows are identifiable after filtering
- **WHEN** any gather output is produced for a dyad-indexed model with rows filtered (mask or diagonal)
- **THEN** each remaining row's `index_i`/`index_j` identify its dyad exactly, and decoding them yields the correct node labels — no positional convention is needed to interpret the stack

### Requirement: DB writer streams gather rows to a DBI table during preprocessing
The db writer SHALL append gather-format rows to the table named by `set_preprocessing(db = <DBIConnection>, db_table = <name>)` in batches during the event loop, holding at most one batch in memory at any time. The long table SHALL carry, alongside `event_id`, `is_selected`, and the `stat_<i>` columns, the row-identity columns of the gather writer (`index_i`/`index_j` as applicable to the model family), so rows are identifiable relationally without positional decoding. After `finalize()`, the table SHALL contain the same rows as the in-memory gather writer would have produced. `compute_stats(..., output = "db")` SHALL error informatively when no `db` connection is configured. A write failure mid-loop SHALL produce an error naming the last successfully written event index.

#### Scenario: Streamed table matches in-memory gather
- **WHEN** `compute_stats(..., output = "db")` runs with an RSQLite in-memory connection configured
- **THEN** reading back the `db_table` yields the same rows — statistics and `index_i`/`index_j` identity columns — as `compute_stats(..., output = "gather")` on the same model

#### Scenario: Missing connection errors clearly
- **WHEN** `compute_stats(..., output = "db")` is called and `set_preprocessing()` has `db = NULL`
- **THEN** an informative error states that a DBI connection is required for `output = "db"`

### Requirement: Extension points documented, not implemented
The writer contract documentation SHALL describe, without implementing them: an alternatives-sampling gather writer (keeps rows for a sample of alternatives; requires estimation adaptation), parallel chunk preprocessing (chunk the event sequence by time points, warm-start each chunk, run recipe + writer per chunk in parallel, merge in `finalize()`), and the per-event simulation hook on the recipe loop (after the stats update for event i, before advancing — visible state and event-stream append semantics documented) reserved for a future `simulate()` method for GoF.

#### Scenario: Contracts are documented
- **WHEN** the writer contract documentation (roxygen for `R/preprocess_writers.R` and the recipe loop) is inspected after this change
- **THEN** it describes the sampling writer, parallel chunking, and the per-event simulation hook as future extension points with their interface obligations

