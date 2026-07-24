# preprocess-output-writers (delta)

## MODIFIED Requirements

### Requirement: Gather writer produces the gather stack format natively
The gather writer SHALL build, during the event loop, the gather stack format: a statistics matrix with one row per event × alternative (`stat_all_events`, columns = effect statistics) together with `selected` (position of the chosen alternative or sender), `n_candidates`, and `timespan` (inter-event time; NA where not applicable). Every row SHALL carry explicit identity in the shared sanitized-index vocabulary: dyad-indexed models (REM, DyNAM-coordination) emit per-row `index_i` and `index_j`, sender-set rows emit `index_i`, receiver-set rows emit `index_j` — the same 1-based sanitized ids used by the internal event representation, decodable to node labels at the export boundary. When a `support_constraint` is active, `stat_all_events` SHALL contain rows only for mask-allowed candidates, with `n_candidates` and `selected` reflecting the constrained set; constraint-role atoms SHALL contribute no statistic columns. One-mode coordination SHALL NOT emit diagonal (self-tie) rows. For models without a constraint, the output SHALL be value-identical to the pre-refactor `gather_model_data()` result for the same model, except the coordination row set (diagonal rows removed) and the added index metadata, both documented in NEWS. The canonical user surface for this output SHALL be `compute_statistics(..., output = "gather")`; `gather_model_data()` SHALL remain a lifecycle soft-deprecated wrapper over it and the `compute_stats` name SHALL be deleted — not exported, no stub (model-data-export capability). The post-hoc `gather_()` C++ conversion SHALL be removed, and `engine = "gather_compute"` SHALL consume the native gather output.

#### Scenario: Gather writer matches legacy gather output
- **WHEN** `compute_statistics(..., output = "gather")` runs on a model previously processed by the pre-refactor `gather_model_data()`
- **THEN** `stat_all_events`, `selected`, `n_candidates`, and `timespan` are identical to the pre-refactor values, except that one-mode coordination omits the diagonal rows

#### Scenario: gather_compute engine uses native output
- **WHEN** `estimate_dynam(..., engine = "gather_compute")` is called after the refactor
- **THEN** estimation completes without invoking `gather_()` and coefficients match the `default_c` engine to within 1e-6

#### Scenario: Constrained gather emits only allowed candidates
- **WHEN** `compute_statistics(..., output = "gather")` runs on a choice model with a `support_constraint` allowing 4 of 10 present receivers at an event
- **THEN** that event contributes exactly 4 rows to `stat_all_events`, `n_candidates` equals 4, and `selected` is the chosen receiver's position among the 4

#### Scenario: Rows are identifiable after filtering
- **WHEN** any gather output is produced for a dyad-indexed model with rows filtered (mask or diagonal)
- **THEN** each remaining row's `index_i`/`index_j` identify its dyad exactly, and decoding them yields the correct node labels — no positional convention is needed to interpret the stack

### Requirement: DB writer streams gather rows to a DBI table during preprocessing
The db writer SHALL append gather-format rows to the table named by `set_preprocessing(db = <DBIConnection>, db_table = <name>)` in batches during the event loop, holding at most one batch in memory at any time. The long table SHALL carry, alongside `event_id`, `is_selected`, and the `stat_<i>` columns, the row-identity columns of the gather writer (`index_i`/`index_j` as applicable to the model family), so rows are identifiable relationally without positional decoding. After `finalize()`, the table SHALL contain the same rows as the in-memory gather writer would have produced. `compute_statistics(..., output = "db")` SHALL error informatively when no `db` connection is configured, and user-facing messages that point to the db route SHALL name `compute_statistics(output = "db")`. A write failure mid-loop SHALL produce an error naming the last successfully written event index.

#### Scenario: Streamed table matches in-memory gather
- **WHEN** `compute_statistics(..., output = "db")` runs with an RSQLite in-memory connection configured
- **THEN** reading back the `db_table` yields the same rows — statistics and `index_i`/`index_j` identity columns — as `compute_statistics(..., output = "gather")` on the same model

#### Scenario: Missing connection errors clearly
- **WHEN** `compute_statistics(..., output = "db")` is called and `set_preprocessing()` has `db = NULL`
- **THEN** an informative error states that a DBI connection is required for `output = "db"`
