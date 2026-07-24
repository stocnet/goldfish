# preprocess-output-writers (delta)

Wording-only: the preprocessing-control constructor is `set_preprocessing()`.
The `compute_stats()` references are deliberately left untouched here — that
consolidation belongs to the revise-gather-output change, which renames it.

## MODIFIED Requirements

### Requirement: DB writer streams gather rows to a DBI table during preprocessing
The db writer SHALL append gather-format rows to the table named by `set_preprocessing(db = <DBIConnection>, db_table = <name>)` in batches during the event loop, holding at most one batch in memory at any time. The long table SHALL carry, alongside `event_id`, `is_selected`, and the `stat_<i>` columns, the row-identity columns of the gather writer (`index_i`/`index_j` as applicable to the model family), so rows are identifiable relationally without positional decoding. After `finalize()`, the table SHALL contain the same rows as the in-memory gather writer would have produced. `compute_stats(..., output = "db")` SHALL error informatively when no `db` connection is configured. A write failure mid-loop SHALL produce an error naming the last successfully written event index.

#### Scenario: Streamed table matches in-memory gather
- **WHEN** `compute_stats(..., output = "db")` runs with an RSQLite in-memory connection configured
- **THEN** reading back the `db_table` yields the same rows — statistics and `index_i`/`index_j` identity columns — as `compute_stats(..., output = "gather")` on the same model

#### Scenario: Missing connection errors clearly
- **WHEN** `compute_stats(..., output = "db")` is called and `set_preprocessing()` has `db = NULL`
- **THEN** an informative error states that a DBI connection is required for `output = "db"`
