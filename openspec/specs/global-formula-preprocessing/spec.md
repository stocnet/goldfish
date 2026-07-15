# global-formula-preprocessing Specification

## Purpose
Classify and preprocess global (whole-network) event streams: identify `isGlobalEvent` streams during preprocessing and route their updates through the recipe loop.

## Requirements

### Requirement: Global events are classified as isGlobalEvent in preprocessing
During preprocessing, events linked to a `global.goldfish` object SHALL be identified by a new `isGlobalEvent` logical vector (one entry per event stream). An event stream is classified as global when its data frame has neither a `"node"` column nor `"sender"`/`"receiver"` columns.

#### Scenario: Global event stream classified correctly
- **WHEN** preprocessing encounters an event data frame with only `time` and `replace` columns (linked from a `global.goldfish` object)
- **THEN** `isGlobalEvent` is `TRUE` for that event stream and `isNodeEvent` is `FALSE`

#### Scenario: Node event stream not misclassified as global
- **WHEN** preprocessing encounters an event data frame with `time`, `node`, and `replace` columns
- **THEN** `isGlobalEvent` is `FALSE` and `isNodeEvent` is `TRUE`

#### Scenario: Network event stream not misclassified as global
- **WHEN** preprocessing encounters an event data frame with `time`, `sender`, `receiver`, and `replace` columns
- **THEN** `isGlobalEvent` is `FALSE` and `isNodeEvent` is `FALSE`

---

### Requirement: Global object state is updated by scalar replacement
During preprocessing, when a global event fires, the `global.goldfish` object's column SHALL be updated by direct scalar replacement: `obj[[col]] <- event$replace`. No node indexing is performed.

#### Scenario: Global attribute value changes
- **WHEN** a global event with `replace = 1` fires for `seasons$winter`
- **THEN** `seasons$winter` equals `1` after the event is processed

#### Scenario: All actors see the new global value
- **WHEN** a global event fires and the stat is recomputed
- **THEN** all n1 rows of the stat column are set to the new scalar value

---

### Requirement: get_events_and_objects_link skips node sanitization for global objects
`get_events_and_objects_link()` SHALL detect when an object has class `global.goldfish` and skip the `sanitizeEvents()` call that requires a `"node"` column. The events data frame is included as-is.

#### Scenario: Global object skips node sanitization
- **WHEN** `get_events_and_objects_link()` processes a formula referencing a `global.goldfish` object
- **THEN** no error is raised about a missing `"node"` column, and the events data frame is included in the output

#### Scenario: Node object still sanitized
- **WHEN** `get_events_and_objects_link()` processes a formula referencing a `nodes.goldfish` attribute
- **THEN** `sanitizeEvents()` is called as before (no regression)

---

### Requirement: Right-censored events are created for global attribute changes
When a global attribute changes value, right-censored dissolution events SHALL be created following the same semantics as node attribute changes: a dissolution event with the previous value is inserted just before the new-value event.

#### Scenario: Right-censored event on global change
- **WHEN** `seasons$winter` changes from `1` to `0`
- **THEN** a dissolution event with `replace = 1` is created at the dissolution time just before the change event
