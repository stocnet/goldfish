## MODIFIED Requirements

### Requirement: initialStats retained in engine-native form
The initial statistics SHALL be stored at each effect's broadcast kind: the
preprocessed object SHALL carry `stat_init`, one slice per effect holding a
scalar for a global-kind effect, a length-`n1` vector for an ego-kind
effect, a length-`n2` vector for an alter-kind effect and an `n1 × n2` matrix
for a point-kind effect (a sender-indexed model's slice is a length-`n1`
vector or a scalar), together with `stat_init_kind`, the broadcast kind per
effect. The object SHALL NOT store a dense `n1 × n2 × nEffects` array, and a
vector-kind effect's initial value SHALL NOT be stored as a grid. The form
each estimation engine uses natively SHALL be produced on demand by one
accessor: for `goldfishAxisSender` models (rate, rate_ordered) `n1 × nEffects`
(2D), for `goldfishAxisDyad` models (choice, choice_coord, REM) `n1 × n2 ×
nEffects` (3D), each slice projected from its kind through the shared
broadcast core with the one-mode diagonal rule applied once; the C++
interface SHALL flatten the 3D form to `n1*n2 × nEffects` at call time, and
preprocessing SHALL NOT pre-flatten or pre-expand it. A preprocessed object
from before this change, carrying a dense `initial_stats` and no `stat_init`,
SHALL be read by the same accessor unchanged.

#### Scenario: Sender model produces 2D initialStats
- **WHEN** `preprocess()` is called for a DyNAM-rate submodel with `n1=10`, `nEffects=2`
- **THEN** `stat_init` has two slices, each a length-10 vector or a scalar, and the accessor's engine-native form has `length(dim(...))` equal to 2 and `dim(...)` equal to `c(10L, 2L)`

#### Scenario: Dyad model produces 3D initialStats
- **WHEN** `preprocess()` is called for a DyNAM-choice submodel with `n1=10`, `n2=10`, `nEffects=3`
- **THEN** `stat_init` has three slices, each at its effect's kind, and the accessor's engine-native form has `length(dim(...))` equal to 3 and `dim(...)` equal to `c(10L, 10L, 3L)`

#### Scenario: a vector-kind effect stores no grid
- **WHEN** a one-mode DyNAM-choice model with `alter(attr)`, `indeg(net)` and
  `inertia(net)` is preprocessed on `n` actors
- **THEN** the `alter` slice is a length-`n` vector, the `indeg` slice a
  length-`n` vector, the `inertia` slice an `n × n` matrix, the object's size
  grows by under two grids less than the dense form, and the accessor's dense
  form equals, cell for cell, the array the dense inits produced before this
  change, including a zero diagonal

#### Scenario: a stale object is read through the accessor
- **WHEN** a preprocessed object saved before this change, with a dense
  `initial_stats` field, is supplied through `preprocessed =`
- **THEN** every backend estimates it with results equal to a fresh object
  within 1e-10, and the accessor returns its dense field as the engine-native
  form
