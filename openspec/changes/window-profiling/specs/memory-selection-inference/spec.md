# memory-selection-inference

## ADDED Requirements

### Requirement: The profile output is the step curve with its selection

The profile result SHALL expose the profile log-likelihood at every
candidate, the selected value (argmax), and a profile-likelihood
interval, and its print/plot methods SHALL display the curve as a step
function. The interval SHALL be reported with an explicit caveat that
the window is not a regular parameter (the likelihood is a step
function in it), printed with the interval rather than relegated to
documentation.

#### Scenario: Curve and caveat printed

- **WHEN** a profile object is printed
- **THEN** the output shows the selected value, the interval, and the
  irregular-parameter caveat in the same block

### Requirement: The default selection test is a Davies-bounded sup-LR

The selection test SHALL compare the maximized-over-candidates fit to
the baseline (unwindowed) model via the sup of the LR statistics over
the grid, and SHALL report a Davies-type upper bound on the p-value
computed from the profile curve, never a naive χ² p-value for the sup.
The output SHALL state the grid size the sup was taken over.

#### Scenario: No naive chi-square for the sup

- **WHEN** the selection test is reported without a bootstrap
- **THEN** the p-value shown is the Davies bound, labeled as an upper
  bound, with the grid size stated

### Requirement: Parametric bootstrap calibrates the search on the same grid

A parametric-bootstrap test SHALL be available that simulates event
sequences from the fitted baseline model, recomputes the sup-LR over
the identical candidate grid on each replicate, and reports the
empirical p-value of the observed sup. The bootstrap SHALL reuse the
recorded grid verbatim, SHALL support parallel execution over
replicates, and SHALL depend on the package's simulation surface — the
test is unavailable, with an informative error naming the dependency,
until that surface exists.

#### Scenario: Same grid per replicate

- **WHEN** a bootstrap with B replicates runs
- **THEN** every replicate's sup is taken over exactly the recorded
  candidate grid, and the returned p-value is the empirical proportion
  of replicate sups at or above the observed sup
