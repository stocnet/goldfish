# Fixtures for substrate parity: the two recipe loops against the merged
# single-clock walk.
#
# Every fixture in test-preprocess_joint.R gives each dyad exactly one event, so
# weighted and unweighted degree coincide there and its byte-identity assertions
# pass over a divergence. These fixtures repeat a dyad, which is the normal case
# in real event data, and they carry the specification shapes the joint file
# never reaches: a global operand feeding an interaction, the two axis broadcast
# kinds meeting in one product, mixed effects over a second network, a windowed
# support constraint, complementary state-forced flavor constraints, and missing
# data.

# The wrapper decorations `estimate_wrapper()` / `preprocess_flavored()` add
# AFTER the walk. The merged driver produces the raw walk output, so these are
# stripped before a byte-identity comparison, exactly as
# test-preprocess_joint.R's `strip_prep_deco()` does.
parity_strip_deco <- function(prep) {
  deco <- c(
    "formula",
    "model",
    "sub_model",
    "nodes",
    "nodes2",
    "node_lookup",
    "model_spec",
    "support_validated"
  )
  prep[setdiff(names(prep), deco)]
}

parity_prep_by <- function(out, family, flavor = NULL) {
  map <- attr(out, "process_map")
  sel <- map$family == family
  if (!is.null(flavor)) {
    sel <- sel & map$flavor == flavor
  }
  out[[as.character(map$fid[sel])]]
}

# The recipe loops' own output, frozen. `preprocess.goldfishKind()` routes
# `compute_statistics()` through the merged walk, so a parity assertion that
# once compared the merged walk against `compute_statistics()` would now compare
# merged walk against itself. These goldens were captured from the two recipe
# loops while they were still reachable and stored under `_fixtures/parity/`, so
# each assertion keeps one side anchored to what the loops produced rather than
# to the substrate under test. They are stripped with `parity_strip_deco()`
# already, so an assertion compares `parity_strip_deco(merged)` against the
# golden directly. Regenerated only by resurrecting the loops; the frozen 1e-6
# coefficient baselines do not cover a statistics object that changed shape.
parity_frozen <- local({
  cache <- NULL
  function(key) {
    if (is.null(cache)) {
      cache <<- readRDS(
        testthat::test_path("_fixtures", "parity", "goldens.rds")
      )
    }
    if (!key %in% names(cache)) {
      stop("no frozen recipe golden for key ", sQuote(key), call. = FALSE)
    }
    cache[[key]]
  }
})

# The union of the effect indices that appear anywhere in a preprocessed
# object's update stream. Point updates land in `stat_mat_update`; an effect
# whose value is constant across an axis (an ego/alter attribute) or across the
# whole matrix (a global) lands in `stat_mat_broadcast` instead, so reading only
# the first stream would call a moving statistic frozen.
parity_touched_effects <- function(prep) {
  point <- if (ncol(prep$stat_mat_update)) {
    prep$stat_mat_update[3, ]
  } else {
    integer(0)
  }
  bcast <- if (ncol(prep$stat_mat_broadcast)) {
    prep$stat_mat_broadcast[3, ]
  } else {
    integer(0)
  }
  sort(unique(c(point, bcast)))
}

parity_n_effects <- function(prep) {
  utils::tail(dim(prep$initial_stats), 1)
}

# The compiled plan a specification reaches the recipe loop with. There is no
# output mode that returns it, so the loop is replaced by a capture that aborts
# with a sentinel condition: the plan is the last thing built before the walk,
# and the walk itself is not wanted here.
parity_plan <- function(spec, family = "choice") {
  captured <- NULL
  testthat::with_mocked_bindings(
    preprocess = function(spec, ...) {
      captured <<- spec$plan
      rlang::abort("captured", class = "parity_plan_captured")
    },
    tryCatch(
      suppressWarnings(compute_statistics(spec, "DyNAM", family)),
      parity_plan_captured = function(cnd) NULL
    ),
    .package = "goldfish"
  )
  captured
}

parity_derived_names <- function(plan, kind) {
  entries <- Filter(function(d) identical(d$kind, kind), plan$derivations)
  sort(unique(vapply(entries, function(d) d$derived_name, character(1))))
}

# ---- (a) five-node repeated-dyad toy, complex specification -----------------

# `1 -> 2` fires at t = 1 and again at t = 3 on an accumulating layer, which is
# what separates weighted from unweighted degree. `net2` drives the mixed
# effects, the nodal changes move the ego/alter axes, and the global moves the
# scalar broadcast, so no declared statistic sits frozen over the sequence.
parity_toy_data <- function() {
  nodes <- data.frame(
    label = paste0("N", 1:5),
    mode = "p",
    a = c(1, 2, 3, 4, 5),
    b = c(5, 4, 3, 2, 1),
    stringsAsFactors = FALSE
  )
  ties <- rbind(
    data.frame(
      from = c(1L, 2L, 1L, 3L, 2L, 4L),
      to = c(2L, 3L, 2L, 4L, 3L, 5L),
      time = c(1, 2, 3, 4, 5, 6),
      layer = "calls",
      stringsAsFactors = FALSE
    ),
    data.frame(
      from = c(3L, 1L, 5L),
      to = c(1L, 4L, 2L),
      time = c(1.5, 3.5, 5.5),
      layer = "net2",
      stringsAsFactors = FALSE
    )
  )
  changes <- data.frame(
    time = c(2.2, 4.2),
    node = c("N1", "N3"),
    var = c("a", "b"),
    value = c(9, 7),
    stringsAsFactors = FALSE
  )
  global <- data.frame(
    time = c(NA, 2.5, 4.5),
    var = "x",
    value = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  info <- list(
    name = "parity_toy",
    focal = "calls",
    update = c(calls = "increment", net2 = "increment"),
    directed = c(calls = TRUE, net2 = TRUE),
    observation = c(calls = "event", net2 = "event")
  )
  list(
    info = info,
    nodes = nodes,
    ties = ties,
    changes = changes,
    global = global
  )
}

# Terms chosen for the update paths they exercise, not for realism:
# `global(x)` broadcasts a scalar to every cell (kind 3) and `global(x):inertia`
# carries that scalar into an interaction's second hop; `ego(a):alter(b)`
# crosses the two axis broadcast kinds in one product; the mixed effects chain
# `calls` into `net2`; and the weighted twins of `indeg` / `inertia` sit beside
# their unweighted defaults on the same object, which is the divergence itself.
parity_toy_spec <- function(data = parity_toy_data()) {
  make_specification(
    rate = ~ 1 + indeg + indeg(calls, weighted = TRUE) + global(x),
    choice = ~ inertia +
      inertia(calls, weighted = TRUE) +
      ego(a):alter(b) +
      global(x):inertia +
      mixed_trans(calls, net2) +
      mixed_cycle(calls, net2),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
}

# The windowed twin. A derived object expires while a second network drives the
# effect, which the merged walk cannot yet build: it is kept separate from
# `parity_toy_spec()` so the unwindowed parity assertions do not wait on
# window support.
parity_toy_spec_windowed <- function(data = parity_toy_data()) {
  make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia +
      mixed_trans(calls, net2, window = 2) +
      mixed_cycle(calls, net2, window = 2),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
}

# The windowed twins for a TIMED engine. `parity_toy_spec_windowed()` windows
# only choice-side terms, so no engine that writes right-censored rows reads a
# derived object there; these put the windowed term on the rate side, once for
# DyNAM and once for REM, where a dissolve row walked past the last real event
# would show up as a stored right-censored row.
parity_toy_spec_windowed_rate <- function(data = parity_toy_data()) {
  make_specification(
    rate = ~ 1 + indeg + indeg(calls, window = 2),
    choice = ~inertia,
    layer = "calls",
    model = "DyNAM",
    data = data
  )
}

# The intercept is explicit because the legacy single-process entry adds a
# time intercept to an REM rate formula that lacks one while the joint entry
# keeps the formula as written; that pre-existing difference is not the one
# this fixture detects.
parity_rem_spec_windowed <- function(data = parity_toy_data()) {
  make_specification(
    rate = ~ 1 + inertia(calls, window = 2),
    layer = "calls",
    model = "REM",
    data = data
  )
}

# The same windowed REM rate with the explicit `1` removed: an exact-time rate
# model carries the baseline-hazard time intercept even when the formula omits
# it. The legacy estimation entry force-adds it; the merged walk raises the same
# flag where the process map is assembled, so the two entries agree.
parity_rem_spec_no_intercept <- function(data = parity_toy_data()) {
  make_specification(
    rate = ~ inertia(calls, window = 2),
    layer = "calls",
    model = "REM",
    data = data
  )
}

# The windowed mixed term with its networks given as a list, which the parser
# records as one derivation per member.
parity_toy_spec_window_list <- function(data = parity_toy_data()) {
  make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + mixed_trans(list(calls, net2), window = 2),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
}

# The recipe loop's event schedule for one family, captured where the recipe
# context builds it. The walk itself is not wanted, so the capture aborts with
# a sentinel condition, as `parity_plan()` does one stage earlier.
parity_recipe_schedule <- function(spec, model = "DyNAM", family = "rate") {
  captured <- NULL
  original <- build_event_schedule
  testthat::with_mocked_bindings(
    build_event_schedule = function(
      events,
      events_objects_link,
      objects_registry
    ) {
      captured <<- list(
        schedule = original(events, events_objects_link, objects_registry),
        object_names = objects_registry$name
      )
      rlang::abort("captured", class = "parity_schedule_captured")
    },
    tryCatch(
      suppressWarnings(compute_statistics(spec, model, family)),
      parity_schedule_captured = function(cnd) NULL
    ),
    .package = "goldfish"
  )
  captured
}

# A schedule reduced to what an event IS -- when, whether it is evaluated or
# applied, on which layer, between whom, with what value -- so two schedules
# built by different builders compare row by row without their indices.
parity_schedule_rows <- function(schedule, layer) {
  data.frame(
    time = schedule$time,
    dependent = schedule$dependent,
    layer = layer,
    sender = schedule$sender,
    receiver = schedule$receiver,
    value = vapply(schedule$value, function(v) as.numeric(v)[1L], numeric(1)),
    stringsAsFactors = FALSE
  )
}

# A real two-process join whose two choice formulas window the same source by
# the same length. `friendship` is event-observed here rather than panel, since
# a window on a panel layer is rejected by the parser.
parity_two_process_windowed <- function() {
  nodes <- data.frame(
    label = paste0("N", 1:6),
    mode = "p",
    stringsAsFactors = FALSE
  )
  ties <- rbind(
    data.frame(
      from = c(1L, 2L, 3L, 4L),
      to = c(2L, 3L, 4L, 5L),
      time = c(1, 2, 3, 4),
      layer = "friendship"
    ),
    data.frame(
      from = c(1L, 2L, 3L, 4L, 5L),
      to = c(2L, 3L, 4L, 5L, 1L),
      time = c(1, 2, 3, 4, 5),
      layer = "calls"
    ),
    data.frame(
      from = c(2L, 3L, 4L),
      to = c(1L, 2L, 3L),
      time = c(1, 2, 3),
      layer = "emails"
    )
  )
  info <- list(
    name = "toy",
    focal = "calls",
    update = c(
      friendship = "increment",
      calls = "increment",
      emails = "increment"
    ),
    directed = c(friendship = TRUE, calls = TRUE, emails = TRUE),
    observation = c(friendship = "event", calls = "event", emails = "event")
  )
  data <- list(info = info, nodes = nodes, ties = ties)
  calls_spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + tie(friendship, window = 2),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  emails_spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + tie(friendship, window = 2),
    layer = "emails",
    model = "DyNAM",
    data = data
  )
  make_joint_specification(calls_spec, emails_spec, data = data)
}

# ---- (b) Social Evolution, asta Copenhagen shape ---------------------------

# The stocnet form of the packaged dataset, built here rather than through
# `make_data()` so the fixture carries no deprecation warnings of its own.
parity_social_evolution_data <- function() {
  env <- new.env()
  utils::data("Social_Evolution", package = "goldfish", envir = env)
  nodes <- data.frame(
    label = env$actors$label,
    mode = "p",
    floor = env$actors$floor,
    gradeType = env$actors$gradeType,
    stringsAsFactors = FALSE
  )
  ties <- rbind(
    data.frame(
      from = env$calls$sender,
      to = env$calls$receiver,
      time = as.numeric(env$calls$time),
      layer = "call_network",
      weight = env$calls$increment,
      stringsAsFactors = FALSE
    ),
    data.frame(
      from = env$friendship$sender,
      to = env$friendship$receiver,
      time = as.numeric(env$friendship$time),
      layer = "friendship",
      weight = env$friendship$replace,
      stringsAsFactors = FALSE
    )
  )
  info <- list(
    name = "social_evolution",
    focal = "call_network",
    update = c(call_network = "increment", friendship = "replace"),
    directed = c(call_network = TRUE, friendship = TRUE),
    observation = c(call_network = "event", friendship = "panel")
  )
  list(info = info, nodes = nodes, ties = ties)
}

# The `rate_1` / `choice_1` shape of the asta Copenhagen benchmark, minus its
# two windowed terms, which the merged walk cannot yet build. Fourteen choice
# columns is a realistic `p`, so this fixture also carries the statistics-layout
# measurement.
parity_social_evolution_spec <- function(
  data = parity_social_evolution_data()
) {
  make_specification(
    rate = ~ 1 +
      indeg +
      indeg(call_network, weighted = TRUE) +
      outdeg(call_network, weighted = TRUE, transformer_fn = log1p) +
      node_trans +
      indeg(friendship) +
      ego(floor),
    choice = ~ inertia +
      inertia(call_network, weighted = TRUE) +
      recip +
      recip(call_network, weighted = TRUE, transformer_fn = log1p) +
      trans +
      trans(call_network, history = "sequential") +
      cycle +
      common_sender +
      indeg +
      indeg(call_network, weighted = TRUE) +
      tie(friendship) +
      alter(floor) +
      same(gradeType) +
      inertia:recip,
    layer = "call_network",
    model = "DyNAM",
    data = data
  )
}

# ---- (c) windowed support constraint ---------------------------------------

# `window` is passed through to the constraint atom. A five-second and a
# thousand-second window on an accumulating layer cannot describe the same risk
# set, so the three masks this builds must differ from each other.
parity_constraint_spec <- function(constraint, data = parity_toy_data()) {
  make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + recip,
    layer = "calls",
    model = "DyNAM",
    data = data,
    support_constraint = constraint
  )
}

parity_constraint_mask <- function(constraint, data = parity_toy_data()) {
  suppressWarnings(
    compute_statistics(
      parity_constraint_spec(constraint, data),
      "DyNAM",
      "choice"
    )
  )$support_mask
}

# ---- (d) flavored creation / dissolution -----------------------------------

# Two flavors on one layer, so `make_specification()` derives the complementary
# pair of constraints: creation is restricted to `~ !tie(calls)` and dissolution
# to `~ tie(calls)`. `inertia` is unusable by construction under that pair --
# identically 0 for creation and 1 for dissolution -- which is the point: the
# fixture cannot lean on tie history and has to ride the constraint machinery.
# `1 -> 2` is created before the first event, dissolved at t = 1, created again
# at t = 6 and dissolved at t = 7, so the mask over that dyad flips four times.
parity_flavored_data <- function() {
  nodes <- data.frame(
    label = c("A", "B", "C", "D"),
    mode = "p",
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    from = c(1L, 3L, 1L, 2L, 3L, 1L, 2L, 1L, 1L),
    to = c(2L, 4L, 2L, 3L, 4L, 3L, 3L, 2L, 2L),
    time = c(NA, NA, 1, 2, 3, 4, 5, 6, 7),
    layer = "calls",
    weight = c(1, 1, -1, 1, -1, 1, -1, 1, -1),
    stringsAsFactors = FALSE
  )
  info <- list(
    name = "parity_flavored",
    focal = "calls",
    update = c(calls = "increment"),
    directed = c(calls = TRUE),
    observation = c(calls = "event")
  )
  add_flavor(
    list(info = info, nodes = nodes, ties = ties),
    layer = "calls",
    values_equivalence = c(creation = 1, dissolution = -1)
  )
}

parity_flavored_spec <- function(data = parity_flavored_data()) {
  make_specification(
    rate = list(
      creation ~ 1 + indeg,
      dissolution ~ 1 + outdeg
    ),
    choice = list(
      creation ~ recip,
      dissolution ~ recip
    ),
    model = "DyNAM",
    data = data
  )
}

# ---- (e) missing data -------------------------------------------------------

# `NA` in a network history row and in a nodal covariate. The network `NA` is
# what puts an imputed matrix in `src$net_override`, which `ds_network()` then
# hands back as the same object on every later call -- the first aliasing site
# an in-place state write would expose.
parity_missing_data <- function() {
  nodes <- data.frame(
    label = paste0("N", 1:5),
    mode = "p",
    a = c(1, NA, 3, 4, NA),
    stringsAsFactors = FALSE
  )
  ties <- rbind(
    data.frame(
      from = c(1L, 3L),
      to = c(2L, 4L),
      time = c(NA, NA),
      layer = "calls",
      weight = c(NA, 1),
      stringsAsFactors = FALSE
    ),
    data.frame(
      from = c(1L, 2L, 1L, 3L),
      to = c(2L, 3L, 2L, 4L),
      time = c(1, 2, 3, 4),
      layer = "calls",
      weight = 1,
      stringsAsFactors = FALSE
    )
  )
  info <- list(
    name = "parity_missing",
    focal = "calls",
    update = c(calls = "increment"),
    directed = c(calls = TRUE),
    observation = c(calls = "event")
  )
  list(info = info, nodes = nodes, ties = ties)
}

# The `replace` twin of the fixture above. A replace layer never reaches the
# sign test that turns a missing cell into a crash, so an unimputed `NA` would
# travel through the effect closures into the statistics instead of stopping
# the walk. Same defect, quieter symptom.
parity_missing_replace_data <- function() {
  data <- parity_missing_data()
  data$info$update <- c(calls = "replace")
  data
}

parity_missing_spec <- function(data = parity_missing_data()) {
  make_specification(
    rate = ~ 1 + indeg + ego(a),
    choice = ~ inertia + alter(a),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
}
