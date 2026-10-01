# Fixtures for the constraint-atoms-as-operands change: one constrained
# preprocessed object per estimated family (a rate, a choice, a REM and a
# coordination model), each carrying a `support_constraint`. These are the
# byte-identity targets the migration of the atoms onto the main walk must not
# move: the mask stream and the folded `active_sender` / `active_dyad`. Two
# more families, a rate and a choice, constrain on an object no formula term
# reads: the case the shared walk did not carry.
#
# The builders are deliberately self-contained (each assembles its own data)
# so a single test can ask for one family without dragging the others in.

# Social Evolution rate / choice / REM share this node set and call layer.
constraint_atoms_social_data <- function(n_events = 40L) {
  data("Social_Evolution", package = "goldfish", envir = environment())
  call_network <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(call_network, calls, nodes = actors)
  dep <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = call_network
  )
  dep <- dep[seq_len(n_events), ]
  list(
    actors = actors,
    calls = calls,
    call_network = call_network,
    dep = dep,
    data = suppressWarnings(make_data(dep, call_network, calls, actors))
  )
}

# An allowed-dyad network dropping `n_excluded` never-observed dyads, so no
# observed dyad is excluded by the constraint (which would abort preprocessing).
constraint_atoms_allowed_net <- function(fx, n_excluded, directed = TRUE) {
  actors <- fx$actors
  lab <- actors$label
  n <- nrow(actors)
  obs <- cbind(
    match(as.data.frame(fx$dep)$sender, lab),
    match(as.data.frame(fx$dep)$receiver, lab)
  )
  allowed <- matrix(1, n, n, dimnames = list(lab, lab))
  diag(allowed) <- 0
  set.seed(1L)
  drawn <- 0L
  while (drawn < n_excluded) {
    i <- sample(n, 1L)
    j <- sample(n, 1L)
    if (i != j && !any(obs[, 1] == i & obs[, 2] == j)) {
      allowed[i, j] <- 0
      drawn <- drawn + 1L
    }
  }
  make_network(matrix = allowed, nodes = actors, directed = directed)
}

constraint_atoms_coord_data <- function(n_excluded = 1000L, seed = 1L) {
  data("Fisheries_Treaties_6070", package = "goldfish", envir = environment())
  states <- make_nodes(states)
  states <- link_events(states, sovchanges, attribute = "present")
  states <- link_events(states, regchanges, attribute = "regime")
  states <- link_events(states, gdpchanges, attribute = "gdp")
  bilatnet <- make_network(bilatnet, nodes = states, directed = FALSE)
  bilatnet <- link_events(bilatnet, bilatchanges, nodes = states)
  contignet <- make_network(contignet, nodes = states, directed = FALSE)
  contignet <- link_events(contignet, contigchanges, nodes = states)
  depc <- bilatchanges[bilatchanges$increment == 1, ]
  create_bilat <- make_dependent_events(
    events = depc,
    nodes = states,
    default_network = bilatnet
  )
  lab <- states$label
  n <- nrow(states)
  obs <- cbind(match(depc$sender, lab), match(depc$receiver, lab))
  allowed <- matrix(1, n, n, dimnames = list(lab, lab))
  diag(allowed) <- 0
  set.seed(seed)
  drawn <- 0L
  while (drawn < n_excluded) {
    i <- sample(n, 1L)
    j <- sample(n, 1L)
    observed <- any(
      (obs[, 1] == i & obs[, 2] == j) | (obs[, 1] == j & obs[, 2] == i)
    )
    if (i != j && !observed && allowed[i, j] == 1) {
      allowed[i, j] <- 0
      allowed[j, i] <- 0
      drawn <- drawn + 1L
    }
  }
  allowedNet <- make_network(matrix = allowed, nodes = states, directed = FALSE)
  suppressWarnings(make_data(
    create_bilat,
    bilatnet,
    contignet,
    allowedNet,
    states,
    bilatchanges,
    contigchanges,
    sovchanges,
    regchanges,
    gdpchanges
  ))
}

# A toy call stream beside an `emails` layer no formula term reads, so a
# constraint on `emails` is the uncovered case: its object is on no unit's
# schedule. No emailed dyad is ever called, so `~ !tie(emails)` excludes
# dyads without excluding an observed one. N6 never calls and emails every
# other node by t = 3.7, so the rate's row-reduction gates it from then on.
constraint_atoms_uncovered_data <- function() {
  ties <- rbind(
    data.frame(
      from = c(1L, 2L, 3L, 4L, 5L),
      to = c(2L, 3L, 4L, 5L, 1L),
      time = c(1, 2, 3, 4, 5),
      layer = "calls"
    ),
    data.frame(
      from = c(2L, 3L, 4L, 6L, 6L, 6L, 6L, 6L),
      to = c(1L, 2L, 3L, 1L, 2L, 3L, 4L, 5L),
      time = c(1.5, 2.5, 3.5, 4.5, 1.7, 2.2, 2.7, 3.7),
      layer = "emails"
    )
  )
  list(
    info = list(
      name = "toy",
      focal = "calls",
      update = c(calls = "increment", emails = "increment"),
      directed = c(calls = TRUE, emails = TRUE),
      observation = c(calls = "event", emails = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:6),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = ties
  )
}

constraint_atoms_uncovered_spec <- function() {
  make_specification(
    rate = ~ 1 + indeg,
    choice = ~inertia,
    layer = "calls",
    model = "DyNAM",
    data = constraint_atoms_uncovered_data(),
    support_constraint = ~ !tie(emails)
  )
}

# The constrained preprocessed object for one estimated family. `family` is one
# of `constraint_atoms_families()`. Warnings (deprecated data path, forced
# choices under the constraint) are silenced: this is a fixture, not an
# assertion about them.
constraint_atoms_prep <- function(family) {
  suppressWarnings(suppressMessages(switch(
    family,
    rate = {
      fx <- constraint_atoms_social_data()
      spec <- make_specification(
        rate = ~ 1 + indeg,
        choice = ~inertia,
        model = "DyNAM",
        layer = "dep",
        support_constraint = ~ tie(call_network),
        data = fx$data
      )
      estimate_dynam(spec, sub_model = "rate", preprocessing_only = TRUE)
    },
    choice = {
      fx <- constraint_atoms_social_data()
      dep <- fx$dep
      estimate_dynam(
        dep ~ inertia + recip,
        sub_model = "choice",
        data = fx$data,
        preprocessing_only = TRUE,
        support_constraint = ~ tie(call_network)
      )
    },
    rem = {
      fx <- constraint_atoms_social_data()
      allowedNet <- constraint_atoms_allowed_net(fx, n_excluded = 50L)
      dep <- fx$dep
      call_network <- fx$call_network
      calls <- fx$calls
      actors <- fx$actors
      d <- suppressWarnings(make_data(
        dep,
        call_network,
        calls,
        actors,
        allowedNet
      ))
      estimate_rem(
        dep ~ 1 + inertia + recip,
        sub_model = "rate",
        data = d,
        support_constraint = ~ tie(allowedNet),
        preprocessing_only = TRUE
      )
    },
    coord = {
      d <- constraint_atoms_coord_data()
      estimate_dynam(
        create_bilat ~ inertia + indeg + trans + tie(contignet),
        sub_model = "choice_coordination",
        data = d,
        support_constraint = ~ tie(allowedNet),
        preprocessing_only = TRUE
      )
    },
    uncovered_rate = estimate_dynam(
      constraint_atoms_uncovered_spec(),
      sub_model = "rate",
      preprocessing_only = TRUE
    ),
    uncovered_choice = estimate_dynam(
      constraint_atoms_uncovered_spec(),
      sub_model = "choice",
      preprocessing_only = TRUE
    ),
    cli::cli_abort("Unknown constraint-atoms family {.val {family}}.")
  )))
}

constraint_atoms_families <- function() {
  c("rate", "choice", "rem", "coord", uncovered_constraint_families())
}

# The families whose constraint reads an object no formula term reads.
uncovered_constraint_families <- function() {
  c("uncovered_rate", "uncovered_choice")
}

# The fields the migration must keep byte-identical: the estimated statistics
# streams, the presence axes, and the whole support-mask stream. Everything a
# constraint touches on the way to the fitter is here.
constraint_atoms_capture <- function(prep) {
  keep <- c(
    "initial_stats",
    "stat_mat_update",
    "stat_mat_pointer",
    "stat_mat_broadcast",
    "stat_mat_broadcast_pointer",
    "active_sender_init",
    "active_sender_changes",
    "active_sender_update",
    "active_sender_update_pointer",
    "active_dyad_init",
    "active_dyad_changes",
    "active_dyad_encoding",
    "active_dyad_update",
    "active_dyad_update_pointer",
    "active_dyad_folded",
    "support_mask",
    "event_time"
  )
  prep[keep]
}

# Generated data large enough that the cost of a walk is measurable: at 1500
# actors one double grid is 18 MB, so a second state container or a
# per-event grid stands out from noise. `emails` is read only by the
# constraint `~ !tie(emails)`, and no emailed dyad is ever called, so the
# constraint excludes dyads without excluding an observed one.
constraint_cost_data <- function(
  n_actors = 1500L,
  n_calls = 3000L,
  n_emails = 3000L,
  seed = 1L
) {
  withr::local_seed(seed)
  calls <- data.frame(
    from = sample.int(n_actors, n_calls, replace = TRUE),
    to = sample.int(n_actors, n_calls, replace = TRUE)
  )
  calls <- calls[calls$from != calls$to, ]
  calls$time <- sort(stats::runif(nrow(calls), 0, 100))
  emails <- data.frame(
    from = sample.int(n_actors, n_emails, replace = TRUE),
    to = sample.int(n_actors, n_emails, replace = TRUE)
  )
  emails <- emails[emails$from != emails$to, ]
  called <- paste(calls$from, calls$to)
  emails <- emails[!paste(emails$from, emails$to) %in% called, ]
  emails$time <- sort(stats::runif(nrow(emails), 0, 100))
  list(
    info = list(
      name = "walk cost",
      focal = "calls",
      update = c(calls = "increment", emails = "increment"),
      directed = c(calls = TRUE, emails = TRUE),
      observation = c(calls = "event", emails = "event")
    ),
    nodes = data.frame(
      label = sprintf("A%04d", seq_len(n_actors)),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = rbind(
      cbind(calls, layer = "calls"),
      cbind(emails, layer = "emails")
    )
  )
}

# The merged walk's substrate for one family: the shared object registry and
# the joint schedule, captured as `build_merged_blocks()` builds them. The
# unconstrained and covered families must keep both byte-identical when a
# constraint-only object is threaded onto the shared walk; the uncovered
# ones may only gain rows after the existing ones.
constraint_atoms_substrate <- function(family) {
  captured <- list()
  orig_objects <- build_shared_objects
  orig_schedule <- build_joint_schedule
  testthat::local_mocked_bindings(
    build_shared_objects = function(...) {
      shared <- orig_objects(...)
      captured$registry <<- shared$registry
      shared
    },
    build_joint_schedule = function(...) {
      schedule <- orig_schedule(...)
      captured$schedule <<- schedule
      schedule
    }
  )
  if (family %in% c("unconstrained_rate", "unconstrained_choice")) {
    spec <- suppressMessages(make_specification(
      rate = ~ 1 + indeg,
      choice = ~inertia,
      layer = "calls",
      model = "DyNAM",
      data = constraint_atoms_uncovered_data()
    ))
    suppressWarnings(estimate_dynam(
      spec,
      sub_model = sub("unconstrained_", "", family),
      preprocessing_only = TRUE
    ))
  } else {
    constraint_atoms_prep(family)
  }
  captured
}

constraint_atoms_substrate_families <- function() {
  c(
    "unconstrained_rate",
    "unconstrained_choice",
    constraint_atoms_families()
  )
}

# Families whose constraint serves several outputs of one walk: a rate and a
# choice sharing one atom store, and a flavored layer's creation and
# dissolution processes under their derived masks. Captured per fid.
constraint_atoms_joint_families <- function() {
  c("joint_rate_choice", "flavored")
}

constraint_atoms_joint_capture <- function(family) {
  spec <- suppressWarnings(suppressMessages(switch(
    family,
    joint_rate_choice = constraint_atoms_uncovered_spec(),
    flavored = make_specification(
      rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg),
      choice = list(creation ~ trans, dissolution ~ trans),
      model = "DyNAM",
      data = flavored_fixture_data()
    ),
    cli::cli_abort("Unknown joint constraint-atoms family {.val {family}}.")
  )))
  outputs <- suppressWarnings(preprocess_joint(single_process_joint(spec)))
  lapply(outputs, constraint_atoms_capture)
}

# Vector-kind constraints on the dyad loops. A REM or coordination model whose
# mask is a per-actor value (alter or ego kind) is fully described by two
# factor vectors, yet it has been folded on a dense point grid; these families
# freeze that point fold so a factor-vector fold can be checked against it cell
# for cell. Every actor the mask excludes is one no dependent event touches, so
# no observed event is excluded, and the receiver composition changes and the
# attribute changes fall on those same idle actors.
#
# The node-attribute changes are linked before the composition changes:
# `link_events()` looks a node set's composition frame up by name, which a
# function-local frame does not survive.
availability_vector_rem_data <- function(n_events = 100L) {
  data("Social_Evolution", package = "goldfish", envir = environment())
  call_network <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(call_network, calls, nodes = actors)
  dep <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = call_network
  )
  dep <- dep[seq_len(n_events), ]
  observed <- as.data.frame(dep)
  lab <- actors$label
  idle <- setdiff(lab, unique(c(observed$sender, observed$receiver)))
  start <- min(observed$time)
  span <- max(observed$time) - start
  comp <- data.frame(
    time = start + span * c(0.3, 0.4, 0.6),
    node = idle[c(1, 2, 1)],
    replace = c(FALSE, FALSE, TRUE)
  )
  actors$allow <- 1
  actors$allow[match(idle[3:5], lab)] <- 0
  allow_changes <- data.frame(
    time = start + span * c(0.5, 0.7),
    node = idle[c(6, 3)],
    replace = c(0, 1)
  )
  # A global switch that turns off and back on inside the longest gap between
  # dependent events: while it is off the risk set is empty, an interval with
  # total rate 0, and no observed event falls in it.
  gap <- which.max(diff(observed$time))
  switch_changes <- data.frame(
    time = observed$time[gap] + diff(observed$time)[gap] * c(0.25, 0.75),
    replace = c(0, 1)
  )
  switches <- make_global_attributes(data.frame(on = 1))
  switches <- link_events(switches, switch_changes)
  actors <- make_nodes(actors)
  actors <- link_events(actors, allow_changes, attribute = "allow")
  actors <- link_events(actors, comp, attribute = "present")
  data <- make_data(
    dep,
    call_network,
    calls,
    actors,
    comp,
    allow_changes,
    switches
  )
  list(data = data, actors = actors, switches = switches)
}

availability_vector_coord_data <- function() {
  data("Fisheries_Treaties_6070", package = "goldfish", envir = environment())
  depc <- bilatchanges[bilatchanges$increment == 1, ]
  lab <- states$label
  idle <- setdiff(lab, unique(c(depc$sender, depc$receiver)))
  states$allow <- 1
  states$allow[match(idle[1:5], lab)] <- 0
  allow_changes <- data.frame(
    time = depc$time[c(10, 40)],
    node = idle[c(6, 1)],
    replace = c(0, 1)
  )
  states <- make_nodes(states)
  states <- link_events(states, allow_changes, attribute = "allow")
  states <- link_events(states, sovchanges, attribute = "present")
  bilatnet <- make_network(bilatnet, nodes = states, directed = FALSE)
  bilatnet <- link_events(bilatnet, bilatchanges, nodes = states)
  create_bilat <- make_dependent_events(
    events = depc,
    nodes = states,
    default_network = bilatnet
  )
  data <- make_data(
    create_bilat,
    bilatnet,
    states,
    bilatchanges,
    sovchanges,
    allow_changes
  )
  list(data = data, states = states)
}

availability_vector_families <- function() {
  c("rem_alter", "rem_ego", "coord_alter")
}

# One vector-kind family's preprocessed object (`backend = NULL`) or fit.
availability_vector_run <- function(family, backend = NULL) {
  suppressWarnings(suppressMessages({
    if (family == "coord_alter") {
      fx <- availability_vector_coord_data()
      states <- fx$states
      args <- list(
        create_bilat ~ inertia + indeg + trans,
        sub_model = "choice_coordination",
        data = fx$data,
        support_constraint = ~ alter(states$allow) > 0
      )
      estimator <- estimate_dynam
    } else {
      fx <- availability_vector_rem_data()
      actors <- fx$actors
      args <- list(
        dep ~ 1 + inertia + recip,
        sub_model = "rate",
        data = fx$data,
        support_constraint = switch(
          family,
          rem_alter = ~ alter(actors$allow) > 0,
          rem_ego = ~ ego(actors$allow) > 0
        )
      )
      estimator <- estimate_rem
    }
    if (is.null(backend)) {
      args$preprocessing_only <- TRUE
    } else {
      args$control_algo <- set_algorithm_newton(backend = backend)
    }
    do.call(estimator, args)
  }))
}

# The dense availability grid at every stored row, replayed from a point-
# encoded object's stream: one logical vector of length `n1 * n2` per row.
availability_point_cells <- function(prep) {
  grid <- as.vector(prep$active_dyad_init)
  n1 <- nrow(prep$active_dyad_init)
  update <- prep$active_dyad_update
  pointer <- prep$active_dyad_update_pointer
  seen <- 0
  cells <- vector("list", length(pointer))
  for (row in seq_along(pointer)) {
    if (pointer[[row]] > seen) {
      cols <- (seen + 1):pointer[[row]]
      grid[update[1L, cols] + n1 * (update[2L, cols] - 1L)] <-
        as.logical(update[3L, cols])
      seen <- pointer[[row]]
    }
    cells[[row]] <- grid
  }
  cells
}
