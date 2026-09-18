# Fixtures for the constraint-atoms-as-operands change: one constrained
# preprocessed object per estimated family (a rate, a choice, a REM and a
# coordination model), each carrying a `support_constraint`. These are the
# byte-identity targets the migration of the atoms onto the main walk must not
# move: the mask stream and the folded `active_sender` / `active_dyad`.
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

# The constrained preprocessed object for one estimated family. `family` is one
# of "rate", "choice", "rem", "coord". Warnings (deprecated data path, forced
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
    cli::cli_abort("Unknown constraint-atoms family {.val {family}}.")
  )))
}

constraint_atoms_families <- function() c("rate", "choice", "rem", "coord")

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
