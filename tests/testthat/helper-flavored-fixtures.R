# Fixtures for the multi-process (flavored) container, shared by every test
# file that needs a fitted specification: `test-estimate_flavored.R` builds
# containers to compare against standalone fits, `test-test_gof.R` to test one.
# They live here rather than in either file because a second copy of a
# 70-line generator is a second thing to keep correct.

# A state-consistent alternating event stream: creations only ever target an
# absent tie and dissolutions only a present one, so a mutually exclusive mask
# never excludes an observed dyad. Sized so the fits are well conditioned --
# small fixtures separate, which would make the estimates meaningless to
# compare.
flavored_event_stream <- function(n_actors = 12L, n_events = 80L, seed = 3L) {
  withr::local_seed(seed)
  state <- matrix(0L, n_actors, n_actors)
  hist_from <- seq_len(n_actors)
  hist_to <- (hist_from %% n_actors) + 1L
  state[cbind(hist_from, hist_to)] <- 1L

  from <- integer(0)
  to <- integer(0)
  weight <- numeric(0)
  for (e in seq_len(n_events)) {
    creating <- (e %% 2L) == 1L
    cand <- which(if (creating) state == 0L else state == 1L, arr.ind = TRUE)
    cand <- cand[cand[, 1] != cand[, 2], , drop = FALSE]
    if (nrow(cand) == 0L) {
      next
    }
    pick <- cand[sample.int(nrow(cand), 1L), ]
    from <- c(from, as.integer(pick[[1]]))
    to <- c(to, as.integer(pick[[2]]))
    weight <- c(weight, if (creating) 1 else -1)
    state[pick[[1]], pick[[2]]] <- if (creating) 1L else 0L
  }

  data.frame(
    from = c(hist_from, from),
    to = c(hist_to, to),
    time = c(rep(NA_real_, n_actors), seq_along(from)),
    layer = "calls",
    weight = c(rep(1, n_actors), weight),
    stringsAsFactors = FALSE
  )
}

flavored_fixture_data <- function(style = "mutually_exclusive", ...) {
  ties <- flavored_event_stream(...)
  n_actors <- max(ties$from, ties$to)
  add_flavor(
    list(
      info = list(
        name = "toy",
        focal = "calls",
        update = c(calls = "increment"),
        directed = c(calls = TRUE),
        observation = c(calls = "event")
      ),
      nodes = data.frame(
        label = paste0("N", seq_len(n_actors)),
        mode = "p",
        stringsAsFactors = FALSE
      ),
      ties = ties
    ),
    layer = "calls",
    values_equivalence = c(creation = 1, dissolution = -1),
    flavor_style = style
  )
}

# The container's fit for one process, found through the process_map rather than
# by position.
fit_of <- function(res, flavor, family) {
  map <- res$process_map
  fid <- map$fid[map$flavor == flavor & map$family == family]
  res$results[[as.character(fid)]]
}

# The standard four-process container -- two flavors by two families -- which is
# the shape every flavored-method test needs. Kept here for the same reason the
# generator is: the specification is identical in each of them, and a second
# copy is a second thing to keep in step.
flavored_container_fit <- function(data = flavored_fixture_data(), ...) {
  suppressWarnings(estimate_dynam(
    make_specification(
      rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg),
      choice = list(creation ~ trans, dissolution ~ trans),
      model = "DyNAM",
      data = data
    ),
    ...
  ))
}

# One family per flavor, so a `flavor =` selection resolves to exactly one
# process. That is the shape the Fisheries Treaties fits have, and the only one
# on which `flavor =` returns the single-fit shape rather than a shorter list.
flavored_single_family_fit <- function(data = flavored_fixture_data(), ...) {
  suppressWarnings(estimate_dynam(
    make_specification(
      rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg),
      model = "DyNAM",
      data = data
    ),
    ...
  ))
}
