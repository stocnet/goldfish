# Frozen classical-equivalence references.
#
# These are NOT goldfish coefficient baselines (those live in `_baselines/`
# and are the package's own regression floor). These are numbers minted by a
# DIFFERENT implementation, so that a goldfish result can be checked against
# something goldfish did not compute.
#
# goldfish's likelihoods are classical likelihoods with a network design
# matrix, so each sub-model has a classical fit that is the SAME likelihood:
#
#   ordinal REM            survival::coxph  (Cox partial likelihood, dyads in
#                                            start/stop form on the event clock)
#   DyNAM choice           survival::clogit (a conditional logit IS a
#                                            stratified Cox; one stratum per
#                                            event)
#   ordinal rate           survival::clogit over the actors at risk
#   exact-time rate / REM  stats::glm poisson with offset(log(dt))
#                                            (piecewise-exponential equivalence)
#
# The design matrix here is walked BY HAND from the event list -- neither
# goldfish's statistic engine nor any other package computes it. That is the
# point: an agreement between goldfish and a reference built from goldfish's
# own statistics would test the likelihood alone, while this tests the
# statistic too. The walk is deliberately naive (a dense adjacency matrix
# updated one event at a time) so that it is obviously right rather than fast.
#
# Only the survival-derived numbers are frozen. The `stats::glm` equivalences
# are NOT stored: stats ships with R, so the test recomputes them live and no
# provenance question arises.
#
# Coordination is absent on purpose -- see the README beside this file.
#
# Regenerating is a deliberate act with a documented justification. From the
# package root:
#
#   Rscript tests/testthat/_references/classical_v1/\
#     generate_classical_references.R
devtools::load_all(".")
library(survival)

data("social_evolution", package = "goldfish")

nodes <- social_evolution$nodes
calls <- social_evolution$ties[social_evolution$ties$layer == "calls", ]
calls <- calls[order(as.numeric(calls$time)), ]
n_actors <- nrow(nodes)
n_events <- nrow(calls)
sender <- calls$from
receiver <- calls$to
times <- as.numeric(calls$time)
# One unit before the first event, so the opening interval has positive
# elapsed time and the Poisson offset is finite.
start_time <- min(times) - 1

# The hand-walked designs ----------------------------------------------------

# Dyad level: one row per event and ordered pair, statistics read BEFORE the
# event is applied.
dyad_design <- function() {
  pairs <- expand.grid(j = seq_len(n_actors), i = seq_len(n_actors))
  pairs <- pairs[pairs$i != pairs$j, c("i", "j")]
  n_dyads <- nrow(pairs)
  ij <- cbind(pairs$i, pairs$j)
  ji <- cbind(pairs$j, pairs$i)

  net <- matrix(0, n_actors, n_actors)
  rows <- n_events * n_dyads
  out <- list(
    event = integer(rows),
    chosen = integer(rows),
    inertia = numeric(rows),
    recip = numeric(rows),
    dt = numeric(rows)
  )
  prev <- start_time
  pos <- 0L
  for (k in seq_len(n_events)) {
    idx <- pos + seq_len(n_dyads)
    out$inertia[idx] <- net[ij]
    out$recip[idx] <- net[ji]
    out$event[idx] <- k
    out$chosen[idx] <- as.integer(
      pairs$i == sender[k] & pairs$j == receiver[k]
    )
    out$dt[idx] <- times[k] - prev
    prev <- times[k]
    pos <- pos + n_dyads
    net[sender[k], receiver[k]] <- net[sender[k], receiver[k]] + 1
  }
  as.data.frame(out)
}

# Receiver level: one row per event and alternative receiver, the sender's own
# row excluded, which is goldfish's default receiver risk set.
receiver_design <- function() {
  net <- matrix(0, n_actors, n_actors)
  rows <- n_events * (n_actors - 1)
  out <- list(
    event = integer(rows),
    chosen = integer(rows),
    inertia = numeric(rows),
    recip = numeric(rows)
  )
  pos <- 0L
  for (k in seq_len(n_events)) {
    alts <- setdiff(seq_len(n_actors), sender[k])
    idx <- pos + seq_along(alts)
    out$inertia[idx] <- net[sender[k], alts]
    out$recip[idx] <- net[alts, sender[k]]
    out$event[idx] <- k
    out$chosen[idx] <- as.integer(alts == receiver[k])
    pos <- pos + length(alts)
    net[sender[k], receiver[k]] <- net[sender[k], receiver[k]] + 1
  }
  as.data.frame(out)
}

# Sender level: one row per event and actor, every actor at risk of acting.
sender_design <- function() {
  net <- matrix(0, n_actors, n_actors)
  rows <- n_events * n_actors
  out <- list(
    event = integer(rows),
    chosen = integer(rows),
    indeg = numeric(rows),
    outdeg = numeric(rows),
    dt = numeric(rows)
  )
  prev <- start_time
  pos <- 0L
  for (k in seq_len(n_events)) {
    idx <- pos + seq_len(n_actors)
    out$indeg[idx] <- colSums(net)
    out$outdeg[idx] <- rowSums(net)
    out$event[idx] <- k
    out$chosen[idx] <- as.integer(seq_len(n_actors) == sender[k])
    out$dt[idx] <- times[k] - prev
    prev <- times[k]
    pos <- pos + n_actors
    net[sender[k], receiver[k]] <- net[sender[k], receiver[k]] + 1
  }
  as.data.frame(out)
}

message("walking the designs ...")
dyads <- dyad_design()
receivers <- receiver_design()
senders <- sender_design()

references <- list()

# ordinal REM <-> coxph ------------------------------------------------------
message("coxph: ordinal REM twin ...")
cox_ord <- coxph(
  Surv(event - 1, event, chosen) ~ inertia + recip,
  data = dyads,
  ties = "breslow"
)
references$rem_ordered <- list(
  formula = paste(
    "calls ~ inertia(calls, weighted = TRUE) +",
    "recip(calls, weighted = TRUE)"
  ),
  sub_model = "rate_ordered",
  reference = "survival::coxph(ties = \"breslow\")",
  coefficients = unname(coef(cox_ord)),
  loglik = as.numeric(logLik(cox_ord))
)

# DyNAM choice <-> clogit ----------------------------------------------------
message("clogit: DyNAM choice twin ...")
cl_choice <- clogit(
  chosen ~ inertia + recip + strata(event),
  data = receivers
)
references$dynam_choice <- list(
  formula = paste(
    "calls ~ inertia(calls, weighted = TRUE) +",
    "recip(calls, weighted = TRUE)"
  ),
  sub_model = "choice",
  reference = "survival::clogit",
  coefficients = unname(coef(cl_choice)),
  loglik = as.numeric(logLik(cl_choice))
)

# ordinal rate <-> clogit ----------------------------------------------------
message("clogit: ordinal rate twin ...")
cl_rate <- clogit(
  chosen ~ indeg + outdeg + strata(event),
  data = senders
)
references$dynam_rate_ordered <- list(
  formula = paste(
    "calls ~ indeg(calls, weighted = TRUE) +",
    "outdeg(calls, weighted = TRUE)"
  ),
  sub_model = "rate_ordered",
  reference = "survival::clogit",
  coefficients = unname(coef(cl_rate)),
  loglik = as.numeric(logLik(cl_rate))
)

# The residual convention, at a shared parameter vector ----------------------
#
# The load-bearing comparison. Both sides are evaluated at the SAME theta, so
# nothing below is optimizer noise: what it pins is the Grambsch-Therneau
# scaling convention (theta + n I^-1 s_k, and which n). On a Cox-expressible
# fixture every likelihood event is a death, so survival's event count and
# goldfish's dependent-event count coincide by construction -- which is
# precisely why the matrix comparison, not the count, is what carries the
# check.
message("coxph at shared theta: residual conventions ...")
fit_gf_ord <- estimate_rem(
  calls ~ inertia(calls, weighted = TRUE) + recip(calls, weighted = TRUE),
  sub_model = "rate_ordered",
  data = social_evolution,
  control_prep = set_preprocessing(start_time = start_time),
  control_algo = set_algorithm_newton(diagnostics = c("loglik", "scores"))
)
shared_theta <- unname(coef(fit_gf_ord))
cox_at <- coxph(
  Surv(event - 1, event, chosen) ~ inertia + recip,
  data = dyads,
  ties = "breslow",
  init = shared_theta,
  control = coxph.control(iter.max = 0)
)
references$residuals_shared_theta <- list(
  formula = paste(
    "calls ~ inertia(calls, weighted = TRUE) +",
    "recip(calls, weighted = TRUE)"
  ),
  sub_model = "rate_ordered",
  reference = "survival::coxph(init = theta, iter.max = 0)",
  theta = shared_theta,
  schoenfeld = unname(residuals(cox_at, type = "schoenfeld")),
  scaled_schoenfeld = unname(residuals(cox_at, type = "scaledsch"))
)

# The proportionality table, for the test_time() work that reads it later ----
message("cox.zph: per-transform table ...")
references$cox_zph <- lapply(
  c(identity = "identity", rank = "rank", km = "km", log = "log"),
  function(tf) {
    zph <- cox.zph(cox_ord, transform = tf, global = TRUE)
    list(table = zph$table, transform = tf)
  }
)

# Provenance -----------------------------------------------------------------
references$provenance <- list(
  minted = as.character(Sys.Date()),
  r_version = paste(R.version$major, R.version$minor, sep = "."),
  survival_version = as.character(packageVersion("survival")),
  goldfish_version = as.character(packageVersion("goldfish")),
  fixture = paste(
    "social_evolution calls layer:",
    n_events,
    "events,",
    n_actors,
    "actors; start_time = min(time) - 1"
  ),
  design = paste(
    "Statistics walked by hand from the event list in this script;",
    "neither goldfish nor remstats computed them."
  )
)

saveRDS(
  references,
  file.path(
    "tests",
    "testthat",
    "_references",
    "classical_v1",
    "classical_references.rds"
  ),
  version = 2
)
message("written.")
