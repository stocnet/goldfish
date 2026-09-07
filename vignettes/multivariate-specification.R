## ----load, message=FALSE------------------------------------------------------
library(goldfish)
library(manynet)
data("social_evolution")
social_evolution


## ----add-meetings-------------------------------------------------------------
set.seed(1)
n_actors <- nrow(social_evolution$nodes)
n_meetings <- 40L
from_idx <- sample.int(n_actors, n_meetings, replace = TRUE)
to_idx <- (from_idx + sample.int(n_actors - 1, n_meetings, replace = TRUE) - 1) %%
  n_actors + 1L
meetings <- data.frame(
  from = from_idx,
  to = to_idx,
  time = sort(as.POSIXct("2008-09-09") + sort(sample(1:1e6, n_meetings))),
  weight = 1,
  layer = "meetings"
)
meetings <- manynet::make_stocnet(
  info = list(
    name = "meetings",
    directed = TRUE,
    update = "increment",
    observation = "event"
  ),
  nodes = social_evolution$nodes,
  ties = meetings
)
joint_data <- manynet::from_ties(
  social_evolution, meetings,
  layer_names = c("social_evolution", "meetings")
)


## ----specs--------------------------------------------------------------------
calls_spec <- make_specification(
  rate = ~ 1 + indeg,
  choice = ~ inertia + recip,
  layer = "calls",
  model = "DyNAM",
  data = joint_data
)

meetings_spec <- make_specification(
  rate = ~ 1 + outdeg,
  choice = ~ inertia + tie(friendship),
  layer = "meetings",
  model = "DyNAM",
  data = joint_data
)

joint_spec <- make_joint_specification(calls_spec, meetings_spec, data = joint_data)
joint_spec


## ----flavored-meetings--------------------------------------------------------
meetings_flavored <- meetings
meetings_flavored$ties$weight[seq(2, n_meetings, by = 2)] <- -1
joint_data_flavored <- from_ties(
  social_evolution, meetings_flavored,
  layer_names = c("social_evolution", "meetings")
)
joint_data_flavored <- add_flavor(
  joint_data_flavored,
  layer = "meetings",
  values_equivalence = c(friendly = 1, tense = -1),
  flavor_style = "redundant"
)


## ----flavored-spec------------------------------------------------------------
meetings_flavored_spec <- make_specification(
  rate = list(friendly ~ 1 + outdeg, tense ~ 1 + outdeg),
  choice = list(friendly ~ inertia, tense ~ inertia),
  layer = "meetings",
  model = "DyNAM",
  data = joint_data_flavored
)

joint_spec_flavored <- make_joint_specification(
  calls_spec, meetings_flavored_spec,
  data = joint_data_flavored
)
joint_spec_flavored


## ----process-map--------------------------------------------------------------
joint_spec_flavored$process_map


## ----separability-------------------------------------------------------------
joint_spec_flavored$process_map[, c("fid", "layer", "coupled")]
joint_spec_flavored$modeled_panel


## ----panel-focal-rejected-----------------------------------------------------
tryCatch(
  make_specification(
    rate = ~ 1 + outdeg,
    choice = ~ inertia,
    layer = "friendship",
    model = "DyNAM",
    data = joint_data
  ),
  error = function(e) cat(conditionMessage(e))
)


## ----estimator-guard----------------------------------------------------------
tryCatch(
  estimate_dynam(joint_spec, sub_model = "choice"),
  error = function(e) cat(conditionMessage(e))
)


## ----half-specified-flavor----------------------------------------------------
meetings_half_spec <- make_specification(
  rate = list(friendly ~ 1 + outdeg),
  choice = list(friendly ~ inertia, tense ~ inertia),
  layer = "meetings",
  model = "DyNAM",
  data = joint_data_flavored
)
meetings_half_spec$completion_gaps


## ----half-specified-estimate--------------------------------------------------
tryCatch(
  estimate_dynam(meetings_half_spec, sub_model = "choice", data = joint_data_flavored),
  error = function(e) cat(conditionMessage(e))
)


## ----wave-diff----------------------------------------------------------------
friendship_snapshot <- as.data.frame(
  joint_data$ties[joint_data$ties$layer == "friendship", c("from", "to", "time")]
)
wave_times <- sort(unique(friendship_snapshot$time))
dyad_key <- function(d) paste(d$from, d$to)
wave1 <- friendship_snapshot[friendship_snapshot$time == wave_times[1], ]
wave2 <- friendship_snapshot[friendship_snapshot$time == wave_times[2], ]
formed <- wave2[!dyad_key(wave2) %in% dyad_key(wave1), c("from", "to")]
dissolved <- wave1[!dyad_key(wave1) %in% dyad_key(wave2), c("from", "to")]
friendship_changes <- rbind(
  data.frame(from = formed$from, to = formed$to,
             time = wave_times[1], weight = 1, layer = "friendship"),
  data.frame(from = dissolved$from, to = dissolved$to,
             time = wave_times[1], weight = 0, layer = "friendship")
)
nrow(formed); nrow(dissolved)


## ----case-a-demo--------------------------------------------------------------
joint_data_changes <- joint_data
joint_data_changes$ties <- dplyr::bind_rows(
  joint_data$ties[joint_data$ties$layer != "friendship", ],
  friendship_changes
)
friendship_flavored <- add_flavor(
  joint_data_changes,
  layer = "friendship",
  values_equivalence = c(formation = 1, dissolution = 0),
  flavor_style = "redundant"
)
event_declared <- friendship_flavored
event_declared$info$observation["friendship"] <- "event"
friendship_spec <- make_specification(
  rate = list(formation ~ 1 + outdeg),      # dissolution named nowhere
  choice = list(formation ~ inertia),       # in EITHER sub-model
  layer = "friendship",
  model = "DyNAM",
  data = event_declared
)
joint_spec2 <- make_joint_specification(
  friendship_spec, meetings_spec,
  data = friendship_flavored                 # friendship is "panel" here
)
tryCatch(
  goldfish:::complete_generative_spec(joint_spec2, consumer = "simulate"),
  error = function(e) cat(conditionMessage(e))
)

friendship_spec_no_choice <- make_specification(
  rate = list(formation ~ 1 + outdeg, dissolution ~ 1 + outdeg),      # dissolution named nowhere
  choice = list(formation ~ inertia),       # in EITHER sub-model
  layer = "friendship",
  model = "DyNAM",
  data = event_declared
)
joint_spec3 <- make_joint_specification(
  friendship_spec_no_choice, meetings_spec,
  data = friendship_flavored                 # friendship is "panel" here
)
tryCatch(
  goldfish:::complete_generative_spec(joint_spec3, consumer = "simulate"),
  error = function(e) cat(conditionMessage(e))
)

  friendship_spec_no_rate <- make_specification(
  rate = list(formation ~ 1 + outdeg),      # dissolution named nowhere
  choice = list(formation ~ inertia, dissolution ~ inertia),       # in EITHER sub-model
  layer = "friendship",
  model = "DyNAM",
  data = event_declared
)
joint_spec4 <- make_joint_specification(
  friendship_spec_no_rate, meetings_spec,
  data = friendship_flavored                 # friendship is "panel" here
)
tryCatch(
  goldfish:::complete_generative_spec(joint_spec4, consumer = "simulate"),
  error = function(e) cat(conditionMessage(e))
)


