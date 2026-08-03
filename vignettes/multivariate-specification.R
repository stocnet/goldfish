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

joint_data <- social_evolution
joint_data$ties <- rbind(as.data.frame(social_evolution$ties), meetings)
joint_data$info$directed <- c(joint_data$info$directed, meetings = TRUE)
joint_data$info$update <- c(joint_data$info$update, meetings = "increment")
joint_data$info$observation <- c(joint_data$info$observation, meetings = "event")


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


## ----process-map--------------------------------------------------------------
joint_spec$process_map


## ----separability-------------------------------------------------------------
joint_spec$process_map[, c("fid", "layer", "coupled")]
joint_spec$modeled_panel


## ----estimator-guard----------------------------------------------------------
tryCatch(
  estimate_dynam(joint_spec, sub_model = "choice"),
  error = function(e) conditionMessage(e)
)

