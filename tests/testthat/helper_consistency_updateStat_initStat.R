data("Social_Evolution")
# callNetwork = matrix(0,84,84)
# callNetwork[83,84] =20
# callNetwork <- defineNetwork(callNetwork,nodes = actors, directed = T)

callNetwork <- defineNetwork(nodes = actors, directed = T)
callNetwork <- linkEvents(x = callNetwork, changeEvent = calls, nodes = actors)
callsDependent <- defineDependentEvents(events = calls, nodes = actors, defaultNetwork = callNetwork)

# model specification
model <- "DyNAM"
subModel <- "choice"

formula <- callsDependent ~ inertia + recip + trans + indeg + tie + four
#+ alter(actors$floor)
# + same(actors$gradeType) + sim(actors$floor)

# preprocess data according with model specification
prep <- estimate(formula,
  model = model, subModel = subModel,
  preprocessingOnly = TRUE, silent = TRUE
)

# preprocess data using the update network without the last event
# preprocess only keep updates of events that had happen before and dependant event update
finalNetwork <- UpdateNetwork(callNetwork, head(calls, -1), nodes = "actors")
# friendNetwork2 <- UpdateNetwork(friendNetwork, subset(friendship, time < max(calls$time)))
# finalNetwork <- UpdateNetwork(callNetwork, calls, nodes = "actors")

callNetwork2 <- defineNetwork(finalNetwork, nodes = actors, directed = TRUE)
newCalls <- tail(calls, 1)
callNetwork2 <- linkEvents(x = callNetwork2, changeEvent = newCalls, nodes = actors)
callsDependent <- defineDependentEvents(events = newCalls, nodes = actors, defaultNetwork = callNetwork2)

prep_new <- estimate(formula,
  model = model, subModel = subModel,
  preprocessingOnly = TRUE, silent = TRUE
)

# reduce preprocess object and update initial stat matrix from prep
finalStat <- mapply(
  function(x, y) {
    UpdateNetwork(x, y)
  },
  lapply(seq(dim(prep$initialStats)[3]), function(x) prep$initialStats[, , x]),
  ReducePreprocess(prep, type = "withoutTime"),
  SIMPLIFY = FALSE
)

initStatList <- lapply(seq(dim(prep_new$initialStats)[3]), function(x) prep_new$initialStats[, , x])
