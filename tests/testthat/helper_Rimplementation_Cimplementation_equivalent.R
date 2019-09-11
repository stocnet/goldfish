# Data

data("Social_Evolution")
callNetwork <- defineNetwork(nodes = actors, directed = TRUE)
callNetwork <- linkEvents(x = callNetwork, changeEvent = calls, nodes = actors)
callsDependent <- defineDependentEvents(events = calls, nodes = actors, defaultNetwork = callNetwork)

# parameterRemOrderedRImplementaion = c(6.1992539183744,1.43600453526761,0.164126783806186)
