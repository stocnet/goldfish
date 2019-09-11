# Data

data("Social_Evolution")
callNetwork <- defineNetwork(nodes = actors, directed = T)
callNetwork <- linkEvents(x = callNetwork, changeEvent = calls, nodes = actors)
callsDependent <- defineDependentEvents(events = calls, nodes = actors, defaultNetwork = callNetwork)

model <- "DyNAM"
subModel <- "choice"
formula  <- callsDependent ~ inertia + recip + trans
