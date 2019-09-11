data("Fisheries_Treaties_6070")
states <- defineNodes(states)
states <- linkEvents(states, sovchanges, attribute = "present")
states <- linkEvents(states, regchanges, attribute = "regime")
states <- linkEvents(states, gdpchanges, attribute = "gdp")


colnames(bilatnet) <- rownames(bilatnet) <- states$label
# bilatnet[1:12,1:12]

bilatnet <- defineNetwork(bilatnet, nodes = states, directed = FALSE)
bilatnet <- linkEvents(bilatnet, bilatchanges, nodes = states)
contignet <- defineNetwork(contignet, nodes = states, directed = FALSE)
contignet <- linkEvents(contignet, contigchanges, nodes = states)

createBilat <- defineDependentEvents(
  events = bilatchanges[bilatchanges$increment == 1, ],
  nodes = states,
  defaultNetwork = bilatnet
)
