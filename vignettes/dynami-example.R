## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----load---------------------------------------------------------------------
library(goldfish)
data("RFID_Validity_Study")
#?RFID_Validity_Study


## ----headParticipants---------------------------------------------------------
head(participants)


## ----headRfid-----------------------------------------------------------------
head(rfid)


## ----headVideo----------------------------------------------------------------
head(video)


## ----defGroups----------------------------------------------------------------
#?defineGroups_interaction
prepdata <- defineGroups_interaction(video, participants,
                                     seed.randomization = 1)


## ----assGroups----------------------------------------------------------------
groups <- prepdata$groups
head(groups)


## ----headDependent------------------------------------------------------------
dependent.events <- prepdata$dependent.events
head(dependent.events)


## ----headExogenous------------------------------------------------------------
exogenous.events <- prepdata$exogenous.events
head(exogenous.events)


## ----headInteraction----------------------------------------------------------
interaction.updates <- prepdata$interaction.updates
head(interaction.updates)


## ----headOpportunities--------------------------------------------------------
opportunities <- prepdata$opportunities
head(opportunities)


## ----defNodes-----------------------------------------------------------------
# goldfish requires character names
participants$label <- as.character(participants$label)
actors <- defineNodes(participants)


## ----groups-------------------------------------------------------------------
groups <- defineNodes(groups)


## ----defNet-------------------------------------------------------------------
init.network <- diag(x = 1, nrow(actors), nrow(groups))
# goldfish check that row/column names agree with the nodes data frame labels
dimnames(init.network) <- list(actors$label, groups$label)
network.interactions <- defineNetwork(
  matrix = init.network, nodes = actors, nodes2 = groups, directed = TRUE
)
network.interactions <- linkEvents(
  x = network.interactions, changeEvent = dependent.events,
  nodes = actors, nodes2 = groups
)
network.interactions <- linkEvents(
  x = network.interactions, changeEvent = exogenous.events,
  nodes = actors, nodes2 = groups
)


## ----defNetPast, warning=FALSE------------------------------------------------
network.past <- defineNetwork(nodes = actors, directed = FALSE)
network.past <- linkEvents(
  x = network.past, changeEvents = interaction.updates, nodes = actors
) # don't worry about the warnings


## ----defEvents----------------------------------------------------------------
dependent.events <- defineDependentEvents(
  events = dependent.events, nodes = actors,
  nodes2 = groups, defaultNetwork = network.interactions
)


## ----modeRateM1---------------------------------------------------------------
formula.rate.M1 <- dependent.events ~  1 +
  intercept(network.interactions, joining = 1) +
  ego(actors$age, joining = 1, subType = "centered") +
  ego(actors$age, joining = -1, subType = "centered") +
  diff(actors$age, joining = -1, subType = "averaged_sum") +
  diff(actors$level, joining = -1, subType = "averaged_sum") +
  same(actors$gender, joining = -1, subType = "proportion") +
  same(actors$group, joining = -1, subType = "proportion") +
  tie(known.before, joining = -1, subType = "proportion")


## ----modeChoiceM1-------------------------------------------------------------
formula.choice.M1 <- dependent.events ~
  diff(actors$age, subType = "averaged_sum") +
  diff(actors$level, subType = "averaged_sum") +
  same(actors$gender, subType = "proportion") +
  same(actors$group, subType = "proportion") +
  tie(known.before, subType = "proportion")


## ----modRateM1Est-------------------------------------------------------------
est.rate.M1 <- estimate(
  formula.rate.M1, model = "DyNAMi", subModel = "rate",
  estimationInit = list(engine = "default")                      
)
summary(est.rate.M1)


## ----modChoiceM1Est-----------------------------------------------------------
est.choice.M1 <- estimate(
  formula.choice.M1,
  model = "DyNAMi", subModel = "choice",
  estimationInit = list(opportunitiesList = opportunities)
)
summary(est.choice.M1)


## ----modeRateM2---------------------------------------------------------------
formula.rate.M2 <- dependent.events ~  1 +
  intercept(network.interactions, joining = 1) +
  ego(actors$age, joining = 1, subType = "centered") +
  ego(actors$age, joining = -1, subType = "centered") +
  diff(actors$age, joining = -1, subType = "averaged_sum") +
  diff(actors$level, joining = -1, subType = "averaged_sum") +
  same(actors$gender, joining = -1, subType = "proportion") +
  same(actors$group, joining = -1, subType = "proportion") +
  tie(known.before, joining = -1, subType = "proportion") +
  size(network.interactions, joining = -1, subType = "identity") +
  egopop(network.past, joining = 1, subType = "normalized") +
  egopop(network.past, joining = -1, subType = "normalized")


## ----modeChoiceM2-------------------------------------------------------------
formula.choice.M2 <- dependent.events ~
  diff(actors$age, subType = "averaged_sum") +
  diff(actors$level, subType = "averaged_sum") +
  same(actors$gender, subType = "proportion") +
  same(actors$group, subType = "proportion") +
  alter(actors$age, subType = "mean") +
  tie(known.before, subType = "proportion") +
  size(network.interactions, subType = "identity") +
  alterpop(network.past, subType = "mean_normalized") +
  inertia(network.past, window = 60, subType = "mean") +
  inertia(network.past, window = 300, subType = "mean")


## ----modRateM2Est-------------------------------------------------------------
est.rate.M2 <- estimate(
  formula.rate.M2, model = "DyNAMi", subModel = "rate",
  estimationInit = list(engine = "default")  
)
summary(est.rate.M2)


## ----modChoiceM2Est-----------------------------------------------------------
est.choice.M2 <- estimate(
  formula.choice.M2,
  model = "DyNAMi", subModel = "choice",
  estimationInit = list(opportunitiesList = opportunities)
)
summary(est.choice.M2)


## ----interceptJoining---------------------------------------------------------
cov.matrix <- vcov(est.rate.M2)

est.interceptjoining <- coef(est.rate.M2)[1] + coef(est.rate.M2)[2]
se.interceptjoining <- sqrt(
  cov.matrix[1, 1] + cov.matrix[2, 2] + 2 * cov.matrix[1, 2]
)
t.interceptjoining <- est.interceptjoining / se.interceptjoining
sprintf(
  "Intercept for joining: %.3f (SE = %.3f, t = %.3f)",
  est.interceptjoining, se.interceptjoining, t.interceptjoining
)


