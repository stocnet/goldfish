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
#?make_groups_interaction
prepdata <- make_groups_interaction(video, participants,
                                    seed_randomization = 1)


## ----assGroups----------------------------------------------------------------
groups <- prepdata$groups
head(groups)


## ----headDependent------------------------------------------------------------
dependentEvents <- prepdata$dependent.events
head(dependentEvents)


## ----headExogenous------------------------------------------------------------
exogenousEvents <- prepdata$exogenous.events
head(exogenousEvents)


## ----headInteraction----------------------------------------------------------
interactionUpdates <- prepdata$interaction.updates
head(interactionUpdates)


## ----headOpportunities--------------------------------------------------------
opportunities <- prepdata$opportunities
head(opportunities)


## ----defNodes-----------------------------------------------------------------
# goldfish requires character names
participants$label <- as.character(participants$label)
actors <- make_nodes(participants)


## ----groups-------------------------------------------------------------------
groups <- make_nodes(groups)


## ----defNet-------------------------------------------------------------------
initNetwork <- diag(x = 1, nrow(actors), nrow(groups))
# goldfish check that row/column names agree with the nodes data frame labels
dimnames(initNetwork) <- list(actors$label, groups$label)
networkInteractions <- make_network(
  matrix = initNetwork, nodes = actors, nodes2 = groups, directed = TRUE
)
networkInteractions <- link_events(
  x = networkInteractions, change_events = dependentEvents,
  nodes = actors, nodes2 = groups
)
networkInteractions <- link_events(
  x = networkInteractions, change_events = exogenousEvents,
  nodes = actors, nodes2 = groups
)


## ----defNetPast, warning=FALSE------------------------------------------------
networkPast <- make_network(nodes = actors, directed = FALSE)
networkPast <- link_events(
  x = networkPast, change_events = interactionUpdates, nodes = actors
) # don't worry about the warnings


## ----defEvents----------------------------------------------------------------
dependentEvents <- make_dependent_events(
  events = dependentEvents, nodes = actors,
  nodes2 = groups, default_network = networkInteractions
)


## ----defData------------------------------------------------------------------
rfidData <- make_data(
  dependentEvents, networkInteractions, networkPast,
  interactionUpdates, exogenousEvents, known.before, opportunities,
  actors, groups
)
rfidData


## ----modeRateM1---------------------------------------------------------------
formulaRateM1 <- dependentEvents ~  1 +
  intercept(networkInteractions, joining = 1) +
  ego(actors$age, joining = 1, subType = "centered") +
  ego(actors$age, joining = -1, subType = "centered") +
  diff(actors$age, joining = -1, subType = "averaged_sum") +
  diff(actors$level, joining = -1, subType = "averaged_sum") +
  same(actors$gender, joining = -1, subType = "proportion") +
  same(actors$group, joining = -1, subType = "proportion") +
  tie(known.before, joining = -1, subType = "proportion")


## ----modeChoiceM1-------------------------------------------------------------
formulaChoiceM1 <- dependentEvents ~
  diff(actors$age, subType = "averaged_sum") +
  diff(actors$level, subType = "averaged_sum") +
  same(actors$gender, subType = "proportion") +
  same(actors$group, subType = "proportion") +
  tie(known.before, subType = "proportion")


## ----modRateM1Est-------------------------------------------------------------
estRateM1 <- estimate_dynami(
  formulaRateM1,
  sub_model = "rate",
  data = rfidData,
  control_estimation = set_estimation_opt(engine = "default")                      
)
summary(estRateM1)


## ----modChoiceM1Est-----------------------------------------------------------
estChoiceM1 <- estimate_dynami(
  formulaChoiceM1,
  sub_model = "choice",
  data = rfidData,
  control_preprocessing =
    set_preprocessing_opt(opportunities_list = opportunities),
  control_estimation = set_estimation_opt(engine = "default")
)
summary(estChoiceM1)


## ----modeRateM2---------------------------------------------------------------
formulaRateM2 <- dependentEvents ~  1 +
  intercept(networkInteractions, joining = 1) +
  ego(actors$age, joining = 1, subType = "centered") +
  ego(actors$age, joining = -1, subType = "centered") +
  diff(actors$age, joining = -1, subType = "averaged_sum") +
  diff(actors$level, joining = -1, subType = "averaged_sum") +
  same(actors$gender, joining = -1, subType = "proportion") +
  same(actors$group, joining = -1, subType = "proportion") +
  tie(known.before, joining = -1, subType = "proportion") +
  size(networkInteractions, joining = -1, subType = "identity") +
  egopop(networkPast, joining = 1, subType = "normalized") +
  egopop(networkPast, joining = -1, subType = "normalized")


## ----modeChoiceM2-------------------------------------------------------------
formulaChoiceM2 <- dependentEvents ~
  diff(actors$age, subType = "averaged_sum") +
  diff(actors$level, subType = "averaged_sum") +
  same(actors$gender, subType = "proportion") +
  same(actors$group, subType = "proportion") +
  alter(actors$age, subType = "mean") +
  tie(known.before, subType = "proportion") +
  size(networkInteractions, subType = "identity") +
  alterpop(networkPast, subType = "mean_normalized") +
  inertia(networkPast, window = 60, subType = "mean") +
  inertia(networkPast, window = 300, subType = "mean")


## ----modRateM2Est-------------------------------------------------------------
estRateM2 <- estimate_dynami(
  formulaRateM2,
  sub_model = "rate",
  data = rfidData,
  control_estimation = set_estimation_opt(engine = "default")  
)
summary(estRateM2)


## ----modChoiceM2Est-----------------------------------------------------------
estChoiceM2 <- estimate_dynami(
  formulaChoiceM2,
  sub_model = "choice",
  data = rfidData,
  control_preprocessing =
    set_preprocessing_opt(opportunities_list = opportunities),
  control_estimation = set_estimation_opt(engine = "default")
)
summary(estChoiceM2)


## ----interceptJoining---------------------------------------------------------
covMatrix <- vcov(estRateM2)

estInterceptJoining <- coef(estRateM2)[1] + coef(estRateM2)[2]
seInterceptJoining <- sqrt(
  covMatrix[1, 1] + covMatrix[2, 2] + 2 * covMatrix[1, 2]
)
tInterceptJoining <- estInterceptJoining / seInterceptJoining
sprintf(
  "Intercept for joining: %.3f (SE = %.3f, t = %.3f)",
  estInterceptJoining, seInterceptJoining, tInterceptJoining
)


