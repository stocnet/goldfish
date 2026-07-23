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
# goldfish requires character labels
participants$label <- as.character(participants$label)
#?make_groups_interaction
rfidData <- make_groups_interaction(
  records = video,
  actors = participants,
  seed_randomization = 1
)


## ----printData----------------------------------------------------------------
rfidData


## ----modeRateM1---------------------------------------------------------------
formulaRateM1 <- interactions ~ 1 +
  intercept(interactions, joining = 1) +
  ego(age, joining = 1, subType = "centered") +
  ego(age, joining = -1, subType = "centered") +
  diff(age, joining = -1, subType = "averaged_sum") +
  diff(level, joining = -1, subType = "averaged_sum") +
  same(gender, joining = -1, subType = "proportion") +
  same(group, joining = -1, subType = "proportion") +
  tie(known.before, joining = -1, subType = "proportion")


## ----modeChoiceM1-------------------------------------------------------------
formulaChoiceM1 <- interactions ~
  diff(age, subType = "averaged_sum") +
  diff(level, subType = "averaged_sum") +
  same(gender, subType = "proportion") +
  same(group, subType = "proportion") +
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
  control_estimation = set_estimation_opt(engine = "default")
)
summary(estChoiceM1)


## ----modeRateM2---------------------------------------------------------------
formulaRateM2 <- interactions ~ 1 +
  intercept(interactions, joining = 1) +
  ego(age, joining = 1, subType = "centered") +
  ego(age, joining = -1, subType = "centered") +
  diff(age, joining = -1, subType = "averaged_sum") +
  diff(level, joining = -1, subType = "averaged_sum") +
  same(gender, joining = -1, subType = "proportion") +
  same(group, joining = -1, subType = "proportion") +
  tie(known.before, joining = -1, subType = "proportion") +
  size(interactions, joining = -1, subType = "identity") +
  egopop(past, joining = 1, subType = "normalized") +
  egopop(past, joining = -1, subType = "normalized")


## ----modeChoiceM2-------------------------------------------------------------
formulaChoiceM2 <- interactions ~
  diff(age, subType = "averaged_sum") +
  diff(level, subType = "averaged_sum") +
  same(gender, subType = "proportion") +
  same(group, subType = "proportion") +
  alter(age, subType = "mean") +
  tie(known.before, subType = "proportion") +
  size(interactions, subType = "identity") +
  alterpop(past, subType = "mean_normalized") +
  inertia(past, subType = "mean")


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


