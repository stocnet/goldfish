#####################################
# Goldfish package
#
# Data for effect tests: two-mode
#####################################
mTwoMode <- matrix(0, 5, 5)
mTwoMode[1,2] <- 1
mTwoMode[3,4] <- 1
mTwoMode[3,2] <- 1
mTwoMode[1,5] <- NA
mTwoMode[3,5] <- 1
mTwoModeStats <- matrix(0, 5, 5)
mTwoModeStats[1,4] <- 1
