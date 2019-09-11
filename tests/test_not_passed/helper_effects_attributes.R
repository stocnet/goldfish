#####################################
# Goldfish package
#
# Data for effect tests: attributes
#####################################
attr <- data.frame(label = as.factor(c("Christoph", "James", "Per", "Timon", "Marion", "Mepham", "Xiaolei",
                                       "Federica")),
                   fishingSkill = c(10, NA, 5, 10, 8, 8, 3, NA),
                   fishCaught = c(1, 99, 15, 12, 15, 8, 0, 2),
                   fishSizeMean = c(9.9, 0.1, 0.5, 0.45, 0.25, 0.3, NA, 10))
attr <- defineNodes(attr)
