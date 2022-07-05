# Precompiled vignettes

# delete old figures
unlink("vignettes/teaching", recursive = TRUE)

# install packages when GitHub action

# compile vignette and generate R files
oldWD <- getwd()

setwd("vignettes/")

library(knitr)

knit("teaching1.Rmd.orig", "teaching1.Rmd")
purl("teaching1.Rmd.orig", "teaching1.R")

knit("teaching2.Rmd.orig", "teaching2.Rmd")
purl("teaching2.Rmd.orig", "teaching2.R")

setwd(oldWD)
