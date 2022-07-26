# Precompiled vignettes

# delete old figures
unlink("vignettes/teaching", recursive = TRUE)

# install packages when GitHub action

# compile vignette and generate R files
setwd("vignettes/")

library(knitr)

knit("teaching1.Rmd.orig", "teaching1.Rmd")
purl("teaching1.Rmd.orig", "teaching1.R")

rm(list = ls())

knit("teaching2.Rmd.orig", "teaching2.Rmd")
purl("teaching2.Rmd.orig", "teaching2.R")

setwd("..")
