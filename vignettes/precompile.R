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

rm(list = ls())

knit("dynami-example.Rmd.orig", "dynami-example.Rmd")
purl("dynami-example.Rmd.orig", "dynami-example.R")


# check if the RMD files contain errors
lapply(
  list("teaching1.Rmd", "teaching2.Rmd", "dynami-example.Rmd"),
  \(x) {
    text <- readLines(x)
    haveErrors <- grepl("Error:", text)
    if (any(haveErrors)) {
      paste("Error in", x, " on line",
            paste(which(haveErrors), collapse = ", "))
    }
  }
)

setwd("..")
