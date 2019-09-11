rm(list=ls())
setwd("~/Desktop/SN Lab/GESS-SN/goldfish package/vignettes/Sweave vignette") ## Set the path of the article
Sweave("article.Rnw")
library("tools")
texi2pdf("article.tex")
Stangle("article.Rnw")


