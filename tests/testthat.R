# Setup ####
# Install packages required for testing
# install.packages("devtools")
# install.packages("testthat")

# Set working directory to Goldfish package
# setwd("./goldfish/")

# Install Goldfish package
# remove.packages("goldfish")
# devtools::install(".")

# Test ####
# library("goldfish")
library("devtools")
library("testthat")
# library(goldfish)
devtools::load_all(".")
testthat::test_dir("tests/testthat")
# testthat::test_check("goldfish")
