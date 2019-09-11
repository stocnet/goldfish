# Setup ####
# Install packages required for testing
# install.packages("devtools")
# install.packages("testthat")

# Set working directory to Goldfish package
setwd("./goldfish package/")

# Install Goldfish package
remove.packages("goldfish")
devtools::install(".")

# Test ####
library("goldfish")
library("devtools")
library("testthat")
devtools::load_all(".")
testthat::test_dir("tests/testthat")
