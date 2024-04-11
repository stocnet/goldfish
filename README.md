# goldfish  <img src="man/figures/logo.png" align="right" width="150"/> 

<!-- badges: start -->
![GitHub release (latest by date)](https://img.shields.io/github/v/release/stocnet/goldfish)
![GitHub Release Date](https://img.shields.io/github/release-date/stocnet/goldfish)
![GitHub issues](https://img.shields.io/github/issues-raw/stocnet/goldfish)
![GitHub All Releases](https://img.shields.io/github/downloads/stocnet/goldfish/total)
[![R-CMD-check](https://github.com/stocnet/goldfish/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/stocnet/goldfish/actions/workflows/R-CMD-check.yml)
[![Codecov test coverage](https://codecov.io/gh/stocnet/goldfish/branch/master/graph/badge.svg)](https://app.codecov.io/gh/stocnet/goldfish?branch=master)
[![CodeFactor](https://www.codefactor.io/repository/github/stocnet/goldfish/badge)](https://www.codefactor.io/repository/github/stocnet/goldfish)
[![CII Best Practices](https://bestpractices.coreinfrastructure.org/projects/4563/badge)](https://bestpractices.coreinfrastructure.org/projects/4563)
[![R-CMD-check](https://github.com/stocnet/goldfish/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stocnet/goldfish/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## About

This project is a joint collaboration between the **Social Networks Lab at ETH Zürich** and the **Graduate Institute Geneva**,
and incorporates and supports several sub-projects.

## Aims

The objective of the project is to introduce, extend, and promote 
the Dynamic Network Actor-Oriented Model (DyNAM)
for the statistical analysis of coordination networks through time.
The chief advantage of the model is that it explicitly addresses 
five common features of data found in 
political science, sociology, and other social scientific disciplines:

1. observations are dependent, 
2. ties reflect the opportunities and preferences of both actors involved, 
3. that the creation of coordination ties is a two-sided process, 
4. that data might be available in a time-stamped format, and 
5. that processes typically differ between tie creation and dissolution (valence of ties), 
between different time windows (salience of ties), and 
between initial and repeated creation of ties (multiplicity of ties).

## Installation

You can install the latest version of the `goldfish` package from source using `remotes`:

```r
remotes::install_github("stocnet/goldfish", build_vignettes = TRUE)
```

Or by downloading and install the latest binary from [the releases page](https://github.com/stocnet/goldfish/releases).

### Installing OpenMP on Mac OSX

In some cases, you may get an error that does not allow installation of `goldfish`
from source on Mac OSX versions, including under R 4.0.0.
The error may relate to compiling the parts of `goldfish` that are written in C++,
or whether OpenMP (for parallelisation) can be found.

Many installation woes can be solved by directing R to use [Homebrew](https://brew.sh) installed `gcc`.
An updated setting up instructions thanks to @timonelmer are available [here](https://github.com/stocnet/goldfish/blob/main/.github/OpenMP_mac.md).

More details can be found [here](https://medium.com/biosyntax/following-up-library-dependency-when-compiling-r-packages-89f191b9f227) (Thank you @Knieps for identifying this.).
Other links that may be helpful include:

- https://asieira.github.io/using-openmp-with-r-packages-in-os-x.html
- https://thecoatlessprofessor.com/programming/cpp/r-compiler-tools-for-rcpp-on-macos/
- https://ryanhomer.github.io/posts/build-openmp-macos-catalina-complete
- https://pat-s.me/transitioning-from-x86-to-arm64-on-macos-experiences-of-an-r-user/

Please share feedback on which of these work and we will update the installation guide accordingly.
