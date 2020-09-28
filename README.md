# goldfish  <img src="inst/hexlogo_goldfish.png" align="right" width="150"/> 

<!-- badges: start -->
![GitHub release (latest by date)](https://img.shields.io/github/v/release/snlab-ch/goldfish)
![GitHub Release Date](https://img.shields.io/github/release-date/snlab-ch/goldfish)
![GitHub issues](https://img.shields.io/github/issues-raw/snlab-ch/goldfish)
![GitHub All Releases](https://img.shields.io/github/downloads/snlab-ch/goldfish/total)
[![Codecov test coverage](https://codecov.io/gh/snlab-ch/goldfish/branch/master/graph/badge.svg)](https://codecov.io/gh/snlab-ch/goldfish?branch=master)
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
remotes::install_github("snlab-ch/goldfish")
```

Or by downloading and install the latest binary from [the releases page](https://github.com/snlab-ch/goldfish/releases).

### Installing OpenMP on Mac OSX

In some cases, you may get an error that does not allow installation of `goldfish`
from source on Mac OSX versions, including under R 4.0.0.
The error may relate to compiling the parts of `goldfish` that are written in C++,
or whether OpenMP (for parallelisation) can be found.

Many installation woes can be solved by directing R to use [Homebrew](https://brew.sh) installed `gcc`.
Once you have installed `gcc` using Homebrew in the Terminal,
you will need to update your `/Library/Frameworks/R.framework/Resources/etc/Makeconf` file like so:

```
# Use Homebrew gcc for OpenMP support
CC = gcc-8
# CC = clang # Original setting
...
# Use Homebrew gcc for OpenMP support
CXX = g++-8
# CXX = clang++ # Original setting
...
# Ask R to find the Homebrew copy of gcc
FLIBS = -L/usr/local/lib/gcc/8/gcc/x86_64-apple-darwin17.5.0/8.1.0 -L/usr/local/lib/gcc/8 -lgfortran -lquadmath -lm
# The original one
# FLIBS =  -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm
```

More details can be found [here](https://medium.com/biosyntax/following-up-library-dependency-when-compiling-r-packages-89f191b9f227).^[Thank you @Knieps for identifying this.]
Other links that may be helpful include:
- https://asieira.github.io/using-openmp-with-r-packages-in-os-x.html
- https://thecoatlessprofessor.com/programming/openmp-in-r-on-os-x/
- https://ryanhomer.github.io/posts/build-openmp-macos-catalina-complete

Please share feedback on which of these work and we will update the installation guide accordingly.
