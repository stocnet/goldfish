# goldfish  <img src="inst/hexlogo_goldfish.png" align="right" width="150"/> 

![GitHub release (latest by date)](https://img.shields.io/github/v/release/snlab-ch/goldfish)
![GitHub Release Date](https://img.shields.io/github/release-date/snlab-ch/goldfish)
![GitHub issues](https://img.shields.io/github/issues-raw/snlab-ch/goldfish)
![GitHub All Releases](https://img.shields.io/github/downloads/snlab-ch/goldfish/total)

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

You can install the latest version of the `goldfish` package from source using `devtools`:

```r
devtools::install_github("snlab-ch/goldfish")
```

Or by downloading and install the latest binary from [the releases page](https://github.com/snlab-ch/goldfish/releases).
