<!-- README.md is generated from README.Rmd. Please edit that file -->

# goldfish <img src="man/figures/logo.png" align="right" width="150"/>

<!-- badges: start -->

![GitHub release (latest by
date)](https://img.shields.io/github/v/release/stocnet/goldfish)
![GitHub Release
Date](https://img.shields.io/github/release-date/stocnet/goldfish)
![GitHub
issues](https://img.shields.io/github/issues-raw/stocnet/goldfish)
![GitHub All
Releases](https://img.shields.io/github/downloads/stocnet/goldfish/total)
[![R-CMD-check](https://github.com/stocnet/goldfish/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/stocnet/goldfish/actions/workflows/R-CMD-check.yml)
[![Codecov test
coverage](https://codecov.io/gh/stocnet/goldfish/branch/main/graph/badge.svg)](https://app.codecov.io/gh/stocnet/goldfish?branch=main)
[![CII Best
Practices](https://bestpractices.coreinfrastructure.org/projects/4563/badge)](https://bestpractices.coreinfrastructure.org/projects/4563)
<!-- badges: end -->

<!-- [![CodeFactor](https://www.codefactor.io/repository/github/stocnet/goldfish/badge)](https://www.codefactor.io/repository/github/stocnet/goldfish) -->

## Description

The `{goldfish}` package offers a collection of tools designed for
applying statistical models to dynamic network data. It primarily focus
on models for relational event data, namely, sequences of interactions
between actors or entities within a network, enriched by fine-grained
time-stamps information. Relational event data emerge in various
domains, such as automatically collected data about interactions in
communication and social media research, social science studies using
social sensors, and archival network studies that provide in-depth
details regarding the timing or sequence of relational actions between
nodes.

Currently, the package includes the following models:

-   **Dynamic Network Actor Models (DyNAM)**: Investigate relational
    event models as an actor-oriented decision process (Stadtfeld and
    Block 2017).
    -   *rate*: Actors compete for creating the next relational event.
    -   *choice*: The active actor choose the receiver of the event.
    -   *choice\_coordination*: The creation of coordination ties as a
        two-sided process (Stadtfeld, Hollway, and Block 2017)
-   **Dynamic Network Actor Models for interactions (DyNAMi)**:
    Investigate dynamics of conversation groups and interpersonal
    interaction in different social contexts from an actor-oriented
    perspective (Hoffman et al. 2020).
    -   *rate*: Actors compete for joining or leaving groups.
    -   *choice*: The active actor choose the group to join.
-   **Relational Event Models (REM)**: Investigate relational event
    models as a tie-oriented process (Butts 2008).

### Vignettes

For detailed documentation on each model, including usage examples,
users are encouraged to consult the package’s vignettes and help files:

-   [Getting Started with goldfish (DyNAM and
    REM)](https://stocnet.github.io/goldfish/articles/teaching1.html)
-   [Coordination ties (DyNAM-choice
    coordination)](https://stocnet.github.io/goldfish/articles/teaching2.html)
-   [Face to face interactions
    (DyNAMi)](https://stocnet.github.io/goldfish/articles/dynami-example.html)
-   [Catalog of available
    effects](https://stocnet.github.io/goldfish/articles/goldfishEffects.html)

## Table of Contents

-   [Installation](#installation)
    -   [Installing OpenMP on Mac OSX](#installing-openmp-on-mac-osx)
-   [Usage](#usage)
    -   [Define data objects and link
        events](#define-data-objects-and-link-events)
    -   [Define dependent events](#define-dependent-events)
    -   [Model specification and
        estimation](#model-specification-and-estimation)
-   [About](#about)
-   [References](#references)

## Installation

You can install `{goldfish}` directly from
[CRAN](https://cran.r-project.org/package=goldfish):

    install.packages("goldfish")

To install the development version from GitHub, use the
[remotes](https://cran.r-project.org/package=remotes) package:

-   For latest stable version:
    `remotes::install_github("stocnet/goldfish", build_vignettes = TRUE)`
-   For latest development version:
    `remotes::install_github("stocnet/goldfish@develop", build_vignettes = TRUE)`

Or by downloading and install the latest binary releases for all major
OSes – Windows, Mac, and Linux – can be found
[here](https://github.com/stocnet/goldfish/releases).

### Installing OpenMP on Mac OSX

In some cases, you may get an error that does not allow installation of
`{goldfish}` from source on Mac OSX versions, including under R 4.0.0.
The error may relate to compiling the parts of `{goldfish}` that are
written in C++, or whether OpenMP (for parallelisation) can be found.

Many installation woes can be solved by directing R to use
[Homebrew](https://brew.sh) installed `gcc`. An updated setting up
instructions thanks to @timonelmer are available
[here](https://github.com/stocnet/goldfish/blob/main/.github/OpenMP_mac.md).

More details can be found
[here](https://medium.com/biosyntax/following-up-library-dependency-when-compiling-r-packages-89f191b9f227)
(Thank you @Knieps for identifying this.). Other links that may be
helpful include:

-   <https://asieira.github.io/using-openmp-with-r-packages-in-os-x.html>
-   <https://thecoatlessprofessor.com/programming/cpp/r-compiler-tools-for-rcpp-on-macos/>
-   <https://ryanhomer.github.io/posts/build-openmp-macos-catalina-complete>
-   <https://pat-s.me/transitioning-from-x86-to-arm64-on-macos-experiences-of-an-r-user/>

Please share feedback on which of these work and we will update the
installation guide accordingly.

## Usage

Below is a quick-start guide to using the `{goldfish}` package. The
dataset used in this example is an abbreviated version of the MIT Social
Evolution data (`?Social_Evolution`).

### Define data objects and link events

The main data objects required for the analysis are the node set(s)
`defineNodes()` and network(s) `defineNetwork()`. The node set object
contains labels and attributes of the actors in the network. In
contrast, a network object contains the information of past relational
events between actors. By default, `defineNetwork()` constructs an empty
matrix, its dimensions defined by the length of the nodeset(s). Data
frames containing event data that modify these data objects can be
linked to them using the `linkEvents()` method.

    library(goldfish)
    data("Social_Evolution")

    callNetwork <- defineNetwork(nodes = actors, directed = TRUE) |> # 1
      linkEvents(changeEvent = calls, nodes = actors) # 2

The events data frame, which indicates the time-varying attributes in
the node set, contains the following columns:

-   `time`: The time when the attribute changes, either a `numeric` or
    `POSIXct` value.
-   `node`: The node for which the attribute changes, a `character`
    value that matches the `label` variable in the node set.
-   `replace`: The new value of the attribute, a `numeric` value.

The events data frame that details the relational events between actors
contains the following columns:

-   `time`: The time when the event occurred, either a `numeric` or
    `POSIXct` value.
-   `sender`: The actor initiating the event, a `character` value that
    matches the `label` variable in the node set.
-   `receiver`: The actor receiving the event, a `character` value that
    matches the `label` variable in the node set.
-   `increment` or `replace`: A `numeric` value indicating either the
    increment that the relational event represents or the new value.

### Define dependent events

The final step in defining the data objects is to identify the dependent
events. Here we would like to model as the dependent variable the calls
between individuals. We specify the event data frame and the node set.

    callsDependent <- defineDependentEvents(
      events = calls, nodes = actors,
      defaultNetwork = callNetwork
      )

### Model specification and estimation

We specify our model using the standard R formula format like:

`goldfish_dependent ~ effects(process_state_element)`

We can see which effects are currently available and how to specify them
here:

    vignette("goldfishEffects")

Now to estimate this model, we use the `?estimate` function.

    mod00Rate <- estimate(
      callsDependent ~ indeg + outdeg,
      model = "DyNAM", subModel = "rate"
    )

    summary(mod00Rate)
    #> 
    #> Call:
    #> estimate(x = callsDependent ~ indeg + outdeg, model = "DyNAM", 
    #>     subModel = "rate")
    #> 
    #> 
    #> Coefficients:
    #>        Estimate Std. Error z-value  Pr(>|z|)    
    #> indeg  0.551445   0.066344  8.3119 < 2.2e-16 ***
    #> outdeg 0.263784   0.028386  9.2927 < 2.2e-16 ***
    #> ---
    #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    #> 
    #>   Converged with max abs. score of 2e-05 
    #>   Log-Likelihood: -1750.9
    #>   AIC:  3505.8 
    #>   AICc: 3505.9 
    #>   BIC:  3514 
    #>   model: "DyNAM" subModel: "rate"

    mod00Choice <- estimate(
      callsDependent ~ inertia + recip + trans,
      model = "DyNAM", subModel = "choice"
    )
    summary(mod00Choice)
    #> 
    #> Call:
    #> estimate(x = callsDependent ~ inertia + recip + trans, model = "DyNAM", 
    #>     subModel = "choice")
    #> 
    #> 
    #> Coefficients:
    #>         Estimate Std. Error z-value  Pr(>|z|)    
    #> inertia  5.19690    0.17397 29.8725 < 2.2e-16 ***
    #> recip    1.39802    0.17300  8.0812 6.661e-16 ***
    #> trans   -0.23036    0.21554 -1.0687    0.2852    
    #> ---
    #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    #> 
    #>   Converged with max abs. score of 7e-05 
    #>   Log-Likelihood: -696.72
    #>   AIC:  1399.4 
    #>   AICc: 1399.5 
    #>   BIC:  1411.7 
    #>   model: "DyNAM" subModel: "choice"

## About

This project is a joint collaboration between the **Social Networks Lab
at ETH Zürich** and the **Graduate Institute Geneva**, and incorporates
and supports several sub-projects.

## References

Butts, C. T. 2008. “A Relational Event Framework for Social Action.”
*Sociological Methodology* 38 (1): 155–200.

Hoffman, Marion, Per Block, Timon Elmer, and Christoph Stadtfeld. 2020.
“A Model for the Dynamics of Face-to-Face Interactions in Social
Groups.” *Network Science* 8 (S1): S4–25.
<https://doi.org/10.1017/nws.2020.3>.

Stadtfeld, C., and P. Block. 2017. “Interactions, Actors, and Time:
Dynamic Network Actor Models for Relational Events.” *Sociological
Science* 4 (14): 318–52. <https://doi.org/10.15195/v4.a14>.

Stadtfeld, C., J. Hollway, and P. Block. 2017. “Dynamic Network Actor
Models: Investigating Coordination Ties Through Time.” *Sociological
Methodology* 47 (1): 1–40. <https://doi.org/10.1177/0081175017709295>.
