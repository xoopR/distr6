# distr6 <img src="man/figures/logo.png" align="right" alt="" width="120" />

[![Travis Build Status](https://travis-ci.com/RaphaelS1/distr6.svg?branch=master)](https://travis-ci.com/RaphaelS1/distr6)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/o68k5t4tn4wojj2f?svg=true)](https://ci.appveyor.com/project/RaphaelS1/distr6)
[![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
[![CRAN Version](http://www.r-pkg.org/badges/version/distr6)](http://www.r-pkg.org/badges/version/distr6)
[![codecov](https://codecov.io/gh/RaphaelS1/distr6/branch/master/graph/badge.svg)](https://codecov.io/gh/RaphaelS1/distr6)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

Currently there are two main ways to interact with probability distributions in R.
1. Using the base and stats package for functions such as `dnorm` \ `pnorm` \ `qnorm` \ `rnorm` for the pdf/cdf/quantile/simulation function of a distribution (in this case Normal).
1. Using the distr family of packages for an object-oriented interface to distributions

distr6 is the R6 upgrade to the distr family of packages. For full details of the distr family, please refer to the [sourceforge page](http://distr.r-forge.r-project.org/). We extend the functionality of distr by utilising R6 for increased speed and decreased computational cost. We have also introduced a new API and class structure, full details of which can be seen in the project [wiki page](https://github.com/RaphaelS1/distr6/wiki) and the [distr6 website](https://RaphaelS1.github.io/distr6/).

## Installation

Before publication to CRAN, the latest stable release is available via:
````R
remotes::install_github("RaphaelS1/distr6", dependencies = TRUE)
````

## What is distr6?

distr6 is a unified and clean interface to organise the hundreds of probability distributions implemented in R into one R6 object oriented package.

Our short-term aims are to implement all distributions in the R stats package as well as some other commonly utilised distributions. We also allow for user-defined probability distributions/families via multiple inheritance. Building the package from the ground up and making use of tried and tested design patterns (as per Gamma et al. 1994), distr6 aims to be the only package needed for probability distributions in R.

distr6 extends the work of Peter Ruckdeschel, Matthias Kohl et al. who created the first object-oriented (OO) interface for distributions using S4. Their [distr package](http://distr.r-forge.r-project.org/) is currently the gold-standard in R for OO distribution handling. Using R6 we aim to take this even further and to create a scalable interface that can continue to grow with the community.


## Main Features

distr6 is not intended to replace the base R distributions function but instead to give an alternative that focuses on distributions as objects that can be manipulated and accessed as required. The main features therefore centre on OOP practices, design patterns and API design. Of particular note:
* All distributions in base R introduced as objects with methods for common statistical functions including pdf, cdf, inverse cdf, simulation, mean, variance, skewness and kurtosis
* Flexible construction of distributions for common parameterisations
* Decorators for extending functionality of distributions to more complex modelling methods
* S3 compatibility to make the interface more flexible for users who are less familiar with OOP
* Wrappers including truncation and huberization for distribution manipulation and including product/joint distributions for distribution composition
* Additionally we introduce a SetSymbol class for a purely symbolic representation of sets for Distribution typing

## Usage

distr6 has three primary use-cases:

1. **Upgrading base** Extend the R distributions functions to classes so that each distribution additionally has basic statisical methods including expectation and variance and properties/traits including discrete/continuous, univariate/multivariate, etc.
1. **Statistics** Implementing decorators and adaptors to manipulate distributions including distribution composition. Aditionally functionality for numeric calculations based on any arbitrary distribution.
1. **Modelling** Probabilistic modelling using distr6 objects as the modelling targets. Objects as targets is an understood ML paradigm and introducing distributions as classes is the first step to implementing probabilistic modelling.
 

## Package Development and Contributing

distr6 is now public on GitHub but developments will continue to be made internally by a team of researchers from UCL working at The Alan Turing Institute. All contributions are released under the [MIT licence](https://opensource.org/licenses/MIT) with acknowledgements to the [LGPL-3 licence of distr](https://github.com/RaphaelS1/distr6/blob/master/Licensing). We welcome external contributors to test our API and notify us of bugs and issues however we are not currently looking for feature requests as the API is still in development. See our [roadmap](https://raphaels1.github.io/distr6/articles/roadmap.html) for short- and long-term plans. Before raising an issue please read through our [contributing guidelines](https://github.com/RaphaelS1/distr6/blob/master/CONTRIBUTING.md) for details including our [code of conduct](https://github.com/RaphaelS1/distr6/blob/master/CODE_OF_CONDUCT.md).
