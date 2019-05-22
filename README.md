# distr6 <img src="man/figures/logo.png" align="right" alt="" width="120" />

[![Build Status](https://travis-ci.com/RaphaelS1/distr6.svg?branch=master)](https://travis-ci.com/RaphaelS1/distr6)
[![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
[![CRAN Version](http://www.r-pkg.org/badges/version/distr6)](http://www.r-pkg.org/badges/version/distr6)
[![codecov](https://codecov.io/gh/RaphaelS1/distr6/branch/master/graph/badge.svg)](https://codecov.io/gh/RaphaelS1/distr6/branch/master/graph/badge.svg)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

Currently there are two main ways to interact with probability distributions in R.
1. Using the base and stats package for functions such as `dnorm` \ `pnorm` \ `qnorm` \ `rnorm` for the pdf/cdf/quantile/simulation function of a distribution (in this case Normal).
1. Using the distr family of packages for an object-oriented interface to distributions

distr6 is the R6 upgrade to the distr family of packages. For full details of the distr family, please refer to the [sourceforge page](http://distr.r-forge.r-project.org/). We extend the functionality of distr by utilising R6 for increased speed and decreased computational cost. We have also introduced a new API and class structure, full details of which can be seen in the UML class diagram and API help pages.

For full details of distr6's design and implementation see the project [wiki page](https://github.com/RaphaelS1/distr6/wiki) and the [distr6 website](https://RaphaelS1.github.io/distr6/).

## Installation

Before publication to CRAN, the latest stable release is available via:
````R
devtools::install_github(RaphaelS1/distr6, dependencies = TRUE)
````
