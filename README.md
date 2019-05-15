# distr6

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

## Why Object-Oriented Programming?

There are many advantages to OOP over functional programming (which is more commmon in R). For probability distributions, the biggest advantages is in the ability to quickly construct and recall any number of required distributions, each with mathematical and statistical methods. There are currently no methods in base R that interact with Distributions to obtain basic properties, for example the mean of a distribution. In distr6 this is made possible as every probability distribution is its own class with specific methods. So to get mathematical properties of the Binomial distribution:
1. `B = Binomial$new(prob, size)`
1. `B$mean()`
1. `B$sd()`

Or try using `B$summary()` for a range of common mathematical and statistical results. With the `listDistributions()` command you can see every Distribution currently implemented in distr6, along with their traits.

Another advantage of OOP is making use of inheritance to implement more complex distributions from base distributions. For example any distribution can be easily truncated with a call to `truncate(distribution, lower, upper)` which produces an object of class `TruncatedDistribution` which inherits all methods from `Distribution`. 

We discuss further advantages of OOP including Design Patterns in the help-pages of the website.

## Installation

Before publication to CRAN, the latest stable release is available via:
````R
devtools::install_github(RaphaelS1/distr6, dependencies = TRUE)
````
