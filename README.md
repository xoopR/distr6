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

distr6 is the R6 upgrade to the distr family of packages. For full details of the distr family, please refer to the [sourceforge page](http://distr.r-forge.r-project.org/). We extend the functionality of distr by utilising R6 for increased speed and decreased computational cost. We have also introduced a new API and class structure, full details of which can be seen in the project [wiki page](https://github.com/RaphaelS1/distr6/wiki) and the [distr6 website](https://RaphaelS1.github.io/distr6/).

## Installation

Before publication to CRAN, the latest stable release is available via:
````R
remotes::install_github("RaphaelS1/distr6", dependencies = TRUE)
````

## Package Development and Contributing

distr6 is now public on GitHub but developments will continue to be made internally by a team of researchers from UCL working at The Alan Turing Institute. All contributions are released under the [MIT licence](https://opensource.org/licenses/MIT) with acknowledgements to the [LGPL-3 licence of distr](https://github.com/RaphaelS1/distr6/blob/master/Licensing). We welcome external contributors to test our API and notify us of bugs and issues however we are not currently looking for feature requests as the API is still in development. See our [roadmap](https://raphaels1.github.io/distr6/articles/roadmap.html) for short- and long-term plans. Before raising an issue please read through our [contributing guidelines](https://github.com/RaphaelS1/distr6/blob/master/CONTRIBUTING.md) for details including our [code of conduct](https://github.com/RaphaelS1/distr6/blob/master/CODE_OF_CONDUCT.md).

## What is distr6?

distr6 is a unified and clean interface to organise the hundreds of probability distributions implemented in R into one R6 object oriented package.

distr6 extends the work of Peter Ruckdeschel, Matthias Kohl et al. who created the first object-oriented (OO) interface for distributions using S4. Their [distr package](http://distr.r-forge.r-project.org/) is currently the gold-standard in R for OO distribution handling. Using R6 we aim to take this even further and to create a scalable interface that can continue to grow with the community.

Our short-term aims are to implement all distributions in the R stats package as well as some other commonly utilised distributions. We also allow for user-defined probability distributions/families via multiple inheritance. Building the package from the ground up and making use of tried and tested design patterns (as per Gamma et al. 1994), distr6 aims to be the only package needed for probability distributions in R.


## Why Object-Oriented Programming?

There are many advantages to OOP over functional programming (which is more commmon in R). For probability distributions, the biggest advantages is in the ability to quickly construct and recall any number of required distributions, each with mathematical and statistical methods. There are currently no methods in base R that interact with Distributions to obtain basic properties, for example the mean of a distribution. In distr6 this is made possible as every probability distribution is its own class with specific methods. So to get mathematical properties of the Binomial distribution:
1. `B = Binomial$new(prob, size)`
2. `B$mean()`
3. `B$sd()`

Or try using `B$summary()` for a range of common mathematical and statistical results. With the `listDistributions()` command you can see every Distribution currently implemented in distr6, along with their traits.

Another advantage of OOP is making use of inheritance to implement more complex distributions from base distributions. For example any distribution can be easily truncated with a call to `truncate(distribution, lower, upper)` which creates an object of class `TruncatedDistribution` that inherits all methods from the truncated distribution. Use `listWrappers()` to see the list of currently implemented wrappers including truncation, huberization and product distributions. 

We discuss further advantages of OOP including Design Patterns [here](https://raphaels1.github.io/distr6/articles/oop_and_design_patterns.html).
