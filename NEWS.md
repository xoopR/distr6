# distr6 1.4.0.9000

## Minor Updates

### Deprecated Functions/Fields/Methods

* Individual property and trait accessors deprecated: `$support, $symmetry, $kurtosisType, $skewnessType, $valueSupport, $variateForm, $type`

## Patches

* Bugfix in `decorate` function that was overwriting the `decorators` private field instead of appending
* Vastly improved speed of construction of `SDistribution` 

# distr6 1.3.5

* Added Erlang distribution
* Fixed mistake in documentation for `Weibull`, thanks to Paul Northrop (@paulnorthrop) for pointing this out.
* Fixed bug in `plot` preventing only `quantile` being plotted on its own
* Converted descriptive public methods to active bindings. Unfortunately this will break reverse dependencies as there's no way to soft deprecate these. Affected methods are:  `decorators, traits, valueSupport, variateForm, type, properties, support, symmetry, sup, inf, dmax, dmin, kurtosisType, skewnessType`

# distr6 1.3.4

- Bug fix in `VectorDistribution` that was causing decorators not to be added properly
- Bug fix in `VectorDistribution` that was causing an error when extracting decorated wrapped distributions
- Moved package `pracma` to suggests. Removed `utils` dependency.
- Added checks to assert all packages are installed when required by distributions
- Deprecated `package` field in distributions, now called `packages`
- `packages` lists all packages required to be installed for a distribution, if `NULL` only distr6 required
- Abstracted SetInterval and related classes to the `set6` package
- Deprecated `listSpecialSets`, use `set6::listSpecialSets` instead
- Removed secondary checks on `update` of `ParameterSet` , any properly defined distributions will have this covered in primary parameter, thus increasing speed.

# distr6 1.3.3

- `as.data.table` has been moved from a `ParameterSet` method to an S3 dispatch. Fixes an important bug of overloading `as.data.table` but may affect backwards compatibility.
- Added `distrSimulate` for convenient simulation from any distribution
- Updated `plot` to be able to handle distributions without `quantile` or `rand`. Optimised runtime by preventing automatic computation of `pdf` and `cdf`.

# distr6 1.3.2

- Bug fix in `VectorDistribution` - missing `which` argument
- Bug fix in `Bernoulli` - `rand` was incorrectly calling `dbinom` not `rbinom`
- Bug fix in the `mode` of distributions
- Added `mode` to documentation

# distr6 1.3.1

- Added wrapper for VectorDistribution for quick concatenation of constructed distributions, `c.Distribution`
- `VectorDistribution` `print` method more in line with base R vectors

# distr6 1.3.0

## Minor Updates

### Added Functions and Classes

- `plot` function for plotting the `pdf, cdf, survival, quantile, hazard, cumhazard` or `distr6` objects
- `lines` function for superimposing `distr6` plots
- `qqplot` function for comparing `distr6` distributions to each other or to other theoretical distributions
- Added `Extract.VectorDistribution` for extracted distributions from inside a `VectorDistribution`, see the big update below.

### Updated Functions

- `decorate` now allows users to specify the Decorator as a character as well as supplying the object, this makes it simpler when using distr6 whilst unattached
- **Big changes** (and hopefully final) to the `VectorDistribution`. Now the `VectorDistribution` only constructs the internal wrapped distributions when they are extracted or when a function, such as d/p/q/r, are called. This massively reduces a bottleneck in constructing the distribution. Additionally added functions for extracting distributions from inside the `VectorDistribution`. The only difference that should affect backwards compatibility is that the `distribution` argument must now be a character and not an object. Custom (i.e. non-`SDistribution`) distributions should be used in conjunction with the `distlist` initializer.
- Changed the lower bound of positive Sets to .Machine$double.xmin as the previous value of 1.1e-15 was too restrictive
- Added `skewness`, `kurtosis`, `entropy`, `mgf`, `cf`, and `pgf` to `WeightedDiscrete` and `Empirical`
- Added support for custom distributions in `VectorDistribution` and for `CoreStatistics` functions as well as support for only one arguments passed to d/p/q/r for fast comparisons between wrapped distributions

## Patches

- Bug fix in `WeightedDiscrete` distribution `variance` calculation
- Fixed bug in `Empirical` that was stopping the cdf of the first point in the distribution being evaluated 
- Fixed bug that allowed invalid parameter values to be set for non-reference parameters
- Updated parameter error messages to be more informative
- Improved speed and efficiency in `Distribution` constructor for wrappers

# distr6 1.2.0

## Minor Updates

### Added Functions and Classes

- Added non-central F, T, Chi-Squared, and Beta distributions
- Added WeightedDiscrete distribution. This allows users to supply a data.frame of samples and pdf/cdfs for more efficient usage with empirical discrete estimators (e.g. Kaplan-Meier)

### Updated Functions

- Added the common survival parameterisation of Weibull distribution
- Updated the listX functions to make them quicker and functional when distr6 is not attached
- Added `suppressMoments` argument to `Distribution` constructor to allow for faster construction

## Patches

- Added `errormsg` argument to assertions so a custom error message can be provided
- Fixed error in quantile function in `Empirical` that was causing results to be dropped
- Fixed bug in `TruncatedDistribution` that prevented multivariate distributions being truncated
- Fixed error in variance of `Empirical` that was calculating sample not population 
- Moved rare imports to suggests

# distr6 1.1.0

## Minor Updates

### Added Functions and Classes

- `Empirical` distribution for distributions arising from observed samples (e.g. from MC sampling methods)
- `simulateEmpiricalDistribution`: function for sampling *without* replacement from an `Empirical` distribution

### Deprecated Functions

- `ArrayDistribution` - This wrapper is now merged with `ProductDistribution` by adding two new parameters to `ProductDistribution` that allow for construction either by a distribution list of by naming a distribution and providing a parameter list or data.table

### Updated Functions

- Added quantile to Categorical distribution and updated its cdf efficiency
- `cdf` of discrete distributions evaluated between integers are now evaluated after rounding down and do not return 0, e.g. `Binomial$new()$cdf(1.8) == Binomial$new()$cdf(1)`
- Added two additional parameters to `ProductDistribution`: `params` and `distribution`, so that it can either function as before or as the `ArrayDistribution` wrapper which is now deprecated.
- Added two additional parameters to `VectorDistribution`: `params` and `distribution`, so that it can either function as before or as a generalisation to the `ArrayDistribution` wrapper which is now deprecated.
- Removed automated rounding of numerics when `ParameterSet` expected integer in `setParameterValue`
- Added parameter `n` to `strprint` and `print` to clean the print method for distributions (especially wrappers) with multiple parameters
- Added optional `vectordist` argument to `MixtureDistribution` for better compatibility with `VectorDistribution`
- Changed `short_name` of  Uniform, Logistic, Normal, and Triangular kernels to remove `Kern`. The `ClassName`s (and thus constructors) remain the same
- `listKernels` and `listSpecialSet` now return data.tables with `stringsAsFactors = FALSE`
- `Degenerate` distribution re-classified as discrete

## Patches

- Bug fix in quantile function of huberization wrapper
- Fixed the `rand` return of a Vector Distribution (transposed data.table and added column names)
- Fixed spelling mistakes in extension guidelines and removed calls to redundant parameters 
- Removed redundant `crayon` dependency
- Fixed bug in `MixtureDistribution` that was causing `rand` to erroneously return integers
- Removed error in `Categorical` documentation
- Fixed bug in multi-modal `Categorical` distribution
- `summary` and `print` now return `self` invisibly for better piping

# distr6 1.0.1

- Updated vignettes to remove redundant distr6 installation via GitHub that was causing a warning on Debian
- Redundant `stopwarn` argument removed from `parameters()` method in `Distribution` and `ParameterSet` classes
- Updated Kernel$new() error message to point user to Distribution class
- Fixed broken if/else statement in liesInSetInterval method of SetInterval class

# distr6 1.0.0

- v1 API is ready to be shipped to CRAN!
- Again [see here](https://alan-turing-institute.github.io/distr6/articles/webs/api_lifecycle.html) for the updated API lifecycle
- Tutorials, extension guidelines and other documentation (in appendices) are now on the [website](https://alan-turing-institute.github.io/distr6/index.html)
- distr6 is a complete, unified, object-oriented interface to probability distributions in R. Complete with 36 probability distributions and another 11 kernels, distr6 also allows functionality for numerical imputation of methods and statistical functions.

# distr6 0.1.0.9000

- Now public!
- distr6 API as whole is still very much experimental but analytic features are fairly stable, [see here](https://alan-turing-institute.github.io/distr6/articles/webs/api_lifecycle.html) for details on feature lifecycles
- Development continues to be internal, through UCL and The Alan Turing Institute, but we welcome external users to test the API and report bugs, see our [contributing guidelines](https://github.com/alan-turing-institute/distr6/blob/master/CONTRIBUTING.md) for details including our [code of conduct](https://github.com/alan-turing-institute/distr6/blob/master/CODE_OF_CONDUCT.md)
