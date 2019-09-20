# distr6 1.2.0

## Major Updates

- None

## Minor Updates

### Added Functions and Classes

- Added non-central F, T, Chi-Squared, and Beta distributions

### Deprecated Functions

### Updated Functions

## Patches

- Added `errormsg` argument to assertions so a custom error message can be provided
- Fixed error in quantile function in `Empirical` that was causing results to be dropped
- Fixed bug in `TruncatedDistribution` that prevented multivariate distributions being truncated

# distr6 1.1.0

## Major Updates

- None

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
