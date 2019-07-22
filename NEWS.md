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
