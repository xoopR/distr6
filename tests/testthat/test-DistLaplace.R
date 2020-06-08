library(testthat)

context("Laplace distribution")

test_that("autotest", {
  autotest_sdistribution(
    sdist = Laplace,
    pars = list(),
    traits = list(
      valueSupport = "continuous",
      variateForm = "univariate",
      type = Reals$new()
    ),
    support = Reals$new(),
    symmetry = "symmetric",
    mean = 0,
    mode = 0,
    median = extraDistr::qlaplace(0.5),
    variance = 2,
    skewness = 0,
    exkur = 3,
    entropy = log(2 * exp(1), 2),
    mgf = NaN,
    cf = as.complex(0.5),
    pgf = NaN,
    pdf = extraDistr::dlaplace(1:3),
    cdf = extraDistr::plaplace(1:3),
    quantile = extraDistr::qlaplace(c(0.24, 0.42, 0.5))
  )
})
