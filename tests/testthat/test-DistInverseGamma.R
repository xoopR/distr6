library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = InverseGamma,
    pars = list(shape = 1, scale = 1),
    traits = list(
      valueSupport = "continuous",
      variateForm = "univariate",
      type = PosReals$new()
    ),
    support = PosReals$new(),
    symmetry = "asymmetric",
    mean = NaN,
    mode = 0.5,
    median = extraDistr::qinvgamma(0.5, 1, 1),
    variance = NaN,
    skewness = NaN,
    exkur = NaN,
    entropy = 1 - digamma(1) * 2,
    mgf = NaN,
    pgf = NaN,
    pdf = extraDistr::dinvgamma(1:3, 1, 1),
    cdf = extraDistr::pinvgamma(1:3, 1, 1),
    quantile = extraDistr::qinvgamma(c(0.24, 0.42, 0.5), 1, 1)
  )
})
