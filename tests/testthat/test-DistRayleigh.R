library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = Rayleigh,
    pars = list(mode = 2),
    traits = list(
      valueSupport = "continuous",
      variateForm = "univariate",
      type = PosReals$new(zero = TRUE)
    ),
    support = PosReals$new(zero = TRUE),
    symmetry = "asymmetric",
    mean = sqrt(pi / 2) * 2,
    mode = 2,
    median = 2 * sqrt(2 * log(2)),
    variance = 8 - pi * 2,
    skewness = 0.63111,
    exkur = 0.24509,
    entropy = 1.78861,
    pgf = NaN,
    pdf = extraDistr::drayleigh(1:3, 2),
    cdf = extraDistr::prayleigh(1:3, 2),
    quantile = extraDistr::qrayleigh(c(0.24, 0.42, 0.5), 2)
  )
})
