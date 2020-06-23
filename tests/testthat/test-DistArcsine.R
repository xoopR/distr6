library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = Arcsine,
    pars = list(lower = 0, upper = 1),
    traits = list(
      valueSupport = "continuous",
      variateForm = "univariate",
      type = Reals$new()
    ),
    support = Interval$new(0, 1),
    symmetry = "symmetric",
    mean = 0.5,
    mode = 0:1,
    median = 0.5,
    variance = 1 / 8,
    skewness = 0,
    exkur = -1.5,
    entropy = log(pi / 4, 2),
    pgf = NaN,
    pdf = dbeta(1:3, 0.5, 0.5),
    cdf = pbeta(1:3, 0.5, 0.5),
    quantile = qbeta(c(0.24, 0.42, 0.5), 0.5, 0.5)
  )
})
