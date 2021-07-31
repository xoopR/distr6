library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = Exponential,
    pars = list(rate = 1),
    traits = list(
      valueSupport = "continuous",
      variateForm = "univariate",
      type = PosReals$new(zero = TRUE)
    ),
    support = PosReals$new(zero = TRUE),
    symmetry = "asymmetric",
    mean = 1,
    mode = 0,
    median = log(2),
    variance = 1,
    skewness = 2,
    exkur = 6,
    entropy = 1 - log(1, 2),
    mgf = NaN,
    cf = 1 / (1 - 1i),
    pgf = NaN,
    pdf = dexp(1:3),
    cdf = pexp(1:3),
    quantile = qexp(c(0.24, 0.42, 0.5))
  )
})
