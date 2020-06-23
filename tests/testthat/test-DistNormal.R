library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = Normal,
    pars = list(mean = 0, var = 1),
    traits = list(
      valueSupport = "continuous",
      variateForm = "univariate",
      type = Reals$new()
    ),
    support = Reals$new(),
    symmetry = "symmetric",
    mean = 0,
    mode = 0,
    median = 0,
    variance = 1,
    skewness = 0,
    exkur = 0,
    entropy = 0.5 * log(2 * pi * exp(1), base = 2),
    mgf = exp(0.5),
    cf = as.complex(exp(-0.5)),
    pgf = NaN,
    pdf = dnorm(1:3),
    cdf = pnorm(1:3),
    quantile = qnorm(c(0.24, 0.42, 0.5))
  )
})
