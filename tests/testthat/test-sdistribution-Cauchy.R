library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = Cauchy,
    pars = list(location = 0, scale = 1),
    traits = list(
      valueSupport = "continuous",
      variateForm = "univariate",
      type = Reals$new()
    ),
    support = Reals$new(),
    symmetry = "symmetric",
    mean = NaN,
    mode = 0,
    median = NaN,
    variance = NaN,
    skewness = NaN,
    exkur = NaN,
    entropy = log(4 * pi, 2),
    mgf = NaN,
    cf = as.complex(exp(-1)),
    pgf = NaN,
    pdf = dcauchy(1:3),
    cdf = pcauchy(1:3),
    quantile = qcauchy(c(0.24, 0.42, 0.5))
  )
})
