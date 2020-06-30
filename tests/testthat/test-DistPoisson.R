library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = Poisson,
    pars = list(rate = 1),
    traits = list(
      valueSupport = "discrete",
      variateForm = "univariate",
      type = Naturals$new()
    ),
    support = Naturals$new(),
    symmetry = "asymmetric",
    mean = 1,
    mode = 1,
    median = 1,
    variance = 1,
    skewness = 1,
    exkur = 1,
    mgf = exp((exp(1) - 1)),
    cf = exp((exp(1i) - 1)),
    pgf = 1,
    pdf = dpois(1:3, 1),
    cdf = ppois(1:3, 1),
    quantile = qpois(c(0.24, 0.42, 0.5), 1)
  )
})
