library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = Lognormal,
    pars = list(meanlog = 0, varlog = 1),
    traits = list(
      valueSupport = "continuous",
      variateForm = "univariate",
      type = PosReals$new()
    ),
    support = PosReals$new(),
    symmetry = "asymmetric",
    mean = exp(1 / 2),
    mode = 0.3678794,
    median = 1,
    variance = (exp(1) - 1) * exp(1),
    skewness = (exp(1) + 2) * sqrt(exp(1) - 1),
    exkur = exp(4) + 2 * exp(3) + 3 * exp(2) - 6,
    entropy = log(exp(0.5) * sqrt(2 * pi), 2),
    mgf = NaN,
    pgf = NaN,
    pdf = dlnorm(1:3),
    cdf = plnorm(1:3),
    quantile = qlnorm(c(0.24, 0.42, 0.5))
  )
})
