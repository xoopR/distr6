library(testthat)

context("Erlang distribution")

test_that("autotest", {
  autotest_sdistribution(
    sdist = Erlang,
    pars = list(),
    traits = list(
      valueSupport = "continuous",
      variateForm = "univariate",
      type = PosReals$new()
    ),
    support = PosReals$new(zero = TRUE),
    symmetry = "asymmetric",
    mean = 1,
    mode = 0,
    median = log(2),
    variance = 1,
    skewness = 2,
    exkur = 6,
    entropy = 1,
    mgf = NaN,
    cf = 1 / (1 - 1i),
    pgf = NaN,
    pdf = dgamma(1:3, shape = 1),
    cdf = pgamma(1:3, shape = 1),
    quantile = qgamma(c(0.24, 0.42, 0.5), shape = 1)
  )
})
