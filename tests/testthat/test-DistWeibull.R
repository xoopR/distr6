library(testthat)

test_that("autottest", {
  autotest_sdistribution(
    Weibull,
    pars = list(shape = 1, scale = 1),
    traits = list(valueSupport = "continuous", variateForm = "univariate",
                  type = PosReals$new(zero = TRUE)),
    support = PosReals$new(zero = TRUE),
    symmetry = "asymmetric",
    mean = 1,
    median = 0.6931,
    mode = 0,
    variance = 1,
    skewness = 2,
    exkur = 6,
    entropy = 1,
    pgf = NaN,
    pdf = dweibull(1:3, shape = 1, scale = 1),
    cdf = pweibull(1:3, shape = 1, scale = 1),
    quantile = qweibull(c(0.24, 0.42, 0.5), shape = 1, scale = 1)
  )
})
