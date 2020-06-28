library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = Uniform,
    pars = list(lower = 0, upper = 1),
    traits = list(valueSupport = "continuous", variateForm = "univariate", type = Reals$new()),
    support = Interval$new(0, 1),
    symmetry = "symmetric",
    mean = 0.5,
    mode = NaN,
    median = 0.5,
    variance = 1 / 12,
    skewness = 0,
    exkur = -6 / 5,
    entropy = 0,
    mgf = exp(1) - 1,
    cf = (exp(1i) - 1) / 1i,
    pgf = NaN,
    pdf = dunif(1:3),
    cdf = punif(1:3),
    quantile = qunif(c(0.24, 0.42, 0.5))
  )
})

test_that("manual", {
  expect_equal(Uniform$new()$mgf(0), 1)
  expect_equal(Uniform$new()$cf(0), 1)
})
