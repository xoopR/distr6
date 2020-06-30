library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = Beta,
    pars = list(shape1 = 1, shape2 = 1),
    traits = list(
      valueSupport = "continuous",
      variateForm = "univariate",
      type = PosReals$new(zero = TRUE)
    ),
    support = Interval$new(0, 1),
    symmetry = "symmetric",
    mean = 0.5,
    mode = NaN,
    median = 0.5,
    variance = 1 / 12,
    skewness = 0,
    exkur = -1.2,
    entropy = 0,
    pgf = NaN,
    pdf = dbeta(1:3, 1, 1),
    cdf = pbeta(1:3, 1, 1),
    quantile = qbeta(c(0.24, 0.42, 0.5), 1, 1)
  )
})

test_that("manual", {
  expect_equal(Beta$new(1, 2)$mode(), 0)
  expect_equal(Beta$new(2, 1)$mode(), 1)
  expect_equal(Beta$new(0.5, 0.5)$mode(), c(0, 1))
  expect_equal(Beta$new(0.5, 0.5)$mode(1), 0)
  expect_equal(Beta$new(2, 2)$mode(1), 0.5)
  expect_false(testSymmetric(Beta$new()$setParameterValue(shape1 = 1, shape2 = 2)))
})

test_that("vector", {
  d <- VectorDistribution$new(distribution = "Beta",
                              params = data.frame(shape1 = 1:2))
  expect_error(d$mode(), "cannot be")
})
