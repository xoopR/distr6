library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = BetaNoncentral,
    pars = list(shape1 = 1, shape2 = 1),
    traits = list(
      valueSupport = "continuous",
      variateForm = "univariate",
      type = PosReals$new(zero = TRUE)
    ),
    support = Interval$new(0, 1),
    symmetry = "symmetric",
    median = 0.5,
    pdf = dbeta(1:3, 1, 1),
    cdf = pbeta(1:3, 1, 1),
    quantile = qbeta(c(0.24, 0.42, 0.5), 1, 1)
  )
})

test_that("manual", {
  expect_equal(BetaNoncentral$new(shape1 = 1, shape2 = 2)$properties$symmetry, "asymmetric")
})
