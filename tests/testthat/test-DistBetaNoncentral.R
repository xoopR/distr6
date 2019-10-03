library(testthat)

context("Noncentral Beta distribution")

test_that("parameterisation",{
  expect_silent(BetaNoncentral$new())
  expect_silent(BetaNoncentral$new(shape1 = 1, shape2 = 1, location = 1))
  expect_equal(BetaNoncentral$new(shape2 = 2)$getParameterValue("shape2"), 2)
  expect_equal(BetaNoncentral$new()$getParameterValue("shape1"), 1)
  expect_equal(BetaNoncentral$new(location = 3)$getParameterValue("location"), 3)
  expect_error(BetaNoncentral$new(location = -2))
})

test_that("properties & traits",{
  expect_equal(BetaNoncentral$new()$valueSupport(), "continuous")
  expect_equal(BetaNoncentral$new()$variateForm(), "univariate")
  expect_equal(BetaNoncentral$new()$symmetry(), "symmetric")
  expect_equal(BetaNoncentral$new(shape2=2)$symmetry(), "asymmetric")
  expect_equal(BetaNoncentral$new()$sup(), 1)
  expect_equal(BetaNoncentral$new()$inf(), 0)
  expect_equal(BetaNoncentral$new()$dmax(), 1)
  expect_equal(BetaNoncentral$new()$dmin(), 0)
})


B = BetaNoncentral$new(location = 2)
test_that("statistics",{
  expect_error(B$mean())
  expect_error(B$variance())
  expect_error(B$skewness())
  expect_error(B$kurtosis())
  expect_error(B$entropy())
  expect_error(B$mgf())
  expect_error(B$cf())
  expect_error(B$mode())
  expect_equal(B$pdf(1), dbeta(1, 1, 1, 2))
  expect_equal(B$cdf(1), pbeta(1, 1, 1, 2))
  expect_equal(B$quantile(0.2), qbeta(0.2, 1, 1, 2))
  expect_equal(B$cdf(B$quantile(0.324)), 0.324)
  expect_silent(B$rand(10))
})
