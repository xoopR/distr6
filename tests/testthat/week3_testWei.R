library(testthat)

context("Weibull distribution")

test_that("constructor",{
  expect_silent(Weibull$new())
  expect_silent(Weibull$new(shape = 2))
  expect_silent(Weibull$new(scale = 2))
  expect_silent(Weibull$new(shape=2.5,scale=1))
})

test_that("properties & traits",{
  expect_equal(Weibull$new()$symmetry(), "asymmetric")
  expect_equal(Weibull$new()$inf(), 0)
  expect_equal(Weibull$new()$sup(), Inf)
  expect_equal(Weibull$new()$dmin(), 0)
  expect_equal(Weibull$new()$dmax(), Inf)
  expect_equal(Weibull$new()$valueSupport(), "continuous")
  expect_equal(Weibull$new()$variateForm(), "univariate")
})

test_that("statistics",{
  expect_equal(Weibull$new()$mean(), 1)
  expect_equal(Weibull$new()$var(), 1)
  expect_equal(Weibull$new()$skewness(), 2)
  expect_equal(Weibull$new()$kurtosis(T), 6)
  expect_equal(Weibull$new()$kurtosis(F), 9)
  expect_equal(Weibull$new()$entropy(), 1)
  expect_error(Weibull$new()$mgf(1))
  expect_cf(Weibull$new()$cf(1), as.complex(exp(- 0.5)))
  expect_equal(Weibull$new()$mode(), 0)
  expect_equal(Weibull$new(shape=0.5)$mode(), 0)
  expect_equal(Weibull$new(shape=2)$mode(), (0.5)^(1/2))
  expect_equal(Weibull$new()$pdf(2), dweibull(2))
  expect_equal(Weibull$new()$cdf(2), pweibull(2))
  expect_equal(Weibull$new()$quantile(0.46), qweibull(0.46))
  expect_silent(Weibull$new()$rand(10))
})