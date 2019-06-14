library(testthat)

context("Arcsine distribution")


test_that("properties & traits",{
  expect_equal(Arcsine$new()$valueSupport(), "continuous")
  expect_equal(Arcsine$new()$variateForm(), "univariate")
  expect_equal(Arcsine$new()$symmetry(), "symmetric")
  expect_equal(Arcsine$new()$sup(), 1)
  expect_equal(Arcsine$new()$inf(), 0)
})

a = Arcsine$new()
test_that("statistics",{
  expect_equal(a$mean(), 1/2)
  expect_equal(a$var(), 1/8)
  expect_equal(a$skewness(), 0)
  expect_equal(a$kurtosis(T), -1.5)
  expect_equal(a$kurtosis(F), 1.5)
  expect_equal(a$entropy(), log(pi/4,2))
  expect_null(a$mgf(0), NaN)
  expect_null(a$cf(1), NaN)
  expect_equal(a$mode(),c(0,1))
  expect_equal(a$mode(2),1)
  expect_equal(a$pdf(1), dbeta(1,0.5,0.5))
  expect_equal(a$cdf(1), pbeta(1,0.5,0.5))
  expect_equal(a$quantile(0.324), qbeta(0.324,0.5,0.5))
  expect_silent(a$rand(10))
  expect_equal(Arcsine$new(2,5)$pdf(3), 1/(pi*sqrt(2)))
  expect_equal(Arcsine$new(2,5)$cdf(3), asin(sqrt(1/3))*2/pi)
  expect_equal(Arcsine$new(2,5)$quantile(0.324), (3*(sin(0.324 * pi * 0.5)^2))+2)
  expect_silent(Arcsine$new(2,5)$rand(10))
})
