library(testthat)

context("InverseGamma distribution")

test_that("parameterisation",{
  expect_silent(InverseGamma$new())
  expect_silent(InverseGamma$new(shape = 2, scale = 3))
  expect_equal(InverseGamma$new(shape = 2, scale = 3)$getParameterValue("scale"), 3)
  expect_equal(InverseGamma$new(shape = 4)$getParameterValue("shape"), 4)
})

test_that("properties & traits",{
  expect_equal(InverseGamma$new()$valueSupport(), "continuous")
  expect_equal(InverseGamma$new()$variateForm(), "univariate")
  expect_equal(InverseGamma$new()$symmetry(), "asymmetric")
  expect_equal(InverseGamma$new()$sup(), Inf)
  expect_equal(InverseGamma$new()$inf(), 0)
  expect_equal(InverseGamma$new()$dmax(), Inf)
  expect_equal(InverseGamma$new()$dmin(), .Machine$double.eps)
})

g = InverseGamma$new()
test_that("statistics",{
  expect_equal(g$mean(), NaN)
  expect_equal(InverseGamma$new(shape = 2)$mean(), 1)

  expect_equal(g$var(), NaN)
  expect_equal(InverseGamma$new(shape = 3)$var(), 0.25)

  expect_equal(g$skewness(), NaN)
  expect_equal(InverseGamma$new(shape = 4)$skewness(), 4*sqrt(2))

  expect_equal(g$kurtosis(), NaN)
  expect_equal(InverseGamma$new(shape = 5)$kurtosis(), 42)
  expect_equal(InverseGamma$new(shape = 5)$kurtosis(F), 45)

  expect_equal(g$entropy(), 1 - digamma(1)*2)

  expect_equal(g$mgf(1), NaN)
  expect_equal(g$cf(1), 2*(-1i * 1 * 1)^0.5 * Bessel::BesselK(sqrt(-4*1i),1))
  expect_equal(g$mode(), 1/2)
  expect_equal(g$pdf(1), extraDistr::dinvgamma(1,1,1))
  expect_equal(g$cdf(1), extraDistr::pinvgamma(1,1,1))
  expect_equal(g$quantile(0.324), extraDistr::qinvgamma(0.324,1,1))
  expect_equal(g$cdf(g$quantile(0.324)), 0.324)
  expect_equal(length(g$rand(10)),10)
})
