library(testthat)

context("Rayleigh distribution")


test_that("parameterisation",{
  expect_silent(Rayleigh$new())
  expect_silent(Rayleigh$new(mode = 10))
  expect_error(Rayleigh$new(mode = -1))
  expect_equal(Rayleigh$new(1.1)$getParameterValue("mode"), 1.1)
})

x = Rayleigh$new()
test_that("properties & traits",{
  expect_equal(x$valueSupport(), "continuous")
  expect_equal(x$variateForm(), "univariate")
  expect_equal(x$symmetry(), "asymmetric")
  expect_equal(x$sup(), Inf)
  expect_equal(x$inf(), 0)
  expect_equal(x$dmax(), Inf)
  expect_equal(x$dmin(), 0)
})

x = Rayleigh$new(2)
test_that("statistics",{
  expect_equal(x$mean(), sqrt(pi/2)*2)
  expect_equal(x$var(), 8-pi*2)
  expect_equal(round(x$skewness(), 5), 0.63111)
  expect_equal(round(x$kurtosis(T),5), 0.24509)
  expect_equal(round(x$kurtosis(F),5), 3.24509)
  expect_equal(round(x$entropy(),5), 1.78861)
  expect_error(x$mgf(0))
  expect_error(x$cf(1))
  expect_equal(x$mode(),2)
  expect_equal(x$pdf(1:2), extraDistr::drayleigh(1:2,2))
  expect_equal(x$cdf(1:2), extraDistr::prayleigh(1:2,2))
  expect_equal(x$quantile(c(0.2,0.42)), extraDistr::qrayleigh(c(0.2,0.42),2))
  expect_equal(x$cdf(x$quantile(0.56)), 0.56)
  expect_equal(length(x$rand(10)),10)
})
