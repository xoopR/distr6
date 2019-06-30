library(testthat)

context("LogLogistic distribution")

test_that("parameterisation",{
  expect_silent(LogLogistic$new())
  expect_silent(LogLogistic$new(shape = 2, scale = 3, location = 4))
  expect_error(LogLogistic$new(location = -2))
  expect_silent(LogLogistic$new(location = 0))
  expect_error(LogLogistic$new(shape = 0))
  expect_error(LogLogistic$new(scale = 0))
  expect_equal(LogLogistic$new(shape = 2, scale = 3)$getParameterValue("scale"), 3)
  expect_equal(LogLogistic$new(shape = 4)$getParameterValue("shape"), 4)
})

test_that("properties & traits",{
  expect_equal(LogLogistic$new()$valueSupport(), "continuous")
  expect_equal(LogLogistic$new()$variateForm(), "univariate")
  expect_equal(LogLogistic$new()$symmetry(), "asymmetric")
  expect_equal(LogLogistic$new()$sup(), Inf)
  expect_equal(LogLogistic$new()$inf(), 0)
  expect_equal(LogLogistic$new(location=2)$inf(), 2)
  expect_equal(LogLogistic$new()$dmax(), Inf)
  expect_equal(LogLogistic$new()$dmin(), .Machine$double.eps)
})

g = LogLogistic$new(location = 1, shape = 2, scale = 3)
test_that("statistics",{
  expect_equal(g$mean(),1 + (3*pi/2)/sin(pi/2))
  expect_equal(g$var(), NaN)
  expect_equal(round(LogLogistic$new(shape = 3, scale = 2)$var(), 5), 3.82494)
  expect_equal(g$skewness(), NaN)
  expect_equal(round(LogLogistic$new(shape = 4, scale = 2)$skewness(),5), 6.70886)
  expect_equal(g$kurtosis(), NaN)
  expect_equal(round(LogLogistic$new(shape = 5, scale = 2)$kurtosis(F),4), 15.0900)
  expect_equal(round(LogLogistic$new(shape = 5, scale = 2)$kurtosis(T),4), 12.0900)
  expect_equal(round(g$mode(),5), 2.73205)

  expect_error(g$entropy())
  expect_error(g$mgf())
  expect_error(g$cf())

  g = LogLogistic$new(location = 0, shape = 2, scale = 3)
  expect_equal(g$pdf(0:2), actuar::dllogis(0:2, shape = 2, scale = 3))
  expect_equal(g$cdf(0:2), actuar::pllogis(0:2, shape = 2, scale = 3))
  expect_equal(g$quantile(c(0.324,0.256)), actuar::qllogis(c(0.324,0.256), shape = 2, scale = 3))
  expect_equal(g$cdf(g$quantile(0.324)), 0.324)
  expect_equal(length(g$rand(1:10)),10)
})
