library(testthat)

context("Gompertz distribution")

test_that("autottest",{
  autotest_sdistribution(Gompertz)
})

test_that("parameterisation",{
  expect_silent(Gompertz$new())
  expect_silent(Gompertz$new(shape = 2, scale = 3))
  expect_equal(Gompertz$new(shape = 2, scale = 3)$getParameterValue("scale"), 3)
  expect_equal(Gompertz$new(shape = 4)$getParameterValue("shape"), 4)
})

test_that("properties & traits",{
  expect_equal(Gompertz$new()$valueSupport, "continuous")
  expect_equal(Gompertz$new()$variateForm, "univariate")
  expect_equal(Gompertz$new()$symmetry, "asymmetric")
  expect_equal(Gompertz$new()$sup, Inf)
  expect_equal(Gompertz$new()$inf, 0)
  expect_equal(Gompertz$new()$dmax, Inf)
  expect_equal(Gompertz$new()$dmin, 0)
})

g = Gompertz$new()
test_that("statistics",{
  expect_error(g$mean())
  expect_error(g$variance())
  expect_error(g$skewness())
  expect_error(g$kurtosis())
  expect_error(g$entropy())
  expect_equal(g$pgf(1), NaN)
  expect_error(g$mgf())
  expect_error(g$cf())
  expect_error(g$mode())
  expect_equal(g$pdf(1), exp(1)^2*exp(-exp(1)))
  expect_equal(g$cdf(1), 1-exp(1-exp(1)))
  expect_equal(g$quantile(0.324), log(1 - log(0.676)))
  expect_equal(g$cdf(g$quantile(0.324)), 0.324)
  expect_silent(g$rand(10))
})
