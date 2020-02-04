library(testthat)

context("Uniform distribution")

test_that("constructor",{
  expect_silent(Uniform$new())
  expect_error(Uniform$new(lower = 5, upper = 3))
  expect_error(Uniform$new(lower = -Inf))
  expect_error(Uniform$new(upper = Inf))
  expect_silent(Uniform$new(lower = 2.3, upper = 3))
  expect_silent(Uniform$new(lower = 2, upper = 8))
})

test_that("parameters", {
  expect_equal(Uniform$new(lower = 2.3, upper = 3)$getParameterValue("lower"),2.3)
  expect_equal(Uniform$new(lower = 2, upper = 3)$getParameterValue("lower"),2)
  expect_equal(Uniform$new(lower = 2, upper = 3)$getParameterValue("upper"),3)
})

test_that("properties & traits",{
  expect_equal(Uniform$new()$valueSupport, "continuous")
  expect_equal(Uniform$new()$variateForm, "univariate")
  expect_equal(Uniform$new()$symmetry, "symmetric")
  expect_equal(Uniform$new()$sup, 1)
  expect_equal(Uniform$new()$inf, 0)
  expect_equal(Uniform$new()$dmax, 1)
  expect_equal(Uniform$new()$dmin, 0)
})

u = Uniform$new()
test_that("statistics",{
  expect_equal(u$mean(), 0.5)
  expect_equal(u$variance(), 1/12)
  expect_equal(u$skewness(), 0)
  expect_equal(u$kurtosis(T), -6/5)
  expect_equal(u$kurtosis(F), 1.8)
  expect_equal(u$entropy(), 0)
  expect_equal(u$mgf(0),  1)
  expect_equal(u$mgf(1),  exp(1) - 1)
  expect_equal(u$cf(0),  1)
  expect_equal(u$cf(1),  (exp(1i) - 1)/1i)
  expect_equal(u$mode(),NaN)
  expect_equal(u$pdf(1), dunif(1))
  expect_equal(u$cdf(1), punif(1))
  expect_equal(u$pgf(1), NaN)
  expect_equal(u$quantile(0.324), qunif(0.324))
  expect_equal(u$cdf(u$quantile(0.324)), 0.324)
  expect_silent(u$rand(10))
})

u = Uniform$new(lower=1,upper=5)
test_that("update parameters",{
  expect_error(u$setParameterValue(lst = list(upper = 0)))
  expect_silent(u$setParameterValue(lst = list(upper = 2)))
  expect_error(u$setParameterValue(lst = list(lower = 3)))
  expect_silent(u$setParameterValue(lst = list(lower = 1)))
  expect_silent(u$setParameterValue(lst = list(lower = 1, upper = 3)))
  expect_error(u$setParameterValue(lst = list(lower = 1, upper = 0)))
})
