library(testthat)

context("Lognormal distribution")

test_that("constructor",{
  expect_silent(Lognormal$new())
  expect_silent(Lognormal$new(var = 1))
  expect_silent(Lognormal$new(sd = 1))
  expect_silent(Lognormal$new(prec = 1))
  expect_silent(Lognormal$new(var = 2, sd = 3))
  expect_silent(Lognormal$new(var = 2, prec = 3))
  expect_silent(Lognormal$new(prec = 2, sd = 3))
  expect_silent(Lognormal$new(var = 2, sd = 3, prec = 4))
  expect_silent(Lognormal$new(sdlog = 2, prec = 4, mean = 5))
})

test_that("parameterisation",{
  expect_equal(Lognormal$new(var = 2)$getParameterValue("var"), 2)
  expect_equal(Lognormal$new(preclog = 2)$getParameterValue("preclog"), 2)
  expect_equal(Lognormal$new(sd = 2)$getParameterValue("sd"), 2)

  expect_equal(Lognormal$new(meanlog = 2, mean = 2)$getParameterValue("mean"),2)


  expect_equal(Lognormal$new(var = 2)$getParameterValue("prec"), 2^-1)
  expect_equal(Lognormal$new(var = 2)$getParameterValue("var"), 2)

  expect_equal(Lognormal$new(sd = 2)$getParameterValue("sd"), 2)
  expect_equal(Lognormal$new(sd = 2)$getParameterValue("prec"), 2^-2)
  expect_equal(Lognormal$new(sd = 2)$getParameterValue("var"), 2^2)

  expect_equal(Lognormal$new(prec = 2)$getParameterValue("sd"), 2^-0.5)
  expect_equal(Lognormal$new(prec = 2)$getParameterValue("prec"), 2)
  expect_equal(Lognormal$new(prec = 2)$getParameterValue("var"), 2^-1)

  expect_equal(Lognormal$new(var = 2, sd = 3)$getParameterValue("sd"), 3)
  expect_equal(Lognormal$new(var = 2, prec = 3)$getParameterValue("var"), 1/3)
  expect_equal(Lognormal$new()$getParameterValue("var"), 1)
  expect_equal(Lognormal$new(sd = 1, prec = 2)$getParameterValue("sd"), sqrt(1/2))
  expect_equal(Lognormal$new(sd = 3, prec = 2)$getParameterValue("prec"), 2)
})

test_that("properties & traits",{
  expect_equal(Lognormal$new()$symmetry(), "symmetric")
  expect_equal(Lognormal$new()$inf(), -Inf)
  expect_equal(Lognormal$new()$sup(), Inf)
  expect_equal(Lognormal$new()$dmin(), -Inf)
  expect_equal(Lognormal$new()$dmax(), Inf)
  expect_equal(Lognormal$new()$valueSupport(), "continuous")
  expect_equal(Lognormal$new()$variateForm(), "univariate")
})

test_that("statistics",{
  expect_equal(Lognormal$new()$mean(), 0)
  expect_equal(Lognormal$new()$var(), 1)
  expect_equal(Lognormal$new()$skewness(), 0)
  expect_equal(Lognormal$new()$kurtosis(T), 0)
  expect_equal(Lognormal$new()$kurtosis(F), 3)
  expect_equal(Lognormal$new()$entropy(), 0.5 * log(2*pi*exp(1),base=2))
  expect_equal(Lognormal$new()$mgf(1), exp(0.5))
  expect_equal(Lognormal$new()$cf(1), as.complex(exp(- 0.5)))
  expect_equal(Lognormal$new()$mode(), 0)
  expect_equal(Lognormal$new()$pdf(2), dnorm(2))
  expect_equal(Lognormal$new()$cdf(2), pnorm(2))
  expect_equal(Lognormal$new()$quantile(0.46), qnorm(0.46))
  expect_silent(Lognormal$new()$rand(10))
})
