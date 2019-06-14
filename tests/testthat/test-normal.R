library(testthat)

context("Normal distribution")

test_that("parameterisation",{
  expect_silent(Normal$new())
  expect_silent(Normal$new(var = 1))
  expect_silent(Normal$new(sd = 1))
  expect_silent(Normal$new(prec = 1))
  expect_silent(Normal$new(var = 2, sd = 3))
  expect_silent(Normal$new(var = 2, prec = 3))
  expect_silent(Normal$new(prec = 2, sd = 3))
  expect_silent(Normal$new(var = 2, sd = 3, prec = 4))

  expect_equal(Normal$new(var = 2)$getParameterValue("sd"), 2^0.5)
  expect_equal(Normal$new(var = 2)$getParameterValue("prec"), 2^-1)
  expect_equal(Normal$new(var = 2)$getParameterValue("var"), 2)

  expect_equal(Normal$new(sd = 2)$getParameterValue("sd"), 2)
  expect_equal(Normal$new(sd = 2)$getParameterValue("prec"), 2^-2)
  expect_equal(Normal$new(sd = 2)$getParameterValue("var"), 2^2)

  expect_equal(Normal$new(prec = 2)$getParameterValue("sd"), 2^-0.5)
  expect_equal(Normal$new(prec = 2)$getParameterValue("prec"), 2)
  expect_equal(Normal$new(prec = 2)$getParameterValue("var"), 2^-1)

  expect_equal(Normal$new(var = 2, sd = 3)$getParameterValue("sd"), 3)
  expect_equal(Normal$new(var = 2, prec = 3)$getParameterValue("var"), 1/3)
  expect_equal(Normal$new()$getParameterValue("var"), 1)
  expect_equal(Normal$new(sd = 1, prec = 2)$getParameterValue("sd"), 1)
  expect_equal(Normal$new(sd = 3, prec = 2)$getParameterValue("prec"), 1/9)
})

test_that("symmetry",{
  expect_equal(Normal$new()$symmetry(), "symmetric")
})

test_that("silent statistics",{
  expect_silent(Normal$new(var=1)$kurtosis(T))
  expect_silent(Normal$new(var=1)$kurtosis(F))
  expect_silent(Normal$new(var=1)$mean())
  expect_silent(Normal$new(var=1)$entropy())
  expect_silent(Normal$new(var=1)$mgf(1))
  expect_silent(Normal$new(var=1)$cf(1))
  expect_silent(Normal$new(var=1)$pdf(1))
  expect_silent(Normal$new(var=1)$cdf(1))
  expect_silent(Normal$new(var=1)$quantile(1))
  expect_silent(Normal$new(var=1)$rand(1))
  expect_equal(Normal$new(var = 3)$var(), 3)
  expect_equal(Normal$new(sd = 2)$sd(), 2)
  expect_equal(Normal$new(mean = 4)$mean(), 4)
  expect_equal(Normal$new(var=1)$mode(), 0)
})

test_that("statistical results",{
  expect_equal(Normal$new()$pdf(1), dnorm(1))
  expect_equal(Normal$new()$cdf(1), pnorm(1))
  expect_equal(Normal$new()$quantile(0.56), qnorm(0.56))
})
