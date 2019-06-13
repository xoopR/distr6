library(testthat)

context("Exponential distribution")

test_that("parameterisation",{
  expect_message(Exponential$new())
  expect_message(Exponential$new(rate = 2, scale = 3))
  expect_equal(Exponential$new(rate = 2, scale = 3)$getParameterValue("rate"), 2)
  expect_equal(Exponential$new(rate = 2, scale = 3)$getParameterValue("scale"), 1/2)
  expect_silent(Exponential$new(scale = 3))
  expect_silent(Exponential$new(rate = 2))
  expect_equal(Exponential$new(rate = 2)$getParameterValue("rate"), 2)
  expect_equal(Exponential$new(rate = 2)$getParameterValue("scale"), 1/2)
  expect_equal(Exponential$new(scale = 2)$getParameterValue("rate"), 1/2)
  expect_equal(Exponential$new(scale = 2)$getParameterValue("scale"), 2)
})

test_that("symmetry",{
  expect_equal(Exponential$new()$symmetry(), "asymmetric")
})

test_that("silent statistics",{
  expect_silent(Exponential$new(rate = 1)$kurtosis(T))
  expect_silent(Exponential$new(rate = 1)$kurtosis(F))
  expect_equal(Exponential$new(rate = 1)$mean(), 1)
  expect_equal(Exponential$new(rate = 1)$mode(), 0)
  expect_silent(Exponential$new(rate = 1)$entropy())
  expect_equal(Exponential$new(rate = 1)$mgf(1), 0)
  expect_equal(Exponential$new(rate = 1)$mgf(0.7), 1/0.3)
  expect_silent(Exponential$new(rate = 1)$cf(1))
  expect_silent(Exponential$new(rate = 1)$pdf(1))
  expect_silent(Exponential$new(rate = 1)$cdf(1))
  expect_silent(Exponential$new(rate = 1)$quantile(1))
  expect_silent(Exponential$new(rate = 1)$rand(1))
  expect_equal(Exponential$new(rate = 1)$var(), 1)
  expect_equal(Exponential$new(rate = 1)$sd(), 1)
  expect_equal(Exponential$new(rate = 1)$mean(), 1)
})

test_that("statistical results",{
  expect_equal(Exponential$new()$pdf(1), dexp(1))
  expect_equal(Exponential$new()$cdf(1), pexp(1))
  expect_equal(Exponential$new()$quantile(0.56), qexp(0.56))
})
