library(testthat)

context("Exponential distribution")

test_that("symmetry",{
  expect_equal(Exponential$new()$symmetry(), "asymmetric")
})

test_that("silent statistics",{
  expect_silent(Exponential$new()$kurtosis(T))
  expect_silent(Exponential$new()$kurtosis(F))
  expect_silent(Exponential$new()$expectation())
  expect_silent(Exponential$new()$entropy())
  expect_equal(Exponential$new()$mgf(1), 0)
  expect_equal(Exponential$new()$mgf(0.7), 1/0.3)
  expect_silent(Exponential$new()$cf(1))
  expect_silent(Exponential$new()$survival(1))
  expect_silent(Exponential$new()$hazard(1))
  expect_silent(Exponential$new()$cumHazard(1))
  expect_silent(Exponential$new()$pdf(1))
  expect_silent(Exponential$new()$cdf(1))
  expect_silent(Exponential$new()$quantile(1))
  expect_silent(Exponential$new()$rand(1))
  expect_equal(Exponential$new()$var(), 1)
  expect_equal(Exponential$new()$sd(), 1)
  expect_equal(Exponential$new()$expectation(), 1)
})

test_that("statistical results",{
  expect_equal(Exponential$new()$pdf(1), dexp(1))
  expect_equal(Exponential$new()$cdf(1), pexp(1))
  expect_equal(Exponential$new()$quantile(0.56), qexp(0.56))
})
