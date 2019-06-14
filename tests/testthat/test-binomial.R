library(testthat)

context("Binomial distribution")

test_that("symmetry",{
  expect_equal(Binomial$new()$symmetry(), "symmetric")
  expect_equal(Binomial$new(prob = 0.1)$symmetry(), "asymmetric")
})

test_that("silent statistics",{
  expect_silent(Binomial$new()$kurtosis(T))
  expect_silent(Binomial$new()$kurtosis(F))
  expect_silent(Binomial$new()$mean())
  expect_silent(Binomial$new()$entropy())
  expect_silent(Binomial$new()$mgf(1))
  expect_silent(Binomial$new()$cf(1))
  expect_silent(Binomial$new()$pgf(1))
  expect_silent(Binomial$new()$pdf(1))
  expect_silent(Binomial$new()$cdf(1))
  expect_silent(Binomial$new()$quantile(1))
  expect_silent(Binomial$new()$rand(1))
})

test_that("statistical results",{
  expect_equal(Binomial$new()$pdf(1), dbinom(1,size=10,0.5))
  expect_equal(Binomial$new()$cdf(1), pbinom(1,size=10,0.5))
  expect_equal(Binomial$new()$quantile(0.56), qbinom(0.56,size=10,0.5))
})
