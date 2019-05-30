library(testthat)

context("Mixture")

test_that("check continuous Mixture wrapper", {
  mixExp = MixtureDistribution$new(list(Exponential$new(),Exponential$new()))
  expect_equal(mixExp$pdf(1:10), Exponential$new()$pdf(1:10))
  expect_equal(mixExp$cdf(1:10), Exponential$new()$cdf(1:10))
})

test_that("check discrete mixture wrapper", {
  mixBin = MixtureDistribution$new(list(Binomial$new(),Binomial$new()))
  expect_equal(mixBin$pdf(1:10), Binomial$new()$pdf(1:10))
  expect_equal(mixBin$cdf(1:10), Binomial$new()$cdf(1:10))
})

test_that("check weights",{
  expect_equal(MixtureDistribution$new(list(Binomial$new(),Binomial$new()))$weights(),c(0.5,0.5))
  expect_equal(MixtureDistribution$new(list(Binomial$new(),Binomial$new()), weights = c(0.1,0.9))$weights(),
               c(0.1,0.9))
  expect_error(MixtureDistribution$new(list(Binomial$new(),Binomial$new()), weights = c(0.1,0.2)))
  expect_error(MixtureDistribution$new(list(Binomial$new(),Binomial$new()), weights = c(0.2)))
})
