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
