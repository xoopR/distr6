library(testthat)

context("Huberization")

test_that("check continuous Truncated wrapper", {
  hubExp = HuberizedDistribution$new(Exponential$new(), lower = 2, upper = 5)
  expect_equal(hubExp$pdf(1), Exponential$new()$cdf(2))
  expect_equal(hubExp$pdf(2), Exponential$new()$cdf(2))
  expect_equal(hubExp$pdf(100), 1-Exponential$new()$cdf(5))
  expect_equal(hubExp$pdf(3), Exponential$new()$pdf(3))
})

test_that("check discrete Truncated wrapper", {
  hubBin = HuberizedDistribution$new(Binomial$new(), lower = 2, upper = 5)
  expect_equal(hubBin$pdf(1), Binomial$new()$cdf(2))
  expect_equal(hubBin$pdf(2), Binomial$new()$cdf(2))
  expect_equal(hubBin$pdf(9), 1-Binomial$new()$cdf(5))
  expect_equal(hubBin$pdf(3), Binomial$new()$pdf(3))
})

