library(testthat)

context("Huberization")

test_that("check continuous Huberized wrapper", {
  hubExp = HuberizedDistribution$new(Exponential$new(), lower = 2, upper = 5)
  expect_equal(hubExp$cdf(1), 0)
  expect_equal(hubExp$cdf(2), Exponential$new()$cdf(2))
  expect_equal(hubExp$cdf(100), 1)
  expect_equal(hubExp$cdf(5), 1)
})

test_that("check discrete Huberized wrapper", {
  hubBin = HuberizedDistribution$new(Binomial$new(), lower = 2, upper = 5)
  expect_equal(hubBin$pdf(1), 0)
  expect_equal(hubBin$pdf(2), Binomial$new()$cdf(2))
  expect_equal(hubBin$pdf(5), Binomial$new()$cdf(5, lower.tail=F)+Binomial$new()$pdf(5))
  expect_equal(hubBin$pdf(3), Binomial$new()$pdf(3))
})

test_that("check huberization constructor",{
  expect_silent(huberize(Binomial$new(),lower = 1, upper = 5))
  expect_silent(huberize.Distribution(Binomial$new(), upper = 5))
  expect_silent(huberize(Binomial$new(),lower = 1))
  expect_silent(huberize(Binomial$new()))
  expect_error(huberize(Distribution$new("Test"),1,2))
})

