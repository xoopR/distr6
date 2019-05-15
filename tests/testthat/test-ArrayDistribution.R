library(testthat)

context("Array Distributions")

test_that("constructor",{
  expect_silent(ArrayDistribution$new(Binomial, list(list(prob = 0.1, size = 2), list(prob = 0.75, size = 3))))
  expect_error(ArrayDistribution$new(Binomial, list(list(prob = 2, size = 2), list(prob = 0.75, size = 3))))
  expect_error(ArrayDistribution$new(Dist, list(list(prob = 0.1, size = 2), list(prob = 0.75, size = 3))))
})
