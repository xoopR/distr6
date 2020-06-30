library(testthat)

context("kernel constructor")

test_that("kernel constructor error", {
  expect_error(Kernel$new())
})

test_that("decorators", {
  expect_equal(UniformKernel$new(decorators = "CoreStatistics")$decorators, "CoreStatistics")
})

test_that("rand", {
  expect_null(Epanechnikov$new()$rand(1))
})
