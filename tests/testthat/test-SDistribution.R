library(testthat)

context("SDistribution constructor")

test_that("SDistribution constructor error", {
  expect_error(SDistribution$new())
})
