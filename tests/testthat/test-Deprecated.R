library(testthat)

context("Deprecated")

test_that("Array Distribution",{
  expect_warning(ArrayDistribution$new())
  expect_warning(ArrayDistribution$new(sds))
})

