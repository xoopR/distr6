library(testthat)

context("kernel constructor")

test_that("kernel constructor error", {
  expect_error(Kernel$new())
})
