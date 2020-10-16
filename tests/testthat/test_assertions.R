library(testthat)

context("assertions")

test_that("symmetry", {
  expect_true(testSymmetric(Normal$new()))
  expect_equal(checkSymmetric(Gamma$new()), "Gamma is not symmetric")
  expect_silent(assertSymmetric(Laplace$new()))
})
