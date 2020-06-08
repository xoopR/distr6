library(testthat)

context("Lies In")

test_that("liesInType", {
  expect_true(Wald$new()$liesInType(c(0, 1), all = TRUE, bound = TRUE))
  expect_false(Wald$new()$liesInType(c(0, 1), all = TRUE, bound = FALSE))
  expect_equal(Wald$new()$liesInType(c(0, 1), all = FALSE, bound = TRUE), c(TRUE, TRUE))
  expect_equal(Wald$new()$liesInType(c(0, 1), all = FALSE, bound = FALSE), c(FALSE, TRUE))
})

test_that("liesInSupport", {
  expect_false(Binomial$new()$liesInSupport(c(10, 11), all = TRUE, bound = TRUE))
  expect_false(Binomial$new()$liesInSupport(c(10, 11), all = TRUE, bound = FALSE))
  expect_true(Binomial$new()$liesInSupport(c(9, 10), all = TRUE, bound = TRUE))
  expect_true(Binomial$new()$liesInSupport(c(9, 10), all = TRUE, bound = FALSE))
  expect_equal(Binomial$new()$liesInSupport(c(9, 10), all = FALSE), c(TRUE, TRUE))
  expect_equal(Binomial$new()$liesInSupport(c(10, 11), all = FALSE), c(TRUE, FALSE))
})
