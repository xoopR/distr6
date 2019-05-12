library(testthat)

context("Abstract decorator")

test_that("abstract decorator",{
  expect_error(DistributionDecorator$new(Binomial$new()))
  expect_error(DistributionDecorator$new())
})
