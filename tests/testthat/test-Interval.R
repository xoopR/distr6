library(testthat)

context("Interval")

test_that("initialize",{
  expect_silent(Interval$new())
  expect_silent(Interval$new(1,10))
  expect_error(Interval$new(10,-10))
  expect_silent(Interval$new(5,type="[)"))
})
